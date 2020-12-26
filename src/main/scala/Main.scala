import org.jsoup.Jsoup
import scalaj.http.Http

import collection.JavaConverters._
import scala.sys.process.Process
import spray.json._
import DefaultJsonProtocol._

import java.net.URLEncoder

case class Cash(free: BigDecimal, cash: BigDecimal, commission: BigDecimal, dividend: BigDecimal, indicator: Int, interest: BigDecimal, margin: BigDecimal, nonRefundable: BigDecimal,  ppl: BigDecimal, result: BigDecimal, total: BigDecimal)
case class Position(
                   averagePrice: BigDecimal,
                   code: String,
                   created: String,
                   currentPrice: BigDecimal,
                   investment: BigDecimal,
                   margin: BigDecimal,
                   positionId: String,
                   ppl: BigDecimal,
                   quantity: Int,
                   value: BigDecimal,
                   swap: BigDecimal
                   )
case class Account(cash: Cash, positions: Option[List[Position]])
case class Pricing(open: Double, high: Double, low: Double, close: Double)
case class Candle(timestamp: Option[Int], birthTime: Option[Int], bid: Pricing, ask: Pricing, volume: Int) {
  def time: Int = timestamp.getOrElse(birthTime.getOrElse(0))
}
case class Request(instCode: String, periodType: String)
case class Candles(request: Request, candles: List[Candle])


object Main extends App {
  implicit val CashFormat = jsonFormat11(Cash)
  implicit val PricingFormat = jsonFormat4(Pricing)
  implicit val CandleFormat = jsonFormat5(Candle)
  implicit val RequestFormat = jsonFormat2(Request)
  implicit val CandlesFormat = jsonFormat2(Candles)
  implicit val PositionFormat = jsonFormat11(Position)
  implicit val AccountFormat = jsonFormat2(Account)

  val environment = "demo"
  val email = ""
  val plainPassword = ""

  val loginPage = Http("https://www.trading212.com/en/login").asString
  val PHPSESSID = loginPage.cookies.find(_.getName.contains("PHPSESSID")).get.getValue

  val doc = Jsoup.parse(loginPage.body)
  val token = doc.select("[name='login[_token]']").asScala.head.attr("value")
  println(token)

  val encodedEmail = URLEncoder.encode(email, "UTF-8")
  val encodedPass = URLEncoder.encode(plainPassword, "UTF-8")
  val authenticate = Process(s"curl 'https://www.trading212.com/en/authenticate'  -H 'Content-Type: application/x-www-form-urlencoded; charset=UTF-8'    -H 'Cookie: PHPSESSID=$PHPSESSID;  cookie_warning=isSet;'   --data-raw 'login%5Busername%5D=$encodedEmail&login%5Bpassword%5D=$encodedPass&login%5BrememberMe%5D=1&login%5B_token%5D=$token'   --compressed").!!
    .parseJson
  val customerSessionCookie = authenticate.asJsObject.fields("data").asJsObject().fields("parameters").asJsObject().fields("customerSessionCookie").asInstanceOf[JsString].value


  val response3 = Http(s"https://$environment.trading212.com")
    .cookie("CUSTOMER_SESSION", customerSessionCookie)
    .asString

  val accountId = response3.body.split("accountId: ").tail.head.split(",").head
  val tradingSession = response3.cookies.find(_.getName.contains("TRADING212_SESSION")).get

  val JSESSIONID = response3.cookies.find(_.getName == "JSESSIONID").get

  val candlesOutput = Http(s"https://$environment.trading212.com/charting/rest/v2/candles")
    .cookie(tradingSession)
    .cookie(JSESSIONID)
    .header("Content-Type", "application/json")
    .header(s"X-Trader-Client", s"application=WC4, version=5.102.5, accountId=$accountId, dUUID=asdfsa")
    .postData("[{\"limit\":500,\"instCode\":\"USDJPY\",\"periodType\":\"FIVE_MINUTES\",\"withFakes\":false}]")
    .asString.body.parseJson

  val candles = candlesOutput.asInstanceOf[JsArray].elements.head.convertTo[Candles]

  val batch = Http(s"https://$environment.trading212.com/charting/rest/batch")
    .cookie(tradingSession)
    .cookie(JSESSIONID)
    .header("Content-Type", "application/json")
    .header(s"X-Trader-Client", s"application=WC4, version=5.102.5, accountId=$accountId, dUUID=asdfsa")
    .postData("{\"candles\":[{\"instCode\":\"AUDNZD\",\"periodType\":\"THIRTY_MINUTES\",\"limit\":49,\"withFakes\":false},{\"instCode\":\"EURCAD\",\"periodType\":\"THIRTY_MINUTES\",\"limit\":49,\"withFakes\":false},{\"instCode\":\"USDJPY\",\"periodType\":\"THIRTY_MINUTES\",\"limit\":49,\"withFakes\":false},{\"instCode\":\"CNHJPY\",\"periodType\":\"THIRTY_MINUTES\",\"limit\":49,\"withFakes\":false},{\"instCode\":\"AUDZAR\",\"periodType\":\"THIRTY_MINUTES\",\"limit\":49,\"withFakes\":false},{\"instCode\":\"GBPAUD\",\"periodType\":\"THIRTY_MINUTES\",\"limit\":49,\"withFakes\":false},{\"instCode\":\"EURUSD\",\"periodType\":\"THIRTY_MINUTES\",\"limit\":49,\"withFakes\":false},{\"instCode\":\"BMW\",\"periodType\":\"TEN_MINUTES\",\"limit\":145,\"withFakes\":false},{\"instCode\":\"FB\",\"periodType\":\"TEN_MINUTES\",\"limit\":145,\"withFakes\":false},{\"instCode\":\"GOOG\",\"periodType\":\"TEN_MINUTES\",\"limit\":145,\"withFakes\":false},{\"instCode\":\"#CTOCT20\",\"periodType\":\"THIRTY_MINUTES\",\"limit\":49,\"withFakes\":false}],\"ticks\":[]}")
    .asString.body.parseJson

  println(batch)

  val batchCandles = batch.asJsObject.fields("candles").asInstanceOf[JsArray].convertTo[List[Candles]]

  val instruments = Http(s"https://$environment.trading212.com/rest/v1/instruments/USDJPY")
    .cookie(tradingSession)
    .cookie(JSESSIONID)
    .header("Content-Type", "application/json")
    .header(s"X-Trader-Client", s"application=WC4, version=5.102.5, accountId=$accountId, dUUID=asdfsa")
    .asString

  println(instruments)

  val tradingAdditionalInfo = Http(s"https://$environment.trading212.com/rest/v1/tradingAdditionalInfo?instrumentCode=EURUSD&positionId=null&quantity=1500")
    .cookie(tradingSession)
    .cookie(JSESSIONID)
    .header("Content-Type", "application/json")
    .header(s"X-Trader-Client", s"application=WC4, version=5.102.5, accountId=$accountId, dUUID=asdfsa")
   .asString

  // + = buy    - = sell
  val openPosition = Http(s"https://$environment.trading212.com/rest/v2/trading/open-positions")
    .cookie(tradingSession)
    .cookie(JSESSIONID)
    .header("Content-Type", "application/json")
    .header(s"X-Trader-Client", s"application=WC4, version=5.102.5, accountId=$accountId, dUUID=asdfsa")
    .postData("{\"notify\":\"NONE\",\"targetPrice\":1.17475,\"limitDistance\":0.01175,\"stopDistance\":0.01175,\"quantity\":1500,\"instrumentCode\":\"EURUSD\"}")
    //.asString

  println(openPosition)

  val closePosition = Http(s"https://$environment.trading212.com/rest/v2/trading/open-positions/close/f73bbe3f-23d1-4b96-bbab-89cd91ae02f7")
    .cookie(tradingSession)
    .cookie(JSESSIONID)
    .header("Content-Type", "application/json")
    .header(s"X-Trader-Client", s"application=WC4, version=5.102.5, accountId=$accountId, dUUID=asdfsa")
    .postData("{\"targetPrice\":null}")
    .method("DELETE")
    //.asString

  println(closePosition)

  val stats = Http(s"https://$environment.trading212.com/rest/v1/customer/accounts/stats")
    .cookie("CUSTOMER_SESSION", customerSessionCookie)
    .asString.body.parseJson.asJsObject.fields(accountId).asJsObject.fields

  val account = stats("account").convertTo[Account]

  println(account)

  val settings = Http(s"https://$environment.trading212.com/rest/v2/account/instruments/settings")
    .cookie(tradingSession)
    .cookie(JSESSIONID)
    .header("Content-Type", "application/json")
    .header(s"X-Trader-Client", s"application=WC4, version=5.102.5, accountId=$accountId, dUUID=asdfsa")
    .postData("[\"EURCAD\",\"EURCHF\",\"USDJPY\",\"EURGBP\",\"AUDUSD\",\"CADJPY\",\"AUDNZD\",\"AUDJPY\",\"EURTRY\",\"USDNOK\",\"GBPUSD\",\"GBPCHF\",\"EURUSD\",\"GBPJPY\",\"NZDUSD\",\"EURJPY\",\"USDCHF\",\"USDCAD\",\"EURNZD\",\"EURNOK\",\"USDTRY\"]")
   .asString

  println(settings)

}

