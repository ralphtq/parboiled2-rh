/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2.examples

import org.parboiled2._
import org.parboiled2.support.hlist.{HList, HNil}

case class DbTable(name: String, columns: Seq[String])

/* Often times parsing a document correctly means discarding slices of input along the way, i.e. code comments.
 *
 * This example illustrates how to create a parser that extracts specific slices from a document while discarding 
 * extraneous slices.
 */

case class SimpleSqlParser(input: ParserInput, columnStart: String) extends Parser with StringBuilding {
//  import CharPredicate.{Digit, Digit19, HexDigit}
  import SimpleSqlParser._

  def DDL: Rule[HNil , HList] = rule {
    zeroOrMore(Statements)
  }
  def Statements    =  rule { Ignore ~ Table }
  def Table         = rule { TableFlag } // ~ TableName ~ Ignore ~ Arguments ~> DbTable }
//  def TableName     = rule { capture(oneOrMore(!EndName ~ ANY)) ~ EndName}
//  def Arguments     = rule { zeroOrMore(Arg).separatedBy(Ignore) }
//  def Arg           = rule { columnStart ~ capture(oneOrMore(!Space ~ ANY)) ~ Space}
  def TableFlag     = rule { CreateTable ~ Space }
//  def EndName       = rule { Space | "(" }
  def Ignore        = rule { (! (CreateTable | columnStart)  ~ ANY).+ }
}

object SimpleSqlParser{
  val WhiteSpaceChar      = CharPredicate(" \n\r\t\f")
//  val NewLine = "\r"
  val Comma = ","
  val Space = " "
  val CreateTable = "table"


}
