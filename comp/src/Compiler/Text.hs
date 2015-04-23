{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.Text
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.Text where

import           Control.Error
import qualified Data.CaseInsensitive  as CI
import           Data.Char
import           Data.Foldable         as Fold
import qualified Data.HashSet          as Set
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.ICU         (Regex)
import           Data.Text.ICU.Replace (Replace)
import qualified Data.Text.ICU.Replace as RE
import           Data.Text.Manipulate
import           Text.Parsec.Language  (haskellDef)
import           Text.Parsec.Token     (reservedNames)

asText :: (Text -> Text) -> String -> String
asText f = Text.unpack . f . Text.pack

dropLower :: Text -> Text
dropLower = Text.dropWhile (not . isUpper)

safeHead :: Text -> Maybe Text
safeHead = fmap (Text.singleton . fst) . Text.uncons

stripLens :: Text -> Text
stripLens t
    | "_" `Text.isPrefixOf` t = lowerHead (dropLower t)
    | otherwise               = t

-- stripPrime :: Text -> Text
-- stripPrime = stripSuffix "'"

stripPrefix :: Text -> Text -> Text
stripPrefix p t = Text.strip . fromMaybe t $ p `Text.stripPrefix` t

stripSuffix :: Text -> Text -> Text
stripSuffix p t = Text.strip . fromMaybe t $ p `Text.stripSuffix` t

-- renameService :: Text -> Text -> Text
-- renameService abbrev full = RE.replaceAll re mempty
--   where
--     n | Text.null abbrev = full
--       | otherwise        = abbrev

--     re = "^Amazon|AWS\s*|\(.*|\s+|\W+"

renameReserved :: Text -> Text
renameReserved x
    | CI.mk x `Set.member` xs = x <> "'"
    | otherwise               = x
  where
    xs = Set.fromList
       . map (CI.mk . Text.pack)
       $ [ "head"
         , "tail"
         , "delete"
         , "filter"
         , "true"
         , "false"
         , "map"
         , "mape"
         , "object"
         , "list"
         , "list1"
         ] ++ reservedNames haskellDef

renameCtor :: Text -> Text
renameCtor = stripSuffix "_"
    . renameAcronym
    . Text.concat
    . map recase
    . splitWords
  where
    recase x
        | Text.null x           = x
        | isDigit (Text.last x) = Text.toUpper x `Text.snoc` '_'
        | isDigit (Text.head x) = upper `Text.snoc` '_'
        | otherwise             = upperHead x
      where
        upper = case Text.uncons x of
            Nothing      -> x
            Just (c, cs) -> c `Text.cons` upperHead cs

renameAcronym :: Text -> Text
renameAcronym x = Fold.foldl' (flip (uncurry RE.replaceAll)) x xs
  where
    xs :: [(Regex, Replace)]
    xs = [ ("Acl",           "ACL")
         , ("Adm([^i]|$)",   "ADM$1")
         , ("Aes",           "AES")
         , ("Api",           "API")
         , ("Ami",           "AMI")
         , ("Apns",          "APNS")
         , ("Arn",           "ARN")
         , ("Asn",           "ASN")
         , ("Aws",           "AWS")
         , ("Bcc([A-Z])",    "BCC$1")
         , ("Bgp",           "BGP")
         , ("Cc([A-Z])",     "CC$1")
         , ("Cidr",          "CIDR")
         , ("Cors",          "CORS")
         , ("Csv",           "CSV")
         , ("Cpu",           "CPU")
         , ("Db",            "DB")
         , ("Dhcp",          "DHCP")
         , ("Dns",           "DNS")
         , ("Ebs",           "EBS")
         , ("Ec2",           "EC2")
         , ("Eip",           "EIP")
         , ("Gcm",           "GCM")
         , ("Html",          "HTML")
         , ("Https",         "HTTPS")
         , ("Http([^s]|$)",  "HTTP$1")
         , ("Hsm",           "HSM")
         , ("Hvm",           "HVM")
         , ("Iam",           "IAM")
         , ("Icmp",          "ICMP")
         , ("Id$",           "ID")
         , ("Id([A-Z])",     "ID$1")
         , ("Idn",           "IDN")
         , ("Ids$",          "IDs")
         , ("Ids([A-Z])",    "IDs$1")
         , ("Iops",          "IOPS")
         , ("Ip",            "IP")
         , ("Jar",           "JAR")
         , ("Json",          "JSON")
         , ("Jvm",           "JVM")
         , ("Kms",           "KMS")
         , ("Mac([^h]|$)",   "MAC$1")
         , ("Md5",           "MD5")
         , ("Mfa",           "MFA")
         , ("Ok",            "OK")
         , ("Os",            "OS")
         , ("Php",           "PHP")
         , ("Raid",          "RAID")
         , ("Ramdisk",       "RAMDisk")
         , ("Rds",           "RDS")
         , ("Sni",           "SNI")
         , ("Sns",           "SNS")
         , ("Sriov",         "SRIOV")
         , ("Ssh",           "SSH")
         , ("Ssl",           "SSL")
         , ("Svn",           "SVN")
         , ("Tar([^g]|$)",   "TAR$1")
         , ("Tde",           "TDE")
         , ("Tcp",           "TCP")
         , ("Tgz",           "TGZ")
         , ("Tls",           "TLS")
         , ("Uri",           "URI")
         , ("Url",           "URL")
         , ("Vgw",           "VGW")
         , ("Vhd",           "VHD")
         , ("Vip",           "VIP")
         , ("Vlan",          "VLAN")
         , ("Vm([^d]|$)",    "VM$1")
         , ("Vmdk",          "VMDK")
         , ("Vpc",           "VPC")
         , ("Vpn",           "VPN")
         , ("Xml",           "XML")
         , ("Xlarge",        "XLarge")
         ]

revertAcronym :: Text -> Text
revertAcronym x = Fold.foldl' (flip (uncurry RE.replaceAll)) x xs
  where
    xs :: [(Regex, Replace)]
    xs = [ ("ACL",      "Acl")
         , ("AES",      "Aes")
         , ("API",      "Api")
         , ("AMI",      "Ami")
         , ("APNS",     "Apns")
         , ("ARN",      "Arn")
         , ("ASN",      "Asn")
         , ("AWS",      "Aws")
         , ("BGP",      "Bgp")
         , ("CIDR",     "Cidr")
         , ("CORS",     "Cors")
         , ("CSV",      "Csv")
         , ("CPU",      "Cpu")
         , ("DB",       "Db")
         , ("DHCP",     "Dhcp")
         , ("DNS",      "Dns")
         , ("EBS",      "Ebs")
         , ("EC2",      "Ec2")
         , ("EIP",      "Eip")
         , ("GCM",      "Gcm")
         , ("HTML",     "Html")
         , ("HTTPS",    "Https")
         , ("HSM",      "Hsm")
         , ("HVM",      "Hvm")
         , ("IAM",      "Iam")
         , ("ICMP",     "Icmp")
         , ("IDN",      "Idn")
         , ("IOPS",     "Iops")
         , ("IP",       "Ip")
         , ("JAR",      "Jar")
         , ("JSON",     "Json")
         , ("JVM",      "Jvm")
         , ("KMS",      "Kms")
         , ("MD5",      "Md5")
         , ("MFA",      "Mfa")
         , ("OK",       "Ok")
         , ("OS",       "Os")
         , ("PHP",      "Php")
         , ("RAID",     "Raid")
         , ("RAMDisk",  "Ramdisk")
         , ("RDS",      "Rds")
         , ("SNI",      "Sni")
         , ("SNS",      "Sns")
         , ("SRIOV",    "Sriov")
         , ("SSH",      "Ssh")
         , ("SSL",      "Ssl")
         , ("SVN",      "Svn")
         , ("TDE",      "Tde")
         , ("TCP",      "Tcp")
         , ("TGZ",      "Tgz")
         , ("TLS",      "Tls")
         , ("URI",      "Uri")
         , ("URL",      "Url")
         , ("VGW",      "Vgw")
         , ("VHD",      "Vhd")
         , ("VIP",      "Vip")
         , ("VLAN",     "Vlan")
         , ("VMDK",     "Vmdk")
         , ("VPC",      "Vpc")
         , ("VPN",      "Vpn")
         , ("XML",      "Xml")
         , ("XLarge",   "Xlarge")
         ]
