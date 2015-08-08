{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Gen.Text
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Text where

import           Control.Error
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import qualified Data.Foldable         as Fold
import qualified Data.HashSet          as Set
import           Data.Monoid
import           Data.String
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

stripPrefix :: Text -> Text -> Text
stripPrefix p t = Text.strip . fromMaybe t $ p `Text.stripPrefix` t

stripSuffix :: Text -> Text -> Text
stripSuffix s t = Text.strip . fromMaybe t $ s `Text.stripSuffix` t

renameOperation :: Text -> Text
renameOperation = Text.dropWhileEnd f . Text.strip
  where
    f x = x == '_'
       || isDigit x

renameService :: Text -> Text
renameService =
      mappend "Amazon "
    . flip mappend " SDK"
    . stripPrefix "Amazon"
    . stripPrefix "AWS"
    . stripPrefix "Service"
    . stripSuffix "SDK"

-- Since 'takeLowerText' is used for FromText parser instances,
-- the branch value is lowercased here.
--
-- Tangentially the 'takeLowerText' function exists to avoid the
-- horrendous inlining that use Data.CaseInsensitive provokes and
-- the subsequent compilation time explosion on a project of this size.
renameBranch :: Text -> (Text, Text)
renameBranch = bimap (renameReserved . go) Text.toLower . join (,)
  where
    go x | Text.length x <= 2 = Text.toUpper x
         | otherwise          = upperAcronym . cat $ split x

    cat   = Fold.foldMap (Text.intercalate "_" . map component . Text.split dot)
    split = Text.split seperator

    dot x = x == '.'

    seperator x =
          x == '\\'
       || x == '/'
       || x == '+'
       || x == ' '
       || x == '('
       || x == ')'
       || x == ':'
       || x == '-'
       || x == '_'
       || x == '*'

    component x
        | Text.length x <= 1    = x
        | isDigit (Text.last x) = Text.toUpper x
        | Text.all isUpper x    = toPascal (Text.toLower x)
        | otherwise             = toPascal x

renameReserved :: Text -> Text
renameReserved x
    | x `Set.member` xs = x <> "'"
    | otherwise         = x
  where
    xs = Set.fromList $
        [ "head"
        , "tail"
        , "delete"
        , "filter"
        , "True"
        , "False"
        , "map"
        , "object"
        , "get"
        , "group"
        , "GT"
        , "LT"
        , "EQ"
        , "Error"
        ] ++ map Text.pack (reservedNames haskellDef)

-- Pass in Relation, check if Uni directional + not shared and then add
-- rq + rs to a simplified acronym prefix algo?

camelAcronym :: Text -> Text
camelAcronym x = replaceAll x xs
  where
    xs = map (bimap fromString fromString) acronyms

lowerFirstAcronym :: Text -> Text
lowerFirstAcronym x = replaceAll x xs
  where
    xs = map (bimap (fromString . ('^':)) (fromString . f)) acronyms

    f (c:cs) = toLower c : cs
    f []     = []

replaceAll :: Text -> [(Regex, Replace)] -> Text
replaceAll = Fold.foldl' (flip (uncurry RE.replaceAll))

upperAcronym :: Text -> Text
upperAcronym x = Fold.foldl' (flip (uncurry RE.replaceAll)) x xs
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
         , ("Idn",           "IDN")
         , ("Io([^a-z])",    "IO$1")
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
         , ("Sso",           "SSO")
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
         , ("xlarge",        "XLarge")
         , ("Hapg",          "HAPG")
         , ("ID",            "Id")
         , ("Eq([^u]|$)",    "EQ$1")
         , ("Lt$",           "LT")
         , ("Gt$",           "GT")
         , ("Gt$",           "GT")
         , ("X8664",         "X86_64")
         ]

acronyms :: [(String, String)]
acronyms =
    [ ("ACL",      "Acl")
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
    , ("SSO",      "Sso")
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
    , ("HAPG",     "Hapg")
    , ("ID",       "Id")
    ]
