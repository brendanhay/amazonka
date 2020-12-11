-- |
-- Module      : Gen.Text
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Text where

import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.Text.ICU (Regex)
import Data.Text.ICU.Replace (Replace)
import qualified Data.Text.ICU.Replace as ICU.Replace
import qualified Data.Text.Manipulate as Manipulate
import Gen.Prelude
import qualified Text.Parsec.Language as Parsec.Language
import qualified Text.Parsec.Token as Parsec.Token

asText :: (Text -> Text) -> String -> String
asText f = Text.unpack . f . Text.pack

dropLower :: Text -> Text
dropLower = Text.dropWhile (not . Char.isUpper)

safeHead :: Text -> Maybe Text
safeHead = fmap (Text.singleton . fst) . Text.uncons

stripLens :: Text -> Text
stripLens t
  | "_" `Text.isPrefixOf` t = Manipulate.lowerHead (dropLower t)
  | otherwise = t

stripPrefix :: Text -> Text -> Text
stripPrefix p t = Text.strip . fromMaybe t $ p `Text.stripPrefix` t

stripSuffix :: Text -> Text -> Text
stripSuffix s t = Text.strip . fromMaybe t $ s `Text.stripSuffix` t

renameOperation :: Text -> Text
renameOperation t
  | "S3" `Text.isSuffixOf` t = t
  | "V2" `Text.isSuffixOf` t = t -- Allow "ListObjectsV2"
  | otherwise = Text.dropWhileEnd f (Text.strip t)
  where
    f x = x == '_' || Char.isDigit x

serviceFunction :: Text -> Text
serviceFunction n =
  let f c = Char.isUpper c || Char.isDigit c
   in (<> "Service") $
        if Text.all f n
          then Text.toLower n
          else Manipulate.lowerHead (lowerFirstAcronym n)

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
renameBranch = first (renameReserved . go) . join (,)
  where
    go x
      | decimal x = Text.cons 'D' . cat $ split x
      | Text.all Char.isDigit x = Text.cons 'N' x
      | Text.length x <= 2 = Text.toUpper x
      | otherwise = upperAcronym . cat $ split x

    cat = Foldable.foldMap (Text.intercalate "_" . map component . Text.split dot)
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
      | Text.length x <= 1 = x
      | Char.isDigit (Text.last x) = Text.toUpper x
      | Text.all Char.isUpper x = Manipulate.toPascal (Text.toLower x)
      | otherwise = Manipulate.toPascal x

    decimal = Text.all (\c -> Char.isDigit c || c == '.')

renameReserved :: Text -> Text
renameReserved x
  | HashSet.member x xs = x <> "'"
  | otherwise = x
  where
    xs =
      HashSet.fromList $
        map Text.pack $
          Parsec.Token.reservedNames Parsec.Language.haskellDef
            ++ [ "role",
                 "pattern"
               ]

-- Pass in Relation, check if Uni directional + not shared and then add
-- rq + rs to a simplified acronym prefix algo?

camelAcronym :: Text -> Text
camelAcronym x = replaceAll x xs
  where
    xs = map (bimap fromString fromString) acronyms

lowerFirstAcronym :: Text -> Text
lowerFirstAcronym x = Manipulate.lowerHead (replaceAll x xs)
  where
    xs = map (bimap (fromString . ('^' :)) (fromString . f)) acronyms

    f (c : cs) = Char.toLower c : cs
    f [] = []

replaceAll :: Text -> [(Regex, Replace)] -> Text
replaceAll = Foldable.foldl' (flip (uncurry ICU.Replace.replaceAll))

upperAcronym :: Text -> Text
upperAcronym x = Foldable.foldl' (flip (uncurry ICU.Replace.replaceAll)) x xs
  where
    xs :: [(Regex, Replace)]
    xs =
      [ ("Acl", "ACL"),
        ("Adm([^i]|$)", "ADM$1"),
        ("Aes", "AES"),
        ("Ami", "AMI"),
        ("Api", "API"),
        ("Apns", "APNS"),
        ("Arn", "ARN"),
        ("Asn", "ASN"),
        ("Aws", "AWS"),
        ("Bcc([A-Z])", "BCC$1"),
        ("Bgp", "BGP"),
        ("Cc([A-Z])", "CC$1"),
        ("Cors", "CORS"),
        ("Cpu", "CPU"),
        ("Csr$", "CSR"),
        ("Csv", "CSV"),
        ("Db", "DB"),
        ("Dhcp", "DHCP"),
        ("Dns", "DNS"),
        ("Ebs", "EBS"),
        ("Ec2", "EC2"),
        ("Eip", "EIP"),
        ("en-US", "EN_US"),
        ("Eq([^u]|$)", "EQ$1"),
        ("Gcm", "GCM"),
        ("Gcm", "GCM"),
        ("Gt$", "GT"),
        ("Hapg", "HAPG"),
        ("Hsm", "HSM"),
        ("Html", "HTML"),
        ("Http([^s]|$)", "HTTP$1"),
        ("Https", "HTTPS"),
        ("Hvm", "HVM"),
        ("ID", "Id"),
        ("Ia$", "IA"),
        ("Iam", "IAM"),
        ("Icmp", "ICMP"),
        ("Idn", "IDN"),
        ("iOS$", "IOS"),
        ("Ios$", "IOS"),
        ("Io([^a-z])", "IO$1"),
        ("Iops", "IOPS"),
        ("Ip", "IP"),
        ("Jar", "JAR"),
        ("Json", "JSON"),
        ("Jvm", "JVM"),
        ("Kms", "KMS"),
        ("Lt$", "LT"),
        ("Mac([^h]|$)", "MAC$1"),
        ("Md5", "MD5"),
        ("Mfa", "MFA"),
        ("Ok", "OK"),
        ("Os", "OS"),
        ("Php", "PHP"),
        ("Qos", "QOS"),
        ("Raid", "RAID"),
        ("Ramdisk", "RAMDisk"),
        ("Rds", "RDS"),
        ("Sdk", "SDK"),
        ("Sgd", "SGD"),
        ("Sni", "SNI"),
        ("Sns", "SNS"),
        ("Sriov", "SRIOV"),
        ("Ssh", "SSH"),
        ("Ssl", "SSL"),
        ("Sso", "SSO"),
        ("Svn", "SVN"),
        ("Tar([^g]|$)", "TAR$1"),
        ("Tcp", "TCP"),
        ("Tde", "TDE"),
        ("Tgz", "TGZ"),
        ("Tls", "TLS"),
        ("Uri", "URI"),
        ("Url", "URL"),
        ("Vgw", "VGW"),
        ("Vhd", "VHD"),
        ("Vip", "VIP"),
        ("Vlan", "VLAN"),
        ("Vm([^d]|$)", "VM$1"),
        ("Vmdk", "VMDK"),
        ("Vpc", "VPC"),
        ("Vpn", "VPN"),
        ("X8664", "X86_64"),
        ("Xlarge", "XLarge"),
        ("Xml", "XML"),
        ("Xss", "XSS"),
        ("xlarge", "XLarge")
      ]

acronyms :: [(String, String)]
acronyms =
  [ ("ACL", "Acl"),
    ("AES", "Aes"),
    ("AMI", "Ami"),
    ("API", "Api"),
    ("APNS", "Apns"),
    ("ARN", "Arn"),
    ("ASN", "Asn"),
    ("AWS", "Aws"),
    ("BGP", "Bgp"),
    ("CORS", "Cors"),
    ("CPU", "Cpu"),
    ("CSV", "Csv"),
    ("DB", "Db"),
    ("DHCP", "Dhcp"),
    ("DNS", "Dns"),
    ("EBS", "Ebs"),
    ("EC2", "Ec2"),
    ("EIP", "Eip"),
    ("EN_US", "en-US"),
    ("GCM", "Gcm"),
    ("GCM", "Gcm"),
    ("HAPG", "Hapg"),
    ("HSM", "Hsm"),
    ("HTML", "Html"),
    ("HTTPS", "Https"),
    ("HVM", "Hvm"),
    ("IAM", "Iam"),
    ("ICMP", "Icmp"),
    ("ID", "Id"),
    ("IDN", "Idn"),
    ("IOS$", "IOS"),
    ("IOPS", "Iops"),
    ("IP", "Ip"),
    ("JAR", "Jar"),
    ("JSON", "Json"),
    ("JVM", "Jvm"),
    ("KMS", "Kms"),
    ("MD5", "Md5"),
    ("MFA", "Mfa"),
    ("OK", "Ok"),
    ("OS", "Os"),
    ("PHP", "Php"),
    ("QOS", "Qos"),
    ("RAID", "Raid"),
    ("RAMDisk", "Ramdisk"),
    ("RDS", "Rds"),
    ("SDK", "Sdk"),
    ("SGD", "Sgd"),
    ("SNI", "Sni"),
    ("SNS", "Sns"),
    ("SRIOV", "Sriov"),
    ("SSH", "Ssh"),
    ("SSL", "Ssl"),
    ("SSO", "Sso"),
    ("SVN", "Svn"),
    ("TCP", "Tcp"),
    ("TDE", "Tde"),
    ("TGZ", "Tgz"),
    ("TLS", "Tls"),
    ("URI", "Uri"),
    ("URL", "Url"),
    ("VGW", "Vgw"),
    ("VHD", "Vhd"),
    ("VIP", "Vip"),
    ("VLAN", "Vlan"),
    ("VMDK", "Vmdk"),
    ("VPC", "Vpc"),
    ("VPN", "Vpn"),
    ("XLarge", "Xlarge"),
    ("XML", "Xml"),
    ("XSS", "Xss")
  ]
