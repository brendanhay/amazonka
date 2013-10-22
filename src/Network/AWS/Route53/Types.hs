{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Module      : Network.AWS.Route53.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.Types where

import           Control.Applicative   ((<$>))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Data.String
import           Data.Text             (Text)
import           Data.Text.Encoding
import           Data.Time
import           Network.AWS.Internal
import           Text.Read

route53Service :: Service
route53Service = Service "route53" route53Version SigningVersion3 $
    Global "route53.amazonaws.com"

-- | Currently supported version of the Route53 service.
route53Version :: ServiceVersion
route53Version = "2012-12-12"

-- | XML namespace to annotate Route53 elements with.
route53NS :: ByteString
route53NS = "https://route53.amazonaws.com/doc/" <> sPack route53Version <> "/"

-- | Helper to define Route53 namespaced XML elements.
route53Elem :: ByteString -> NName ByteString
route53Elem = mkNName route53NS

class Prefixed a where
    prefixed :: a -> Text

data ErrorType = ErrorType
    { etType    :: !Text
    , etCode    :: !Text
    , etMessage :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML ErrorType where
    xmlPickler = withNS route53NS

data ErrorResponse
    = ErrorResponse
      { erError     :: !ErrorType
      , erRequestId :: !Text
      }
    | InvalidChangeBatch
      { erMessages :: [Text]
      }
    deriving (Eq, Show, Generic)

instance ToError ErrorResponse where
    toError = Err . show

instance IsXML ErrorResponse where
    xmlPickler = withNS route53NS

-- instance IsXML ResourceRecordSet where
--     xmlPickler = genericXMLPickler $ (namespacedXMLOptions route53NS)
--         { xmlCtorModifier = const $ route53Elem "ResourceRecordSet"
--         }

-- <ErrorResponse xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--     <Error>
--         <Type>Sender</Type>
--         <Code>InvalidChangeBatch</Code>
--         <Message>
--         Tried to create an alias that targets asd., type CNAME in zone ZORL6RE8A5Z9D, but the alias target name does not lie within the target zone</Message>
--     </Error>
--     <RequestId>8e36cb8f-1566-11e3-96d1-856ead110654</RequestId>
-- </ErrorResponse>

-- <InvalidChangeBatch xmlns="https://route53.amazonaws.com/doc/2012-12-12/">
--    <Messages>
--       <Message>
--       Tried to create resource record set duplicate.example.com. type A,
--       but it already exists
--       </Message>
--    </Messages>
-- </InvalidChangeBatch>

newtype CallerReference = CallerReference { unCallerReference :: Text }
    deriving (Eq, Ord, Show, Generic)

instance IsXML CallerReference where
    xmlPickler = (CallerReference, unCallerReference) `xpWrap` xpTextContent

callerRef :: IO CallerReference
callerRef = CallerReference . decodeUtf8 . awsTime <$> getCurrentTime

data Protocol = HTTP | TCP
    deriving (Eq, Read, Show, Generic)

instance IsXML Protocol where
    xmlPickler = xpContent xpPrim

data RecordType = A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT
    deriving (Eq, Read, Show, Generic)

instance IsXML RecordType where
    xmlPickler = xpContent xpPrim

instance IsQuery RecordType

data Config = Config
    { cComment :: Maybe Text
    } deriving (Eq, Show, Generic)

instance IsXML Config where
    xmlPickler = withNS route53NS

newtype HostedZoneId = HostedZoneId { unHostedZoneId :: Text }
   deriving (Eq, IsString)

instance Show HostedZoneId where
    show = BS.unpack . prefixed

instance Prefixed HostedZoneId where
    prefixed = sEnsurePrefix "/hostedzone/" . encodeUtf8 . unHostedZoneId

instance IsXML HostedZoneId where
    xmlPickler = (HostedZoneId, unHostedZoneId) `xpWrap` xmlPickler

instance IsQuery HostedZoneId where
    queryPickler = (HostedZoneId, unHostedZoneId) `qpWrap` queryPickler

data HostedZone = HostedZone
    { hzId                     :: !HostedZoneId
      -- ^ The ID of the hosted zone.
    , hzName                   :: !Text
      -- ^ The name of the domain. For resource record types that include a
      -- domain name, specify a fully qualified domain name, for example,
      -- www.example.com. The trailing dot is optional; Route 53 assumes
      -- that the domain name is fully qualified. This means that Route 53
      -- treats www.example.com (without a trailing dot) and
      -- www.example.com. (with a trailing dot) as identical.
    , hzCallerReference        :: !CallerReference
      -- ^ A unique string that identifies the request to create the hosted
      -- zone.
    , hzConfig                 :: !Config
      -- ^ A complex type that contains an optional comment about your
      -- hosted zone.
    , hzResourceRecordSetCount :: !Integer
      -- ^ The number of resource record sets in the hosted zone.
    } deriving (Eq, Show, Generic)

instance IsXML HostedZone where
    xmlPickler = withNS route53NS

newtype DelegationSet = DelegationSet { dsNameServers :: [Text] }
    deriving (Eq, Show, Generic)

instance IsXML DelegationSet where
    xmlPickler = xpWrap (DelegationSet, dsNameServers)
        (xpElem (route53Elem "NameServers")
            $ xpElemList (route53Elem "NameServer") xmlPickler)

newtype ChangeId = ChangeId { unChangeId :: Text }
    deriving (Eq, IsString)

instance Show ChangeId where
    show = BS.unpack . prefixed

instance Prefixed ChangeId where
    prefixed = sEnsurePrefix "/change/" . encodeUtf8 . unChangeId

instance IsXML ChangeId where
    xmlPickler = (ChangeId, unChangeId) `xpWrap` xmlPickler

data ChangeStatus = PENDING | INSYNC
    deriving (Eq, Read, Show, Generic)

instance IsXML ChangeStatus where
    xmlPickler = xpContent xpPrim

data ChangeInfo = ChangeInfo
    { ciId          :: !ChangeId
    , ciStatus      :: !ChangeStatus
    , ciSubmittedAt :: !UTCTime
    } deriving (Eq, Show, Generic)

instance IsXML ChangeInfo where
    xmlPickler = withNS route53NS

data ChangeAction = CreateAction | DeleteAction
    deriving (Eq)

instance Show ChangeAction where
    show CreateAction = "CREATE"
    show DeleteAction = "DELETE"

instance Read ChangeAction where
    readPrec = readAssocList
         [ ("CREATE", CreateAction)
         , ("DELETE", DeleteAction)
         ]

instance IsXML ChangeAction where
    xmlPickler = xpContent xpPrim

data Change = Change
    { cAction            :: !ChangeAction
    , cResourceRecordSet :: !ResourceRecordSet
    } deriving (Eq, Show, Generic)

instance IsXML Change where
    xmlPickler = withNS route53NS

data ChangeBatch = ChangeBatch
    { cbComment :: Maybe Text
    , cbChanges :: [Change]
    } deriving (Eq, Show, Generic)

instance IsXML ChangeBatch where
    xmlPickler = withNS route53NS

newtype ResourceRecords = ResourceRecords { rrValues :: [Text] }
    deriving (Eq, Show, Generic)

instance IsXML ResourceRecords where
    xmlPickler = xpWrap (ResourceRecords, rrValues)
        (xpElemList (route53Elem "ResourceRecord")
            $ xpElem (route53Elem "Value") xmlPickler)

data AliasTarget = AliasTarget
    { atHostedZoneId         :: !HostedZoneId
    , atDNSName              :: !Text
    , atEvaluateTargetHealth :: !Bool
    } deriving (Eq, Show, Generic)

instance IsXML AliasTarget where
    xmlPickler = withNS route53NS

data Failover = PRIMARY | SECONDARY
    deriving (Eq, Read, Show, Generic)

instance IsXML Failover where
    xmlPickler = xpContent xpPrim

data ResourceRecordSet
    = FailoverRecordSet
      { rrsName            :: !Text
      , rrsType            :: !RecordType
      , rrsSetIdentifier   :: !Text
      , rrsFailover        :: !Failover
      , rrsTTL             :: !Integer
      , rrsResourceRecords :: !ResourceRecords
      , rrsHealthCheckId   :: Maybe HealthCheckId
      }

    | FailoverAliasRecordSet
      { rrsName          :: !Text
      , rrsType          :: !RecordType
      , rrsSetIdentifier :: !Text
      , rrsFailover      :: !Failover
      , rrsAliasTarget   :: !AliasTarget
      , rrsHealthCheckId :: Maybe HealthCheckId
      }

    | LatencyRecordSet
      { rrsName            :: !Text
      , rrsType            :: !RecordType
      , rrsSetIdentifier   :: !Text
      , rrsRegion          :: !Region
      , rrsTTL             :: !Integer
      , rrsResourceRecords :: !ResourceRecords
      , rrsHealthCheckId   :: Maybe HealthCheckId
      }

    | LatencyAliasRecordSet
      { rrsName          :: !Text
      , rrsType          :: !RecordType
      , rrsSetIdentifier :: !Text
      , rrsRegion        :: !Region
      , rrsAliasTarget   :: !AliasTarget
      , rrsHealthCheckId :: Maybe HealthCheckId
      }

    | WeightedRecordSet
      { rrsName            :: !Text
      , rrsType            :: !RecordType
      , rrsSetIdentifier   :: !Text
      , rrsWeight          :: !Integer
      , rrsTTL             :: !Integer
      , rrsResourceRecords :: !ResourceRecords
      , rrsHealthCheckId   :: Maybe HealthCheckId
      }

    | WeightedAliasRecordSet
      { rrsName          :: !Text
      , rrsType          :: !RecordType
      , rrsSetIdentifier :: !Text
      , rrsWeight        :: !Integer
      , rrsAliasTarget   :: !AliasTarget
      , rrsHealthCheckId :: Maybe HealthCheckId
      }

    | BasicRecordSet
      { rrsName            :: !Text
      , rrsType            :: !RecordType
      , rrsTTL             :: !Integer
      , rrsResourceRecords :: !ResourceRecords
      , rrsHealthCheckId   :: Maybe HealthCheckId
      }

    | AliasRecordSet
      { rrsName          :: !Text
      , rrsType          :: !RecordType
      , rrsAliasTarget   :: !AliasTarget
      , rrsHealthCheckId :: Maybe HealthCheckId
      }

    deriving (Eq, Show, Generic)

instance IsXML ResourceRecordSet where
    xmlPickler = genericXMLPickler $ (namespacedXMLOptions route53NS)
        { xmlCtorModifier = const $ route53Elem "ResourceRecordSet"
        }

data HealthCheckConfig = HealthCheckConfig
    { hccIPAddress                :: !Text
    , hccPort                     :: !Int
    , hccType                     :: !Protocol
    , hccResourcePath             :: !Text
    , hccFullyQualifiedDomainName :: !Text
    } deriving (Eq, Show, Generic)

instance IsXML HealthCheckConfig where
    xmlPickler = withNS route53NS

newtype HealthCheckId = HealthCheckId { unHealthCheckId :: Text }
    deriving (Eq, IsString)

instance Show HealthCheckId where
    show = BS.unpack . prefixed

instance Prefixed HealthCheckId where
    prefixed = sEnsurePrefix "/healthcheck/" . encodeUtf8 . unHealthCheckId

instance IsXML HealthCheckId where
    xmlPickler = (HealthCheckId, unHealthCheckId) `xpWrap` xmlPickler

data HealthCheck = HealthCheck
    { hcId                :: !HealthCheckId
    , hcCallerReference   :: !CallerReference
    , hcHealthCheckConfig :: !HealthCheckConfig
    } deriving (Eq, Show, Generic)

instance IsXML HealthCheck where
    xmlPickler = withNS route53NS
