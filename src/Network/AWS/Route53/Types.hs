{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Applicative  ((<$>))
import Data.ByteString      (ByteString)
import Data.Monoid
import Data.Text            (Text)
import Data.Time
import Network.AWS.Internal
import Text.Read

-- | Currently supported version of the Route53 service.
route53Version :: ByteString
route53Version = "2012-12-12"

-- | XML namespace to annotate Route53 elements with.
route53NS :: ByteString
route53NS = "https://route53.amazonaws.com/doc/" <> route53Version <> "/"

-- | Helper to define Route53 namespaced XML elements.
route53Elem :: ByteString -> NName ByteString
route53Elem = mkNName route53NS

newtype CallerReference = CallerReference { unCallerReference :: Text }
    deriving (Eq, Ord, Show, Generic)

instance IsXML CallerReference where
    xmlPickler = (CallerReference, unCallerReference) `xpWrap` xpTextContent

callerRef :: IO CallerReference
callerRef = CallerReference . toText <$> getCurrentTime

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

data HostedZone = HostedZone
    { hzId                     :: !Text
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

data ChangeStatus = PENDING | INSYNC
    deriving (Eq, Read, Show, Generic)

instance IsXML ChangeStatus where
    xmlPickler = xpContent xpPrim

data ChangeInfo = ChangeInfo
    { ciId          :: !Text
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
    { atHostedZoneId         :: !Text
    , atDNSName              :: !Text
    , atEvaluateTargetHealth :: Maybe Bool
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
      , rrsHealthCheckId   :: Maybe Text
      }

    | FailoverAliasRecordSet
      { rrsName          :: !Text
      , rrsType          :: !RecordType
      , rrsSetIdentifier :: !Text
      , rrsFailover      :: !Failover
      , rrsAliasTarget   :: !AliasTarget
      , rrsHealthCheckId :: Maybe Text
      }

    | LatencyRecordSet
      { rrsName            :: !Text
      , rrsType            :: !RecordType
      , rrsSetIdentifier   :: !Text
      , rrsRegion          :: !Region
      , rrsTTL             :: !Integer
      , rrsResourceRecords :: !ResourceRecords
      , rrsHealthCheckId   :: Maybe Text
      }

    | LatencyAliasRecordSet
      { rrsName          :: !Text
      , rrsType          :: !RecordType
      , rrsSetIdentifier :: !Text
      , rrsRegion        :: !Region
      , rrsAliasTarget   :: !AliasTarget
      , rrsHealthCheckId :: Maybe Text
      }

    | WeightedRecordSet
      { rrsName            :: !Text
      , rrsType            :: !RecordType
      , rrsSetIdentifier   :: !Text
      , rrsWeight          :: !Integer
      , rrsTTL             :: !Integer
      , rrsResourceRecords :: !ResourceRecords
      , rrsHealthCheckId   :: Maybe Text
      }

    | WeightedAliasRecordSet
      { rrsName          :: !Text
      , rrsType          :: !RecordType
      , rrsSetIdentifier :: !Text
      , rrsWeight        :: !Integer
      , rrsAliasTarget   :: !AliasTarget
      , rrsHealthCheckId :: Maybe Text
      }

    | BasicRecordSet
      { rrsName            :: !Text
      , rrsType            :: !RecordType
      , rrsTTL             :: !Integer
      , rrsResourceRecords :: !ResourceRecords
      , rrsHealthCheckId   :: Maybe Text
      }

    | AliasRecordSet
      { rrsName          :: !Text
      , rrsType          :: !RecordType
      , rrsAliasTarget   :: !AliasTarget
      , rrsHealthCheckId :: Maybe Text
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

data HealthCheck = HealthCheck
    { hcId                :: !Text
    , hcCallerReference   :: !CallerReference
    , hcHealthCheckConfig :: !HealthCheckConfig
    } deriving (Eq, Show, Generic)

instance IsXML HealthCheck where
    xmlPickler = withNS route53NS
