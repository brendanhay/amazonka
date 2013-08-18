{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

-- |
-- Module      : Network.AWS.Route53.V20121212.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V20121212.Types where

import Control.Applicative
import Data.ByteString              (ByteString)
import Data.Data
import Data.Time
import Network.AWS.Internal
import Text.ParserCombinators.ReadP (string)
import Text.Read

newtype CallerRef = CallerRef ByteString
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance IsXML CallerRef

callerRef :: IO CallerRef
callerRef = CallerRef . toBS <$> getCurrentTime

data Protocol = HTTP | TCP
    deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML Protocol

data RecordAction = CreateAction | DeleteAction
    deriving (Eq, Data, Typeable)

instance Show RecordAction where
    show CreateAction = "CREATE"
    show DeleteAction = "DELETE"

instance Read RecordAction where
    readPrec = choice $ map (\(x, y) -> lift $ string x >> return y)
         [ ("CREATE", CreateAction)
         , ("DELETE", DeleteAction)
         ]

instance IsXML RecordAction where
    xmlPickler = xpPrim

data RecordType = A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT
    deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML RecordType

instance IsQuery RecordType

data ChangeStatus = PENDING | INSYNC
    deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML ChangeStatus

data Config = Config
    { cComment :: !ByteString
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML Config

data HostedZone = HostedZone
    { hzId                     :: !ByteString
    , hzName                   :: !ByteString
    , hzCallerRef              :: !CallerRef
    , hzConfig                 :: !Config
    , hzResourceRecordSetCount :: !Integer
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML HostedZone

data ChangeInfo = ChangeInfo
    { ciId          :: !ByteString
    , ciStatus      :: !ChangeStatus
    , ciSubmittedAt :: !UTCTime
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML ChangeInfo

data DelegationSet = DelegationSet
    { dsNameServers :: ![ByteString]
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML DelegationSet

--
-- Hosted Zones
--

data CreateHostedZoneResponse = CreateHostedZoneResponse
    { chzrHostedZone    :: !HostedZone
    , chzrChangeInfo    :: !ChangeInfo
    , chzrDelegationSet :: !DelegationSet
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML CreateHostedZoneResponse

data GetHostedZoneResponse = GetHostedZoneResponse
    { ghzrHostZone      :: !HostedZone
    , ghzrDelegationSet :: !DelegationSet
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML GetHostedZoneResponse

data ListHostedZonesResponse = ListHostedZonesResponse
    { lhzrHostedZones :: ![HostedZone]
    , lhzrIsTruncated :: !Bool
    , lhzrMarker      :: !ByteString
    , lhzrNextMarker  :: !(Maybe ByteString)
    , lhzrMaxItems    :: !Integer
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML ListHostedZonesResponse

data DeleteHostedZoneResponse = DeleteHostedZoneResponse
    { dhzrChangeInfo :: !ChangeInfo
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML DeleteHostedZoneResponse

--
-- Record Sets
--

data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse
    { crrsrChangeInfo :: !ChangeInfo
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML ChangeResourceRecordSetsResponse

data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse
    deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML ListResourceRecordSetsResponse

data GetChangeResponse = GetChangeResponse
    { gcrChangeInfo :: !ChangeInfo
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML GetChangeResponse

--
-- Health Checks
--

data HealthCheckConfig = HealthCheckConfig
    { hccIPAddress                :: !ByteString
    , hccPort                     :: !Int
    , hccType                     :: !Protocol
    , hccResourcePath             :: !ByteString
    , hccFullyQualifiedDomainName :: !ByteString
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML HealthCheckConfig

data HealthCheck = HealthCheck
    { hcId                :: !ByteString
    , hcCallerRef         :: !CallerRef
    , hcHealthCheckConfig :: !HealthCheckConfig
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML HealthCheck

type CreateHealthCheckResponse = HealthCheck

type GetHealthCheckResponse = HealthCheck

data ListHealthChecksResponse = ListHealthChecksResponse
    { lhcrIsTruncated  :: !ByteString
    , lhcrHealthChecks :: ![HealthCheck]
    , lhcrMaxItems     :: !ByteString
    , lhcrMarker       :: !(Maybe ByteString)
    , lhcrNextMarker   :: !(Maybe ByteString)
    } deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML ListHealthChecksResponse

data DeleteHealthCheckResponse = DeleteHealthCheckResponse
    deriving (Eq, Show, Data, Typeable, Generic)

instance IsXML DeleteHealthCheckResponse
