{-# LANGUAGE DeriveGeneric     #-}
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

import Control.Applicative          ((<$>))
import Data.ByteString              (ByteString)
import Data.Time
import Network.AWS.Internal
import Text.ParserCombinators.ReadP (string)
import Text.Read

newtype CallerReference = CallerReference { unCallerReference :: ByteString }
    deriving (Eq, Ord, Show, Generic)

instance IsXML CallerReference where
    xmlPickler = (CallerReference, unCallerReference) `xpWrap` xpContent xpText

callerRef :: IO CallerReference
callerRef = CallerReference . toBS <$> getCurrentTime

data Protocol = HTTP | TCP
    deriving (Eq, Read, Show, Generic)

instance IsXML Protocol where
    xmlPickler = xpContent xpPrim

data RecordAction = CreateAction | DeleteAction
    deriving (Eq)

instance Show RecordAction where
    show CreateAction = "CREATE"
    show DeleteAction = "DELETE"

instance Read RecordAction where
    readPrec = choice $ map (\(x, y) -> lift $ string x >> return y)
         [ ("CREATE", CreateAction)
         , ("DELETE", DeleteAction)
         ]

instance IsXML RecordAction where
    xmlPickler = xpContent xpPrim

data RecordType = A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT
    deriving (Eq, Read, Show, Generic)

instance IsXML RecordType where
    xmlPickler = xpContent xpPrim

instance IsQuery RecordType

data ChangeStatus = PENDING | INSYNC
    deriving (Eq, Read, Show, Generic)

instance IsXML ChangeStatus where
    xmlPickler = xpContent xpPrim

data Config = Config
    { cComment :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML Config

data HostedZone = HostedZone
    { hzId                     :: !ByteString
    , hzName                   :: !ByteString
    , hzCallerReference        :: !CallerReference
    , hzConfig                 :: !Config
    , hzResourceRecordSetCount :: !Integer
    } deriving (Eq, Show, Generic)

instance IsXML HostedZone

data ChangeInfo = ChangeInfo
    { ciId          :: !ByteString
    , ciStatus      :: !ChangeStatus
    , ciSubmittedAt :: !UTCTime
    } deriving (Eq, Show, Generic)

instance IsXML ChangeInfo

newtype DelegationSet = DelegationSet { dsNameServers :: [ByteString] }
    deriving (Eq, Show, Generic)

instance IsXML DelegationSet where
    xmlPickler = xpWrap (DelegationSet, dsNameServers)
        (xpElem "NameServers" $ xpElemList "NameServer" xmlPickler)

newtype ResourceRecords = ResourceRecords { rrValues :: [ByteString] }
    deriving (Eq, Show, Generic)

instance IsXML ResourceRecords where
    xmlPickler = xpWrap (ResourceRecords, rrValues)
        (xpElemList "ResourceRecord" $ xpElem "Value" xmlPickler)

data ResourceRecordSet = ResourceRecordSet
    { rrsAction          :: !RecordAction
    , rrsName            :: !ByteString
    , rrsType            :: !RecordType
    , rrsTTL             :: !Integer
    , rrsHealthCheckId   :: !(Maybe ByteString)
    , rrsResourceRecords :: !ResourceRecords
    } deriving (Eq, Show, Generic)

instance IsXML ResourceRecordSet

data HealthCheckConfig = HealthCheckConfig
    { hccIPAddress                :: !ByteString
    , hccPort                     :: !Int
    , hccType                     :: !Protocol
    , hccResourcePath             :: !ByteString
    , hccFullyQualifiedDomainName :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML HealthCheckConfig

data HealthCheck = HealthCheck
    { hcId                :: !ByteString
    , hcCallerReference   :: !CallerReference
    , hcHealthCheckConfig :: !HealthCheckConfig
    } deriving (Eq, Show, Generic)

instance IsXML HealthCheck
