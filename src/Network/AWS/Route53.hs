{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Route 53 is a web service that enables you to manage your DNS records.
module Network.AWS.Route53
    (
    -- * Route53 API Version
      route53Version

    -- * Actions on Hosted Zones
    -- ** POST CreateHostedZone
    , CreateHostedZone                 (..)
    , CreateHostedZoneResponse         (..)

    -- ** GET GetHostedZone
    , GetHostedZone                    (..)
    , GetHostedZoneResponse            (..)

    -- ** GET ListHostedZones
    , ListHostedZones                  (..)
    , ListHostedZonesResponse          (..)

    -- ** DELETE DeleteHostedZone
    , DeleteHostedZone                 (..)
    , DeleteHostedZoneResponse         (..)

    -- * Actions on Record Sets
    -- ** POST ChangeResourceRecordSets
    , ChangeResourceRecordSets         (..)
    , ChangeResourceRecordSetsResponse (..)

    -- ** GET ListResourceRecordSets
    , ListResourceRecordSets           (..)
    , ListResourceRecordSetsResponse   (..)

    -- ** GET GetChange
    , GetChange                        (..)
    , GetChangeResponse                (..)

    -- * Actions on Health Checks
    -- ** POST CreateHealthCheck
    , CreateHealthCheck                (..)
    , CreateHealthCheckResponse        (..)

    -- ** GET GetHealthCheck
    , GetHealthCheck                   (..)
    , GetHealthCheckResponse           (..)

    -- ** GET ListHealthChecks
    , ListHealthChecks                 (..)
    , ListHealthChecksResponse         (..)

    -- ** DELETE DeleteHealthCheck
    , DeleteHealthCheck                (..)
    , DeleteHealthCheckResponse        (..)

    -- * Data Types
    , module Network.AWS.Route53.Types
    ) where

import Control.Applicative       ((<$>))
import Data.ByteString           (ByteString)
import Data.Monoid
import Data.String
import Network.AWS.Internal
import Network.AWS.Route53.Types
import Network.Http.Client       (Method(..))

data R53

instance AWSService R53 where
    service _ = Service "route53" route53Version "route53.amazonaws.com"
        SigningVersion3 <$> currentRegion

req :: IsQuery a => Method -> ByteString -> a -> RawRequest R53 b
req meth path qry = (emptyRequest meth FormEncoded (ver path) Nothing)
    { rqQuery = toQuery qry
    }

body :: IsXML a => Method -> ByteString -> a -> RawRequest R53 b
body meth path = (emptyRequest meth XML $ ver path) . Just . toIndentedXML 2 -- Indented for Debugging/Testing purposes

ver :: ByteString -> ByteString
ver = mappend ("/" <> route53Version <> "/")

--
-- Hosted Zones
--

-- | Creates a new hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html>
data CreateHostedZone = CreateHostedZone
    { chzName             :: !ByteString
    , chzCallerReference  :: !CallerReference
    , chzHostedZoneConfig :: Maybe Config
    } deriving (Eq, Show, Generic)

instance IsXML CreateHostedZone where
    xmlPickler = withRootNS route53NS "CreateHostedZoneRequest"

instance AWSRequest R53 CreateHostedZone CreateHostedZoneResponse where
    request = body POST "hostedzone"

data CreateHostedZoneResponse = CreateHostedZoneResponse
    { chzrHostedZone    :: !HostedZone
    , chzrChangeInfo    :: !ChangeInfo
    , chzrDelegationSet :: !DelegationSet
    } deriving (Eq, Show, Generic)

instance IsXML CreateHostedZoneResponse where
    xmlPickler = withNS route53NS

-- | Gets information about a specified hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHostedZone.html>
newtype GetHostedZone = GetHostedZone { ghzId :: ByteString }
    deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 GetHostedZone GetHostedZoneResponse where
    request chk = req GET ("hostedzone/" <> toBS chk) ()

data GetHostedZoneResponse = GetHostedZoneResponse
    { ghzrHostedZone    :: !HostedZone
    , ghzrDelegationSet :: !DelegationSet
    } deriving (Eq, Show, Generic)

instance IsXML GetHostedZoneResponse where
    xmlPickler = withNS route53NS

-- | Gets a list of the hosted zones that are associated with the
-- current AWS account.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZones.html>
data ListHostedZones = ListHostedZones
    { lhzMarker   :: Maybe ByteString
    , lhzMaxItems :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance IsQuery ListHostedZones where
    queryPickler = genericQueryPickler loweredQueryOptions

instance AWSRequest R53 ListHostedZones ListHostedZonesResponse where
    request = req GET "hostedzone"

data ListHostedZonesResponse = ListHostedZonesResponse
    { lhzrHostedZones :: [HostedZone]
    , lhzrIsTruncated :: !Bool
    , lhzrMarker      :: !ByteString
    , lhzrNextMarker  :: Maybe ByteString
    , lhzrMaxItems    :: !Integer
    } deriving (Eq, Show, Generic)

instance IsXML ListHostedZonesResponse where
    xmlPickler = withNS route53NS

-- | Deletes a hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteHostedZone.html>
newtype DeleteHostedZone = DeleteHostedZone { dhzId :: ByteString }
    deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 DeleteHostedZone DeleteHostedZoneResponse where
    request chk = req DELETE ("hostedzone/" <> toBS chk) ()

data DeleteHostedZoneResponse = DeleteHostedZoneResponse
    { dhzrChangeInfo :: !ChangeInfo
    } deriving (Eq, Show, Generic)

instance IsXML DeleteHostedZoneResponse where
    xmlPickler = withNS route53NS

--
-- Record Sets
--

-- | Adds, deletes, and changes resource record sets in a Route 53 hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html>
data ChangeResourceRecordSets = ChangeResourceRecordSets
    { crrsZoneId      :: !ByteString
    , crrsChangeBatch :: !ChangeBatch
    } deriving (Eq, Show, Generic)

instance IsXML ChangeResourceRecordSets where
    xmlPickler = (xpWrap
        (ChangeResourceRecordSets "", \(ChangeResourceRecordSets _ cs) -> cs)
        (xpElem (route53Elem "ChangeBatch") $ genericXMLPickler defaultXMLOptions))
            { root = Just $ mkNName route53NS "ChangeResourceRecordSetsRequest"
            }

instance AWSRequest R53 ChangeResourceRecordSets ChangeResourceRecordSetsResponse where
    request rs@ChangeResourceRecordSets{..} =
        body POST ("hostedzone/" <> crrsZoneId <> "/rrset") rs

data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse
    { crrsrChangeInfo :: !ChangeInfo
    } deriving (Eq, Show, Generic)

instance IsXML ChangeResourceRecordSetsResponse where
    xmlPickler = withNS route53NS

-- | Lists details about all of the resource record sets in a hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListResourceRecordSets.html>
data ListResourceRecordSets = ListResourceRecordSets
    { lrrsZoneId     :: !ByteString
    , lrrsName       :: Maybe ByteString
    , lrrsType       :: Maybe RecordType
    , lrrsIdentifier :: Maybe ByteString
    , lrrsMaxItems   :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance IsQuery ListResourceRecordSets where
    queryPickler = genericQueryPickler loweredQueryOptions

instance AWSRequest R53 ListResourceRecordSets ListResourceRecordSetsResponse where
    request rs@ListResourceRecordSets{..} =
        req GET ("hostedzone/" <> lrrsZoneId <> "/rrset") rs

data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse
    { lrrsrResourceRecordSets   :: [ResourceRecordSet]
    , lrrsrIsTruncated          :: !Bool
    , lrrsrMaxItems             :: !Integer
    , lrrsrNextRecordName       :: Maybe ByteString
    , lrrsrNextRecordType       :: Maybe RecordType
    , lrrsrNextRecordIdentifier :: Maybe ByteString
    } deriving (Eq, Show, Generic)

instance IsXML ListResourceRecordSetsResponse where
    xmlPickler = withNS route53NS

-- | Returns the current status of a change batch request that you
-- submitted by using ChangeResourceRecordSets.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html>
newtype GetChange = GetChange { gcId :: ByteString }
    deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 GetChange GetChangeResponse where
    request chk = req GET ("change/" <> toBS chk) ()

data GetChangeResponse = GetChangeResponse
    { gcrChangeInfo :: !ChangeInfo
    } deriving (Eq, Show, Generic)

instance IsXML GetChangeResponse where
    xmlPickler = withNS route53NS

--
-- Health Checks
--

-- | Creates a new health check.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHealthCheck.html>
data CreateHealthCheck = CreateHealthCheck
    { chcCallerReference   :: !CallerReference
    , chcHealthCheckConfig :: !HealthCheckConfig
    } deriving (Eq, Show, Generic)

instance IsXML CreateHealthCheck where
    xmlPickler = withRootNS route53NS "CreateHealthCheckRequest"

instance AWSRequest R53 CreateHealthCheck CreateHealthCheckResponse where
    request = body POST "healthcheck"

data CreateHealthCheckResponse = CreateHealthCheckResponse
    { chcrHealthCheck :: !HealthCheck
    } deriving (Eq, Show, Generic)

instance IsXML CreateHealthCheckResponse where
    xmlPickler = withNS route53NS

-- | Gets information about a specified health check.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheck.html>
newtype GetHealthCheck = GetHealthCheck { ghcId :: ByteString }
    deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 GetHealthCheck GetHealthCheckResponse where
    request chk = req GET ("healthcheck/" <> toBS chk) ()

data GetHealthCheckResponse = GetHealthCheckResponse
    { ghcrHealthCheck :: !HealthCheck
    } deriving (Eq, Show, Generic)

instance IsXML GetHealthCheckResponse where
    xmlPickler = withNS route53NS

-- | Gets a list of the health checks that are associated
-- with the current AWS account.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHealthChecks.html>
data ListHealthChecks = ListHealthChecks
    { lhcMarker   :: Maybe ByteString
    , lhcMaxItems :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance IsQuery ListHealthChecks where
    queryPickler = genericQueryPickler loweredQueryOptions

instance AWSRequest R53 ListHealthChecks ListHealthChecksResponse where
    request = req GET "healthcheck"

data ListHealthChecksResponse = ListHealthChecksResponse
    { lhcrHealthChecks :: [HealthCheck]
    , lhcrIsTruncated  :: !Bool
    , lhcrMarker       :: Maybe ByteString
    , lhcrNextMarker   :: Maybe ByteString
    , lhcrMaxItems     :: !Integer
    } deriving (Eq, Show, Generic)

instance IsXML ListHealthChecksResponse where
    xmlPickler = withNS route53NS

-- | Deletes a health check.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteHealthCheck.html>
newtype DeleteHealthCheck = DeleteHealthCheck { dhcId :: ByteString }
    deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 DeleteHealthCheck DeleteHealthCheckResponse where
    request chk = req DELETE ("healthcheck/" <> toBS chk) ()

data DeleteHealthCheckResponse = DeleteHealthCheckResponse
    deriving (Eq, Read, Show, Generic)

instance IsXML DeleteHealthCheckResponse where
    xmlPickler = xpEmpty
