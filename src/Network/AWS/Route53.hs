 {-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
import Data.Text                 (Text)
import Network.AWS.Internal
import Network.AWS.Route53.Types
import Network.Http.Client       (Method(..))

data R53

instance AWSService R53 where
    service _ = Service "route53" route53Version "route53.amazonaws.com"
        SigningVersion3 <$> currentRegion

instance IsXML b => AWSResponse R53 b where
    response bstr = eitherXML (fromXML bstr :: EitherXML b ErrorResponse)

req :: IsQuery a => Method -> Text -> a -> RawRequest R53 b
req meth path qry = (emptyRequest meth FormEncoded (ver path) Nothing)
    { rqQuery = toQuery qry
    }

body :: IsXML a => Method -> Text -> a -> RawRequest R53 b
body meth path = (emptyRequest meth XML . ver $ toText path) . Just . toXML

ver :: IsByteString a => a -> ByteString
ver = mappend ("/" <> route53Version <> "/") . toBS

--
-- Hosted Zones
--

-- | Creates a new hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html>
data CreateHostedZone = CreateHostedZone
    { chzName             :: !Text
      -- ^ The name of the domain.
    , chzCallerReference  :: !CallerReference
      -- ^ A unique string that identifies the request and that allows
      -- failed CreateHostedZone requests to be retried without the risk
      -- of executing the operation twice.
    , chzHostedZoneConfig :: Maybe Config
      -- ^ A complex type that contains an optional comment about your hosted zone.
    } deriving (Eq, Show, Generic)

instance IsXML CreateHostedZone where
    xmlPickler = withRootNS route53NS "CreateHostedZoneRequest"

instance AWSRequest R53 CreateHostedZone CreateHostedZoneResponse where
    request = body POST "hostedzone"

data CreateHostedZoneResponse = CreateHostedZoneResponse
    { chzrHostedZone    :: !HostedZone
      -- ^ Information about the hosted zone.
    , chzrChangeInfo    :: !ChangeInfo
      -- ^ Information about the changes being made to the hosted zone.
    , chzrDelegationSet :: !DelegationSet
      -- ^ The name servers for this hosted zone.
    } deriving (Eq, Show, Generic)

instance IsXML CreateHostedZoneResponse where
    xmlPickler = withNS route53NS

-- | Gets information about a specified hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHostedZone.html>
newtype GetHostedZone = GetHostedZone
    { ghzId :: HostedZoneId
      -- ^ Hosted Zone Id.
    } deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 GetHostedZone GetHostedZoneResponse where
    request chk = req GET (toText chk) ()

data GetHostedZoneResponse = GetHostedZoneResponse
    { ghzrHostedZone    :: !HostedZone
      -- ^ Information about the hosted zone.
    , ghzrDelegationSet :: !DelegationSet
      -- ^ The name servers for this hosted zone.
    } deriving (Eq, Show, Generic)

instance IsXML GetHostedZoneResponse where
    xmlPickler = withNS route53NS

-- | Gets a list of the hosted zones that are associated with the
-- current AWS account.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZones.html>
data ListHostedZones = ListHostedZones
    { lhzMarker   :: Maybe Text
      -- ^ To get the next group of maxitems hosted zones, submit another request
      -- with the value of the NextMarker element that was returned in the
      -- previous response.
    , lhzMaxItems :: Maybe Integer
      -- ^ Maximum number of hosted zones to include in the response.
    } deriving (Eq, Show, Generic)

instance IsQuery ListHostedZones where
    queryPickler = genericQueryPickler loweredQueryOptions

instance AWSRequest R53 ListHostedZones ListHostedZonesResponse where
    request = req GET "hostedzone"

data ListHostedZonesResponse = ListHostedZonesResponse
    { lhzrHostedZones :: [HostedZone]
      -- ^ A list of hosted zone descriptions.
    , lhzrIsTruncated :: !Bool
      -- ^ Whether the result list has been truncated.
    , lhzrMarker      :: Maybe Text
      -- ^ Value of the marker in the previous request.
    , lhzrNextMarker  :: Maybe Text
      -- ^ If IsTruncated is true, the hosted zone ID of the first hosted zone
      -- in the next group of maxitems hosted zones.
    , lhzrMaxItems    :: !Integer
      -- ^ Value of maxitems in the previous request.
    } deriving (Eq, Show, Generic)

instance IsXML ListHostedZonesResponse where
    xmlPickler = withNS route53NS

-- | Deletes a hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteHostedZone.html>
newtype DeleteHostedZone = DeleteHostedZone
    { dhzId :: HostedZoneId
      -- ^ Hosted Zone Id.
    } deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 DeleteHostedZone DeleteHostedZoneResponse where
    request chk = req DELETE (toText chk) ()

data DeleteHostedZoneResponse = DeleteHostedZoneResponse
    { dhzrChangeInfo :: !ChangeInfo
      -- ^ Information about the changes being made to the hosted zone.
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
    { crrsZoneId      :: !HostedZoneId
      -- ^ Hosted Zone Id.
    , crrsChangeBatch :: !ChangeBatch
      -- ^ ChangeBatch describing the record set modifications you wish to perform.
    } deriving (Eq, Show, Generic)

instance IsXML ChangeResourceRecordSets where
    xmlPickler = (xpWrap
        (ChangeResourceRecordSets "", \(ChangeResourceRecordSets _ cs) -> cs)
        (xpElem (route53Elem "ChangeBatch") $ genericXMLPickler defaultXMLOptions))
            { root = Just $ mkNName route53NS "ChangeResourceRecordSetsRequest"
            }

instance AWSRequest R53 ChangeResourceRecordSets ChangeResourceRecordSetsResponse where
    request rs@ChangeResourceRecordSets{..} =
        body POST (toText crrsZoneId <> "/rrset") rs

data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse
    { crrsrChangeInfo :: !ChangeInfo
    } deriving (Eq, Show, Generic)

instance IsXML ChangeResourceRecordSetsResponse where
    xmlPickler = withNS route53NS

-- | Lists details about all of the resource record sets in a hosted zone.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListResourceRecordSets.html>
data ListResourceRecordSets = ListResourceRecordSets
    { lrrsZoneId     :: !HostedZoneId
      -- ^ The ID of the hosted zone containing the resource
      -- records sets to be retrieved.
    , lrrsName       :: Maybe Text
      -- ^ The first name in the lexicographic ordering of domain names
      -- to be retrieved in the response to the ListResourceRecordSets request.
    , lrrsType       :: Maybe RecordType
      -- ^ The type of resource record set to begin the record listing from.
    , lrrsIdentifier :: Maybe Text
      -- ^ Weighted and latency resource record sets only: If results were
      -- truncated for a given DNS name and type, the value of SetIdentifier
      -- for the next resource record set that has the current DNS name and type.
    , lrrsMaxItems   :: Maybe Integer
      -- ^ The maximum number of resource records sets to include in the
      -- response body for this request. If the response includes more than
      -- maxitems resource record sets, the value of the IsTruncated element
      -- in the response is true, and the values of the NextRecordName and
      -- NextRecordType elements in the response identify the first resource
      -- record set in the next group of maxitems resource record sets.
    } deriving (Eq, Show, Generic)

instance IsQuery ListResourceRecordSets where
    queryPickler = genericQueryPickler loweredQueryOptions

instance AWSRequest R53 ListResourceRecordSets ListResourceRecordSetsResponse where
    request rs@ListResourceRecordSets{..} =
        req GET (toText lrrsZoneId <> "/rrset") rs

data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse
    { lrrsrResourceRecordSets   :: [ResourceRecordSet]
      -- ^ A list of resource record sets.
    , lrrsrIsTruncated          :: !Bool
      -- ^ Whether the result list has been truncated.
    , lrrsrMaxItems             :: !Integer
      -- ^ Value of maxitems in the previous request.
    , lrrsrNextRecordName       :: Maybe Text
      -- ^ If IsTruncated is true, the DNS domain name of the first resource
      -- record set in the next group of maxitems resource record sets.
    , lrrsrNextRecordType       :: Maybe RecordType
      -- ^ If IsTruncated is true, the DNS record type of the first resource
      -- record set in the next group of maxitems resource record sets.
    , lrrsrNextRecordIdentifier :: Maybe Text
      -- ^ If IsTruncated is true and results were truncated for a weighted,
      -- latency, or failover resource record set, the value of SetIdentifier
      -- for the first resource record set in the next group of maxitems resource
      -- record sets.
    } deriving (Eq, Show, Generic)

instance IsXML ListResourceRecordSetsResponse where
    xmlPickler = withNS route53NS

-- | Returns the current status of a change batch request that you
-- submitted by using ChangeResourceRecordSets.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html>
newtype GetChange = GetChange
    { gcId :: ChangeId
      -- ^ The ID of the change batch request. The value that you specify here
      -- is the value that POST ChangeResourceRecordSets returned in the Id
      -- element when you submitted the request.
    } deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 GetChange GetChangeResponse where
    request chk = req GET (toText chk) ()

data GetChangeResponse = GetChangeResponse
    { gcrChangeInfo :: !ChangeInfo
      -- ^ A description of the changes being made to the hosted zone.
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
      -- ^ A unique string that identifies the request and that allows failed
      -- CreateHealthCheck requests to be retried without the risk of executing
      -- the operation twice. You must use a unique CallerReference string every
      -- time you create a health check.
    , chcHealthCheckConfig :: !HealthCheckConfig
      -- ^ Health check configuration.
    } deriving (Eq, Show, Generic)

instance IsXML CreateHealthCheck where
    xmlPickler = withRootNS route53NS "CreateHealthCheckRequest"

instance AWSRequest R53 CreateHealthCheck CreateHealthCheckResponse where
    request = body POST "healthcheck"

data CreateHealthCheckResponse = CreateHealthCheckResponse
    { chcrHealthCheck :: !HealthCheck
      -- ^ Information about the created health check.
    } deriving (Eq, Show, Generic)

instance IsXML CreateHealthCheckResponse where
    xmlPickler = withNS route53NS

-- | Gets information about a specified health check.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetHealthCheck.html>
newtype GetHealthCheck = GetHealthCheck
    { ghcId :: HealthCheckId
      -- ^ The ID for the health check for which you want detailed information.
      -- When you created the health check, CreateHealthCheck returned the ID
      -- in the response, in the HealthCheckId element.
    } deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 GetHealthCheck GetHealthCheckResponse where
    request chk = req GET ("healthcheck/" <> toText chk) ()

data GetHealthCheckResponse = GetHealthCheckResponse
    { ghcrHealthCheck :: !HealthCheck
      -- ^ Information about a health check.
    } deriving (Eq, Show, Generic)

instance IsXML GetHealthCheckResponse where
    xmlPickler = withNS route53NS

-- | Gets a list of the health checks that are associated
-- with the current AWS account.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHealthChecks.html>
data ListHealthChecks = ListHealthChecks
    { lhcMarker   :: Maybe Text
      -- ^ If the response to a ListHealthChecks is more than one page,
      --  marker is the health check ID for the first health check on the
      -- next page of results.
    , lhcMaxItems :: Maybe Integer
      -- ^ The maximum number of HealthCheck elements you want ListHealthChecks
      -- to return on each page of the response body. If the AWS account includes
      -- more HealthCheck elements than the value of maxitems, the response is
      -- broken into pages. Each page contains the number of HealthCheck elements
      -- specified by maxitems.
    } deriving (Eq, Show, Generic)

instance IsQuery ListHealthChecks where
    queryPickler = genericQueryPickler loweredQueryOptions

instance AWSRequest R53 ListHealthChecks ListHealthChecksResponse where
    request = req GET "healthcheck"

data ListHealthChecksResponse = ListHealthChecksResponse
    { lhcrHealthChecks :: [HealthCheck]
      -- ^ A list of health check descriptions.
    , lhcrIsTruncated :: !Bool
      -- ^ Whether the result list has been truncated.
    , lhcrMarker      :: Maybe Text
      -- ^ Value of the marker in the previous request.
    , lhcrNextMarker  :: Maybe Text
      -- ^ If IsTruncated is true, the hosted zone ID of the first hosted zone
      -- in the next group of maxitems hosted zones.
    , lhcrMaxItems    :: !Integer
      -- ^ Value of maxitems in the previous request.
    } deriving (Eq, Show, Generic)

instance IsXML ListHealthChecksResponse where
    xmlPickler = withNS route53NS

-- | Deletes a health check.
--
-- Caution: Route 53 does not prevent you from deleting a health check even if
-- the health check is associated with one or more resource record sets.
-- If you delete a health check and you don't update the associated resource
-- record sets, the future status of the health check cannot be predicted and
-- may change. This will affect the routing of DNS queries for your DNS failover
-- configuration.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteHealthCheck.html>
newtype DeleteHealthCheck = DeleteHealthCheck
    { dhcId :: HealthCheckId
      -- ^ Health Check Id.
    } deriving (Eq, Show, IsString, IsByteString)

instance AWSRequest R53 DeleteHealthCheck DeleteHealthCheckResponse where
    request chk = req DELETE (toText chk) ()

data DeleteHealthCheckResponse = DeleteHealthCheckResponse
    deriving (Eq, Read, Show, Generic)

instance IsXML DeleteHealthCheckResponse where
    xmlPickler = xpEmpty $ Just route53NS
