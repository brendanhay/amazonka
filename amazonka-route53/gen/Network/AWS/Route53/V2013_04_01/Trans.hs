{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.Route53.V2013_04_01.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Route 53 is a scalable Domain Name System (DNS) web service. It
-- provides secure and reliable routing to your infrastructure that uses
-- Amazon Web Services (AWS) products, such as Amazon Elastic Compute Cloud
-- (Amazon EC2), Elastic Load Balancing, or Amazon Simple Storage Service
-- (Amazon S3). You can also use Amazon Route 53 to route users to your
-- infrastructure outside of AWS.
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.Route53.V2013_04_01.Trans
    (
    -- * ChangeResourceRecordSets
    -- $ChangeResourceRecordSets
      changeResourceRecordSets

    -- * ChangeTagsForResource
    -- $ChangeTagsForResource
    , changeTagsForResource

    -- * CreateHealthCheck
    -- $CreateHealthCheck
    , createHealthCheck

    -- * CreateHostedZone
    -- $CreateHostedZone
    , createHostedZone

    -- * DeleteHealthCheck
    -- $DeleteHealthCheck
    , deleteHealthCheck

    -- * DeleteHostedZone
    -- $DeleteHostedZone
    , deleteHostedZone

    -- * GetChange
    -- $GetChange
    , getChange

    -- * GetCheckerIpRanges
    -- $GetCheckerIpRanges
    , getCheckerIpRanges

    -- * GetGeoLocation
    -- $GetGeoLocation
    , getGeoLocation

    -- * GetHealthCheck
    -- $GetHealthCheck
    , getHealthCheck

    -- * GetHealthCheckCount
    -- $GetHealthCheckCount
    , getHealthCheckCount

    -- * GetHostedZone
    -- $GetHostedZone
    , getHostedZone

    -- * ListGeoLocations
    -- $ListGeoLocations
    , listGeoLocations

    -- * ListHealthChecks
    -- $ListHealthChecks
    , listHealthChecks

    -- * ListHostedZones
    -- $ListHostedZones
    , listHostedZones

    -- * ListResourceRecordSets
    -- $ListResourceRecordSets
    , listResourceRecordSets

    -- * ListTagsForResource
    -- $ListTagsForResource
    , listTagsForResource

    -- * ListTagsForResources
    -- $ListTagsForResources
    , listTagsForResources

    -- * UpdateHealthCheck
    -- $UpdateHealthCheck
    , updateHealthCheck

    -- * Re-exported
    , module AWS
    , module Network.AWS.Route53.V2013_04_01
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.Route53.V2013_04_01

-- $ChangeResourceRecordSets
-- Use this action to create or change your authoritative DNS information. To
-- use this action, send a POST request to the 2013-04-01/hostedzone/hosted
-- Zone ID/rrset resource. The request body must include an XML document with
-- a ChangeResourceRecordSetsRequest element. Changes are a list of change
-- items and are considered transactional. For more information on
-- transactional changes, also known as change batches, see Creating,
-- Changing, and Deleting Resource Record Sets Using the Route 53 API in the
-- Amazon Route 53 Developer Guide. Due to the nature of transactional
-- changes, you cannot delete the same resource record set more than once in a
-- single change batch. If you attempt to delete the same change batch more
-- than once, Route 53 returns an InvalidChangeBatch error. In response to a
-- ChangeResourceRecordSets request, your DNS data is changed on all Route 53
-- DNS servers. Initially, the status of a change is PENDING. This means the
-- change has not yet propagated to all the authoritative Route 53 DNS
-- servers. When the change is propagated to all hosts, the change returns a
-- status of INSYNC. Note the following limitations on a
-- ChangeResourceRecordSets request: - A request cannot contain more than 100
-- Change elements. - A request cannot contain more than 1000 ResourceRecord
-- elements. The sum of the number of characters (including spaces) in all
-- Value elements in a request cannot exceed 32,000 characters.
--
-- See: 'Network.AWS.Route53.V2013_04_01.ChangeResourceRecordSets'

changeResourceRecordSets :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
                         => Text -- ^ 'crrsHostedZoneId'
                         -> ChangeBatch -- ^ 'crrsChangeBatch'
                         -> State ChangeResourceRecordSets a
                         -> m ChangeResourceRecordSetsResponse
changeResourceRecordSets p1 p2 s =
    send $ (mkChangeResourceRecordSets p1 p2) &~ s

-- $ChangeTagsForResource
-- See: 'Network.AWS.Route53.V2013_04_01.ChangeTagsForResource'

changeTagsForResource :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
                      => TagResourceType -- ^ 'ctfrResourceType'
                      -> Text -- ^ 'ctfrResourceId'
                      -> State ChangeTagsForResource a
                      -> m ChangeTagsForResourceResponse
changeTagsForResource p1 p2 s =
    send $ (mkChangeTagsForResource p1 p2) &~ s

-- $CreateHealthCheck
-- This action creates a new health check. To create a new health check, send
-- a POST request to the 2013-04-01/healthcheck resource. The request body
-- must include an XML document with a CreateHealthCheckRequest element. The
-- response returns the CreateHealthCheckResponse element that contains
-- metadata about the health check.
--
-- See: 'Network.AWS.Route53.V2013_04_01.CreateHealthCheck'

createHealthCheck :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => Text -- ^ 'chcCallerReference'
                  -> HealthCheckConfig -- ^ 'chcHealthCheckConfig'
                  -> State CreateHealthCheck a
                  -> m CreateHealthCheckResponse
createHealthCheck p1 p2 s =
    send $ (mkCreateHealthCheck p1 p2) &~ s

-- $CreateHostedZone
-- This action creates a new hosted zone. To create a new hosted zone, send a
-- POST request to the 2013-04-01/hostedzone resource. The request body must
-- include an XML document with a CreateHostedZoneRequest element. The
-- response returns the CreateHostedZoneResponse element that contains
-- metadata about the hosted zone. Route 53 automatically creates a default
-- SOA record and four NS records for the zone. The NS records in the hosted
-- zone are the name servers you give your registrar to delegate your domain
-- to. For more information about SOA and NS records, see NS and SOA Records
-- that Route 53 Creates for a Hosted Zone in the Amazon Route 53 Developer
-- Guide. When you create a zone, its initial status is PENDING. This means
-- that it is not yet available on all DNS servers. The status of the zone
-- changes to INSYNC when the NS and SOA records are available on all Route 53
-- DNS servers.
--
-- See: 'Network.AWS.Route53.V2013_04_01.CreateHostedZone'

createHostedZone :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
                 => Text -- ^ 'chzName'
                 -> Text -- ^ 'chzCallerReference'
                 -> State CreateHostedZone a
                 -> m CreateHostedZoneResponse
createHostedZone p1 p2 s =
    send $ (mkCreateHostedZone p1 p2) &~ s

-- $DeleteHealthCheck
-- This action deletes a health check. To delete a health check, send a DELETE
-- request to the 2013-04-01/healthcheck/health check ID resource. You can
-- delete a health check only if there are no resource record sets associated
-- with this health check. If resource record sets are associated with this
-- health check, you must disassociate them before you can delete your health
-- check. If you try to delete a health check that is associated with resource
-- record sets, Route 53 will deny your request with a HealthCheckInUse error.
-- For information about disassociating the records from your health check,
-- see ChangeResourceRecordSets.
--
-- See: 'Network.AWS.Route53.V2013_04_01.DeleteHealthCheck'

deleteHealthCheck :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => Text -- ^ 'dhcHealthCheckId'
                  -> State DeleteHealthCheck a
                  -> m DeleteHealthCheckResponse
deleteHealthCheck p1 s =
    send $ (mkDeleteHealthCheck p1) &~ s

-- $DeleteHostedZone
-- This action deletes a hosted zone. To delete a hosted zone, send a DELETE
-- request to the 2013-04-01/hostedzone/hosted zone ID resource. For more
-- information about deleting a hosted zone, see Deleting a Hosted Zone in the
-- Amazon Route 53 Developer Guide. You can delete a hosted zone only if there
-- are no resource record sets other than the default SOA record and NS
-- resource record sets. If your hosted zone contains other resource record
-- sets, you must delete them before you can delete your hosted zone. If you
-- try to delete a hosted zone that contains other resource record sets, Route
-- 53 will deny your request with a HostedZoneNotEmpty error. For information
-- about deleting records from your hosted zone, see ChangeResourceRecordSets.
--
-- See: 'Network.AWS.Route53.V2013_04_01.DeleteHostedZone'

deleteHostedZone :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
                 => Text -- ^ 'dhzId'
                 -> State DeleteHostedZone a
                 -> m DeleteHostedZoneResponse
deleteHostedZone p1 s =
    send $ (mkDeleteHostedZone p1) &~ s

-- $GetChange
-- This action returns the current status of a change batch request. The
-- status is one of the following values: - PENDING indicates that the changes
-- in this request have not replicated to all Route 53 DNS servers. This is
-- the initial status of all change batch requests. - INSYNC indicates that
-- the changes have replicated to all Amazon Route 53 DNS servers.
--
-- See: 'Network.AWS.Route53.V2013_04_01.GetChange'

getChange :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
          => Text -- ^ 'gcId'
          -> State GetChange a
          -> m GetChangeResponse
getChange p1 s =
    send $ (mkGetChange p1) &~ s

-- $GetCheckerIpRanges
-- To retrieve a list of the IP ranges used by Amazon Route 53 health checkers
-- to check the health of your resources, send a GET request to the
-- 2013-04-01/checkeripranges resource. You can use these IP addresses to
-- configure router and firewall rules to allow health checkers to check the
-- health of your resources.
--
-- See: 'Network.AWS.Route53.V2013_04_01.GetCheckerIpRanges'

getCheckerIpRanges :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
                   => State GetCheckerIpRanges a
                   -> m GetCheckerIpRangesResponse
getCheckerIpRanges s =
    send (mkGetCheckerIpRanges &~ s)

-- $GetGeoLocation
-- See: 'Network.AWS.Route53.V2013_04_01.GetGeoLocation'

getGeoLocation :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => State GetGeoLocation a
               -> m GetGeoLocationResponse
getGeoLocation s =
    send (mkGetGeoLocation &~ s)

-- $GetHealthCheck
-- To retrieve the health check, send a GET request to the
-- 2013-04-01/healthcheck/health check ID resource.
--
-- See: 'Network.AWS.Route53.V2013_04_01.GetHealthCheck'

getHealthCheck :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
               => Text -- ^ 'ghcHealthCheckId'
               -> State GetHealthCheck a
               -> m GetHealthCheckResponse
getHealthCheck p1 s =
    send $ (mkGetHealthCheck p1) &~ s

-- $GetHealthCheckCount
-- To retrieve a count of all your health checks, send a GET request to the
-- 2013-04-01/healthcheckcount resource.
--
-- See: 'Network.AWS.Route53.V2013_04_01.GetHealthCheckCount'

getHealthCheckCount :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
                    => State GetHealthCheckCount a
                    -> m GetHealthCheckCountResponse
getHealthCheckCount s =
    send (mkGetHealthCheckCount &~ s)

-- $GetHostedZone
-- To retrieve the delegation set for a hosted zone, send a GET request to the
-- 2013-04-01/hostedzone/hosted zone ID resource. The delegation set is the
-- four Route 53 name servers that were assigned to the hosted zone when you
-- created it.
--
-- See: 'Network.AWS.Route53.V2013_04_01.GetHostedZone'

getHostedZone :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
              => Text -- ^ 'ghzId'
              -> State GetHostedZone a
              -> m GetHostedZoneResponse
getHostedZone p1 s =
    send $ (mkGetHostedZone p1) &~ s

-- $ListGeoLocations
-- See: 'Network.AWS.Route53.V2013_04_01.ListGeoLocations'

listGeoLocations :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
                 => State ListGeoLocations a
                 -> m ListGeoLocationsResponse
listGeoLocations s =
    send (mkListGeoLocations &~ s)

-- $ListHealthChecks
-- To retrieve a list of your health checks, send a GET request to the
-- 2013-04-01/healthcheck resource. The response to this request includes a
-- HealthChecks element with zero, one, or multiple HealthCheck child
-- elements. By default, the list of health checks is displayed on a single
-- page. You can control the length of the page that is displayed by using the
-- MaxItems parameter. You can use the Marker parameter to control the health
-- check that the list begins with. Amazon Route 53 returns a maximum of 100
-- items. If you set MaxItems to a value greater than 100, Amazon Route 53
-- returns only the first 100.
--
-- See: 'Network.AWS.Route53.V2013_04_01.ListHealthChecks'

listHealthChecks :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env (ResumableSource m)
                    )
                 => State ListHealthChecks a
                 -> ResumableSource m ListHealthChecksResponse
listHealthChecks s =
    paginate (mkListHealthChecks &~ s)

-- $ListHostedZones
-- To retrieve a list of your hosted zones, send a GET request to the
-- 2013-04-01/hostedzone resource. The response to this request includes a
-- HostedZones element with zero, one, or multiple HostedZone child elements.
-- By default, the list of hosted zones is displayed on a single page. You can
-- control the length of the page that is displayed by using the MaxItems
-- parameter. You can use the Marker parameter to control the hosted zone that
-- the list begins with. Amazon Route 53 returns a maximum of 100 items. If
-- you set MaxItems to a value greater than 100, Amazon Route 53 returns only
-- the first 100.
--
-- See: 'Network.AWS.Route53.V2013_04_01.ListHostedZones'

listHostedZones :: ( MonadCatch m
                   , MonadResource m
                   , MonadError AWS.Error m
                   , MonadReader Env (ResumableSource m)
                   )
                => State ListHostedZones a
                -> ResumableSource m ListHostedZonesResponse
listHostedZones s =
    paginate (mkListHostedZones &~ s)

-- $ListResourceRecordSets
-- Imagine all the resource record sets in a zone listed out in front of you.
-- Imagine them sorted lexicographically first by DNS name (with the labels
-- reversed, like "com.amazon.www" for example), and secondarily,
-- lexicographically by record type. This operation retrieves at most MaxItems
-- resource record sets from this list, in order, starting at a position
-- specified by the Name and Type arguments: If both Name and Type are
-- omitted, this means start the results at the first RRSET in the HostedZone.
-- If Name is specified but Type is omitted, this means start the results at
-- the first RRSET in the list whose name is greater than or equal to Name. If
-- both Name and Type are specified, this means start the results at the first
-- RRSET in the list whose name is greater than or equal to Name and whose
-- type is greater than or equal to Type. It is an error to specify the Type
-- but not the Name. Use ListResourceRecordSets to retrieve a single known
-- record set by specifying the record set's name and type, and setting
-- MaxItems = 1 To retrieve all the records in a HostedZone, first pause any
-- processes making calls to ChangeResourceRecordSets. Initially call
-- ListResourceRecordSets without a Name and Type to get the first page of
-- record sets. For subsequent calls, set Name and Type to the NextName and
-- NextType values returned by the previous response. In the presence of
-- concurrent ChangeResourceRecordSets calls, there is no consistency of
-- results across calls to ListResourceRecordSets. The only way to get a
-- consistent multi-page snapshot of all RRSETs in a zone is to stop making
-- changes while pagination is in progress. However, the results from
-- ListResourceRecordSets are consistent within a page. If MakeChange calls
-- are taking place concurrently, the result of each one will either be
-- completely visible in your results or not at all. You will not see partial
-- changes, or changes that do not ultimately succeed. (This follows from the
-- fact that MakeChange is atomic) The results from ListResourceRecordSets are
-- strongly consistent with ChangeResourceRecordSets. To be precise, if a
-- single process makes a call to ChangeResourceRecordSets and receives a
-- successful response, the effects of that change will be visible in a
-- subsequent call to ListResourceRecordSets by that process.
--
-- See: 'Network.AWS.Route53.V2013_04_01.ListResourceRecordSets'

listResourceRecordSets :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env (ResumableSource m)
                          )
                       => Text -- ^ 'lrrsHostedZoneId'
                       -> State ListResourceRecordSets a
                       -> ResumableSource m ListResourceRecordSetsResponse
listResourceRecordSets p1 s =
    paginate $ (mkListResourceRecordSets p1) &~ s

-- $ListTagsForResource
-- See: 'Network.AWS.Route53.V2013_04_01.ListTagsForResource'

listTagsForResource :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
                    => TagResourceType -- ^ 'ltfrResourceType'
                    -> Text -- ^ 'ltfrResourceId'
                    -> State ListTagsForResource a
                    -> m ListTagsForResourceResponse
listTagsForResource p1 p2 s =
    send $ (mkListTagsForResource p1 p2) &~ s

-- $ListTagsForResources
-- See: 'Network.AWS.Route53.V2013_04_01.ListTagsForResources'

listTagsForResources :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
                     => TagResourceType -- ^ 'ltfr1ResourceType'
                     -> List1 Text -- ^ 'ltfr1ResourceIds'
                     -> State ListTagsForResources a
                     -> m ListTagsForResourcesResponse
listTagsForResources p1 p2 s =
    send $ (mkListTagsForResources p1 p2) &~ s

-- $UpdateHealthCheck
-- This action updates an existing health check. To update a health check,
-- send a POST request to the 2013-05-27/healthcheck/health check ID resource.
-- The request body must include an XML document with an
-- UpdateHealthCheckRequest element. The response returns an
-- UpdateHealthCheckResponse element, which contains metadata about the health
-- check.
--
-- See: 'Network.AWS.Route53.V2013_04_01.UpdateHealthCheck'

updateHealthCheck :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
                  => Text -- ^ 'uhcHealthCheckId'
                  -> State UpdateHealthCheck a
                  -> m UpdateHealthCheckResponse
updateHealthCheck p1 s =
    send $ (mkUpdateHealthCheck p1) &~ s
