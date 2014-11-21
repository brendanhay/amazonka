-- Module      : Network.AWS.Route53
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
module Network.AWS.Route53
    ( module Network.AWS.Route53.AssociateVPCWithHostedZone
    , module Network.AWS.Route53.ChangeResourceRecordSets
    , module Network.AWS.Route53.ChangeTagsForResource
    , module Network.AWS.Route53.CreateHealthCheck
    , module Network.AWS.Route53.CreateHostedZone
    , module Network.AWS.Route53.CreateReusableDelegationSet
    , module Network.AWS.Route53.DeleteHealthCheck
    , module Network.AWS.Route53.DeleteHostedZone
    , module Network.AWS.Route53.DeleteReusableDelegationSet
    , module Network.AWS.Route53.DisassociateVPCFromHostedZone
    , module Network.AWS.Route53.GetChange
    , module Network.AWS.Route53.GetCheckerIpRanges
    , module Network.AWS.Route53.GetGeoLocation
    , module Network.AWS.Route53.GetHealthCheck
    , module Network.AWS.Route53.GetHealthCheckCount
    , module Network.AWS.Route53.GetHealthCheckLastFailureReason
    , module Network.AWS.Route53.GetHealthCheckStatus
    , module Network.AWS.Route53.GetHostedZone
    , module Network.AWS.Route53.GetReusableDelegationSet
    , module Network.AWS.Route53.ListGeoLocations
    , module Network.AWS.Route53.ListHealthChecks
    , module Network.AWS.Route53.ListHostedZones
    , module Network.AWS.Route53.ListResourceRecordSets
    , module Network.AWS.Route53.ListReusableDelegationSets
    , module Network.AWS.Route53.ListTagsForResource
    , module Network.AWS.Route53.ListTagsForResources
    , module Network.AWS.Route53.Types
    , module Network.AWS.Route53.UpdateHealthCheck
    ) where

import Network.AWS.Route53.AssociateVPCWithHostedZone
import Network.AWS.Route53.ChangeResourceRecordSets
import Network.AWS.Route53.ChangeTagsForResource
import Network.AWS.Route53.CreateHealthCheck
import Network.AWS.Route53.CreateHostedZone
import Network.AWS.Route53.CreateReusableDelegationSet
import Network.AWS.Route53.DeleteHealthCheck
import Network.AWS.Route53.DeleteHostedZone
import Network.AWS.Route53.DeleteReusableDelegationSet
import Network.AWS.Route53.DisassociateVPCFromHostedZone
import Network.AWS.Route53.GetChange
import Network.AWS.Route53.GetCheckerIpRanges
import Network.AWS.Route53.GetGeoLocation
import Network.AWS.Route53.GetHealthCheck
import Network.AWS.Route53.GetHealthCheckCount
import Network.AWS.Route53.GetHealthCheckLastFailureReason
import Network.AWS.Route53.GetHealthCheckStatus
import Network.AWS.Route53.GetHostedZone
import Network.AWS.Route53.GetReusableDelegationSet
import Network.AWS.Route53.ListGeoLocations
import Network.AWS.Route53.ListHealthChecks
import Network.AWS.Route53.ListHostedZones
import Network.AWS.Route53.ListResourceRecordSets
import Network.AWS.Route53.ListReusableDelegationSets
import Network.AWS.Route53.ListTagsForResource
import Network.AWS.Route53.ListTagsForResources
import Network.AWS.Route53.Types
import Network.AWS.Route53.UpdateHealthCheck
