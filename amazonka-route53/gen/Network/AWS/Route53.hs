-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Route 53 is a scalable Domain Name System (DNS) web service. It
-- provides secure and reliable routing to your infrastructure that uses
-- Amazon Web Services (AWS) products, such as Amazon Elastic Compute Cloud
-- (Amazon EC2), Elastic Load Balancing, or Amazon Simple Storage Service
-- (Amazon S3). You can also use Amazon Route 53 to route users to your
-- infrastructure outside of AWS.
module Network.AWS.Route53
    ( module Export
    ) where

import Network.AWS.Route53.AssociateVPCWithHostedZone as Export
import Network.AWS.Route53.ChangeResourceRecordSets as Export
import Network.AWS.Route53.ChangeTagsForResource as Export
import Network.AWS.Route53.CreateHealthCheck as Export
import Network.AWS.Route53.CreateHostedZone as Export
import Network.AWS.Route53.CreateReusableDelegationSet as Export
import Network.AWS.Route53.DeleteHealthCheck as Export
import Network.AWS.Route53.DeleteHostedZone as Export
import Network.AWS.Route53.DeleteReusableDelegationSet as Export
import Network.AWS.Route53.DisassociateVPCFromHostedZone as Export
import Network.AWS.Route53.GetChange as Export
import Network.AWS.Route53.GetCheckerIPRanges as Export
import Network.AWS.Route53.GetGeoLocation as Export
import Network.AWS.Route53.GetHealthCheck as Export
import Network.AWS.Route53.GetHealthCheckCount as Export
import Network.AWS.Route53.GetHealthCheckLastFailureReason as Export
import Network.AWS.Route53.GetHealthCheckStatus as Export
import Network.AWS.Route53.GetHostedZone as Export
import Network.AWS.Route53.GetHostedZoneCount as Export
import Network.AWS.Route53.GetReusableDelegationSet as Export
import Network.AWS.Route53.ListGeoLocations as Export
import Network.AWS.Route53.ListHealthChecks as Export
import Network.AWS.Route53.ListHostedZones as Export
import Network.AWS.Route53.ListHostedZonesByName as Export
import Network.AWS.Route53.ListResourceRecordSets as Export
import Network.AWS.Route53.ListReusableDelegationSets as Export
import Network.AWS.Route53.ListTagsForResource as Export
import Network.AWS.Route53.ListTagsForResources as Export
import Network.AWS.Route53.Types as Export
import Network.AWS.Route53.UpdateHealthCheck as Export
import Network.AWS.Route53.UpdateHostedZoneComment as Export
import Network.AWS.Route53.Waiters as Export
import Network.AWS.Route53.Internal as Export
