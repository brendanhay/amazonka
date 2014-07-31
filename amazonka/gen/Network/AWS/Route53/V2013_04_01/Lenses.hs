{-# LANGUAGE TemplateHaskell             #-}

-- Module      : Network.AWS.Route53.V2013_04_01.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V2013_04_01.Lenses where

import Control.Lens.TH
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Route53.V2013_04_01.GetCheckerIpRanges
import Network.AWS.Route53.V2013_04_01.ListTagsForResource
import Network.AWS.Route53.V2013_04_01.GetChange
import Network.AWS.Route53.V2013_04_01.ChangeResourceRecordSets
import Network.AWS.Route53.V2013_04_01.DeleteHealthCheck
import Network.AWS.Route53.V2013_04_01.UpdateHealthCheck
import Network.AWS.Route53.V2013_04_01.CreateHostedZone
import Network.AWS.Route53.V2013_04_01.CreateHealthCheck
import Network.AWS.Route53.V2013_04_01.ChangeTagsForResource
import Network.AWS.Route53.V2013_04_01.ListHostedZones
import Network.AWS.Route53.V2013_04_01.GetHostedZone
import Network.AWS.Route53.V2013_04_01.GetHealthCheck
import Network.AWS.Route53.V2013_04_01.ListResourceRecordSets
import Network.AWS.Route53.V2013_04_01.GetHealthCheckCount
import Network.AWS.Route53.V2013_04_01.ListHealthChecks
import Network.AWS.Route53.V2013_04_01.DeleteHostedZone
import Network.AWS.Route53.V2013_04_01.ListTagsForResources

-- Newtypes
makeIso ''DelegationSet
makeIso ''HostedZoneConfig
makeIso ''ResourceRecord

-- Products
makeLenses ''AliasTarget
makeLenses ''Change
makeLenses ''ChangeBatch
makeLenses ''ChangeInfo
makeLenses ''HealthCheck
makeLenses ''HealthCheckConfig
makeLenses ''HostedZone
makeLenses ''ResourceRecordSet
makeLenses ''ResourceTagSet
makeLenses ''Tag

-- Requests
makeLenses ''GetCheckerIpRanges
makeLenses ''ListTagsForResource
makeLenses ''GetChange
makeLenses ''ChangeResourceRecordSets
makeLenses ''DeleteHealthCheck
makeLenses ''UpdateHealthCheck
makeLenses ''CreateHostedZone
makeLenses ''CreateHealthCheck
makeLenses ''ChangeTagsForResource
makeLenses ''ListHostedZones
makeLenses ''GetHostedZone
makeLenses ''GetHealthCheck
makeLenses ''ListResourceRecordSets
makeLenses ''GetHealthCheckCount
makeLenses ''ListHealthChecks
makeLenses ''DeleteHostedZone
makeLenses ''ListTagsForResources

-- Responses
makeLenses ''GetCheckerIpRangesResponse
makeLenses ''ListTagsForResourceResponse
makeLenses ''GetChangeResponse
makeLenses ''ChangeResourceRecordSetsResponse
makeLenses ''DeleteHealthCheckResponse
makeLenses ''UpdateHealthCheckResponse
makeLenses ''CreateHostedZoneResponse
makeLenses ''CreateHealthCheckResponse
makeLenses ''ChangeTagsForResourceResponse
makeLenses ''ListHostedZonesResponse
makeLenses ''GetHostedZoneResponse
makeLenses ''GetHealthCheckResponse
makeLenses ''ListResourceRecordSetsResponse
makeLenses ''GetHealthCheckCountResponse
makeLenses ''ListHealthChecksResponse
makeLenses ''DeleteHostedZoneResponse
makeLenses ''ListTagsForResourcesResponse
