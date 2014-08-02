-- Module      : Network.AWS.CloudFront.V2014_05_31
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudFront is a web service that speeds up distribution of your
-- static and dynamic web content, for example, .html, .css, .php, image, and
-- media files, to end users. CloudFront delivers your content through a
-- worldwide network of edge locations. When an end user requests content that
-- you're serving with CloudFront, the user is routed to the edge location
-- that provides the lowest latency, so content is delivered with the best
-- possible performance. If the content is already in that edge location,
-- CloudFront delivers it immediately. If the content is not currently in that
-- edge location, CloudFront retrieves it from an Amazon S3 bucket or an HTTP
-- server (for example, a web server) that you have identified as the source
-- for the definitive version of your content.
module Network.AWS.CloudFront.V2014_05_31 (module Export) where

import Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity as Export
import Network.AWS.CloudFront.V2014_05_31.CreateDistribution as Export
import Network.AWS.CloudFront.V2014_05_31.CreateInvalidation as Export
import Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution as Export
import Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity as Export
import Network.AWS.CloudFront.V2014_05_31.DeleteDistribution as Export
import Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution as Export
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity as Export
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig as Export
import Network.AWS.CloudFront.V2014_05_31.GetDistribution as Export
import Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig as Export
import Network.AWS.CloudFront.V2014_05_31.GetInvalidation as Export
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution as Export
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig as Export
import Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities as Export
import Network.AWS.CloudFront.V2014_05_31.ListDistributions as Export
import Network.AWS.CloudFront.V2014_05_31.ListInvalidations as Export
import Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions as Export
import Network.AWS.CloudFront.V2014_05_31.Types as Export
import Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity as Export
import Network.AWS.CloudFront.V2014_05_31.UpdateDistribution as Export
import Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution as Export
