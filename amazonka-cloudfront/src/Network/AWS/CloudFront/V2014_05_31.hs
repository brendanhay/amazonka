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
module Network.AWS.CloudFront.V2014_05_31
    ( module Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.V2014_05_31.CreateDistribution
    , module Network.AWS.CloudFront.V2014_05_31.CreateInvalidation
    , module Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution
    , module Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.V2014_05_31.DeleteDistribution
    , module Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution
    , module Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig
    , module Network.AWS.CloudFront.V2014_05_31.GetDistribution
    , module Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig
    , module Network.AWS.CloudFront.V2014_05_31.GetInvalidation
    , module Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution
    , module Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig
    , module Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities
    , module Network.AWS.CloudFront.V2014_05_31.ListDistributions
    , module Network.AWS.CloudFront.V2014_05_31.ListInvalidations
    , module Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions
    , module Network.AWS.CloudFront.V2014_05_31.Types
    , module Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.V2014_05_31.UpdateDistribution
    , module Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution
    ) where

import Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.V2014_05_31.CreateDistribution
import Network.AWS.CloudFront.V2014_05_31.CreateInvalidation
import Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution
import Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.V2014_05_31.DeleteDistribution
import Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig
import Network.AWS.CloudFront.V2014_05_31.GetDistribution
import Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig
import Network.AWS.CloudFront.V2014_05_31.GetInvalidation
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig
import Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities
import Network.AWS.CloudFront.V2014_05_31.ListDistributions
import Network.AWS.CloudFront.V2014_05_31.ListInvalidations
import Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.V2014_05_31.UpdateDistribution
import Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution
