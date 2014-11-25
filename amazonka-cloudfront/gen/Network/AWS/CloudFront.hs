-- Module      : Network.AWS.CloudFront
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudFront is a web service that speeds up distribution of your static
-- and dynamic web content, for example, .html, .css, .php, image, and media
-- files, to end users. CloudFront delivers your content through a worldwide
-- network of edge locations. When an end user requests content that you're
-- serving with CloudFront, the user is routed to the edge location that
-- provides the lowest latency, so content is delivered with the best possible
-- performance. If the content is already in that edge location, CloudFront
-- delivers it immediately. If the content is not currently in that edge
-- location, CloudFront retrieves it from an Amazon S3 bucket or an HTTP server
-- (for example, a web server) that you have identified as the source for the
-- definitive version of your content.
module Network.AWS.CloudFront
    ( module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.CreateDistribution
    , module Network.AWS.CloudFront.CreateInvalidation
    , module Network.AWS.CloudFront.CreateStreamingDistribution
    , module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.DeleteDistribution
    , module Network.AWS.CloudFront.DeleteStreamingDistribution
    , module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
    , module Network.AWS.CloudFront.GetDistribution
    , module Network.AWS.CloudFront.GetDistributionConfig
    , module Network.AWS.CloudFront.GetInvalidation
    , module Network.AWS.CloudFront.GetStreamingDistribution
    , module Network.AWS.CloudFront.GetStreamingDistributionConfig
    , module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
    , module Network.AWS.CloudFront.ListDistributions
    , module Network.AWS.CloudFront.ListInvalidations
    , module Network.AWS.CloudFront.ListStreamingDistributions
    , module Network.AWS.CloudFront.Types
    , module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.UpdateDistribution
    , module Network.AWS.CloudFront.UpdateStreamingDistribution
    ) where

import Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.CreateDistribution
import Network.AWS.CloudFront.CreateInvalidation
import Network.AWS.CloudFront.CreateStreamingDistribution
import Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.DeleteDistribution
import Network.AWS.CloudFront.DeleteStreamingDistribution
import Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
import Network.AWS.CloudFront.GetDistribution
import Network.AWS.CloudFront.GetDistributionConfig
import Network.AWS.CloudFront.GetInvalidation
import Network.AWS.CloudFront.GetStreamingDistribution
import Network.AWS.CloudFront.GetStreamingDistributionConfig
import Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
import Network.AWS.CloudFront.ListDistributions
import Network.AWS.CloudFront.ListInvalidations
import Network.AWS.CloudFront.ListStreamingDistributions
import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.UpdateDistribution
import Network.AWS.CloudFront.UpdateStreamingDistribution
