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
    ( module Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.CreateDistribution2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.CreateInvalidation2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.DeleteDistribution2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.GetDistribution2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.GetInvalidation2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.Lenses
    , module Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.ListDistributions2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.ListInvalidations2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.Types
    , module Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.UpdateDistribution2014_05_31
    , module Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution2014_05_31
    ) where

import Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.V2014_05_31.CreateDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.CreateInvalidation2014_05_31
import Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.V2014_05_31.DeleteDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetInvalidation2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig2014_05_31
import Network.AWS.CloudFront.V2014_05_31.Lenses
import Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities2014_05_31
import Network.AWS.CloudFront.V2014_05_31.ListDistributions2014_05_31
import Network.AWS.CloudFront.V2014_05_31.ListInvalidations2014_05_31
import Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions2014_05_31
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.V2014_05_31.UpdateDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution2014_05_31
