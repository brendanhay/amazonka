-- Module      : Network.AWS.CloudFront
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
module Network.AWS.CloudFront
    ( module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity2014_05_31
    , module Network.AWS.CloudFront.CreateDistribution2014_05_31
    , module Network.AWS.CloudFront.CreateInvalidation2014_05_31
    , module Network.AWS.CloudFront.CreateStreamingDistribution2014_05_31
    , module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity2014_05_31
    , module Network.AWS.CloudFront.DeleteDistribution2014_05_31
    , module Network.AWS.CloudFront.DeleteStreamingDistribution2014_05_31
    , module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity2014_05_31
    , module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig2014_05_31
    , module Network.AWS.CloudFront.GetDistribution2014_05_31
    , module Network.AWS.CloudFront.GetDistributionConfig2014_05_31
    , module Network.AWS.CloudFront.GetInvalidation2014_05_31
    , module Network.AWS.CloudFront.GetStreamingDistribution2014_05_31
    , module Network.AWS.CloudFront.GetStreamingDistributionConfig2014_05_31
    , module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities2014_05_31
    , module Network.AWS.CloudFront.ListDistributions2014_05_31
    , module Network.AWS.CloudFront.ListInvalidations2014_05_31
    , module Network.AWS.CloudFront.ListStreamingDistributions2014_05_31
    , module Network.AWS.CloudFront.Types
    , module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity2014_05_31
    , module Network.AWS.CloudFront.UpdateDistribution2014_05_31
    , module Network.AWS.CloudFront.UpdateStreamingDistribution2014_05_31
    ) where

import Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.CreateDistribution2014_05_31
import Network.AWS.CloudFront.CreateInvalidation2014_05_31
import Network.AWS.CloudFront.CreateStreamingDistribution2014_05_31
import Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.DeleteDistribution2014_05_31
import Network.AWS.CloudFront.DeleteStreamingDistribution2014_05_31
import Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig2014_05_31
import Network.AWS.CloudFront.GetDistribution2014_05_31
import Network.AWS.CloudFront.GetDistributionConfig2014_05_31
import Network.AWS.CloudFront.GetInvalidation2014_05_31
import Network.AWS.CloudFront.GetStreamingDistribution2014_05_31
import Network.AWS.CloudFront.GetStreamingDistributionConfig2014_05_31
import Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities2014_05_31
import Network.AWS.CloudFront.ListDistributions2014_05_31
import Network.AWS.CloudFront.ListInvalidations2014_05_31
import Network.AWS.CloudFront.ListStreamingDistributions2014_05_31
import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.UpdateDistribution2014_05_31
import Network.AWS.CloudFront.UpdateStreamingDistribution2014_05_31
