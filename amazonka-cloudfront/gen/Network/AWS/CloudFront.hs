{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Amazon CloudFront is a web service that speeds up distribution of your
-- static and dynamic web content, for example, .html, .css, .php, image,
-- and media files, to end users. CloudFront delivers your content through
-- a worldwide network of edge locations. When an end user requests content
-- that you\'re serving with CloudFront, the user is routed to the edge
-- location that provides the lowest latency, so content is delivered with
-- the best possible performance. If the content is already in that edge
-- location, CloudFront delivers it immediately. If the content is not
-- currently in that edge location, CloudFront retrieves it from an Amazon
-- S3 bucket or an HTTP server (for example, a web server) that you have
-- identified as the source for the definitive version of your content.
module Network.AWS.CloudFront
    ( module Export
    ) where

import           Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity    as Export
import           Network.AWS.CloudFront.CreateDistribution                      as Export
import           Network.AWS.CloudFront.CreateInvalidation                      as Export
import           Network.AWS.CloudFront.CreateStreamingDistribution             as Export
import           Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity    as Export
import           Network.AWS.CloudFront.DeleteDistribution                      as Export
import           Network.AWS.CloudFront.DeleteStreamingDistribution             as Export
import           Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity       as Export
import           Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig as Export
import           Network.AWS.CloudFront.GetDistribution                         as Export
import           Network.AWS.CloudFront.GetDistributionConfig                   as Export
import           Network.AWS.CloudFront.GetInvalidation                         as Export
import           Network.AWS.CloudFront.GetStreamingDistribution                as Export
import           Network.AWS.CloudFront.GetStreamingDistributionConfig          as Export
import           Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities    as Export
import           Network.AWS.CloudFront.ListDistributions                       as Export
import           Network.AWS.CloudFront.ListInvalidations                       as Export
import           Network.AWS.CloudFront.ListStreamingDistributions              as Export
import           Network.AWS.CloudFront.Types                                   as Export
import           Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity    as Export
import           Network.AWS.CloudFront.UpdateDistribution                      as Export
import           Network.AWS.CloudFront.UpdateStreamingDistribution             as Export
import           Network.AWS.CloudFront.Waiters                                 as Export
