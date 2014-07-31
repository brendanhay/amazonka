{-# LANGUAGE TemplateHaskell             #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudFront.V2014_05_31.Lenses where

import Control.Lens.TH
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.CreateDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig2014_05_31
import Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetInvalidation2014_05_31
import Network.AWS.CloudFront.V2014_05_31.ListInvalidations2014_05_31
import Network.AWS.CloudFront.V2014_05_31.CreateInvalidation2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity2014_05_31
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.UpdateDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.DeleteDistribution2014_05_31
import Network.AWS.CloudFront.V2014_05_31.ListDistributions2014_05_31

-- Newtypes
makeIso ''Restrictions
makeIso ''S3OriginConfig

-- Products
makeLenses ''ActiveTrustedSigners
makeLenses ''Aliases
makeLenses ''AllowedMethods
makeLenses ''CacheBehavior
makeLenses ''CacheBehaviors
makeLenses ''CloudFrontOriginAccessIdentity
makeLenses ''CloudFrontOriginAccessIdentityConfig
makeLenses ''CloudFrontOriginAccessIdentityList
makeLenses ''CloudFrontOriginAccessIdentitySummary
makeLenses ''CookieNames
makeLenses ''CookiePreference
makeLenses ''CustomErrorResponse
makeLenses ''CustomErrorResponses
makeLenses ''CustomOriginConfig
makeLenses ''DefaultCacheBehavior
makeLenses ''Distribution
makeLenses ''DistributionConfig
makeLenses ''DistributionList
makeLenses ''DistributionSummary
makeLenses ''ForwardedValues
makeLenses ''GeoRestriction
makeLenses ''Headers
makeLenses ''Invalidation
makeLenses ''InvalidationBatch
makeLenses ''InvalidationList
makeLenses ''InvalidationSummary
makeLenses ''KeyPairIds
makeLenses ''LoggingConfig
makeLenses ''Origin
makeLenses ''Origins
makeLenses ''Paths
makeLenses ''S3Origin
makeLenses ''Signer
makeLenses ''StreamingDistribution
makeLenses ''StreamingDistributionConfig
makeLenses ''StreamingDistributionList
makeLenses ''StreamingDistributionSummary
makeLenses ''StreamingLoggingConfig
makeLenses ''TrustedSigners
makeLenses ''ViewerCertificate

-- Requests
makeLenses ''DeleteStreamingDistribution2014_05_31
makeLenses ''UpdateStreamingDistribution2014_05_31
makeLenses ''CreateDistribution2014_05_31
makeLenses ''GetDistributionConfig2014_05_31
makeLenses ''GetDistribution2014_05_31
makeLenses ''UpdateCloudFrontOriginAccessIdentity2014_05_31
makeLenses ''DeleteCloudFrontOriginAccessIdentity2014_05_31
makeLenses ''ListStreamingDistributions2014_05_31
makeLenses ''GetStreamingDistributionConfig2014_05_31
makeLenses ''GetCloudFrontOriginAccessIdentityConfig2014_05_31
makeLenses ''CreateStreamingDistribution2014_05_31
makeLenses ''CreateCloudFrontOriginAccessIdentity2014_05_31
makeLenses ''ListCloudFrontOriginAccessIdentities2014_05_31
makeLenses ''GetInvalidation2014_05_31
makeLenses ''ListInvalidations2014_05_31
makeLenses ''CreateInvalidation2014_05_31
makeLenses ''GetCloudFrontOriginAccessIdentity2014_05_31
makeLenses ''GetStreamingDistribution2014_05_31
makeLenses ''UpdateDistribution2014_05_31
makeLenses ''DeleteDistribution2014_05_31
makeLenses ''ListDistributions2014_05_31

-- Responses
makeLenses ''DeleteStreamingDistribution2014_05_31Response
makeLenses ''UpdateStreamingDistribution2014_05_31Response
makeLenses ''CreateDistribution2014_05_31Response
makeLenses ''GetDistributionConfig2014_05_31Response
makeLenses ''GetDistribution2014_05_31Response
makeLenses ''UpdateCloudFrontOriginAccessIdentity2014_05_31Response
makeLenses ''DeleteCloudFrontOriginAccessIdentity2014_05_31Response
makeLenses ''ListStreamingDistributions2014_05_31Response
makeLenses ''GetStreamingDistributionConfig2014_05_31Response
makeLenses ''GetCloudFrontOriginAccessIdentityConfig2014_05_31Response
makeLenses ''CreateStreamingDistribution2014_05_31Response
makeLenses ''CreateCloudFrontOriginAccessIdentity2014_05_31Response
makeLenses ''ListCloudFrontOriginAccessIdentities2014_05_31Response
makeLenses ''GetInvalidation2014_05_31Response
makeLenses ''ListInvalidations2014_05_31Response
makeLenses ''CreateInvalidation2014_05_31Response
makeLenses ''GetCloudFrontOriginAccessIdentity2014_05_31Response
makeLenses ''GetStreamingDistribution2014_05_31Response
makeLenses ''UpdateDistribution2014_05_31Response
makeLenses ''DeleteDistribution2014_05_31Response
makeLenses ''ListDistributions2014_05_31Response
