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
import Network.AWS.CloudFront.V2014_05_31.DeleteStreamingDistribution
import Network.AWS.CloudFront.V2014_05_31.UpdateStreamingDistribution
import Network.AWS.CloudFront.V2014_05_31.CreateDistribution
import Network.AWS.CloudFront.V2014_05_31.GetDistributionConfig
import Network.AWS.CloudFront.V2014_05_31.GetDistribution
import Network.AWS.CloudFront.V2014_05_31.UpdateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.V2014_05_31.DeleteCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.V2014_05_31.ListStreamingDistributions
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentityConfig
import Network.AWS.CloudFront.V2014_05_31.CreateStreamingDistribution
import Network.AWS.CloudFront.V2014_05_31.CreateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities
import Network.AWS.CloudFront.V2014_05_31.GetInvalidation
import Network.AWS.CloudFront.V2014_05_31.ListInvalidations
import Network.AWS.CloudFront.V2014_05_31.CreateInvalidation
import Network.AWS.CloudFront.V2014_05_31.GetCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.V2014_05_31.GetStreamingDistribution
import Network.AWS.CloudFront.V2014_05_31.UpdateDistribution
import Network.AWS.CloudFront.V2014_05_31.DeleteDistribution
import Network.AWS.CloudFront.V2014_05_31.ListDistributions

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
makeLenses ''DeleteStreamingDistribution
makeLenses ''UpdateStreamingDistribution
makeLenses ''CreateDistribution
makeLenses ''GetDistributionConfig
makeLenses ''GetDistribution
makeLenses ''UpdateCloudFrontOriginAccessIdentity
makeLenses ''DeleteCloudFrontOriginAccessIdentity
makeLenses ''ListStreamingDistributions
makeLenses ''GetStreamingDistributionConfig
makeLenses ''GetCloudFrontOriginAccessIdentityConfig
makeLenses ''CreateStreamingDistribution
makeLenses ''CreateCloudFrontOriginAccessIdentity
makeLenses ''ListCloudFrontOriginAccessIdentities
makeLenses ''GetInvalidation
makeLenses ''ListInvalidations
makeLenses ''CreateInvalidation
makeLenses ''GetCloudFrontOriginAccessIdentity
makeLenses ''GetStreamingDistribution
makeLenses ''UpdateDistribution
makeLenses ''DeleteDistribution
makeLenses ''ListDistributions

-- Responses
makeLenses ''DeleteStreamingDistributionResponse
makeLenses ''UpdateStreamingDistributionResponse
makeLenses ''CreateDistributionResponse
makeLenses ''GetDistributionConfigResponse
makeLenses ''GetDistributionResponse
makeLenses ''UpdateCloudFrontOriginAccessIdentityResponse
makeLenses ''DeleteCloudFrontOriginAccessIdentityResponse
makeLenses ''ListStreamingDistributionsResponse
makeLenses ''GetStreamingDistributionConfigResponse
makeLenses ''GetCloudFrontOriginAccessIdentityConfigResponse
makeLenses ''CreateStreamingDistributionResponse
makeLenses ''CreateCloudFrontOriginAccessIdentityResponse
makeLenses ''ListCloudFrontOriginAccessIdentitiesResponse
makeLenses ''GetInvalidationResponse
makeLenses ''ListInvalidationsResponse
makeLenses ''CreateInvalidationResponse
makeLenses ''GetCloudFrontOriginAccessIdentityResponse
makeLenses ''GetStreamingDistributionResponse
makeLenses ''UpdateDistributionResponse
makeLenses ''DeleteDistributionResponse
makeLenses ''ListDistributionsResponse
