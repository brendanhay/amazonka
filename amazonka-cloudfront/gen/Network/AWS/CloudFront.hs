{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon CloudFront is a web service that speeds up distribution of your
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
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.CloudFront
    (
    -- * Service
      CloudFront

    -- * Errors
    -- $errors

    -- ** InvalidErrorCode
    , _InvalidErrorCode

    -- ** TooManyCacheBehaviors
    , _TooManyCacheBehaviors

    -- ** DistributionNotDisabled
    , _DistributionNotDisabled

    -- ** InvalidOriginAccessIdentity
    , _InvalidOriginAccessIdentity

    -- ** TooManyCloudFrontOriginAccessIdentities
    , _TooManyCloudFrontOriginAccessIdentities

    -- ** TooManyStreamingDistributions
    , _TooManyStreamingDistributions

    -- ** InvalidArgument
    , _InvalidArgument

    -- ** NoSuchCloudFrontOriginAccessIdentity
    , _NoSuchCloudFrontOriginAccessIdentity

    -- ** NoSuchStreamingDistribution
    , _NoSuchStreamingDistribution

    -- ** CloudFrontOriginAccessIdentityInUse
    , _CloudFrontOriginAccessIdentityInUse

    -- ** InconsistentQuantities
    , _InconsistentQuantities

    -- ** TooManyInvalidationsInProgress
    , _TooManyInvalidationsInProgress

    -- ** TooManyDistributionCNAMEs
    , _TooManyDistributionCNAMEs

    -- ** InvalidForwardCookies
    , _InvalidForwardCookies

    -- ** TooManyCookieNamesInWhiteList
    , _TooManyCookieNamesInWhiteList

    -- ** BatchTooLarge
    , _BatchTooLarge

    -- ** InvalidOrigin
    , _InvalidOrigin

    -- ** TooManyTrustedSigners
    , _TooManyTrustedSigners

    -- ** NoSuchOrigin
    , _NoSuchOrigin

    -- ** NoSuchInvalidation
    , _NoSuchInvalidation

    -- ** StreamingDistributionNotDisabled
    , _StreamingDistributionNotDisabled

    -- ** InvalidTTLOrder
    , _InvalidTTLOrder

    -- ** TooManyStreamingDistributionCNAMEs
    , _TooManyStreamingDistributionCNAMEs

    -- ** TooManyDistributions
    , _TooManyDistributions

    -- ** InvalidRequiredProtocol
    , _InvalidRequiredProtocol

    -- ** TooManyHeadersInForwardedValues
    , _TooManyHeadersInForwardedValues

    -- ** TooManyCertificates
    , _TooManyCertificates

    -- ** MissingBody
    , _MissingBody

    -- ** DistributionAlreadyExists
    , _DistributionAlreadyExists

    -- ** IllegalUpdate
    , _IllegalUpdate

    -- ** InvalidResponseCode
    , _InvalidResponseCode

    -- ** InvalidIfMatchVersion
    , _InvalidIfMatchVersion

    -- ** PreconditionFailed
    , _PreconditionFailed

    -- ** InvalidProtocolSettings
    , _InvalidProtocolSettings

    -- ** TrustedSignerDoesNotExist
    , _TrustedSignerDoesNotExist

    -- ** InvalidHeadersForS3Origin
    , _InvalidHeadersForS3Origin

    -- ** CNAMEAlreadyExists
    , _CNAMEAlreadyExists

    -- ** StreamingDistributionAlreadyExists
    , _StreamingDistributionAlreadyExists

    -- ** TooManyOrigins
    , _TooManyOrigins

    -- ** CloudFrontOriginAccessIdentityAlreadyExists
    , _CloudFrontOriginAccessIdentityAlreadyExists

    -- ** InvalidRelativePath
    , _InvalidRelativePath

    -- ** InvalidMinimumProtocolVersion
    , _InvalidMinimumProtocolVersion

    -- ** AccessDenied
    , _AccessDenied

    -- ** NoSuchDistribution
    , _NoSuchDistribution

    -- ** InvalidViewerCertificate
    , _InvalidViewerCertificate

    -- ** InvalidDefaultRootObject
    , _InvalidDefaultRootObject

    -- ** InvalidLocationCode
    , _InvalidLocationCode

    -- ** InvalidGeoRestrictionParameter
    , _InvalidGeoRestrictionParameter

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteStreamingDistribution
    , module Network.AWS.CloudFront.DeleteStreamingDistribution

    -- ** UpdateStreamingDistribution
    , module Network.AWS.CloudFront.UpdateStreamingDistribution

    -- ** CreateDistribution
    , module Network.AWS.CloudFront.CreateDistribution

    -- ** GetDistributionConfig
    , module Network.AWS.CloudFront.GetDistributionConfig

    -- ** GetDistribution
    , module Network.AWS.CloudFront.GetDistribution

    -- ** DeleteCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity

    -- ** UpdateCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity

    -- ** ListStreamingDistributions
    , module Network.AWS.CloudFront.ListStreamingDistributions

    -- ** GetStreamingDistributionConfig
    , module Network.AWS.CloudFront.GetStreamingDistributionConfig

    -- ** GetCloudFrontOriginAccessIdentityConfig
    , module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig

    -- ** CreateStreamingDistribution
    , module Network.AWS.CloudFront.CreateStreamingDistribution

    -- ** CreateCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity

    -- ** ListCloudFrontOriginAccessIdentities
    , module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities

    -- ** GetInvalidation
    , module Network.AWS.CloudFront.GetInvalidation

    -- ** ListInvalidations
    , module Network.AWS.CloudFront.ListInvalidations

    -- ** GetStreamingDistribution
    , module Network.AWS.CloudFront.GetStreamingDistribution

    -- ** GetCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity

    -- ** CreateInvalidation
    , module Network.AWS.CloudFront.CreateInvalidation

    -- ** UpdateDistribution
    , module Network.AWS.CloudFront.UpdateDistribution

    -- ** DeleteDistribution
    , module Network.AWS.CloudFront.DeleteDistribution

    -- ** ListDistributions
    , module Network.AWS.CloudFront.ListDistributions

    -- * Types

    -- ** GeoRestrictionType
    , GeoRestrictionType (..)

    -- ** ItemSelection
    , ItemSelection (..)

    -- ** Method
    , Method (..)

    -- ** MinimumProtocolVersion
    , MinimumProtocolVersion (..)

    -- ** OriginProtocolPolicy
    , OriginProtocolPolicy (..)

    -- ** PriceClass
    , PriceClass (..)

    -- ** SSLSupportMethod
    , SSLSupportMethod (..)

    -- ** ViewerProtocolPolicy
    , ViewerProtocolPolicy (..)

    -- ** ActiveTrustedSigners
    , ActiveTrustedSigners
    , activeTrustedSigners
    , atsItems
    , atsEnabled
    , atsQuantity

    -- ** Aliases
    , Aliases
    , aliases
    , aItems
    , aQuantity

    -- ** AllowedMethods
    , AllowedMethods
    , allowedMethods
    , amCachedMethods
    , amQuantity
    , amItems

    -- ** CacheBehavior
    , CacheBehavior
    , cacheBehavior
    , cbAllowedMethods
    , cbMaxTTL
    , cbSmoothStreaming
    , cbDefaultTTL
    , cbPathPattern
    , cbTargetOriginId
    , cbForwardedValues
    , cbTrustedSigners
    , cbViewerProtocolPolicy
    , cbMinTTL

    -- ** CacheBehaviors
    , CacheBehaviors
    , cacheBehaviors
    , cbItems
    , cbQuantity

    -- ** CachedMethods
    , CachedMethods
    , cachedMethods
    , cmQuantity
    , cmItems

    -- ** CloudFrontOriginAccessIdentity
    , CloudFrontOriginAccessIdentity
    , cloudFrontOriginAccessIdentity
    , cfoaiCloudFrontOriginAccessIdentityConfig
    , cfoaiId
    , cfoaiS3CanonicalUserId

    -- ** CloudFrontOriginAccessIdentityConfig
    , CloudFrontOriginAccessIdentityConfig
    , cloudFrontOriginAccessIdentityConfig
    , cfoaicCallerReference
    , cfoaicComment

    -- ** CloudFrontOriginAccessIdentityList
    , CloudFrontOriginAccessIdentityList
    , cloudFrontOriginAccessIdentityList
    , cfoailItems
    , cfoailNextMarker
    , cfoailMarker
    , cfoailMaxItems
    , cfoailIsTruncated
    , cfoailQuantity

    -- ** CloudFrontOriginAccessIdentitySummary
    , CloudFrontOriginAccessIdentitySummary
    , cloudFrontOriginAccessIdentitySummary
    , cfoaisId
    , cfoaisS3CanonicalUserId
    , cfoaisComment

    -- ** CookieNames
    , CookieNames
    , cookieNames
    , cnItems
    , cnQuantity

    -- ** CookiePreference
    , CookiePreference
    , cookiePreference
    , cpWhitelistedNames
    , cpForward

    -- ** CustomErrorResponse
    , CustomErrorResponse
    , customErrorResponse
    , ceResponsePagePath
    , ceResponseCode
    , ceErrorCachingMinTTL
    , ceErrorCode

    -- ** CustomErrorResponses
    , CustomErrorResponses
    , customErrorResponses
    , cerItems
    , cerQuantity

    -- ** CustomOriginConfig
    , CustomOriginConfig
    , customOriginConfig
    , cocHTTPPort
    , cocHTTPSPort
    , cocOriginProtocolPolicy

    -- ** DefaultCacheBehavior
    , DefaultCacheBehavior
    , defaultCacheBehavior
    , dcbAllowedMethods
    , dcbMaxTTL
    , dcbSmoothStreaming
    , dcbDefaultTTL
    , dcbTargetOriginId
    , dcbForwardedValues
    , dcbTrustedSigners
    , dcbViewerProtocolPolicy
    , dcbMinTTL

    -- ** Distribution
    , Distribution
    , distribution
    , dId
    , dStatus
    , dLastModifiedTime
    , dInProgressInvalidationBatches
    , dDomainName
    , dActiveTrustedSigners
    , dDistributionConfig

    -- ** DistributionConfig
    , DistributionConfig
    , distributionConfig
    , dcDefaultRootObject
    , dcAliases
    , dcCustomErrorResponses
    , dcPriceClass
    , dcViewerCertificate
    , dcRestrictions
    , dcCacheBehaviors
    , dcLogging
    , dcCallerReference
    , dcOrigins
    , dcDefaultCacheBehavior
    , dcComment
    , dcEnabled

    -- ** DistributionList
    , DistributionList
    , distributionList
    , dlItems
    , dlNextMarker
    , dlMarker
    , dlMaxItems
    , dlIsTruncated
    , dlQuantity

    -- ** DistributionSummary
    , DistributionSummary
    , distributionSummary
    , dsId
    , dsStatus
    , dsLastModifiedTime
    , dsDomainName
    , dsAliases
    , dsOrigins
    , dsDefaultCacheBehavior
    , dsCacheBehaviors
    , dsCustomErrorResponses
    , dsComment
    , dsPriceClass
    , dsEnabled
    , dsViewerCertificate
    , dsRestrictions

    -- ** ForwardedValues
    , ForwardedValues
    , forwardedValues
    , fvHeaders
    , fvQueryString
    , fvCookies

    -- ** GeoRestriction
    , GeoRestriction
    , geoRestriction
    , grItems
    , grRestrictionType
    , grQuantity

    -- ** Headers
    , Headers
    , headers
    , hItems
    , hQuantity

    -- ** Invalidation
    , Invalidation
    , invalidation
    , iId
    , iStatus
    , iCreateTime
    , iInvalidationBatch

    -- ** InvalidationBatch
    , InvalidationBatch
    , invalidationBatch
    , ibPaths
    , ibCallerReference

    -- ** InvalidationList
    , InvalidationList
    , invalidationList
    , ilItems
    , ilNextMarker
    , ilMarker
    , ilMaxItems
    , ilIsTruncated
    , ilQuantity

    -- ** InvalidationSummary
    , InvalidationSummary
    , invalidationSummary
    , isId
    , isCreateTime
    , isStatus

    -- ** KeyPairIds
    , KeyPairIds
    , keyPairIds
    , kpiItems
    , kpiQuantity

    -- ** LoggingConfig
    , LoggingConfig
    , loggingConfig
    , lcEnabled
    , lcIncludeCookies
    , lcBucket
    , lcPrefix

    -- ** Origin
    , Origin
    , origin
    , oCustomOriginConfig
    , oS3OriginConfig
    , oOriginPath
    , oId
    , oDomainName

    -- ** Origins
    , Origins
    , origins
    , oItems
    , oQuantity

    -- ** Paths
    , Paths
    , paths
    , pItems
    , pQuantity

    -- ** Restrictions
    , Restrictions
    , restrictions
    , rGeoRestriction

    -- ** S3Origin
    , S3Origin
    , s3Origin
    , soDomainName
    , soOriginAccessIdentity

    -- ** S3OriginConfig
    , S3OriginConfig
    , s3OriginConfig
    , socOriginAccessIdentity

    -- ** Signer
    , Signer
    , signer
    , sAWSAccountNumber
    , sKeyPairIds

    -- ** StreamingDistribution
    , StreamingDistribution
    , streamingDistribution
    , sdLastModifiedTime
    , sdId
    , sdStatus
    , sdDomainName
    , sdActiveTrustedSigners
    , sdStreamingDistributionConfig

    -- ** StreamingDistributionConfig
    , StreamingDistributionConfig
    , streamingDistributionConfig
    , sdcAliases
    , sdcPriceClass
    , sdcLogging
    , sdcCallerReference
    , sdcS3Origin
    , sdcComment
    , sdcTrustedSigners
    , sdcEnabled

    -- ** StreamingDistributionList
    , StreamingDistributionList
    , streamingDistributionList
    , sdlItems
    , sdlNextMarker
    , sdlMarker
    , sdlMaxItems
    , sdlIsTruncated
    , sdlQuantity

    -- ** StreamingDistributionSummary
    , StreamingDistributionSummary
    , streamingDistributionSummary
    , sdsId
    , sdsStatus
    , sdsLastModifiedTime
    , sdsDomainName
    , sdsS3Origin
    , sdsAliases
    , sdsTrustedSigners
    , sdsComment
    , sdsPriceClass
    , sdsEnabled

    -- ** StreamingLoggingConfig
    , StreamingLoggingConfig
    , streamingLoggingConfig
    , slcEnabled
    , slcBucket
    , slcPrefix

    -- ** TrustedSigners
    , TrustedSigners
    , trustedSigners
    , tsItems
    , tsEnabled
    , tsQuantity

    -- ** ViewerCertificate
    , ViewerCertificate
    , viewerCertificate
    , vcSSLSupportMethod
    , vcMinimumProtocolVersion
    , vcIAMCertificateId
    , vcCloudFrontDefaultCertificate
    ) where

import           Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
import           Network.AWS.CloudFront.CreateDistribution
import           Network.AWS.CloudFront.CreateInvalidation
import           Network.AWS.CloudFront.CreateStreamingDistribution
import           Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
import           Network.AWS.CloudFront.DeleteDistribution
import           Network.AWS.CloudFront.DeleteStreamingDistribution
import           Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
import           Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
import           Network.AWS.CloudFront.GetDistribution
import           Network.AWS.CloudFront.GetDistributionConfig
import           Network.AWS.CloudFront.GetInvalidation
import           Network.AWS.CloudFront.GetStreamingDistribution
import           Network.AWS.CloudFront.GetStreamingDistributionConfig
import           Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
import           Network.AWS.CloudFront.ListDistributions
import           Network.AWS.CloudFront.ListInvalidations
import           Network.AWS.CloudFront.ListStreamingDistributions
import           Network.AWS.CloudFront.Types
import           Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
import           Network.AWS.CloudFront.UpdateDistribution
import           Network.AWS.CloudFront.UpdateStreamingDistribution
import           Network.AWS.CloudFront.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudFront'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

{- $pager
This operation can return paginated results.
-}
