{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types
    (
    -- * Service Configuration
      cloudFront

    -- * Errors
    , _InvalidErrorCode
    , _TooManyCacheBehaviors
    , _DistributionNotDisabled
    , _InvalidOriginAccessIdentity
    , _TooManyCloudFrontOriginAccessIdentities
    , _TooManyStreamingDistributions
    , _InvalidArgument
    , _NoSuchCloudFrontOriginAccessIdentity
    , _NoSuchStreamingDistribution
    , _CloudFrontOriginAccessIdentityInUse
    , _InconsistentQuantities
    , _TooManyInvalidationsInProgress
    , _TooManyDistributionCNAMEs
    , _InvalidForwardCookies
    , _TooManyCookieNamesInWhiteList
    , _BatchTooLarge
    , _InvalidOrigin
    , _TooManyTrustedSigners
    , _NoSuchOrigin
    , _NoSuchInvalidation
    , _StreamingDistributionNotDisabled
    , _InvalidTTLOrder
    , _TooManyStreamingDistributionCNAMEs
    , _TooManyDistributions
    , _InvalidRequiredProtocol
    , _TooManyHeadersInForwardedValues
    , _TooManyCertificates
    , _MissingBody
    , _DistributionAlreadyExists
    , _IllegalUpdate
    , _InvalidResponseCode
    , _InvalidIfMatchVersion
    , _PreconditionFailed
    , _InvalidProtocolSettings
    , _TrustedSignerDoesNotExist
    , _InvalidHeadersForS3Origin
    , _CNAMEAlreadyExists
    , _StreamingDistributionAlreadyExists
    , _TooManyOrigins
    , _CloudFrontOriginAccessIdentityAlreadyExists
    , _InvalidRelativePath
    , _InvalidMinimumProtocolVersion
    , _AccessDenied
    , _NoSuchDistribution
    , _InvalidViewerCertificate
    , _InvalidDefaultRootObject
    , _InvalidLocationCode
    , _InvalidGeoRestrictionParameter

    -- * GeoRestrictionType
    , GeoRestrictionType (..)

    -- * ItemSelection
    , ItemSelection (..)

    -- * Method
    , Method (..)

    -- * MinimumProtocolVersion
    , MinimumProtocolVersion (..)

    -- * OriginProtocolPolicy
    , OriginProtocolPolicy (..)

    -- * PriceClass
    , PriceClass (..)

    -- * SSLSupportMethod
    , SSLSupportMethod (..)

    -- * ViewerProtocolPolicy
    , ViewerProtocolPolicy (..)

    -- * ActiveTrustedSigners
    , ActiveTrustedSigners
    , activeTrustedSigners
    , atsItems
    , atsEnabled
    , atsQuantity

    -- * Aliases
    , Aliases
    , aliases
    , aItems
    , aQuantity

    -- * AllowedMethods
    , AllowedMethods
    , allowedMethods
    , amCachedMethods
    , amQuantity
    , amItems

    -- * CacheBehavior
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

    -- * CacheBehaviors
    , CacheBehaviors
    , cacheBehaviors
    , cbItems
    , cbQuantity

    -- * CachedMethods
    , CachedMethods
    , cachedMethods
    , cmQuantity
    , cmItems

    -- * CloudFrontOriginAccessIdentity
    , CloudFrontOriginAccessIdentity
    , cloudFrontOriginAccessIdentity
    , cfoaiCloudFrontOriginAccessIdentityConfig
    , cfoaiId
    , cfoaiS3CanonicalUserId

    -- * CloudFrontOriginAccessIdentityConfig
    , CloudFrontOriginAccessIdentityConfig
    , cloudFrontOriginAccessIdentityConfig
    , cfoaicCallerReference
    , cfoaicComment

    -- * CloudFrontOriginAccessIdentityList
    , CloudFrontOriginAccessIdentityList
    , cloudFrontOriginAccessIdentityList
    , cfoailItems
    , cfoailNextMarker
    , cfoailMarker
    , cfoailMaxItems
    , cfoailIsTruncated
    , cfoailQuantity

    -- * CloudFrontOriginAccessIdentitySummary
    , CloudFrontOriginAccessIdentitySummary
    , cloudFrontOriginAccessIdentitySummary
    , cfoaisId
    , cfoaisS3CanonicalUserId
    , cfoaisComment

    -- * CookieNames
    , CookieNames
    , cookieNames
    , cnItems
    , cnQuantity

    -- * CookiePreference
    , CookiePreference
    , cookiePreference
    , cpWhitelistedNames
    , cpForward

    -- * CustomErrorResponse
    , CustomErrorResponse
    , customErrorResponse
    , ceResponsePagePath
    , ceResponseCode
    , ceErrorCachingMinTTL
    , ceErrorCode

    -- * CustomErrorResponses
    , CustomErrorResponses
    , customErrorResponses
    , cerItems
    , cerQuantity

    -- * CustomOriginConfig
    , CustomOriginConfig
    , customOriginConfig
    , cocHTTPPort
    , cocHTTPSPort
    , cocOriginProtocolPolicy

    -- * DefaultCacheBehavior
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

    -- * Distribution
    , Distribution
    , distribution
    , dId
    , dStatus
    , dLastModifiedTime
    , dInProgressInvalidationBatches
    , dDomainName
    , dActiveTrustedSigners
    , dDistributionConfig

    -- * DistributionConfig
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

    -- * DistributionList
    , DistributionList
    , distributionList
    , dlItems
    , dlNextMarker
    , dlMarker
    , dlMaxItems
    , dlIsTruncated
    , dlQuantity

    -- * DistributionSummary
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

    -- * ForwardedValues
    , ForwardedValues
    , forwardedValues
    , fvHeaders
    , fvQueryString
    , fvCookies

    -- * GeoRestriction
    , GeoRestriction
    , geoRestriction
    , grItems
    , grRestrictionType
    , grQuantity

    -- * Headers
    , Headers
    , headers
    , hItems
    , hQuantity

    -- * Invalidation
    , Invalidation
    , invalidation
    , iId
    , iStatus
    , iCreateTime
    , iInvalidationBatch

    -- * InvalidationBatch
    , InvalidationBatch
    , invalidationBatch
    , ibPaths
    , ibCallerReference

    -- * InvalidationList
    , InvalidationList
    , invalidationList
    , ilItems
    , ilNextMarker
    , ilMarker
    , ilMaxItems
    , ilIsTruncated
    , ilQuantity

    -- * InvalidationSummary
    , InvalidationSummary
    , invalidationSummary
    , isId
    , isCreateTime
    , isStatus

    -- * KeyPairIds
    , KeyPairIds
    , keyPairIds
    , kpiItems
    , kpiQuantity

    -- * LoggingConfig
    , LoggingConfig
    , loggingConfig
    , lcEnabled
    , lcIncludeCookies
    , lcBucket
    , lcPrefix

    -- * Origin
    , Origin
    , origin
    , oCustomOriginConfig
    , oS3OriginConfig
    , oOriginPath
    , oId
    , oDomainName

    -- * Origins
    , Origins
    , origins
    , oItems
    , oQuantity

    -- * Paths
    , Paths
    , paths
    , pItems
    , pQuantity

    -- * Restrictions
    , Restrictions
    , restrictions
    , rGeoRestriction

    -- * S3Origin
    , S3Origin
    , s3Origin
    , soDomainName
    , soOriginAccessIdentity

    -- * S3OriginConfig
    , S3OriginConfig
    , s3OriginConfig
    , socOriginAccessIdentity

    -- * Signer
    , Signer
    , signer
    , sAWSAccountNumber
    , sKeyPairIds

    -- * StreamingDistribution
    , StreamingDistribution
    , streamingDistribution
    , sdLastModifiedTime
    , sdId
    , sdStatus
    , sdDomainName
    , sdActiveTrustedSigners
    , sdStreamingDistributionConfig

    -- * StreamingDistributionConfig
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

    -- * StreamingDistributionList
    , StreamingDistributionList
    , streamingDistributionList
    , sdlItems
    , sdlNextMarker
    , sdlMarker
    , sdlMaxItems
    , sdlIsTruncated
    , sdlQuantity

    -- * StreamingDistributionSummary
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

    -- * StreamingLoggingConfig
    , StreamingLoggingConfig
    , streamingLoggingConfig
    , slcEnabled
    , slcBucket
    , slcPrefix

    -- * TrustedSigners
    , TrustedSigners
    , trustedSigners
    , tsItems
    , tsEnabled
    , tsQuantity

    -- * ViewerCertificate
    , ViewerCertificate
    , viewerCertificate
    , vcSSLSupportMethod
    , vcMinimumProtocolVersion
    , vcIAMCertificateId
    , vcCloudFrontDefaultCertificate
    ) where

import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.CloudFront.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-04-17' of the Amazon CloudFront SDK configuration.
cloudFront :: Service
cloudFront =
    Service
    { _svcAbbrev = "CloudFront"
    , _svcSigner = v4
    , _svcPrefix = "cloudfront"
    , _svcVersion = "2015-04-17"
    , _svcEndpoint = defaultEndpoint cloudFront
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for InvalidErrorCode' errors.
_InvalidErrorCode :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidErrorCode = _ServiceError . hasStatus 400 . hasCode "InvalidErrorCode"

-- | You cannot create anymore cache behaviors for the distribution.
_TooManyCacheBehaviors :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyCacheBehaviors =
    _ServiceError . hasStatus 400 . hasCode "TooManyCacheBehaviors"

-- | Prism for DistributionNotDisabled' errors.
_DistributionNotDisabled :: AsError a => Getting (First ServiceError) a ServiceError
_DistributionNotDisabled =
    _ServiceError . hasStatus 409 . hasCode "DistributionNotDisabled"

-- | The origin access identity is not valid or doesn\'t exist.
_InvalidOriginAccessIdentity :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOriginAccessIdentity =
    _ServiceError . hasStatus 400 . hasCode "InvalidOriginAccessIdentity"

-- | Processing your request would cause you to exceed the maximum number of
-- origin access identities allowed.
_TooManyCloudFrontOriginAccessIdentities :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyCloudFrontOriginAccessIdentities =
    _ServiceError .
    hasStatus 400 . hasCode "TooManyCloudFrontOriginAccessIdentities"

-- | Processing your request would cause you to exceed the maximum number of
-- streaming distributions allowed.
_TooManyStreamingDistributions :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyStreamingDistributions =
    _ServiceError . hasStatus 400 . hasCode "TooManyStreamingDistributions"

-- | The argument is invalid.
_InvalidArgument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgument = _ServiceError . hasStatus 400 . hasCode "InvalidArgument"

-- | The specified origin access identity does not exist.
_NoSuchCloudFrontOriginAccessIdentity :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchCloudFrontOriginAccessIdentity =
    _ServiceError .
    hasStatus 404 . hasCode "NoSuchCloudFrontOriginAccessIdentity"

-- | The specified streaming distribution does not exist.
_NoSuchStreamingDistribution :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchStreamingDistribution =
    _ServiceError . hasStatus 404 . hasCode "NoSuchStreamingDistribution"

-- | Prism for CloudFrontOriginAccessIdentityInUse' errors.
_CloudFrontOriginAccessIdentityInUse :: AsError a => Getting (First ServiceError) a ServiceError
_CloudFrontOriginAccessIdentityInUse =
    _ServiceError .
    hasStatus 409 . hasCode "CloudFrontOriginAccessIdentityInUse"

-- | The value of Quantity and the size of Items do not match.
_InconsistentQuantities :: AsError a => Getting (First ServiceError) a ServiceError
_InconsistentQuantities =
    _ServiceError . hasStatus 400 . hasCode "InconsistentQuantities"

-- | You have exceeded the maximum number of allowable InProgress
-- invalidation batch requests, or invalidation objects.
_TooManyInvalidationsInProgress :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyInvalidationsInProgress =
    _ServiceError . hasStatus 400 . hasCode "TooManyInvalidationsInProgress"

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyDistributionCNAMEs :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyDistributionCNAMEs =
    _ServiceError . hasStatus 400 . hasCode "TooManyDistributionCNAMEs"

-- | Your request contains forward cookies option which doesn\'t match with
-- the expectation for the whitelisted list of cookie names. Either list of
-- cookie names has been specified when not allowed or list of cookie names
-- is missing when expected.
_InvalidForwardCookies :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidForwardCookies =
    _ServiceError . hasStatus 400 . hasCode "InvalidForwardCookies"

-- | Your request contains more cookie names in the whitelist than are
-- allowed per cache behavior.
_TooManyCookieNamesInWhiteList :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyCookieNamesInWhiteList =
    _ServiceError . hasStatus 400 . hasCode "TooManyCookieNamesInWhiteList"

-- | Prism for BatchTooLarge' errors.
_BatchTooLarge :: AsError a => Getting (First ServiceError) a ServiceError
_BatchTooLarge = _ServiceError . hasStatus 413 . hasCode "BatchTooLarge"

-- | The Amazon S3 origin server specified does not refer to a valid Amazon
-- S3 bucket.
_InvalidOrigin :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOrigin = _ServiceError . hasStatus 400 . hasCode "InvalidOrigin"

-- | Your request contains more trusted signers than are allowed per
-- distribution.
_TooManyTrustedSigners :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTrustedSigners =
    _ServiceError . hasStatus 400 . hasCode "TooManyTrustedSigners"

-- | No origin exists with the specified Origin Id.
_NoSuchOrigin :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchOrigin = _ServiceError . hasStatus 404 . hasCode "NoSuchOrigin"

-- | The specified invalidation does not exist.
_NoSuchInvalidation :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchInvalidation =
    _ServiceError . hasStatus 404 . hasCode "NoSuchInvalidation"

-- | Prism for StreamingDistributionNotDisabled' errors.
_StreamingDistributionNotDisabled :: AsError a => Getting (First ServiceError) a ServiceError
_StreamingDistributionNotDisabled =
    _ServiceError . hasStatus 409 . hasCode "StreamingDistributionNotDisabled"

-- | Prism for InvalidTTLOrder' errors.
_InvalidTTLOrder :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTTLOrder = _ServiceError . hasStatus 400 . hasCode "InvalidTTLOrder"

-- | Prism for TooManyStreamingDistributionCNAMEs' errors.
_TooManyStreamingDistributionCNAMEs :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyStreamingDistributionCNAMEs =
    _ServiceError .
    hasStatus 400 . hasCode "TooManyStreamingDistributionCNAMEs"

-- | Processing your request would cause you to exceed the maximum number of
-- distributions allowed.
_TooManyDistributions :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyDistributions =
    _ServiceError . hasStatus 400 . hasCode "TooManyDistributions"

-- | This operation requires the HTTPS protocol. Ensure that you specify the
-- HTTPS protocol in your request, or omit the RequiredProtocols element
-- from your distribution configuration.
_InvalidRequiredProtocol :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequiredProtocol =
    _ServiceError . hasStatus 400 . hasCode "InvalidRequiredProtocol"

-- | Prism for TooManyHeadersInForwardedValues' errors.
_TooManyHeadersInForwardedValues :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyHeadersInForwardedValues =
    _ServiceError . hasStatus 400 . hasCode "TooManyHeadersInForwardedValues"

-- | You cannot create anymore custom ssl certificates.
_TooManyCertificates :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyCertificates =
    _ServiceError . hasStatus 400 . hasCode "TooManyCertificates"

-- | This operation requires a body. Ensure that the body is present and the
-- Content-Type header is set.
_MissingBody :: AsError a => Getting (First ServiceError) a ServiceError
_MissingBody = _ServiceError . hasStatus 400 . hasCode "MissingBody"

-- | The caller reference you attempted to create the distribution with is
-- associated with another distribution.
_DistributionAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_DistributionAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "DistributionAlreadyExists"

-- | Origin and CallerReference cannot be updated.
_IllegalUpdate :: AsError a => Getting (First ServiceError) a ServiceError
_IllegalUpdate = _ServiceError . hasStatus 400 . hasCode "IllegalUpdate"

-- | Prism for InvalidResponseCode' errors.
_InvalidResponseCode :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResponseCode =
    _ServiceError . hasStatus 400 . hasCode "InvalidResponseCode"

-- | The If-Match version is missing or not valid for the distribution.
_InvalidIfMatchVersion :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIfMatchVersion =
    _ServiceError . hasStatus 400 . hasCode "InvalidIfMatchVersion"

-- | The precondition given in one or more of the request-header fields
-- evaluated to false.
_PreconditionFailed :: AsError a => Getting (First ServiceError) a ServiceError
_PreconditionFailed =
    _ServiceError . hasStatus 412 . hasCode "PreconditionFailed"

-- | You cannot specify SSLv3 as the minimum protocol version if you only
-- want to support only clients that Support Server Name Indication (SNI).
_InvalidProtocolSettings :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidProtocolSettings =
    _ServiceError . hasStatus 400 . hasCode "InvalidProtocolSettings"

-- | One or more of your trusted signers do not exist.
_TrustedSignerDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_TrustedSignerDoesNotExist =
    _ServiceError . hasStatus 400 . hasCode "TrustedSignerDoesNotExist"

-- | Prism for InvalidHeadersForS3Origin' errors.
_InvalidHeadersForS3Origin :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidHeadersForS3Origin =
    _ServiceError . hasStatus 400 . hasCode "InvalidHeadersForS3Origin"

-- | Prism for CNAMEAlreadyExists' errors.
_CNAMEAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_CNAMEAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "CNAMEAlreadyExists"

-- | Prism for StreamingDistributionAlreadyExists' errors.
_StreamingDistributionAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_StreamingDistributionAlreadyExists =
    _ServiceError .
    hasStatus 409 . hasCode "StreamingDistributionAlreadyExists"

-- | You cannot create anymore origins for the distribution.
_TooManyOrigins :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyOrigins = _ServiceError . hasStatus 400 . hasCode "TooManyOrigins"

-- | If the CallerReference is a value you already sent in a previous request
-- to create an identity but the content of the
-- CloudFrontOriginAccessIdentityConfig is different from the original
-- request, CloudFront returns a
-- CloudFrontOriginAccessIdentityAlreadyExists error.
_CloudFrontOriginAccessIdentityAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_CloudFrontOriginAccessIdentityAlreadyExists =
    _ServiceError .
    hasStatus 409 . hasCode "CloudFrontOriginAccessIdentityAlreadyExists"

-- | The relative path is too big, is not URL-encoded, or does not begin with
-- a slash (\/).
_InvalidRelativePath :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRelativePath =
    _ServiceError . hasStatus 400 . hasCode "InvalidRelativePath"

-- | Prism for InvalidMinimumProtocolVersion' errors.
_InvalidMinimumProtocolVersion :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMinimumProtocolVersion =
    _ServiceError . hasStatus 400 . hasCode "InvalidMinimumProtocolVersion"

-- | Access denied.
_AccessDenied :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDenied = _ServiceError . hasStatus 403 . hasCode "AccessDenied"

-- | The specified distribution does not exist.
_NoSuchDistribution :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDistribution =
    _ServiceError . hasStatus 404 . hasCode "NoSuchDistribution"

-- | Prism for InvalidViewerCertificate' errors.
_InvalidViewerCertificate :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidViewerCertificate =
    _ServiceError . hasStatus 400 . hasCode "InvalidViewerCertificate"

-- | The default root object file name is too big or contains an invalid
-- character.
_InvalidDefaultRootObject :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDefaultRootObject =
    _ServiceError . hasStatus 400 . hasCode "InvalidDefaultRootObject"

-- | Prism for InvalidLocationCode' errors.
_InvalidLocationCode :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLocationCode =
    _ServiceError . hasStatus 400 . hasCode "InvalidLocationCode"

-- | Prism for InvalidGeoRestrictionParameter' errors.
_InvalidGeoRestrictionParameter :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGeoRestrictionParameter =
    _ServiceError . hasStatus 400 . hasCode "InvalidGeoRestrictionParameter"
