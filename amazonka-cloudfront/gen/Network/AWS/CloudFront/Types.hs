{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
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
    , _TooManyOriginCustomHeaders
    , _InvalidTagging
    , _InvalidErrorCode
    , _TooManyCacheBehaviors
    , _TooManyCloudFrontOriginAccessIdentities
    , _InvalidOriginAccessIdentity
    , _DistributionNotDisabled
    , _NoSuchStreamingDistribution
    , _InconsistentQuantities
    , _InvalidArgument
    , _TooManyInvalidationsInProgress
    , _InvalidWebACLId
    , _TooManyQueryStringParameters
    , _TooManyDistributionCNAMEs
    , _NoSuchCloudFrontOriginAccessIdentity
    , _CloudFrontOriginAccessIdentityInUse
    , _TooManyStreamingDistributions
    , _BatchTooLarge
    , _TooManyCookieNamesInWhiteList
    , _InvalidForwardCookies
    , _TooManyTrustedSigners
    , _InvalidOrigin
    , _NoSuchInvalidation
    , _NoSuchOrigin
    , _InvalidTTLOrder
    , _StreamingDistributionNotDisabled
    , _TooManyHeadersInForwardedValues
    , _NoSuchResource
    , _TooManyStreamingDistributionCNAMEs
    , _InvalidRequiredProtocol
    , _TooManyDistributions
    , _TooManyCertificates
    , _DistributionAlreadyExists
    , _InvalidQueryStringParameters
    , _MissingBody
    , _IllegalUpdate
    , _InvalidIfMatchVersion
    , _PreconditionFailed
    , _InvalidResponseCode
    , _InvalidHeadersForS3Origin
    , _CNAMEAlreadyExists
    , _TrustedSignerDoesNotExist
    , _InvalidProtocolSettings
    , _CloudFrontOriginAccessIdentityAlreadyExists
    , _TooManyOrigins
    , _InvalidRelativePath
    , _StreamingDistributionAlreadyExists
    , _InvalidMinimumProtocolVersion
    , _AccessDenied
    , _InvalidViewerCertificate
    , _NoSuchDistribution
    , _InvalidDefaultRootObject
    , _InvalidGeoRestrictionParameter
    , _InvalidLocationCode

    -- * CertificateSource
    , CertificateSource (..)

    -- * GeoRestrictionType
    , GeoRestrictionType (..)

    -- * HTTPVersion
    , HTTPVersion (..)

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

    -- * SSLProtocol
    , SSLProtocol (..)

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
    , cbCompress
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

    -- * CustomHeaders
    , CustomHeaders
    , customHeaders
    , chItems
    , chQuantity

    -- * CustomOriginConfig
    , CustomOriginConfig
    , customOriginConfig
    , cocOriginSSLProtocols
    , cocHTTPPort
    , cocHTTPSPort
    , cocOriginProtocolPolicy

    -- * DefaultCacheBehavior
    , DefaultCacheBehavior
    , defaultCacheBehavior
    , dcbAllowedMethods
    , dcbMaxTTL
    , dcbCompress
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
    , dARN
    , dStatus
    , dLastModifiedTime
    , dInProgressInvalidationBatches
    , dDomainName
    , dActiveTrustedSigners
    , dDistributionConfig

    -- * DistributionConfig
    , DistributionConfig
    , distributionConfig
    , dcHTTPVersion
    , dcAliases
    , dcDefaultRootObject
    , dcPriceClass
    , dcCustomErrorResponses
    , dcWebACLId
    , dcViewerCertificate
    , dcRestrictions
    , dcLogging
    , dcCacheBehaviors
    , dcCallerReference
    , dcOrigins
    , dcDefaultCacheBehavior
    , dcComment
    , dcEnabled

    -- * DistributionConfigWithTags
    , DistributionConfigWithTags
    , distributionConfigWithTags
    , dcwtDistributionConfig
    , dcwtTags

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
    , dsARN
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
    , dsWebACLId
    , dsHTTPVersion

    -- * ForwardedValues
    , ForwardedValues
    , forwardedValues
    , fvQueryStringCacheKeys
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
    , oCustomHeaders
    , oCustomOriginConfig
    , oS3OriginConfig
    , oOriginPath
    , oId
    , oDomainName

    -- * OriginCustomHeader
    , OriginCustomHeader
    , originCustomHeader
    , ochHeaderName
    , ochHeaderValue

    -- * OriginSSLProtocols
    , OriginSSLProtocols
    , originSSLProtocols
    , ospQuantity
    , ospItems

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

    -- * QueryStringCacheKeys
    , QueryStringCacheKeys
    , queryStringCacheKeys
    , qsckItems
    , qsckQuantity

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
    , sdARN
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

    -- * StreamingDistributionConfigWithTags
    , StreamingDistributionConfigWithTags
    , streamingDistributionConfigWithTags
    , sdcwtStreamingDistributionConfig
    , sdcwtTags

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
    , sdsARN
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

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TagKeys
    , TagKeys
    , tagKeys
    , tkItems

    -- * Tags
    , Tags
    , tags
    , tItems

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
    , vcACMCertificateARN
    , vcCertificateSource
    , vcMinimumProtocolVersion
    , vcCertificate
    , vcIAMCertificateId
    , vcCloudFrontDefaultCertificate
    ) where

import           Network.AWS.CloudFront.Types.Product
import           Network.AWS.CloudFront.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2016-09-07' of the Amazon CloudFront SDK configuration.
cloudFront :: Service
cloudFront =
    Service
    { _svcAbbrev = "CloudFront"
    , _svcSigner = v4
    , _svcPrefix = "cloudfront"
    , _svcVersion = "2016-09-07"
    , _svcEndpoint = defaultEndpoint cloudFront
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "CloudFront"
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Prism for TooManyOriginCustomHeaders' errors.
_TooManyOriginCustomHeaders :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyOriginCustomHeaders =
    _ServiceError . hasStatus 400 . hasCode "TooManyOriginCustomHeaders"

-- | The specified tagging for a CloudFront resource is invalid. For more information, see the error text.
_InvalidTagging :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagging = _ServiceError . hasStatus 400 . hasCode "InvalidTagging"

-- | Prism for InvalidErrorCode' errors.
_InvalidErrorCode :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidErrorCode = _ServiceError . hasStatus 400 . hasCode "InvalidErrorCode"

-- | You cannot create anymore cache behaviors for the distribution.
_TooManyCacheBehaviors :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyCacheBehaviors =
    _ServiceError . hasStatus 400 . hasCode "TooManyCacheBehaviors"

-- | Processing your request would cause you to exceed the maximum number of origin access identities allowed.
_TooManyCloudFrontOriginAccessIdentities :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyCloudFrontOriginAccessIdentities =
    _ServiceError .
    hasStatus 400 . hasCode "TooManyCloudFrontOriginAccessIdentities"

-- | The origin access identity is not valid or doesn\'t exist.
_InvalidOriginAccessIdentity :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOriginAccessIdentity =
    _ServiceError . hasStatus 400 . hasCode "InvalidOriginAccessIdentity"

-- | Prism for DistributionNotDisabled' errors.
_DistributionNotDisabled :: AsError a => Getting (First ServiceError) a ServiceError
_DistributionNotDisabled =
    _ServiceError . hasStatus 409 . hasCode "DistributionNotDisabled"

-- | The specified streaming distribution does not exist.
_NoSuchStreamingDistribution :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchStreamingDistribution =
    _ServiceError . hasStatus 404 . hasCode "NoSuchStreamingDistribution"

-- | The value of Quantity and the size of Items do not match.
_InconsistentQuantities :: AsError a => Getting (First ServiceError) a ServiceError
_InconsistentQuantities =
    _ServiceError . hasStatus 400 . hasCode "InconsistentQuantities"

-- | The argument is invalid.
_InvalidArgument :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgument = _ServiceError . hasStatus 400 . hasCode "InvalidArgument"

-- | You have exceeded the maximum number of allowable InProgress invalidation batch requests, or invalidation objects.
_TooManyInvalidationsInProgress :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyInvalidationsInProgress =
    _ServiceError . hasStatus 400 . hasCode "TooManyInvalidationsInProgress"

-- | Prism for InvalidWebACLId' errors.
_InvalidWebACLId :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidWebACLId = _ServiceError . hasStatus 400 . hasCode "InvalidWebACLId"

-- | Prism for TooManyQueryStringParameters' errors.
_TooManyQueryStringParameters :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyQueryStringParameters =
    _ServiceError . hasStatus 400 . hasCode "TooManyQueryStringParameters"

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyDistributionCNAMEs :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyDistributionCNAMEs =
    _ServiceError . hasStatus 400 . hasCode "TooManyDistributionCNAMEs"

-- | The specified origin access identity does not exist.
_NoSuchCloudFrontOriginAccessIdentity :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchCloudFrontOriginAccessIdentity =
    _ServiceError .
    hasStatus 404 . hasCode "NoSuchCloudFrontOriginAccessIdentity"

-- | Prism for CloudFrontOriginAccessIdentityInUse' errors.
_CloudFrontOriginAccessIdentityInUse :: AsError a => Getting (First ServiceError) a ServiceError
_CloudFrontOriginAccessIdentityInUse =
    _ServiceError .
    hasStatus 409 . hasCode "CloudFrontOriginAccessIdentityInUse"

-- | Processing your request would cause you to exceed the maximum number of streaming distributions allowed.
_TooManyStreamingDistributions :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyStreamingDistributions =
    _ServiceError . hasStatus 400 . hasCode "TooManyStreamingDistributions"

-- | Prism for BatchTooLarge' errors.
_BatchTooLarge :: AsError a => Getting (First ServiceError) a ServiceError
_BatchTooLarge = _ServiceError . hasStatus 413 . hasCode "BatchTooLarge"

-- | Your request contains more cookie names in the whitelist than are allowed per cache behavior.
_TooManyCookieNamesInWhiteList :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyCookieNamesInWhiteList =
    _ServiceError . hasStatus 400 . hasCode "TooManyCookieNamesInWhiteList"

-- | Your request contains forward cookies option which doesn\'t match with the expectation for the whitelisted list of cookie names. Either list of cookie names has been specified when not allowed or list of cookie names is missing when expected.
_InvalidForwardCookies :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidForwardCookies =
    _ServiceError . hasStatus 400 . hasCode "InvalidForwardCookies"

-- | Your request contains more trusted signers than are allowed per distribution.
_TooManyTrustedSigners :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTrustedSigners =
    _ServiceError . hasStatus 400 . hasCode "TooManyTrustedSigners"

-- | The Amazon S3 origin server specified does not refer to a valid Amazon S3 bucket.
_InvalidOrigin :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOrigin = _ServiceError . hasStatus 400 . hasCode "InvalidOrigin"

-- | The specified invalidation does not exist.
_NoSuchInvalidation :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchInvalidation =
    _ServiceError . hasStatus 404 . hasCode "NoSuchInvalidation"

-- | No origin exists with the specified Origin Id.
_NoSuchOrigin :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchOrigin = _ServiceError . hasStatus 404 . hasCode "NoSuchOrigin"

-- | Prism for InvalidTTLOrder' errors.
_InvalidTTLOrder :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTTLOrder = _ServiceError . hasStatus 400 . hasCode "InvalidTTLOrder"

-- | Prism for StreamingDistributionNotDisabled' errors.
_StreamingDistributionNotDisabled :: AsError a => Getting (First ServiceError) a ServiceError
_StreamingDistributionNotDisabled =
    _ServiceError . hasStatus 409 . hasCode "StreamingDistributionNotDisabled"

-- | Prism for TooManyHeadersInForwardedValues' errors.
_TooManyHeadersInForwardedValues :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyHeadersInForwardedValues =
    _ServiceError . hasStatus 400 . hasCode "TooManyHeadersInForwardedValues"

-- | The specified CloudFront resource does not exist.
_NoSuchResource :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchResource = _ServiceError . hasStatus 404 . hasCode "NoSuchResource"

-- | Prism for TooManyStreamingDistributionCNAMEs' errors.
_TooManyStreamingDistributionCNAMEs :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyStreamingDistributionCNAMEs =
    _ServiceError .
    hasStatus 400 . hasCode "TooManyStreamingDistributionCNAMEs"

-- | This operation requires the HTTPS protocol. Ensure that you specify the HTTPS protocol in your request, or omit the RequiredProtocols element from your distribution configuration.
_InvalidRequiredProtocol :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequiredProtocol =
    _ServiceError . hasStatus 400 . hasCode "InvalidRequiredProtocol"

-- | Processing your request would cause you to exceed the maximum number of distributions allowed.
_TooManyDistributions :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyDistributions =
    _ServiceError . hasStatus 400 . hasCode "TooManyDistributions"

-- | You cannot create anymore custom ssl certificates.
_TooManyCertificates :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyCertificates =
    _ServiceError . hasStatus 400 . hasCode "TooManyCertificates"

-- | The caller reference you attempted to create the distribution with is associated with another distribution.
_DistributionAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_DistributionAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "DistributionAlreadyExists"

-- | Prism for InvalidQueryStringParameters' errors.
_InvalidQueryStringParameters :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidQueryStringParameters =
    _ServiceError . hasStatus 400 . hasCode "InvalidQueryStringParameters"

-- | This operation requires a body. Ensure that the body is present and the Content-Type header is set.
_MissingBody :: AsError a => Getting (First ServiceError) a ServiceError
_MissingBody = _ServiceError . hasStatus 400 . hasCode "MissingBody"

-- | Origin and CallerReference cannot be updated.
_IllegalUpdate :: AsError a => Getting (First ServiceError) a ServiceError
_IllegalUpdate = _ServiceError . hasStatus 400 . hasCode "IllegalUpdate"

-- | The If-Match version is missing or not valid for the distribution.
_InvalidIfMatchVersion :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIfMatchVersion =
    _ServiceError . hasStatus 400 . hasCode "InvalidIfMatchVersion"

-- | The precondition given in one or more of the request-header fields evaluated to false.
_PreconditionFailed :: AsError a => Getting (First ServiceError) a ServiceError
_PreconditionFailed =
    _ServiceError . hasStatus 412 . hasCode "PreconditionFailed"

-- | Prism for InvalidResponseCode' errors.
_InvalidResponseCode :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResponseCode =
    _ServiceError . hasStatus 400 . hasCode "InvalidResponseCode"

-- | Prism for InvalidHeadersForS3Origin' errors.
_InvalidHeadersForS3Origin :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidHeadersForS3Origin =
    _ServiceError . hasStatus 400 . hasCode "InvalidHeadersForS3Origin"

-- | Prism for CNAMEAlreadyExists' errors.
_CNAMEAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_CNAMEAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "CNAMEAlreadyExists"

-- | One or more of your trusted signers do not exist.
_TrustedSignerDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_TrustedSignerDoesNotExist =
    _ServiceError . hasStatus 400 . hasCode "TrustedSignerDoesNotExist"

-- | You cannot specify SSLv3 as the minimum protocol version if you only want to support only clients that Support Server Name Indication (SNI).
_InvalidProtocolSettings :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidProtocolSettings =
    _ServiceError . hasStatus 400 . hasCode "InvalidProtocolSettings"

-- | If the CallerReference is a value you already sent in a previous request to create an identity but the content of the CloudFrontOriginAccessIdentityConfig is different from the original request, CloudFront returns a CloudFrontOriginAccessIdentityAlreadyExists error.
_CloudFrontOriginAccessIdentityAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_CloudFrontOriginAccessIdentityAlreadyExists =
    _ServiceError .
    hasStatus 409 . hasCode "CloudFrontOriginAccessIdentityAlreadyExists"

-- | You cannot create anymore origins for the distribution.
_TooManyOrigins :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyOrigins = _ServiceError . hasStatus 400 . hasCode "TooManyOrigins"

-- | The relative path is too big, is not URL-encoded, or does not begin with a slash (\/).
_InvalidRelativePath :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRelativePath =
    _ServiceError . hasStatus 400 . hasCode "InvalidRelativePath"

-- | Prism for StreamingDistributionAlreadyExists' errors.
_StreamingDistributionAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_StreamingDistributionAlreadyExists =
    _ServiceError .
    hasStatus 409 . hasCode "StreamingDistributionAlreadyExists"

-- | Prism for InvalidMinimumProtocolVersion' errors.
_InvalidMinimumProtocolVersion :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMinimumProtocolVersion =
    _ServiceError . hasStatus 400 . hasCode "InvalidMinimumProtocolVersion"

-- | Access denied.
_AccessDenied :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDenied = _ServiceError . hasStatus 403 . hasCode "AccessDenied"

-- | Prism for InvalidViewerCertificate' errors.
_InvalidViewerCertificate :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidViewerCertificate =
    _ServiceError . hasStatus 400 . hasCode "InvalidViewerCertificate"

-- | The specified distribution does not exist.
_NoSuchDistribution :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDistribution =
    _ServiceError . hasStatus 404 . hasCode "NoSuchDistribution"

-- | The default root object file name is too big or contains an invalid character.
_InvalidDefaultRootObject :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDefaultRootObject =
    _ServiceError . hasStatus 400 . hasCode "InvalidDefaultRootObject"

-- | Prism for InvalidGeoRestrictionParameter' errors.
_InvalidGeoRestrictionParameter :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGeoRestrictionParameter =
    _ServiceError . hasStatus 400 . hasCode "InvalidGeoRestrictionParameter"

-- | Prism for InvalidLocationCode' errors.
_InvalidLocationCode :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLocationCode =
    _ServiceError . hasStatus 400 . hasCode "InvalidLocationCode"
