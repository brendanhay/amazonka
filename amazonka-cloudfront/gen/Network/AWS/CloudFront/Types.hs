{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types
    (
    -- * Service
      CloudFront

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
    , aliItems
    , aliQuantity

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
    , cerResponsePagePath
    , cerResponseCode
    , cerErrorCachingMinTTL
    , cerErrorCode

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
    , disId
    , disStatus
    , disLastModifiedTime
    , disInProgressInvalidationBatches
    , disDomainName
    , disActiveTrustedSigners
    , disDistributionConfig

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
    , heaItems
    , heaQuantity

    -- * Invalidation
    , Invalidation
    , invalidation
    , invId
    , invStatus
    , invCreateTime
    , invInvalidationBatch

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
    , oriCustomOriginConfig
    , oriS3OriginConfig
    , oriOriginPath
    , oriId
    , oriDomainName

    -- * Origins
    , Origins
    , origins
    , oriItems
    , oriQuantity

    -- * Paths
    , Paths
    , paths
    , patItems
    , patQuantity

    -- * Restrictions
    , Restrictions
    , restrictions
    , resGeoRestriction

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
    , sigAWSAccountNumber
    , sigKeyPairIds

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

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2015-04-17@ of the Amazon CloudFront SDK.
data CloudFront

instance AWSService CloudFront where
    type Sg CloudFront = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "CloudFront"
            , _svcPrefix = "cloudfront"
            , _svcVersion = "2015-04-17"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
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
_InvalidErrorCode :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidErrorCode = _ServiceError . hasStatus 400 . hasCode "InvalidErrorCode"

-- | You cannot create anymore cache behaviors for the distribution.
_TooManyCacheBehaviors :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyCacheBehaviors =
    _ServiceError . hasStatus 400 . hasCode "TooManyCacheBehaviors"

-- | Prism for DistributionNotDisabled' errors.
_DistributionNotDisabled :: AWSError a => Getting (First ServiceError) a ServiceError
_DistributionNotDisabled =
    _ServiceError . hasStatus 409 . hasCode "DistributionNotDisabled"

-- | The origin access identity is not valid or doesn\'t exist.
_InvalidOriginAccessIdentity :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidOriginAccessIdentity =
    _ServiceError . hasStatus 400 . hasCode "InvalidOriginAccessIdentity"

-- | Processing your request would cause you to exceed the maximum number of
-- origin access identities allowed.
_TooManyCloudFrontOriginAccessIdentities :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyCloudFrontOriginAccessIdentities =
    _ServiceError .
    hasStatus 400 . hasCode "TooManyCloudFrontOriginAccessIdentities"

-- | Processing your request would cause you to exceed the maximum number of
-- streaming distributions allowed.
_TooManyStreamingDistributions :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyStreamingDistributions =
    _ServiceError . hasStatus 400 . hasCode "TooManyStreamingDistributions"

-- | The argument is invalid.
_InvalidArgument :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidArgument = _ServiceError . hasStatus 400 . hasCode "InvalidArgument"

-- | The specified origin access identity does not exist.
_NoSuchCloudFrontOriginAccessIdentity :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchCloudFrontOriginAccessIdentity =
    _ServiceError .
    hasStatus 404 . hasCode "NoSuchCloudFrontOriginAccessIdentity"

-- | The specified streaming distribution does not exist.
_NoSuchStreamingDistribution :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchStreamingDistribution =
    _ServiceError . hasStatus 404 . hasCode "NoSuchStreamingDistribution"

-- | Prism for CloudFrontOriginAccessIdentityInUse' errors.
_CloudFrontOriginAccessIdentityInUse :: AWSError a => Getting (First ServiceError) a ServiceError
_CloudFrontOriginAccessIdentityInUse =
    _ServiceError .
    hasStatus 409 . hasCode "CloudFrontOriginAccessIdentityInUse"

-- | The value of Quantity and the size of Items do not match.
_InconsistentQuantities :: AWSError a => Getting (First ServiceError) a ServiceError
_InconsistentQuantities =
    _ServiceError . hasStatus 400 . hasCode "InconsistentQuantities"

-- | You have exceeded the maximum number of allowable InProgress
-- invalidation batch requests, or invalidation objects.
_TooManyInvalidationsInProgress :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyInvalidationsInProgress =
    _ServiceError . hasStatus 400 . hasCode "TooManyInvalidationsInProgress"

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyDistributionCNAMEs :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyDistributionCNAMEs =
    _ServiceError . hasStatus 400 . hasCode "TooManyDistributionCNAMEs"

-- | Your request contains forward cookies option which doesn\'t match with
-- the expectation for the whitelisted list of cookie names. Either list of
-- cookie names has been specified when not allowed or list of cookie names
-- is missing when expected.
_InvalidForwardCookies :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidForwardCookies =
    _ServiceError . hasStatus 400 . hasCode "InvalidForwardCookies"

-- | Your request contains more cookie names in the whitelist than are
-- allowed per cache behavior.
_TooManyCookieNamesInWhiteList :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyCookieNamesInWhiteList =
    _ServiceError . hasStatus 400 . hasCode "TooManyCookieNamesInWhiteList"

-- | Prism for BatchTooLarge' errors.
_BatchTooLarge :: AWSError a => Getting (First ServiceError) a ServiceError
_BatchTooLarge = _ServiceError . hasStatus 413 . hasCode "BatchTooLarge"

-- | The Amazon S3 origin server specified does not refer to a valid Amazon
-- S3 bucket.
_InvalidOrigin :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidOrigin = _ServiceError . hasStatus 400 . hasCode "InvalidOrigin"

-- | Your request contains more trusted signers than are allowed per
-- distribution.
_TooManyTrustedSigners :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyTrustedSigners =
    _ServiceError . hasStatus 400 . hasCode "TooManyTrustedSigners"

-- | No origin exists with the specified Origin Id.
_NoSuchOrigin :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchOrigin = _ServiceError . hasStatus 404 . hasCode "NoSuchOrigin"

-- | The specified invalidation does not exist.
_NoSuchInvalidation :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchInvalidation =
    _ServiceError . hasStatus 404 . hasCode "NoSuchInvalidation"

-- | Prism for StreamingDistributionNotDisabled' errors.
_StreamingDistributionNotDisabled :: AWSError a => Getting (First ServiceError) a ServiceError
_StreamingDistributionNotDisabled =
    _ServiceError . hasStatus 409 . hasCode "StreamingDistributionNotDisabled"

-- | Prism for InvalidTTLOrder' errors.
_InvalidTTLOrder :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidTTLOrder = _ServiceError . hasStatus 400 . hasCode "InvalidTTLOrder"

-- | Prism for TooManyStreamingDistributionCNAMEs' errors.
_TooManyStreamingDistributionCNAMEs :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyStreamingDistributionCNAMEs =
    _ServiceError .
    hasStatus 400 . hasCode "TooManyStreamingDistributionCNAMEs"

-- | Processing your request would cause you to exceed the maximum number of
-- distributions allowed.
_TooManyDistributions :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyDistributions =
    _ServiceError . hasStatus 400 . hasCode "TooManyDistributions"

-- | This operation requires the HTTPS protocol. Ensure that you specify the
-- HTTPS protocol in your request, or omit the RequiredProtocols element
-- from your distribution configuration.
_InvalidRequiredProtocol :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRequiredProtocol =
    _ServiceError . hasStatus 400 . hasCode "InvalidRequiredProtocol"

-- | Prism for TooManyHeadersInForwardedValues' errors.
_TooManyHeadersInForwardedValues :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyHeadersInForwardedValues =
    _ServiceError . hasStatus 400 . hasCode "TooManyHeadersInForwardedValues"

-- | You cannot create anymore custom ssl certificates.
_TooManyCertificates :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyCertificates =
    _ServiceError . hasStatus 400 . hasCode "TooManyCertificates"

-- | This operation requires a body. Ensure that the body is present and the
-- Content-Type header is set.
_MissingBody :: AWSError a => Getting (First ServiceError) a ServiceError
_MissingBody = _ServiceError . hasStatus 400 . hasCode "MissingBody"

-- | The caller reference you attempted to create the distribution with is
-- associated with another distribution.
_DistributionAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_DistributionAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "DistributionAlreadyExists"

-- | Origin and CallerReference cannot be updated.
_IllegalUpdate :: AWSError a => Getting (First ServiceError) a ServiceError
_IllegalUpdate = _ServiceError . hasStatus 400 . hasCode "IllegalUpdate"

-- | Prism for InvalidResponseCode' errors.
_InvalidResponseCode :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidResponseCode =
    _ServiceError . hasStatus 400 . hasCode "InvalidResponseCode"

-- | The If-Match version is missing or not valid for the distribution.
_InvalidIfMatchVersion :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidIfMatchVersion =
    _ServiceError . hasStatus 400 . hasCode "InvalidIfMatchVersion"

-- | The precondition given in one or more of the request-header fields
-- evaluated to false.
_PreconditionFailed :: AWSError a => Getting (First ServiceError) a ServiceError
_PreconditionFailed =
    _ServiceError . hasStatus 412 . hasCode "PreconditionFailed"

-- | You cannot specify SSLv3 as the minimum protocol version if you only
-- want to support only clients that Support Server Name Indication (SNI).
_InvalidProtocolSettings :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidProtocolSettings =
    _ServiceError . hasStatus 400 . hasCode "InvalidProtocolSettings"

-- | One or more of your trusted signers do not exist.
_TrustedSignerDoesNotExist :: AWSError a => Getting (First ServiceError) a ServiceError
_TrustedSignerDoesNotExist =
    _ServiceError . hasStatus 400 . hasCode "TrustedSignerDoesNotExist"

-- | Prism for InvalidHeadersForS3Origin' errors.
_InvalidHeadersForS3Origin :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidHeadersForS3Origin =
    _ServiceError . hasStatus 400 . hasCode "InvalidHeadersForS3Origin"

-- | Prism for CNAMEAlreadyExists' errors.
_CNAMEAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_CNAMEAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "CNAMEAlreadyExists"

-- | Prism for StreamingDistributionAlreadyExists' errors.
_StreamingDistributionAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_StreamingDistributionAlreadyExists =
    _ServiceError .
    hasStatus 409 . hasCode "StreamingDistributionAlreadyExists"

-- | You cannot create anymore origins for the distribution.
_TooManyOrigins :: AWSError a => Getting (First ServiceError) a ServiceError
_TooManyOrigins = _ServiceError . hasStatus 400 . hasCode "TooManyOrigins"

-- | If the CallerReference is a value you already sent in a previous request
-- to create an identity but the content of the
-- CloudFrontOriginAccessIdentityConfig is different from the original
-- request, CloudFront returns a
-- CloudFrontOriginAccessIdentityAlreadyExists error.
_CloudFrontOriginAccessIdentityAlreadyExists :: AWSError a => Getting (First ServiceError) a ServiceError
_CloudFrontOriginAccessIdentityAlreadyExists =
    _ServiceError .
    hasStatus 409 . hasCode "CloudFrontOriginAccessIdentityAlreadyExists"

-- | The relative path is too big, is not URL-encoded, or does not begin with
-- a slash (\/).
_InvalidRelativePath :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRelativePath =
    _ServiceError . hasStatus 400 . hasCode "InvalidRelativePath"

-- | Prism for InvalidMinimumProtocolVersion' errors.
_InvalidMinimumProtocolVersion :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidMinimumProtocolVersion =
    _ServiceError . hasStatus 400 . hasCode "InvalidMinimumProtocolVersion"

-- | Access denied.
_AccessDenied :: AWSError a => Getting (First ServiceError) a ServiceError
_AccessDenied = _ServiceError . hasStatus 403 . hasCode "AccessDenied"

-- | The specified distribution does not exist.
_NoSuchDistribution :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchDistribution =
    _ServiceError . hasStatus 404 . hasCode "NoSuchDistribution"

-- | Prism for InvalidViewerCertificate' errors.
_InvalidViewerCertificate :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidViewerCertificate =
    _ServiceError . hasStatus 400 . hasCode "InvalidViewerCertificate"

-- | The default root object file name is too big or contains an invalid
-- character.
_InvalidDefaultRootObject :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDefaultRootObject =
    _ServiceError . hasStatus 400 . hasCode "InvalidDefaultRootObject"

-- | Prism for InvalidLocationCode' errors.
_InvalidLocationCode :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidLocationCode =
    _ServiceError . hasStatus 400 . hasCode "InvalidLocationCode"

-- | Prism for InvalidGeoRestrictionParameter' errors.
_InvalidGeoRestrictionParameter :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidGeoRestrictionParameter =
    _ServiceError . hasStatus 400 . hasCode "InvalidGeoRestrictionParameter"

data GeoRestrictionType
    = None
    | Whitelist
    | Blacklist
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText GeoRestrictionType where
    parser = takeLowerText >>= \case
        "blacklist" -> pure Blacklist
        "none" -> pure None
        "whitelist" -> pure Whitelist
        e -> fromTextError $ "Failure parsing GeoRestrictionType from value: '" <> e
           <> "'. Accepted values: blacklist, none, whitelist"

instance ToText GeoRestrictionType where
    toText = \case
        Blacklist -> "blacklist"
        None -> "none"
        Whitelist -> "whitelist"

instance Hashable GeoRestrictionType
instance ToQuery GeoRestrictionType
instance ToHeader GeoRestrictionType

instance FromXML GeoRestrictionType where
    parseXML = parseXMLText "GeoRestrictionType"

instance ToXML GeoRestrictionType where
    toXML = toXMLText

data ItemSelection
    = ISWhitelist
    | ISNone
    | ISAll
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ItemSelection where
    parser = takeLowerText >>= \case
        "all" -> pure ISAll
        "none" -> pure ISNone
        "whitelist" -> pure ISWhitelist
        e -> fromTextError $ "Failure parsing ItemSelection from value: '" <> e
           <> "'. Accepted values: all, none, whitelist"

instance ToText ItemSelection where
    toText = \case
        ISAll -> "all"
        ISNone -> "none"
        ISWhitelist -> "whitelist"

instance Hashable ItemSelection
instance ToQuery ItemSelection
instance ToHeader ItemSelection

instance FromXML ItemSelection where
    parseXML = parseXMLText "ItemSelection"

instance ToXML ItemSelection where
    toXML = toXMLText

data Method
    = Head
    | Post
    | Patch
    | Get
    | Options
    | Put
    | Delete
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Method where
    parser = takeLowerText >>= \case
        "delete" -> pure Delete
        "get" -> pure Get
        "head" -> pure Head
        "options" -> pure Options
        "patch" -> pure Patch
        "post" -> pure Post
        "put" -> pure Put
        e -> fromTextError $ "Failure parsing Method from value: '" <> e
           <> "'. Accepted values: delete, get, head, options, patch, post, put"

instance ToText Method where
    toText = \case
        Delete -> "delete"
        Get -> "get"
        Head -> "head"
        Options -> "options"
        Patch -> "patch"
        Post -> "post"
        Put -> "put"

instance Hashable Method
instance ToQuery Method
instance ToHeader Method

instance FromXML Method where
    parseXML = parseXMLText "Method"

instance ToXML Method where
    toXML = toXMLText

data MinimumProtocolVersion
    = TLSV1
    | SSLV3
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText MinimumProtocolVersion where
    parser = takeLowerText >>= \case
        "sslv3" -> pure SSLV3
        "tlsv1" -> pure TLSV1
        e -> fromTextError $ "Failure parsing MinimumProtocolVersion from value: '" <> e
           <> "'. Accepted values: sslv3, tlsv1"

instance ToText MinimumProtocolVersion where
    toText = \case
        SSLV3 -> "sslv3"
        TLSV1 -> "tlsv1"

instance Hashable MinimumProtocolVersion
instance ToQuery MinimumProtocolVersion
instance ToHeader MinimumProtocolVersion

instance FromXML MinimumProtocolVersion where
    parseXML = parseXMLText "MinimumProtocolVersion"

instance ToXML MinimumProtocolVersion where
    toXML = toXMLText

data OriginProtocolPolicy
    = HTTPOnly
    | MatchViewer
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OriginProtocolPolicy where
    parser = takeLowerText >>= \case
        "http-only" -> pure HTTPOnly
        "match-viewer" -> pure MatchViewer
        e -> fromTextError $ "Failure parsing OriginProtocolPolicy from value: '" <> e
           <> "'. Accepted values: http-only, match-viewer"

instance ToText OriginProtocolPolicy where
    toText = \case
        HTTPOnly -> "http-only"
        MatchViewer -> "match-viewer"

instance Hashable OriginProtocolPolicy
instance ToQuery OriginProtocolPolicy
instance ToHeader OriginProtocolPolicy

instance FromXML OriginProtocolPolicy where
    parseXML = parseXMLText "OriginProtocolPolicy"

instance ToXML OriginProtocolPolicy where
    toXML = toXMLText

data PriceClass
    = PriceClass200
    | PriceClass100
    | PriceClassAll
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText PriceClass where
    parser = takeLowerText >>= \case
        "priceclass_100" -> pure PriceClass100
        "priceclass_200" -> pure PriceClass200
        "priceclass_all" -> pure PriceClassAll
        e -> fromTextError $ "Failure parsing PriceClass from value: '" <> e
           <> "'. Accepted values: priceclass_100, priceclass_200, priceclass_all"

instance ToText PriceClass where
    toText = \case
        PriceClass100 -> "priceclass_100"
        PriceClass200 -> "priceclass_200"
        PriceClassAll -> "priceclass_all"

instance Hashable PriceClass
instance ToQuery PriceClass
instance ToHeader PriceClass

instance FromXML PriceClass where
    parseXML = parseXMLText "PriceClass"

instance ToXML PriceClass where
    toXML = toXMLText

data SSLSupportMethod
    = VIP
    | SNIOnly
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SSLSupportMethod where
    parser = takeLowerText >>= \case
        "sni-only" -> pure SNIOnly
        "vip" -> pure VIP
        e -> fromTextError $ "Failure parsing SSLSupportMethod from value: '" <> e
           <> "'. Accepted values: sni-only, vip"

instance ToText SSLSupportMethod where
    toText = \case
        SNIOnly -> "sni-only"
        VIP -> "vip"

instance Hashable SSLSupportMethod
instance ToQuery SSLSupportMethod
instance ToHeader SSLSupportMethod

instance FromXML SSLSupportMethod where
    parseXML = parseXMLText "SSLSupportMethod"

instance ToXML SSLSupportMethod where
    toXML = toXMLText

data ViewerProtocolPolicy
    = HTTPSOnly
    | RedirectTOHTTPS
    | AllowAll
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ViewerProtocolPolicy where
    parser = takeLowerText >>= \case
        "allow-all" -> pure AllowAll
        "https-only" -> pure HTTPSOnly
        "redirect-to-https" -> pure RedirectTOHTTPS
        e -> fromTextError $ "Failure parsing ViewerProtocolPolicy from value: '" <> e
           <> "'. Accepted values: allow-all, https-only, redirect-to-https"

instance ToText ViewerProtocolPolicy where
    toText = \case
        AllowAll -> "allow-all"
        HTTPSOnly -> "https-only"
        RedirectTOHTTPS -> "redirect-to-https"

instance Hashable ViewerProtocolPolicy
instance ToQuery ViewerProtocolPolicy
instance ToHeader ViewerProtocolPolicy

instance FromXML ViewerProtocolPolicy where
    parseXML = parseXMLText "ViewerProtocolPolicy"

instance ToXML ViewerProtocolPolicy where
    toXML = toXMLText

-- | A complex type that lists the AWS accounts, if any, that you included in
-- the TrustedSigners complex type for the default cache behavior or for
-- any of the other cache behaviors for this distribution. These are
-- accounts that you want to allow to create signed URLs for private
-- content.
--
-- /See:/ 'activeTrustedSigners' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atsItems'
--
-- * 'atsEnabled'
--
-- * 'atsQuantity'
data ActiveTrustedSigners = ActiveTrustedSigners'
    { _atsItems    :: !(Maybe [Signer])
    , _atsEnabled  :: !Bool
    , _atsQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ActiveTrustedSigners' smart constructor.
activeTrustedSigners :: Bool -> Int -> ActiveTrustedSigners
activeTrustedSigners pEnabled pQuantity =
    ActiveTrustedSigners'
    { _atsItems = Nothing
    , _atsEnabled = pEnabled
    , _atsQuantity = pQuantity
    }

-- | A complex type that contains one Signer complex type for each unique
-- trusted signer that is specified in the TrustedSigners complex type,
-- including trusted signers in the default cache behavior and in all of
-- the other cache behaviors.
atsItems :: Lens' ActiveTrustedSigners [Signer]
atsItems = lens _atsItems (\ s a -> s{_atsItems = a}) . _Default;

-- | Each active trusted signer.
atsEnabled :: Lens' ActiveTrustedSigners Bool
atsEnabled = lens _atsEnabled (\ s a -> s{_atsEnabled = a});

-- | The number of unique trusted signers included in all cache behaviors.
-- For example, if three cache behaviors all list the same three AWS
-- accounts, the value of Quantity for ActiveTrustedSigners will be 3.
atsQuantity :: Lens' ActiveTrustedSigners Int
atsQuantity = lens _atsQuantity (\ s a -> s{_atsQuantity = a});

instance FromXML ActiveTrustedSigners where
        parseXML x
          = ActiveTrustedSigners' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Signer"))
                <*> (x .@ "Enabled")
                <*> (x .@ "Quantity")

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
--
-- /See:/ 'aliases' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aliItems'
--
-- * 'aliQuantity'
data Aliases = Aliases'
    { _aliItems    :: !(Maybe [Text])
    , _aliQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Aliases' smart constructor.
aliases :: Int -> Aliases
aliases pQuantity =
    Aliases'
    { _aliItems = Nothing
    , _aliQuantity = pQuantity
    }

-- | Optional: A complex type that contains CNAME elements, if any, for this
-- distribution. If Quantity is 0, you can omit Items.
aliItems :: Lens' Aliases [Text]
aliItems = lens _aliItems (\ s a -> s{_aliItems = a}) . _Default;

-- | The number of CNAMEs, if any, for this distribution.
aliQuantity :: Lens' Aliases Int
aliQuantity = lens _aliQuantity (\ s a -> s{_aliQuantity = a});

instance FromXML Aliases where
        parseXML x
          = Aliases' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "CNAME"))
                <*> (x .@ "Quantity")

instance ToXML Aliases where
        toXML Aliases'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "CNAME" <$> _aliItems),
               "Quantity" @= _aliQuantity]

-- | A complex type that controls which HTTP methods CloudFront processes and
-- forwards to your Amazon S3 bucket or your custom origin. There are three
-- choices: - CloudFront forwards only GET and HEAD requests. - CloudFront
-- forwards only GET, HEAD and OPTIONS requests. - CloudFront forwards GET,
-- HEAD, OPTIONS, PUT, PATCH, POST, and DELETE requests. If you pick the
-- third choice, you may need to restrict access to your Amazon S3 bucket
-- or to your custom origin so users can\'t perform operations that you
-- don\'t want them to. For example, you may not want users to have
-- permission to delete objects from your origin.
--
-- /See:/ 'allowedMethods' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amCachedMethods'
--
-- * 'amQuantity'
--
-- * 'amItems'
data AllowedMethods = AllowedMethods'
    { _amCachedMethods :: !(Maybe CachedMethods)
    , _amQuantity      :: !Int
    , _amItems         :: ![Method]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AllowedMethods' smart constructor.
allowedMethods :: Int -> AllowedMethods
allowedMethods pQuantity =
    AllowedMethods'
    { _amCachedMethods = Nothing
    , _amQuantity = pQuantity
    , _amItems = mempty
    }

-- | FIXME: Undocumented member.
amCachedMethods :: Lens' AllowedMethods (Maybe CachedMethods)
amCachedMethods = lens _amCachedMethods (\ s a -> s{_amCachedMethods = a});

-- | The number of HTTP methods that you want CloudFront to forward to your
-- origin. Valid values are 2 (for GET and HEAD requests), 3 (for GET, HEAD
-- and OPTIONS requests) and 7 (for GET, HEAD, OPTIONS, PUT, PATCH, POST,
-- and DELETE requests).
amQuantity :: Lens' AllowedMethods Int
amQuantity = lens _amQuantity (\ s a -> s{_amQuantity = a});

-- | A complex type that contains the HTTP methods that you want CloudFront
-- to process and forward to your origin.
amItems :: Lens' AllowedMethods [Method]
amItems = lens _amItems (\ s a -> s{_amItems = a});

instance FromXML AllowedMethods where
        parseXML x
          = AllowedMethods' <$>
              (x .@? "CachedMethods") <*> (x .@ "Quantity") <*>
                (x .@? "Items" .!@ mempty >>= parseXMLList "Method")

instance ToXML AllowedMethods where
        toXML AllowedMethods'{..}
          = mconcat
              ["CachedMethods" @= _amCachedMethods,
               "Quantity" @= _amQuantity,
               "Items" @= toXMLList "Method" _amItems]

-- | A complex type that describes how CloudFront processes requests. You can
-- create up to 10 cache behaviors.You must create at least as many cache
-- behaviors (including the default cache behavior) as you have origins if
-- you want CloudFront to distribute objects from all of the origins. Each
-- cache behavior specifies the one origin from which you want CloudFront
-- to get objects. If you have two origins and only the default cache
-- behavior, the default cache behavior will cause CloudFront to get
-- objects from one of the origins, but the other origin will never be
-- used. If you don\'t want to specify any cache behaviors, include only an
-- empty CacheBehaviors element. Don\'t include an empty CacheBehavior
-- element, or CloudFront returns a MalformedXML error. To delete all cache
-- behaviors in an existing distribution, update the distribution
-- configuration and include only an empty CacheBehaviors element. To add,
-- change, or remove one or more cache behaviors, update the distribution
-- configuration and specify all of the cache behaviors that you want to
-- include in the updated distribution.
--
-- /See:/ 'cacheBehavior' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbAllowedMethods'
--
-- * 'cbMaxTTL'
--
-- * 'cbSmoothStreaming'
--
-- * 'cbDefaultTTL'
--
-- * 'cbPathPattern'
--
-- * 'cbTargetOriginId'
--
-- * 'cbForwardedValues'
--
-- * 'cbTrustedSigners'
--
-- * 'cbViewerProtocolPolicy'
--
-- * 'cbMinTTL'
data CacheBehavior = CacheBehavior'
    { _cbAllowedMethods       :: !(Maybe AllowedMethods)
    , _cbMaxTTL               :: !(Maybe Integer)
    , _cbSmoothStreaming      :: !(Maybe Bool)
    , _cbDefaultTTL           :: !(Maybe Integer)
    , _cbPathPattern          :: !Text
    , _cbTargetOriginId       :: !Text
    , _cbForwardedValues      :: !ForwardedValues
    , _cbTrustedSigners       :: !TrustedSigners
    , _cbViewerProtocolPolicy :: !ViewerProtocolPolicy
    , _cbMinTTL               :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CacheBehavior' smart constructor.
cacheBehavior :: Text -> Text -> ForwardedValues -> TrustedSigners -> ViewerProtocolPolicy -> Integer -> CacheBehavior
cacheBehavior pPathPattern pTargetOriginId pForwardedValues pTrustedSigners pViewerProtocolPolicy pMinTTL =
    CacheBehavior'
    { _cbAllowedMethods = Nothing
    , _cbMaxTTL = Nothing
    , _cbSmoothStreaming = Nothing
    , _cbDefaultTTL = Nothing
    , _cbPathPattern = pPathPattern
    , _cbTargetOriginId = pTargetOriginId
    , _cbForwardedValues = pForwardedValues
    , _cbTrustedSigners = pTrustedSigners
    , _cbViewerProtocolPolicy = pViewerProtocolPolicy
    , _cbMinTTL = pMinTTL
    }

-- | FIXME: Undocumented member.
cbAllowedMethods :: Lens' CacheBehavior (Maybe AllowedMethods)
cbAllowedMethods = lens _cbAllowedMethods (\ s a -> s{_cbAllowedMethods = a});

-- | The maximum amount of time (in seconds) that an object is in a
-- CloudFront cache before CloudFront forwards another request to your
-- origin to determine whether the object has been updated. The value that
-- you specify applies only when your origin adds HTTP headers such as
-- Cache-Control max-age, Cache-Control s-maxage, and Expires to objects.
-- You can specify a value from 0 to 3,153,600,000 seconds (100 years).
cbMaxTTL :: Lens' CacheBehavior (Maybe Integer)
cbMaxTTL = lens _cbMaxTTL (\ s a -> s{_cbMaxTTL = a});

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
cbSmoothStreaming :: Lens' CacheBehavior (Maybe Bool)
cbSmoothStreaming = lens _cbSmoothStreaming (\ s a -> s{_cbSmoothStreaming = a});

-- | If you don\'t configure your origin to add a Cache-Control max-age
-- directive or an Expires header, DefaultTTL is the default amount of time
-- (in seconds) that an object is in a CloudFront cache before CloudFront
-- forwards another request to your origin to determine whether the object
-- has been updated. The value that you specify applies only when your
-- origin does not add HTTP headers such as Cache-Control max-age,
-- Cache-Control s-maxage, and Expires to objects. You can specify a value
-- from 0 to 3,153,600,000 seconds (100 years).
cbDefaultTTL :: Lens' CacheBehavior (Maybe Integer)
cbDefaultTTL = lens _cbDefaultTTL (\ s a -> s{_cbDefaultTTL = a});

-- | The pattern (for example, images\/*.jpg) that specifies which requests
-- you want this cache behavior to apply to. When CloudFront receives an
-- end-user request, the requested path is compared with path patterns in
-- the order in which cache behaviors are listed in the distribution. The
-- path pattern for the default cache behavior is * and cannot be changed.
-- If the request for an object does not match the path pattern for any
-- cache behaviors, CloudFront applies the behavior in the default cache
-- behavior.
cbPathPattern :: Lens' CacheBehavior Text
cbPathPattern = lens _cbPathPattern (\ s a -> s{_cbPathPattern = a});

-- | The value of ID for the origin that you want CloudFront to route
-- requests to when a request matches the path pattern either for a cache
-- behavior or for the default cache behavior.
cbTargetOriginId :: Lens' CacheBehavior Text
cbTargetOriginId = lens _cbTargetOriginId (\ s a -> s{_cbTargetOriginId = a});

-- | A complex type that specifies how CloudFront handles query strings,
-- cookies and headers.
cbForwardedValues :: Lens' CacheBehavior ForwardedValues
cbForwardedValues = lens _cbForwardedValues (\ s a -> s{_cbForwardedValues = a});

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
cbTrustedSigners :: Lens' CacheBehavior TrustedSigners
cbTrustedSigners = lens _cbTrustedSigners (\ s a -> s{_cbTrustedSigners = a});

-- | Use this element to specify the protocol that users can use to access
-- the files in the origin specified by TargetOriginId when a request
-- matches the path pattern in PathPattern. If you want CloudFront to allow
-- end users to use any available protocol, specify allow-all. If you want
-- CloudFront to require HTTPS, specify https. If you want CloudFront to
-- respond to an HTTP request with an HTTP status code of 301 (Moved
-- Permanently) and the HTTPS URL, specify redirect-to-https. The viewer
-- then resubmits the request using the HTTPS URL.
cbViewerProtocolPolicy :: Lens' CacheBehavior ViewerProtocolPolicy
cbViewerProtocolPolicy = lens _cbViewerProtocolPolicy (\ s a -> s{_cbViewerProtocolPolicy = a});

-- | The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object
-- has been updated.You can specify a value from 0 to 3,153,600,000 seconds
-- (100 years).
cbMinTTL :: Lens' CacheBehavior Integer
cbMinTTL = lens _cbMinTTL (\ s a -> s{_cbMinTTL = a});

instance FromXML CacheBehavior where
        parseXML x
          = CacheBehavior' <$>
              (x .@? "AllowedMethods") <*> (x .@? "MaxTTL") <*>
                (x .@? "SmoothStreaming")
                <*> (x .@? "DefaultTTL")
                <*> (x .@ "PathPattern")
                <*> (x .@ "TargetOriginId")
                <*> (x .@ "ForwardedValues")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "ViewerProtocolPolicy")
                <*> (x .@ "MinTTL")

instance ToXML CacheBehavior where
        toXML CacheBehavior'{..}
          = mconcat
              ["AllowedMethods" @= _cbAllowedMethods,
               "MaxTTL" @= _cbMaxTTL,
               "SmoothStreaming" @= _cbSmoothStreaming,
               "DefaultTTL" @= _cbDefaultTTL,
               "PathPattern" @= _cbPathPattern,
               "TargetOriginId" @= _cbTargetOriginId,
               "ForwardedValues" @= _cbForwardedValues,
               "TrustedSigners" @= _cbTrustedSigners,
               "ViewerProtocolPolicy" @= _cbViewerProtocolPolicy,
               "MinTTL" @= _cbMinTTL]

-- | A complex type that contains zero or more CacheBehavior elements.
--
-- /See:/ 'cacheBehaviors' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbItems'
--
-- * 'cbQuantity'
data CacheBehaviors = CacheBehaviors'
    { _cbItems    :: !(Maybe [CacheBehavior])
    , _cbQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CacheBehaviors' smart constructor.
cacheBehaviors :: Int -> CacheBehaviors
cacheBehaviors pQuantity =
    CacheBehaviors'
    { _cbItems = Nothing
    , _cbQuantity = pQuantity
    }

-- | Optional: A complex type that contains cache behaviors for this
-- distribution. If Quantity is 0, you can omit Items.
cbItems :: Lens' CacheBehaviors [CacheBehavior]
cbItems = lens _cbItems (\ s a -> s{_cbItems = a}) . _Default;

-- | The number of cache behaviors for this distribution.
cbQuantity :: Lens' CacheBehaviors Int
cbQuantity = lens _cbQuantity (\ s a -> s{_cbQuantity = a});

instance FromXML CacheBehaviors where
        parseXML x
          = CacheBehaviors' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "CacheBehavior"))
                <*> (x .@ "Quantity")

instance ToXML CacheBehaviors where
        toXML CacheBehaviors'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "CacheBehavior" <$> _cbItems),
               "Quantity" @= _cbQuantity]

-- | A complex type that controls whether CloudFront caches the response to
-- requests using the specified HTTP methods. There are two choices: -
-- CloudFront caches responses to GET and HEAD requests. - CloudFront
-- caches responses to GET, HEAD, and OPTIONS requests. If you pick the
-- second choice for your S3 Origin, you may need to forward
-- Access-Control-Request-Method, Access-Control-Request-Headers and Origin
-- headers for the responses to be cached correctly.
--
-- /See:/ 'cachedMethods' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmQuantity'
--
-- * 'cmItems'
data CachedMethods = CachedMethods'
    { _cmQuantity :: !Int
    , _cmItems    :: ![Method]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CachedMethods' smart constructor.
cachedMethods :: Int -> CachedMethods
cachedMethods pQuantity =
    CachedMethods'
    { _cmQuantity = pQuantity
    , _cmItems = mempty
    }

-- | The number of HTTP methods for which you want CloudFront to cache
-- responses. Valid values are 2 (for caching responses to GET and HEAD
-- requests) and 3 (for caching responses to GET, HEAD, and OPTIONS
-- requests).
cmQuantity :: Lens' CachedMethods Int
cmQuantity = lens _cmQuantity (\ s a -> s{_cmQuantity = a});

-- | A complex type that contains the HTTP methods that you want CloudFront
-- to cache responses to.
cmItems :: Lens' CachedMethods [Method]
cmItems = lens _cmItems (\ s a -> s{_cmItems = a});

instance FromXML CachedMethods where
        parseXML x
          = CachedMethods' <$>
              (x .@ "Quantity") <*>
                (x .@? "Items" .!@ mempty >>= parseXMLList "Method")

instance ToXML CachedMethods where
        toXML CachedMethods'{..}
          = mconcat
              ["Quantity" @= _cmQuantity,
               "Items" @= toXMLList "Method" _cmItems]

-- | CloudFront origin access identity.
--
-- /See:/ 'cloudFrontOriginAccessIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfoaiCloudFrontOriginAccessIdentityConfig'
--
-- * 'cfoaiId'
--
-- * 'cfoaiS3CanonicalUserId'
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity'
    { _cfoaiCloudFrontOriginAccessIdentityConfig :: !(Maybe CloudFrontOriginAccessIdentityConfig)
    , _cfoaiId                                   :: !Text
    , _cfoaiS3CanonicalUserId                    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CloudFrontOriginAccessIdentity' smart constructor.
cloudFrontOriginAccessIdentity :: Text -> Text -> CloudFrontOriginAccessIdentity
cloudFrontOriginAccessIdentity pId pS3CanonicalUserId =
    CloudFrontOriginAccessIdentity'
    { _cfoaiCloudFrontOriginAccessIdentityConfig = Nothing
    , _cfoaiId = pId
    , _cfoaiS3CanonicalUserId = pS3CanonicalUserId
    }

-- | The current configuration information for the identity.
cfoaiCloudFrontOriginAccessIdentityConfig :: Lens' CloudFrontOriginAccessIdentity (Maybe CloudFrontOriginAccessIdentityConfig)
cfoaiCloudFrontOriginAccessIdentityConfig = lens _cfoaiCloudFrontOriginAccessIdentityConfig (\ s a -> s{_cfoaiCloudFrontOriginAccessIdentityConfig = a});

-- | The ID for the origin access identity. For example: E74FTE3AJFJ256A.
cfoaiId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiId = lens _cfoaiId (\ s a -> s{_cfoaiId = a});

-- | The Amazon S3 canonical user ID for the origin access identity, which
-- you use when giving the origin access identity read permission to an
-- object in Amazon S3.
cfoaiS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentity Text
cfoaiS3CanonicalUserId = lens _cfoaiS3CanonicalUserId (\ s a -> s{_cfoaiS3CanonicalUserId = a});

instance FromXML CloudFrontOriginAccessIdentity where
        parseXML x
          = CloudFrontOriginAccessIdentity' <$>
              (x .@? "CloudFrontOriginAccessIdentityConfig") <*>
                (x .@ "Id")
                <*> (x .@ "S3CanonicalUserId")

-- | Origin access identity configuration.
--
-- /See:/ 'cloudFrontOriginAccessIdentityConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfoaicCallerReference'
--
-- * 'cfoaicComment'
data CloudFrontOriginAccessIdentityConfig = CloudFrontOriginAccessIdentityConfig'
    { _cfoaicCallerReference :: !Text
    , _cfoaicComment         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CloudFrontOriginAccessIdentityConfig' smart constructor.
cloudFrontOriginAccessIdentityConfig :: Text -> Text -> CloudFrontOriginAccessIdentityConfig
cloudFrontOriginAccessIdentityConfig pCallerReference pComment =
    CloudFrontOriginAccessIdentityConfig'
    { _cfoaicCallerReference = pCallerReference
    , _cfoaicComment = pComment
    }

-- | A unique number that ensures the request can\'t be replayed. If the
-- CallerReference is new (no matter the content of the
-- CloudFrontOriginAccessIdentityConfig object), a new origin access
-- identity is created. If the CallerReference is a value you already sent
-- in a previous request to create an identity, and the content of the
-- CloudFrontOriginAccessIdentityConfig is identical to the original
-- request (ignoring white space), the response includes the same
-- information returned to the original request. If the CallerReference is
-- a value you already sent in a previous request to create an identity but
-- the content of the CloudFrontOriginAccessIdentityConfig is different
-- from the original request, CloudFront returns a
-- CloudFrontOriginAccessIdentityAlreadyExists error.
cfoaicCallerReference :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicCallerReference = lens _cfoaicCallerReference (\ s a -> s{_cfoaicCallerReference = a});

-- | Any comments you want to include about the origin access identity.
cfoaicComment :: Lens' CloudFrontOriginAccessIdentityConfig Text
cfoaicComment = lens _cfoaicComment (\ s a -> s{_cfoaicComment = a});

instance FromXML CloudFrontOriginAccessIdentityConfig
         where
        parseXML x
          = CloudFrontOriginAccessIdentityConfig' <$>
              (x .@ "CallerReference") <*> (x .@ "Comment")

instance ToXML CloudFrontOriginAccessIdentityConfig
         where
        toXML CloudFrontOriginAccessIdentityConfig'{..}
          = mconcat
              ["CallerReference" @= _cfoaicCallerReference,
               "Comment" @= _cfoaicComment]

-- | The CloudFrontOriginAccessIdentityList type.
--
-- /See:/ 'cloudFrontOriginAccessIdentityList' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfoailItems'
--
-- * 'cfoailNextMarker'
--
-- * 'cfoailMarker'
--
-- * 'cfoailMaxItems'
--
-- * 'cfoailIsTruncated'
--
-- * 'cfoailQuantity'
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList'
    { _cfoailItems       :: !(Maybe [CloudFrontOriginAccessIdentitySummary])
    , _cfoailNextMarker  :: !(Maybe Text)
    , _cfoailMarker      :: !Text
    , _cfoailMaxItems    :: !Int
    , _cfoailIsTruncated :: !Bool
    , _cfoailQuantity    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CloudFrontOriginAccessIdentityList' smart constructor.
cloudFrontOriginAccessIdentityList :: Text -> Int -> Bool -> Int -> CloudFrontOriginAccessIdentityList
cloudFrontOriginAccessIdentityList pMarker pMaxItems pIsTruncated pQuantity =
    CloudFrontOriginAccessIdentityList'
    { _cfoailItems = Nothing
    , _cfoailNextMarker = Nothing
    , _cfoailMarker = pMarker
    , _cfoailMaxItems = pMaxItems
    , _cfoailIsTruncated = pIsTruncated
    , _cfoailQuantity = pQuantity
    }

-- | A complex type that contains one CloudFrontOriginAccessIdentitySummary
-- element for each origin access identity that was created by the current
-- AWS account.
cfoailItems :: Lens' CloudFrontOriginAccessIdentityList [CloudFrontOriginAccessIdentitySummary]
cfoailItems = lens _cfoailItems (\ s a -> s{_cfoailItems = a}) . _Default;

-- | If IsTruncated is true, this element is present and contains the value
-- you can use for the Marker request parameter to continue listing your
-- origin access identities where they left off.
cfoailNextMarker :: Lens' CloudFrontOriginAccessIdentityList (Maybe Text)
cfoailNextMarker = lens _cfoailNextMarker (\ s a -> s{_cfoailNextMarker = a});

-- | The value you provided for the Marker request parameter.
cfoailMarker :: Lens' CloudFrontOriginAccessIdentityList Text
cfoailMarker = lens _cfoailMarker (\ s a -> s{_cfoailMarker = a});

-- | The value you provided for the MaxItems request parameter.
cfoailMaxItems :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailMaxItems = lens _cfoailMaxItems (\ s a -> s{_cfoailMaxItems = a});

-- | A flag that indicates whether more origin access identities remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request using the Marker request parameter to retrieve more
-- items in the list.
cfoailIsTruncated :: Lens' CloudFrontOriginAccessIdentityList Bool
cfoailIsTruncated = lens _cfoailIsTruncated (\ s a -> s{_cfoailIsTruncated = a});

-- | The number of CloudFront origin access identities that were created by
-- the current AWS account.
cfoailQuantity :: Lens' CloudFrontOriginAccessIdentityList Int
cfoailQuantity = lens _cfoailQuantity (\ s a -> s{_cfoailQuantity = a});

instance FromXML CloudFrontOriginAccessIdentityList
         where
        parseXML x
          = CloudFrontOriginAccessIdentityList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may
                   (parseXMLList
                      "CloudFrontOriginAccessIdentitySummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "Marker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "IsTruncated")
                <*> (x .@ "Quantity")

-- | Summary of the information about a CloudFront origin access identity.
--
-- /See:/ 'cloudFrontOriginAccessIdentitySummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cfoaisId'
--
-- * 'cfoaisS3CanonicalUserId'
--
-- * 'cfoaisComment'
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary'
    { _cfoaisId                :: !Text
    , _cfoaisS3CanonicalUserId :: !Text
    , _cfoaisComment           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CloudFrontOriginAccessIdentitySummary' smart constructor.
cloudFrontOriginAccessIdentitySummary :: Text -> Text -> Text -> CloudFrontOriginAccessIdentitySummary
cloudFrontOriginAccessIdentitySummary pId pS3CanonicalUserId pComment =
    CloudFrontOriginAccessIdentitySummary'
    { _cfoaisId = pId
    , _cfoaisS3CanonicalUserId = pS3CanonicalUserId
    , _cfoaisComment = pComment
    }

-- | The ID for the origin access identity. For example: E74FTE3AJFJ256A.
cfoaisId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisId = lens _cfoaisId (\ s a -> s{_cfoaisId = a});

-- | The Amazon S3 canonical user ID for the origin access identity, which
-- you use when giving the origin access identity read permission to an
-- object in Amazon S3.
cfoaisS3CanonicalUserId :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisS3CanonicalUserId = lens _cfoaisS3CanonicalUserId (\ s a -> s{_cfoaisS3CanonicalUserId = a});

-- | The comment for this origin access identity, as originally specified
-- when created.
cfoaisComment :: Lens' CloudFrontOriginAccessIdentitySummary Text
cfoaisComment = lens _cfoaisComment (\ s a -> s{_cfoaisComment = a});

instance FromXML
         CloudFrontOriginAccessIdentitySummary where
        parseXML x
          = CloudFrontOriginAccessIdentitySummary' <$>
              (x .@ "Id") <*> (x .@ "S3CanonicalUserId") <*>
                (x .@ "Comment")

-- | A complex type that specifies the whitelisted cookies, if any, that you
-- want CloudFront to forward to your origin that is associated with this
-- cache behavior.
--
-- /See:/ 'cookieNames' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnItems'
--
-- * 'cnQuantity'
data CookieNames = CookieNames'
    { _cnItems    :: !(Maybe [Text])
    , _cnQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CookieNames' smart constructor.
cookieNames :: Int -> CookieNames
cookieNames pQuantity =
    CookieNames'
    { _cnItems = Nothing
    , _cnQuantity = pQuantity
    }

-- | Optional: A complex type that contains whitelisted cookies for this
-- cache behavior. If Quantity is 0, you can omit Items.
cnItems :: Lens' CookieNames [Text]
cnItems = lens _cnItems (\ s a -> s{_cnItems = a}) . _Default;

-- | The number of whitelisted cookies for this cache behavior.
cnQuantity :: Lens' CookieNames Int
cnQuantity = lens _cnQuantity (\ s a -> s{_cnQuantity = a});

instance FromXML CookieNames where
        parseXML x
          = CookieNames' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Name"))
                <*> (x .@ "Quantity")

instance ToXML CookieNames where
        toXML CookieNames'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Name" <$> _cnItems),
               "Quantity" @= _cnQuantity]

-- | A complex type that specifies the cookie preferences associated with
-- this cache behavior.
--
-- /See:/ 'cookiePreference' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpWhitelistedNames'
--
-- * 'cpForward'
data CookiePreference = CookiePreference'
    { _cpWhitelistedNames :: !(Maybe CookieNames)
    , _cpForward          :: !ItemSelection
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CookiePreference' smart constructor.
cookiePreference :: ItemSelection -> CookiePreference
cookiePreference pForward =
    CookiePreference'
    { _cpWhitelistedNames = Nothing
    , _cpForward = pForward
    }

-- | A complex type that specifies the whitelisted cookies, if any, that you
-- want CloudFront to forward to your origin that is associated with this
-- cache behavior.
cpWhitelistedNames :: Lens' CookiePreference (Maybe CookieNames)
cpWhitelistedNames = lens _cpWhitelistedNames (\ s a -> s{_cpWhitelistedNames = a});

-- | Use this element to specify whether you want CloudFront to forward
-- cookies to the origin that is associated with this cache behavior. You
-- can specify all, none or whitelist. If you choose All, CloudFront
-- forwards all cookies regardless of how many your application uses.
cpForward :: Lens' CookiePreference ItemSelection
cpForward = lens _cpForward (\ s a -> s{_cpForward = a});

instance FromXML CookiePreference where
        parseXML x
          = CookiePreference' <$>
              (x .@? "WhitelistedNames") <*> (x .@ "Forward")

instance ToXML CookiePreference where
        toXML CookiePreference'{..}
          = mconcat
              ["WhitelistedNames" @= _cpWhitelistedNames,
               "Forward" @= _cpForward]

-- | A complex type that describes how you\'d prefer CloudFront to respond to
-- requests that result in either a 4xx or 5xx response. You can control
-- whether a custom error page should be displayed, what the desired
-- response code should be for this error page and how long should the
-- error response be cached by CloudFront. If you don\'t want to specify
-- any custom error responses, include only an empty CustomErrorResponses
-- element. To delete all custom error responses in an existing
-- distribution, update the distribution configuration and include only an
-- empty CustomErrorResponses element. To add, change, or remove one or
-- more custom error responses, update the distribution configuration and
-- specify all of the custom error responses that you want to include in
-- the updated distribution.
--
-- /See:/ 'customErrorResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerResponsePagePath'
--
-- * 'cerResponseCode'
--
-- * 'cerErrorCachingMinTTL'
--
-- * 'cerErrorCode'
data CustomErrorResponse = CustomErrorResponse'
    { _cerResponsePagePath   :: !(Maybe Text)
    , _cerResponseCode       :: !(Maybe Text)
    , _cerErrorCachingMinTTL :: !(Maybe Integer)
    , _cerErrorCode          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CustomErrorResponse' smart constructor.
customErrorResponse :: Int -> CustomErrorResponse
customErrorResponse pErrorCode =
    CustomErrorResponse'
    { _cerResponsePagePath = Nothing
    , _cerResponseCode = Nothing
    , _cerErrorCachingMinTTL = Nothing
    , _cerErrorCode = pErrorCode
    }

-- | The path of the custom error page (for example, \/custom_404.html). The
-- path is relative to the distribution and must begin with a slash (\/).
-- If the path includes any non-ASCII characters or unsafe characters as
-- defined in RFC 1783 (http:\/\/www.ietf.org\/rfc\/rfc1738.txt), URL
-- encode those characters. Do not URL encode any other characters in the
-- path, or CloudFront will not return the custom error page to the viewer.
cerResponsePagePath :: Lens' CustomErrorResponse (Maybe Text)
cerResponsePagePath = lens _cerResponsePagePath (\ s a -> s{_cerResponsePagePath = a});

-- | The HTTP status code that you want CloudFront to return with the custom
-- error page to the viewer. For a list of HTTP status codes that you can
-- replace, see CloudFront Documentation.
cerResponseCode :: Lens' CustomErrorResponse (Maybe Text)
cerResponseCode = lens _cerResponseCode (\ s a -> s{_cerResponseCode = a});

-- | The minimum amount of time you want HTTP error codes to stay in
-- CloudFront caches before CloudFront queries your origin to see whether
-- the object has been updated. You can specify a value from 0 to
-- 31,536,000.
cerErrorCachingMinTTL :: Lens' CustomErrorResponse (Maybe Integer)
cerErrorCachingMinTTL = lens _cerErrorCachingMinTTL (\ s a -> s{_cerErrorCachingMinTTL = a});

-- | The 4xx or 5xx HTTP status code that you want to customize. For a list
-- of HTTP status codes that you can customize, see CloudFront
-- documentation.
cerErrorCode :: Lens' CustomErrorResponse Int
cerErrorCode = lens _cerErrorCode (\ s a -> s{_cerErrorCode = a});

instance FromXML CustomErrorResponse where
        parseXML x
          = CustomErrorResponse' <$>
              (x .@? "ResponsePagePath") <*> (x .@? "ResponseCode")
                <*> (x .@? "ErrorCachingMinTTL")
                <*> (x .@ "ErrorCode")

instance ToXML CustomErrorResponse where
        toXML CustomErrorResponse'{..}
          = mconcat
              ["ResponsePagePath" @= _cerResponsePagePath,
               "ResponseCode" @= _cerResponseCode,
               "ErrorCachingMinTTL" @= _cerErrorCachingMinTTL,
               "ErrorCode" @= _cerErrorCode]

-- | A complex type that contains zero or more CustomErrorResponse elements.
--
-- /See:/ 'customErrorResponses' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerItems'
--
-- * 'cerQuantity'
data CustomErrorResponses = CustomErrorResponses'
    { _cerItems    :: !(Maybe [CustomErrorResponse])
    , _cerQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CustomErrorResponses' smart constructor.
customErrorResponses :: Int -> CustomErrorResponses
customErrorResponses pQuantity =
    CustomErrorResponses'
    { _cerItems = Nothing
    , _cerQuantity = pQuantity
    }

-- | Optional: A complex type that contains custom error responses for this
-- distribution. If Quantity is 0, you can omit Items.
cerItems :: Lens' CustomErrorResponses [CustomErrorResponse]
cerItems = lens _cerItems (\ s a -> s{_cerItems = a}) . _Default;

-- | The number of custom error responses for this distribution.
cerQuantity :: Lens' CustomErrorResponses Int
cerQuantity = lens _cerQuantity (\ s a -> s{_cerQuantity = a});

instance FromXML CustomErrorResponses where
        parseXML x
          = CustomErrorResponses' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "CustomErrorResponse"))
                <*> (x .@ "Quantity")

instance ToXML CustomErrorResponses where
        toXML CustomErrorResponses'{..}
          = mconcat
              ["Items" @=
                 toXML
                   (toXMLList "CustomErrorResponse" <$> _cerItems),
               "Quantity" @= _cerQuantity]

-- | A customer origin.
--
-- /See:/ 'customOriginConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cocHTTPPort'
--
-- * 'cocHTTPSPort'
--
-- * 'cocOriginProtocolPolicy'
data CustomOriginConfig = CustomOriginConfig'
    { _cocHTTPPort             :: !Int
    , _cocHTTPSPort            :: !Int
    , _cocOriginProtocolPolicy :: !OriginProtocolPolicy
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CustomOriginConfig' smart constructor.
customOriginConfig :: Int -> Int -> OriginProtocolPolicy -> CustomOriginConfig
customOriginConfig pHTTPPort pHTTPSPort pOriginProtocolPolicy =
    CustomOriginConfig'
    { _cocHTTPPort = pHTTPPort
    , _cocHTTPSPort = pHTTPSPort
    , _cocOriginProtocolPolicy = pOriginProtocolPolicy
    }

-- | The HTTP port the custom origin listens on.
cocHTTPPort :: Lens' CustomOriginConfig Int
cocHTTPPort = lens _cocHTTPPort (\ s a -> s{_cocHTTPPort = a});

-- | The HTTPS port the custom origin listens on.
cocHTTPSPort :: Lens' CustomOriginConfig Int
cocHTTPSPort = lens _cocHTTPSPort (\ s a -> s{_cocHTTPSPort = a});

-- | The origin protocol policy to apply to your origin.
cocOriginProtocolPolicy :: Lens' CustomOriginConfig OriginProtocolPolicy
cocOriginProtocolPolicy = lens _cocOriginProtocolPolicy (\ s a -> s{_cocOriginProtocolPolicy = a});

instance FromXML CustomOriginConfig where
        parseXML x
          = CustomOriginConfig' <$>
              (x .@ "HTTPPort") <*> (x .@ "HTTPSPort") <*>
                (x .@ "OriginProtocolPolicy")

instance ToXML CustomOriginConfig where
        toXML CustomOriginConfig'{..}
          = mconcat
              ["HTTPPort" @= _cocHTTPPort,
               "HTTPSPort" @= _cocHTTPSPort,
               "OriginProtocolPolicy" @= _cocOriginProtocolPolicy]

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don\'t match any of the
-- values of PathPattern in CacheBehavior elements.You must create exactly
-- one default cache behavior.
--
-- /See:/ 'defaultCacheBehavior' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcbAllowedMethods'
--
-- * 'dcbMaxTTL'
--
-- * 'dcbSmoothStreaming'
--
-- * 'dcbDefaultTTL'
--
-- * 'dcbTargetOriginId'
--
-- * 'dcbForwardedValues'
--
-- * 'dcbTrustedSigners'
--
-- * 'dcbViewerProtocolPolicy'
--
-- * 'dcbMinTTL'
data DefaultCacheBehavior = DefaultCacheBehavior'
    { _dcbAllowedMethods       :: !(Maybe AllowedMethods)
    , _dcbMaxTTL               :: !(Maybe Integer)
    , _dcbSmoothStreaming      :: !(Maybe Bool)
    , _dcbDefaultTTL           :: !(Maybe Integer)
    , _dcbTargetOriginId       :: !Text
    , _dcbForwardedValues      :: !ForwardedValues
    , _dcbTrustedSigners       :: !TrustedSigners
    , _dcbViewerProtocolPolicy :: !ViewerProtocolPolicy
    , _dcbMinTTL               :: !Integer
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DefaultCacheBehavior' smart constructor.
defaultCacheBehavior :: Text -> ForwardedValues -> TrustedSigners -> ViewerProtocolPolicy -> Integer -> DefaultCacheBehavior
defaultCacheBehavior pTargetOriginId pForwardedValues pTrustedSigners pViewerProtocolPolicy pMinTTL =
    DefaultCacheBehavior'
    { _dcbAllowedMethods = Nothing
    , _dcbMaxTTL = Nothing
    , _dcbSmoothStreaming = Nothing
    , _dcbDefaultTTL = Nothing
    , _dcbTargetOriginId = pTargetOriginId
    , _dcbForwardedValues = pForwardedValues
    , _dcbTrustedSigners = pTrustedSigners
    , _dcbViewerProtocolPolicy = pViewerProtocolPolicy
    , _dcbMinTTL = pMinTTL
    }

-- | FIXME: Undocumented member.
dcbAllowedMethods :: Lens' DefaultCacheBehavior (Maybe AllowedMethods)
dcbAllowedMethods = lens _dcbAllowedMethods (\ s a -> s{_dcbAllowedMethods = a});

-- | The maximum amount of time (in seconds) that an object is in a
-- CloudFront cache before CloudFront forwards another request to your
-- origin to determine whether the object has been updated. The value that
-- you specify applies only when your origin adds HTTP headers such as
-- Cache-Control max-age, Cache-Control s-maxage, and Expires to objects.
-- You can specify a value from 0 to 3,153,600,000 seconds (100 years).
dcbMaxTTL :: Lens' DefaultCacheBehavior (Maybe Integer)
dcbMaxTTL = lens _dcbMaxTTL (\ s a -> s{_dcbMaxTTL = a});

-- | Indicates whether you want to distribute media files in Microsoft Smooth
-- Streaming format using the origin that is associated with this cache
-- behavior. If so, specify true; if not, specify false.
dcbSmoothStreaming :: Lens' DefaultCacheBehavior (Maybe Bool)
dcbSmoothStreaming = lens _dcbSmoothStreaming (\ s a -> s{_dcbSmoothStreaming = a});

-- | If you don\'t configure your origin to add a Cache-Control max-age
-- directive or an Expires header, DefaultTTL is the default amount of time
-- (in seconds) that an object is in a CloudFront cache before CloudFront
-- forwards another request to your origin to determine whether the object
-- has been updated. The value that you specify applies only when your
-- origin does not add HTTP headers such as Cache-Control max-age,
-- Cache-Control s-maxage, and Expires to objects. You can specify a value
-- from 0 to 3,153,600,000 seconds (100 years).
dcbDefaultTTL :: Lens' DefaultCacheBehavior (Maybe Integer)
dcbDefaultTTL = lens _dcbDefaultTTL (\ s a -> s{_dcbDefaultTTL = a});

-- | The value of ID for the origin that you want CloudFront to route
-- requests to when a request matches the path pattern either for a cache
-- behavior or for the default cache behavior.
dcbTargetOriginId :: Lens' DefaultCacheBehavior Text
dcbTargetOriginId = lens _dcbTargetOriginId (\ s a -> s{_dcbTargetOriginId = a});

-- | A complex type that specifies how CloudFront handles query strings,
-- cookies and headers.
dcbForwardedValues :: Lens' DefaultCacheBehavior ForwardedValues
dcbForwardedValues = lens _dcbForwardedValues (\ s a -> s{_dcbForwardedValues = a});

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
dcbTrustedSigners :: Lens' DefaultCacheBehavior TrustedSigners
dcbTrustedSigners = lens _dcbTrustedSigners (\ s a -> s{_dcbTrustedSigners = a});

-- | Use this element to specify the protocol that users can use to access
-- the files in the origin specified by TargetOriginId when a request
-- matches the path pattern in PathPattern. If you want CloudFront to allow
-- end users to use any available protocol, specify allow-all. If you want
-- CloudFront to require HTTPS, specify https. If you want CloudFront to
-- respond to an HTTP request with an HTTP status code of 301 (Moved
-- Permanently) and the HTTPS URL, specify redirect-to-https. The viewer
-- then resubmits the request using the HTTPS URL.
dcbViewerProtocolPolicy :: Lens' DefaultCacheBehavior ViewerProtocolPolicy
dcbViewerProtocolPolicy = lens _dcbViewerProtocolPolicy (\ s a -> s{_dcbViewerProtocolPolicy = a});

-- | The minimum amount of time that you want objects to stay in CloudFront
-- caches before CloudFront queries your origin to see whether the object
-- has been updated.You can specify a value from 0 to 3,153,600,000 seconds
-- (100 years).
dcbMinTTL :: Lens' DefaultCacheBehavior Integer
dcbMinTTL = lens _dcbMinTTL (\ s a -> s{_dcbMinTTL = a});

instance FromXML DefaultCacheBehavior where
        parseXML x
          = DefaultCacheBehavior' <$>
              (x .@? "AllowedMethods") <*> (x .@? "MaxTTL") <*>
                (x .@? "SmoothStreaming")
                <*> (x .@? "DefaultTTL")
                <*> (x .@ "TargetOriginId")
                <*> (x .@ "ForwardedValues")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "ViewerProtocolPolicy")
                <*> (x .@ "MinTTL")

instance ToXML DefaultCacheBehavior where
        toXML DefaultCacheBehavior'{..}
          = mconcat
              ["AllowedMethods" @= _dcbAllowedMethods,
               "MaxTTL" @= _dcbMaxTTL,
               "SmoothStreaming" @= _dcbSmoothStreaming,
               "DefaultTTL" @= _dcbDefaultTTL,
               "TargetOriginId" @= _dcbTargetOriginId,
               "ForwardedValues" @= _dcbForwardedValues,
               "TrustedSigners" @= _dcbTrustedSigners,
               "ViewerProtocolPolicy" @= _dcbViewerProtocolPolicy,
               "MinTTL" @= _dcbMinTTL]

-- | A distribution.
--
-- /See:/ 'distribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disId'
--
-- * 'disStatus'
--
-- * 'disLastModifiedTime'
--
-- * 'disInProgressInvalidationBatches'
--
-- * 'disDomainName'
--
-- * 'disActiveTrustedSigners'
--
-- * 'disDistributionConfig'
data Distribution = Distribution'
    { _disId                            :: !Text
    , _disStatus                        :: !Text
    , _disLastModifiedTime              :: !ISO8601
    , _disInProgressInvalidationBatches :: !Int
    , _disDomainName                    :: !Text
    , _disActiveTrustedSigners          :: !ActiveTrustedSigners
    , _disDistributionConfig            :: !DistributionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Distribution' smart constructor.
distribution :: Text -> Text -> UTCTime -> Int -> Text -> ActiveTrustedSigners -> DistributionConfig -> Distribution
distribution pId pStatus pLastModifiedTime pInProgressInvalidationBatches pDomainName pActiveTrustedSigners pDistributionConfig =
    Distribution'
    { _disId = pId
    , _disStatus = pStatus
    , _disLastModifiedTime = _Time # pLastModifiedTime
    , _disInProgressInvalidationBatches = pInProgressInvalidationBatches
    , _disDomainName = pDomainName
    , _disActiveTrustedSigners = pActiveTrustedSigners
    , _disDistributionConfig = pDistributionConfig
    }

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
disId :: Lens' Distribution Text
disId = lens _disId (\ s a -> s{_disId = a});

-- | This response element indicates the current status of the distribution.
-- When the status is Deployed, the distribution\'s information is fully
-- propagated throughout the Amazon CloudFront system.
disStatus :: Lens' Distribution Text
disStatus = lens _disStatus (\ s a -> s{_disStatus = a});

-- | The date and time the distribution was last modified.
disLastModifiedTime :: Lens' Distribution UTCTime
disLastModifiedTime = lens _disLastModifiedTime (\ s a -> s{_disLastModifiedTime = a}) . _Time;

-- | The number of invalidation batches currently in progress.
disInProgressInvalidationBatches :: Lens' Distribution Int
disInProgressInvalidationBatches = lens _disInProgressInvalidationBatches (\ s a -> s{_disInProgressInvalidationBatches = a});

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
disDomainName :: Lens' Distribution Text
disDomainName = lens _disDomainName (\ s a -> s{_disDomainName = a});

-- | CloudFront automatically adds this element to the response only if
-- you\'ve set up the distribution to serve private content with signed
-- URLs. The element lists the key pair IDs that CloudFront is aware of for
-- each trusted signer. The Signer child element lists the AWS account
-- number of the trusted signer (or an empty Self element if the signer is
-- you). The Signer element also includes the IDs of any active key pairs
-- associated with the trusted signer\'s AWS account. If no KeyPairId
-- element appears for a Signer, that signer can\'t create working signed
-- URLs.
disActiveTrustedSigners :: Lens' Distribution ActiveTrustedSigners
disActiveTrustedSigners = lens _disActiveTrustedSigners (\ s a -> s{_disActiveTrustedSigners = a});

-- | The current configuration information for the distribution.
disDistributionConfig :: Lens' Distribution DistributionConfig
disDistributionConfig = lens _disDistributionConfig (\ s a -> s{_disDistributionConfig = a});

instance FromXML Distribution where
        parseXML x
          = Distribution' <$>
              (x .@ "Id") <*> (x .@ "Status") <*>
                (x .@ "LastModifiedTime")
                <*> (x .@ "InProgressInvalidationBatches")
                <*> (x .@ "DomainName")
                <*> (x .@ "ActiveTrustedSigners")
                <*> (x .@ "DistributionConfig")

-- | A distribution Configuration.
--
-- /See:/ 'distributionConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcDefaultRootObject'
--
-- * 'dcAliases'
--
-- * 'dcCustomErrorResponses'
--
-- * 'dcPriceClass'
--
-- * 'dcViewerCertificate'
--
-- * 'dcRestrictions'
--
-- * 'dcCacheBehaviors'
--
-- * 'dcLogging'
--
-- * 'dcCallerReference'
--
-- * 'dcOrigins'
--
-- * 'dcDefaultCacheBehavior'
--
-- * 'dcComment'
--
-- * 'dcEnabled'
data DistributionConfig = DistributionConfig'
    { _dcDefaultRootObject    :: !(Maybe Text)
    , _dcAliases              :: !(Maybe Aliases)
    , _dcCustomErrorResponses :: !(Maybe CustomErrorResponses)
    , _dcPriceClass           :: !(Maybe PriceClass)
    , _dcViewerCertificate    :: !(Maybe ViewerCertificate)
    , _dcRestrictions         :: !(Maybe Restrictions)
    , _dcCacheBehaviors       :: !(Maybe CacheBehaviors)
    , _dcLogging              :: !(Maybe LoggingConfig)
    , _dcCallerReference      :: !Text
    , _dcOrigins              :: !Origins
    , _dcDefaultCacheBehavior :: !DefaultCacheBehavior
    , _dcComment              :: !Text
    , _dcEnabled              :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DistributionConfig' smart constructor.
distributionConfig :: Text -> Origins -> DefaultCacheBehavior -> Text -> Bool -> DistributionConfig
distributionConfig pCallerReference pOrigins pDefaultCacheBehavior pComment pEnabled =
    DistributionConfig'
    { _dcDefaultRootObject = Nothing
    , _dcAliases = Nothing
    , _dcCustomErrorResponses = Nothing
    , _dcPriceClass = Nothing
    , _dcViewerCertificate = Nothing
    , _dcRestrictions = Nothing
    , _dcCacheBehaviors = Nothing
    , _dcLogging = Nothing
    , _dcCallerReference = pCallerReference
    , _dcOrigins = pOrigins
    , _dcDefaultCacheBehavior = pDefaultCacheBehavior
    , _dcComment = pComment
    , _dcEnabled = pEnabled
    }

-- | The object that you want CloudFront to return (for example, index.html)
-- when an end user requests the root URL for your distribution
-- (http:\/\/www.example.com) instead of an object in your distribution
-- (http:\/\/www.example.com\/index.html). Specifying a default root object
-- avoids exposing the contents of your distribution. If you don\'t want to
-- specify a default root object when you create a distribution, include an
-- empty DefaultRootObject element. To delete the default root object from
-- an existing distribution, update the distribution configuration and
-- include an empty DefaultRootObject element. To replace the default root
-- object, update the distribution configuration and specify the new
-- object.
dcDefaultRootObject :: Lens' DistributionConfig (Maybe Text)
dcDefaultRootObject = lens _dcDefaultRootObject (\ s a -> s{_dcDefaultRootObject = a});

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dcAliases :: Lens' DistributionConfig (Maybe Aliases)
dcAliases = lens _dcAliases (\ s a -> s{_dcAliases = a});

-- | A complex type that contains zero or more CustomErrorResponse elements.
dcCustomErrorResponses :: Lens' DistributionConfig (Maybe CustomErrorResponses)
dcCustomErrorResponses = lens _dcCustomErrorResponses (\ s a -> s{_dcCustomErrorResponses = a});

-- | A complex type that contains information about price class for this
-- distribution.
dcPriceClass :: Lens' DistributionConfig (Maybe PriceClass)
dcPriceClass = lens _dcPriceClass (\ s a -> s{_dcPriceClass = a});

-- | FIXME: Undocumented member.
dcViewerCertificate :: Lens' DistributionConfig (Maybe ViewerCertificate)
dcViewerCertificate = lens _dcViewerCertificate (\ s a -> s{_dcViewerCertificate = a});

-- | FIXME: Undocumented member.
dcRestrictions :: Lens' DistributionConfig (Maybe Restrictions)
dcRestrictions = lens _dcRestrictions (\ s a -> s{_dcRestrictions = a});

-- | A complex type that contains zero or more CacheBehavior elements.
dcCacheBehaviors :: Lens' DistributionConfig (Maybe CacheBehaviors)
dcCacheBehaviors = lens _dcCacheBehaviors (\ s a -> s{_dcCacheBehaviors = a});

-- | A complex type that controls whether access logs are written for the
-- distribution.
dcLogging :: Lens' DistributionConfig (Maybe LoggingConfig)
dcLogging = lens _dcLogging (\ s a -> s{_dcLogging = a});

-- | A unique number that ensures the request can\'t be replayed. If the
-- CallerReference is new (no matter the content of the DistributionConfig
-- object), a new distribution is created. If the CallerReference is a
-- value you already sent in a previous request to create a distribution,
-- and the content of the DistributionConfig is identical to the original
-- request (ignoring white space), the response includes the same
-- information returned to the original request. If the CallerReference is
-- a value you already sent in a previous request to create a distribution
-- but the content of the DistributionConfig is different from the original
-- request, CloudFront returns a DistributionAlreadyExists error.
dcCallerReference :: Lens' DistributionConfig Text
dcCallerReference = lens _dcCallerReference (\ s a -> s{_dcCallerReference = a});

-- | A complex type that contains information about origins for this
-- distribution.
dcOrigins :: Lens' DistributionConfig Origins
dcOrigins = lens _dcOrigins (\ s a -> s{_dcOrigins = a});

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don\'t match any of the
-- values of PathPattern in CacheBehavior elements.You must create exactly
-- one default cache behavior.
dcDefaultCacheBehavior :: Lens' DistributionConfig DefaultCacheBehavior
dcDefaultCacheBehavior = lens _dcDefaultCacheBehavior (\ s a -> s{_dcDefaultCacheBehavior = a});

-- | Any comments you want to include about the distribution.
dcComment :: Lens' DistributionConfig Text
dcComment = lens _dcComment (\ s a -> s{_dcComment = a});

-- | Whether the distribution is enabled to accept end user requests for
-- content.
dcEnabled :: Lens' DistributionConfig Bool
dcEnabled = lens _dcEnabled (\ s a -> s{_dcEnabled = a});

instance FromXML DistributionConfig where
        parseXML x
          = DistributionConfig' <$>
              (x .@? "DefaultRootObject") <*> (x .@? "Aliases") <*>
                (x .@? "CustomErrorResponses")
                <*> (x .@? "PriceClass")
                <*> (x .@? "ViewerCertificate")
                <*> (x .@? "Restrictions")
                <*> (x .@? "CacheBehaviors")
                <*> (x .@? "Logging")
                <*> (x .@ "CallerReference")
                <*> (x .@ "Origins")
                <*> (x .@ "DefaultCacheBehavior")
                <*> (x .@ "Comment")
                <*> (x .@ "Enabled")

instance ToXML DistributionConfig where
        toXML DistributionConfig'{..}
          = mconcat
              ["DefaultRootObject" @= _dcDefaultRootObject,
               "Aliases" @= _dcAliases,
               "CustomErrorResponses" @= _dcCustomErrorResponses,
               "PriceClass" @= _dcPriceClass,
               "ViewerCertificate" @= _dcViewerCertificate,
               "Restrictions" @= _dcRestrictions,
               "CacheBehaviors" @= _dcCacheBehaviors,
               "Logging" @= _dcLogging,
               "CallerReference" @= _dcCallerReference,
               "Origins" @= _dcOrigins,
               "DefaultCacheBehavior" @= _dcDefaultCacheBehavior,
               "Comment" @= _dcComment, "Enabled" @= _dcEnabled]

-- | A distribution list.
--
-- /See:/ 'distributionList' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlItems'
--
-- * 'dlNextMarker'
--
-- * 'dlMarker'
--
-- * 'dlMaxItems'
--
-- * 'dlIsTruncated'
--
-- * 'dlQuantity'
data DistributionList = DistributionList'
    { _dlItems       :: !(Maybe [DistributionSummary])
    , _dlNextMarker  :: !(Maybe Text)
    , _dlMarker      :: !Text
    , _dlMaxItems    :: !Int
    , _dlIsTruncated :: !Bool
    , _dlQuantity    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DistributionList' smart constructor.
distributionList :: Text -> Int -> Bool -> Int -> DistributionList
distributionList pMarker pMaxItems pIsTruncated pQuantity =
    DistributionList'
    { _dlItems = Nothing
    , _dlNextMarker = Nothing
    , _dlMarker = pMarker
    , _dlMaxItems = pMaxItems
    , _dlIsTruncated = pIsTruncated
    , _dlQuantity = pQuantity
    }

-- | A complex type that contains one DistributionSummary element for each
-- distribution that was created by the current AWS account.
dlItems :: Lens' DistributionList [DistributionSummary]
dlItems = lens _dlItems (\ s a -> s{_dlItems = a}) . _Default;

-- | If IsTruncated is true, this element is present and contains the value
-- you can use for the Marker request parameter to continue listing your
-- distributions where they left off.
dlNextMarker :: Lens' DistributionList (Maybe Text)
dlNextMarker = lens _dlNextMarker (\ s a -> s{_dlNextMarker = a});

-- | The value you provided for the Marker request parameter.
dlMarker :: Lens' DistributionList Text
dlMarker = lens _dlMarker (\ s a -> s{_dlMarker = a});

-- | The value you provided for the MaxItems request parameter.
dlMaxItems :: Lens' DistributionList Int
dlMaxItems = lens _dlMaxItems (\ s a -> s{_dlMaxItems = a});

-- | A flag that indicates whether more distributions remain to be listed. If
-- your results were truncated, you can make a follow-up pagination request
-- using the Marker request parameter to retrieve more distributions in the
-- list.
dlIsTruncated :: Lens' DistributionList Bool
dlIsTruncated = lens _dlIsTruncated (\ s a -> s{_dlIsTruncated = a});

-- | The number of distributions that were created by the current AWS
-- account.
dlQuantity :: Lens' DistributionList Int
dlQuantity = lens _dlQuantity (\ s a -> s{_dlQuantity = a});

instance FromXML DistributionList where
        parseXML x
          = DistributionList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "DistributionSummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "Marker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "IsTruncated")
                <*> (x .@ "Quantity")

-- | A summary of the information for an Amazon CloudFront distribution.
--
-- /See:/ 'distributionSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsId'
--
-- * 'dsStatus'
--
-- * 'dsLastModifiedTime'
--
-- * 'dsDomainName'
--
-- * 'dsAliases'
--
-- * 'dsOrigins'
--
-- * 'dsDefaultCacheBehavior'
--
-- * 'dsCacheBehaviors'
--
-- * 'dsCustomErrorResponses'
--
-- * 'dsComment'
--
-- * 'dsPriceClass'
--
-- * 'dsEnabled'
--
-- * 'dsViewerCertificate'
--
-- * 'dsRestrictions'
data DistributionSummary = DistributionSummary'
    { _dsId                   :: !Text
    , _dsStatus               :: !Text
    , _dsLastModifiedTime     :: !ISO8601
    , _dsDomainName           :: !Text
    , _dsAliases              :: !Aliases
    , _dsOrigins              :: !Origins
    , _dsDefaultCacheBehavior :: !DefaultCacheBehavior
    , _dsCacheBehaviors       :: !CacheBehaviors
    , _dsCustomErrorResponses :: !CustomErrorResponses
    , _dsComment              :: !Text
    , _dsPriceClass           :: !PriceClass
    , _dsEnabled              :: !Bool
    , _dsViewerCertificate    :: !ViewerCertificate
    , _dsRestrictions         :: !Restrictions
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DistributionSummary' smart constructor.
distributionSummary :: Text -> Text -> UTCTime -> Text -> Aliases -> Origins -> DefaultCacheBehavior -> CacheBehaviors -> CustomErrorResponses -> Text -> PriceClass -> Bool -> ViewerCertificate -> Restrictions -> DistributionSummary
distributionSummary pId pStatus pLastModifiedTime pDomainName pAliases pOrigins pDefaultCacheBehavior pCacheBehaviors pCustomErrorResponses pComment pPriceClass pEnabled pViewerCertificate pRestrictions =
    DistributionSummary'
    { _dsId = pId
    , _dsStatus = pStatus
    , _dsLastModifiedTime = _Time # pLastModifiedTime
    , _dsDomainName = pDomainName
    , _dsAliases = pAliases
    , _dsOrigins = pOrigins
    , _dsDefaultCacheBehavior = pDefaultCacheBehavior
    , _dsCacheBehaviors = pCacheBehaviors
    , _dsCustomErrorResponses = pCustomErrorResponses
    , _dsComment = pComment
    , _dsPriceClass = pPriceClass
    , _dsEnabled = pEnabled
    , _dsViewerCertificate = pViewerCertificate
    , _dsRestrictions = pRestrictions
    }

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
dsId :: Lens' DistributionSummary Text
dsId = lens _dsId (\ s a -> s{_dsId = a});

-- | This response element indicates the current status of the distribution.
-- When the status is Deployed, the distribution\'s information is fully
-- propagated throughout the Amazon CloudFront system.
dsStatus :: Lens' DistributionSummary Text
dsStatus = lens _dsStatus (\ s a -> s{_dsStatus = a});

-- | The date and time the distribution was last modified.
dsLastModifiedTime :: Lens' DistributionSummary UTCTime
dsLastModifiedTime = lens _dsLastModifiedTime (\ s a -> s{_dsLastModifiedTime = a}) . _Time;

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
dsDomainName :: Lens' DistributionSummary Text
dsDomainName = lens _dsDomainName (\ s a -> s{_dsDomainName = a});

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this distribution.
dsAliases :: Lens' DistributionSummary Aliases
dsAliases = lens _dsAliases (\ s a -> s{_dsAliases = a});

-- | A complex type that contains information about origins for this
-- distribution.
dsOrigins :: Lens' DistributionSummary Origins
dsOrigins = lens _dsOrigins (\ s a -> s{_dsOrigins = a});

-- | A complex type that describes the default cache behavior if you do not
-- specify a CacheBehavior element or if files don\'t match any of the
-- values of PathPattern in CacheBehavior elements.You must create exactly
-- one default cache behavior.
dsDefaultCacheBehavior :: Lens' DistributionSummary DefaultCacheBehavior
dsDefaultCacheBehavior = lens _dsDefaultCacheBehavior (\ s a -> s{_dsDefaultCacheBehavior = a});

-- | A complex type that contains zero or more CacheBehavior elements.
dsCacheBehaviors :: Lens' DistributionSummary CacheBehaviors
dsCacheBehaviors = lens _dsCacheBehaviors (\ s a -> s{_dsCacheBehaviors = a});

-- | A complex type that contains zero or more CustomErrorResponses elements.
dsCustomErrorResponses :: Lens' DistributionSummary CustomErrorResponses
dsCustomErrorResponses = lens _dsCustomErrorResponses (\ s a -> s{_dsCustomErrorResponses = a});

-- | The comment originally specified when this distribution was created.
dsComment :: Lens' DistributionSummary Text
dsComment = lens _dsComment (\ s a -> s{_dsComment = a});

-- | FIXME: Undocumented member.
dsPriceClass :: Lens' DistributionSummary PriceClass
dsPriceClass = lens _dsPriceClass (\ s a -> s{_dsPriceClass = a});

-- | Whether the distribution is enabled to accept end user requests for
-- content.
dsEnabled :: Lens' DistributionSummary Bool
dsEnabled = lens _dsEnabled (\ s a -> s{_dsEnabled = a});

-- | FIXME: Undocumented member.
dsViewerCertificate :: Lens' DistributionSummary ViewerCertificate
dsViewerCertificate = lens _dsViewerCertificate (\ s a -> s{_dsViewerCertificate = a});

-- | FIXME: Undocumented member.
dsRestrictions :: Lens' DistributionSummary Restrictions
dsRestrictions = lens _dsRestrictions (\ s a -> s{_dsRestrictions = a});

instance FromXML DistributionSummary where
        parseXML x
          = DistributionSummary' <$>
              (x .@ "Id") <*> (x .@ "Status") <*>
                (x .@ "LastModifiedTime")
                <*> (x .@ "DomainName")
                <*> (x .@ "Aliases")
                <*> (x .@ "Origins")
                <*> (x .@ "DefaultCacheBehavior")
                <*> (x .@ "CacheBehaviors")
                <*> (x .@ "CustomErrorResponses")
                <*> (x .@ "Comment")
                <*> (x .@ "PriceClass")
                <*> (x .@ "Enabled")
                <*> (x .@ "ViewerCertificate")
                <*> (x .@ "Restrictions")

-- | A complex type that specifies how CloudFront handles query strings,
-- cookies and headers.
--
-- /See:/ 'forwardedValues' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fvHeaders'
--
-- * 'fvQueryString'
--
-- * 'fvCookies'
data ForwardedValues = ForwardedValues'
    { _fvHeaders     :: !(Maybe Headers)
    , _fvQueryString :: !Bool
    , _fvCookies     :: !CookiePreference
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ForwardedValues' smart constructor.
forwardedValues :: Bool -> CookiePreference -> ForwardedValues
forwardedValues pQueryString pCookies =
    ForwardedValues'
    { _fvHeaders = Nothing
    , _fvQueryString = pQueryString
    , _fvCookies = pCookies
    }

-- | A complex type that specifies the Headers, if any, that you want
-- CloudFront to vary upon for this cache behavior.
fvHeaders :: Lens' ForwardedValues (Maybe Headers)
fvHeaders = lens _fvHeaders (\ s a -> s{_fvHeaders = a});

-- | Indicates whether you want CloudFront to forward query strings to the
-- origin that is associated with this cache behavior. If so, specify true;
-- if not, specify false.
fvQueryString :: Lens' ForwardedValues Bool
fvQueryString = lens _fvQueryString (\ s a -> s{_fvQueryString = a});

-- | A complex type that specifies how CloudFront handles cookies.
fvCookies :: Lens' ForwardedValues CookiePreference
fvCookies = lens _fvCookies (\ s a -> s{_fvCookies = a});

instance FromXML ForwardedValues where
        parseXML x
          = ForwardedValues' <$>
              (x .@? "Headers") <*> (x .@ "QueryString") <*>
                (x .@ "Cookies")

instance ToXML ForwardedValues where
        toXML ForwardedValues'{..}
          = mconcat
              ["Headers" @= _fvHeaders,
               "QueryString" @= _fvQueryString,
               "Cookies" @= _fvCookies]

-- | A complex type that controls the countries in which your content is
-- distributed. For more information about geo restriction, go to
-- Customizing Error Responses in the Amazon CloudFront Developer Guide.
-- CloudFront determines the location of your users using MaxMind GeoIP
-- databases. For information about the accuracy of these databases, see
-- How accurate are your GeoIP databases? on the MaxMind website.
--
-- /See:/ 'geoRestriction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grItems'
--
-- * 'grRestrictionType'
--
-- * 'grQuantity'
data GeoRestriction = GeoRestriction'
    { _grItems           :: !(Maybe [Text])
    , _grRestrictionType :: !GeoRestrictionType
    , _grQuantity        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GeoRestriction' smart constructor.
geoRestriction :: GeoRestrictionType -> Int -> GeoRestriction
geoRestriction pRestrictionType pQuantity =
    GeoRestriction'
    { _grItems = Nothing
    , _grRestrictionType = pRestrictionType
    , _grQuantity = pQuantity
    }

-- | A complex type that contains a Location element for each country in
-- which you want CloudFront either to distribute your content (whitelist)
-- or not distribute your content (blacklist). The Location element is a
-- two-letter, uppercase country code for a country that you want to
-- include in your blacklist or whitelist. Include one Location element for
-- each country. CloudFront and MaxMind both use ISO 3166 country codes.
-- For the current list of countries and the corresponding codes, see ISO
-- 3166-1-alpha-2 code on the International Organization for
-- Standardization website. You can also refer to the country list in the
-- CloudFront console, which includes both country names and codes.
grItems :: Lens' GeoRestriction [Text]
grItems = lens _grItems (\ s a -> s{_grItems = a}) . _Default;

-- | The method that you want to use to restrict distribution of your content
-- by country: - none: No geo restriction is enabled, meaning access to
-- content is not restricted by client geo location. - blacklist: The
-- Location elements specify the countries in which you do not want
-- CloudFront to distribute your content. - whitelist: The Location
-- elements specify the countries in which you want CloudFront to
-- distribute your content.
grRestrictionType :: Lens' GeoRestriction GeoRestrictionType
grRestrictionType = lens _grRestrictionType (\ s a -> s{_grRestrictionType = a});

-- | When geo restriction is enabled, this is the number of countries in your
-- whitelist or blacklist. Otherwise, when it is not enabled, Quantity is
-- 0, and you can omit Items.
grQuantity :: Lens' GeoRestriction Int
grQuantity = lens _grQuantity (\ s a -> s{_grQuantity = a});

instance FromXML GeoRestriction where
        parseXML x
          = GeoRestriction' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Location"))
                <*> (x .@ "RestrictionType")
                <*> (x .@ "Quantity")

instance ToXML GeoRestriction where
        toXML GeoRestriction'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "Location" <$> _grItems),
               "RestrictionType" @= _grRestrictionType,
               "Quantity" @= _grQuantity]

-- | A complex type that specifies the headers that you want CloudFront to
-- forward to the origin for this cache behavior. For the headers that you
-- specify, CloudFront also caches separate versions of a given object
-- based on the header values in viewer requests; this is known as varying
-- on headers. For example, suppose viewer requests for logo.jpg contain a
-- custom Product header that has a value of either Acme or Apex, and you
-- configure CloudFront to vary on the Product header. CloudFront forwards
-- the Product header to the origin and caches the response from the origin
-- once for each header value.
--
-- /See:/ 'headers' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'heaItems'
--
-- * 'heaQuantity'
data Headers = Headers'
    { _heaItems    :: !(Maybe [Text])
    , _heaQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Headers' smart constructor.
headers :: Int -> Headers
headers pQuantity =
    Headers'
    { _heaItems = Nothing
    , _heaQuantity = pQuantity
    }

-- | Optional: A complex type that contains a Name element for each header
-- that you want CloudFront to forward to the origin and to vary on for
-- this cache behavior. If Quantity is 0, omit Items.
heaItems :: Lens' Headers [Text]
heaItems = lens _heaItems (\ s a -> s{_heaItems = a}) . _Default;

-- | The number of different headers that you want CloudFront to forward to
-- the origin and to vary on for this cache behavior. The maximum number of
-- headers that you can specify by name is 10. If you want CloudFront to
-- forward all headers to the origin and vary on all of them, specify 1 for
-- Quantity and * for Name. If you don\'t want CloudFront to forward any
-- additional headers to the origin or to vary on any headers, specify 0
-- for Quantity and omit Items.
heaQuantity :: Lens' Headers Int
heaQuantity = lens _heaQuantity (\ s a -> s{_heaQuantity = a});

instance FromXML Headers where
        parseXML x
          = Headers' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Name"))
                <*> (x .@ "Quantity")

instance ToXML Headers where
        toXML Headers'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Name" <$> _heaItems),
               "Quantity" @= _heaQuantity]

-- | An invalidation.
--
-- /See:/ 'invalidation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'invId'
--
-- * 'invStatus'
--
-- * 'invCreateTime'
--
-- * 'invInvalidationBatch'
data Invalidation = Invalidation'
    { _invId                :: !Text
    , _invStatus            :: !Text
    , _invCreateTime        :: !ISO8601
    , _invInvalidationBatch :: !InvalidationBatch
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Invalidation' smart constructor.
invalidation :: Text -> Text -> UTCTime -> InvalidationBatch -> Invalidation
invalidation pId pStatus pCreateTime pInvalidationBatch =
    Invalidation'
    { _invId = pId
    , _invStatus = pStatus
    , _invCreateTime = _Time # pCreateTime
    , _invInvalidationBatch = pInvalidationBatch
    }

-- | The identifier for the invalidation request. For example:
-- IDFDVBD632BHDS5.
invId :: Lens' Invalidation Text
invId = lens _invId (\ s a -> s{_invId = a});

-- | The status of the invalidation request. When the invalidation batch is
-- finished, the status is Completed.
invStatus :: Lens' Invalidation Text
invStatus = lens _invStatus (\ s a -> s{_invStatus = a});

-- | The date and time the invalidation request was first made.
invCreateTime :: Lens' Invalidation UTCTime
invCreateTime = lens _invCreateTime (\ s a -> s{_invCreateTime = a}) . _Time;

-- | The current invalidation information for the batch request.
invInvalidationBatch :: Lens' Invalidation InvalidationBatch
invInvalidationBatch = lens _invInvalidationBatch (\ s a -> s{_invInvalidationBatch = a});

instance FromXML Invalidation where
        parseXML x
          = Invalidation' <$>
              (x .@ "Id") <*> (x .@ "Status") <*>
                (x .@ "CreateTime")
                <*> (x .@ "InvalidationBatch")

-- | An invalidation batch.
--
-- /See:/ 'invalidationBatch' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ibPaths'
--
-- * 'ibCallerReference'
data InvalidationBatch = InvalidationBatch'
    { _ibPaths           :: !Paths
    , _ibCallerReference :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InvalidationBatch' smart constructor.
invalidationBatch :: Paths -> Text -> InvalidationBatch
invalidationBatch pPaths pCallerReference =
    InvalidationBatch'
    { _ibPaths = pPaths
    , _ibCallerReference = pCallerReference
    }

-- | The path of the object to invalidate. The path is relative to the
-- distribution and must begin with a slash (\/). You must enclose each
-- invalidation object with the Path element tags. If the path includes
-- non-ASCII characters or unsafe characters as defined in RFC 1783
-- (http:\/\/www.ietf.org\/rfc\/rfc1738.txt), URL encode those characters.
-- Do not URL encode any other characters in the path, or CloudFront will
-- not invalidate the old version of the updated object.
ibPaths :: Lens' InvalidationBatch Paths
ibPaths = lens _ibPaths (\ s a -> s{_ibPaths = a});

-- | A unique name that ensures the request can\'t be replayed. If the
-- CallerReference is new (no matter the content of the Path object), a new
-- distribution is created. If the CallerReference is a value you already
-- sent in a previous request to create an invalidation batch, and the
-- content of each Path element is identical to the original request, the
-- response includes the same information returned to the original request.
-- If the CallerReference is a value you already sent in a previous request
-- to create a distribution but the content of any Path is different from
-- the original request, CloudFront returns an
-- InvalidationBatchAlreadyExists error.
ibCallerReference :: Lens' InvalidationBatch Text
ibCallerReference = lens _ibCallerReference (\ s a -> s{_ibCallerReference = a});

instance FromXML InvalidationBatch where
        parseXML x
          = InvalidationBatch' <$>
              (x .@ "Paths") <*> (x .@ "CallerReference")

instance ToXML InvalidationBatch where
        toXML InvalidationBatch'{..}
          = mconcat
              ["Paths" @= _ibPaths,
               "CallerReference" @= _ibCallerReference]

-- | An invalidation list.
--
-- /See:/ 'invalidationList' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ilItems'
--
-- * 'ilNextMarker'
--
-- * 'ilMarker'
--
-- * 'ilMaxItems'
--
-- * 'ilIsTruncated'
--
-- * 'ilQuantity'
data InvalidationList = InvalidationList'
    { _ilItems       :: !(Maybe [InvalidationSummary])
    , _ilNextMarker  :: !(Maybe Text)
    , _ilMarker      :: !Text
    , _ilMaxItems    :: !Int
    , _ilIsTruncated :: !Bool
    , _ilQuantity    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InvalidationList' smart constructor.
invalidationList :: Text -> Int -> Bool -> Int -> InvalidationList
invalidationList pMarker pMaxItems pIsTruncated pQuantity =
    InvalidationList'
    { _ilItems = Nothing
    , _ilNextMarker = Nothing
    , _ilMarker = pMarker
    , _ilMaxItems = pMaxItems
    , _ilIsTruncated = pIsTruncated
    , _ilQuantity = pQuantity
    }

-- | A complex type that contains one InvalidationSummary element for each
-- invalidation batch that was created by the current AWS account.
ilItems :: Lens' InvalidationList [InvalidationSummary]
ilItems = lens _ilItems (\ s a -> s{_ilItems = a}) . _Default;

-- | If IsTruncated is true, this element is present and contains the value
-- you can use for the Marker request parameter to continue listing your
-- invalidation batches where they left off.
ilNextMarker :: Lens' InvalidationList (Maybe Text)
ilNextMarker = lens _ilNextMarker (\ s a -> s{_ilNextMarker = a});

-- | The value you provided for the Marker request parameter.
ilMarker :: Lens' InvalidationList Text
ilMarker = lens _ilMarker (\ s a -> s{_ilMarker = a});

-- | The value you provided for the MaxItems request parameter.
ilMaxItems :: Lens' InvalidationList Int
ilMaxItems = lens _ilMaxItems (\ s a -> s{_ilMaxItems = a});

-- | A flag that indicates whether more invalidation batch requests remain to
-- be listed. If your results were truncated, you can make a follow-up
-- pagination request using the Marker request parameter to retrieve more
-- invalidation batches in the list.
ilIsTruncated :: Lens' InvalidationList Bool
ilIsTruncated = lens _ilIsTruncated (\ s a -> s{_ilIsTruncated = a});

-- | The number of invalidation batches that were created by the current AWS
-- account.
ilQuantity :: Lens' InvalidationList Int
ilQuantity = lens _ilQuantity (\ s a -> s{_ilQuantity = a});

instance FromXML InvalidationList where
        parseXML x
          = InvalidationList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "InvalidationSummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "Marker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "IsTruncated")
                <*> (x .@ "Quantity")

-- | Summary of an invalidation request.
--
-- /See:/ 'invalidationSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isId'
--
-- * 'isCreateTime'
--
-- * 'isStatus'
data InvalidationSummary = InvalidationSummary'
    { _isId         :: !Text
    , _isCreateTime :: !ISO8601
    , _isStatus     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InvalidationSummary' smart constructor.
invalidationSummary :: Text -> UTCTime -> Text -> InvalidationSummary
invalidationSummary pId pCreateTime pStatus =
    InvalidationSummary'
    { _isId = pId
    , _isCreateTime = _Time # pCreateTime
    , _isStatus = pStatus
    }

-- | The unique ID for an invalidation request.
isId :: Lens' InvalidationSummary Text
isId = lens _isId (\ s a -> s{_isId = a});

-- | FIXME: Undocumented member.
isCreateTime :: Lens' InvalidationSummary UTCTime
isCreateTime = lens _isCreateTime (\ s a -> s{_isCreateTime = a}) . _Time;

-- | The status of an invalidation request.
isStatus :: Lens' InvalidationSummary Text
isStatus = lens _isStatus (\ s a -> s{_isStatus = a});

instance FromXML InvalidationSummary where
        parseXML x
          = InvalidationSummary' <$>
              (x .@ "Id") <*> (x .@ "CreateTime") <*>
                (x .@ "Status")

-- | A complex type that lists the active CloudFront key pairs, if any, that
-- are associated with AwsAccountNumber.
--
-- /See:/ 'keyPairIds' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kpiItems'
--
-- * 'kpiQuantity'
data KeyPairIds = KeyPairIds'
    { _kpiItems    :: !(Maybe [Text])
    , _kpiQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'KeyPairIds' smart constructor.
keyPairIds :: Int -> KeyPairIds
keyPairIds pQuantity =
    KeyPairIds'
    { _kpiItems = Nothing
    , _kpiQuantity = pQuantity
    }

-- | A complex type that lists the active CloudFront key pairs, if any, that
-- are associated with AwsAccountNumber.
kpiItems :: Lens' KeyPairIds [Text]
kpiItems = lens _kpiItems (\ s a -> s{_kpiItems = a}) . _Default;

-- | The number of active CloudFront key pairs for AwsAccountNumber.
kpiQuantity :: Lens' KeyPairIds Int
kpiQuantity = lens _kpiQuantity (\ s a -> s{_kpiQuantity = a});

instance FromXML KeyPairIds where
        parseXML x
          = KeyPairIds' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "KeyPairId"))
                <*> (x .@ "Quantity")

-- | A complex type that controls whether access logs are written for the
-- distribution.
--
-- /See:/ 'loggingConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcEnabled'
--
-- * 'lcIncludeCookies'
--
-- * 'lcBucket'
--
-- * 'lcPrefix'
data LoggingConfig = LoggingConfig'
    { _lcEnabled        :: !Bool
    , _lcIncludeCookies :: !Bool
    , _lcBucket         :: !Text
    , _lcPrefix         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoggingConfig' smart constructor.
loggingConfig :: Bool -> Bool -> Text -> Text -> LoggingConfig
loggingConfig pEnabled pIncludeCookies pBucket pPrefix =
    LoggingConfig'
    { _lcEnabled = pEnabled
    , _lcIncludeCookies = pIncludeCookies
    , _lcBucket = pBucket
    , _lcPrefix = pPrefix
    }

-- | Specifies whether you want CloudFront to save access logs to an Amazon
-- S3 bucket. If you do not want to enable logging when you create a
-- distribution or if you want to disable logging for an existing
-- distribution, specify false for Enabled, and specify empty Bucket and
-- Prefix elements. If you specify false for Enabled but you specify values
-- for Bucket, prefix and IncludeCookies, the values are automatically
-- deleted.
lcEnabled :: Lens' LoggingConfig Bool
lcEnabled = lens _lcEnabled (\ s a -> s{_lcEnabled = a});

-- | Specifies whether you want CloudFront to include cookies in access logs,
-- specify true for IncludeCookies. If you choose to include cookies in
-- logs, CloudFront logs all cookies regardless of how you configure the
-- cache behaviors for this distribution. If you do not want to include
-- cookies when you create a distribution or if you want to disable include
-- cookies for an existing distribution, specify false for IncludeCookies.
lcIncludeCookies :: Lens' LoggingConfig Bool
lcIncludeCookies = lens _lcIncludeCookies (\ s a -> s{_lcIncludeCookies = a});

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
lcBucket :: Lens' LoggingConfig Text
lcBucket = lens _lcBucket (\ s a -> s{_lcBucket = a});

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this distribution, for example, myprefix\/. If you want to
-- enable logging, but you do not want to specify a prefix, you still must
-- include an empty Prefix element in the Logging element.
lcPrefix :: Lens' LoggingConfig Text
lcPrefix = lens _lcPrefix (\ s a -> s{_lcPrefix = a});

instance FromXML LoggingConfig where
        parseXML x
          = LoggingConfig' <$>
              (x .@ "Enabled") <*> (x .@ "IncludeCookies") <*>
                (x .@ "Bucket")
                <*> (x .@ "Prefix")

instance ToXML LoggingConfig where
        toXML LoggingConfig'{..}
          = mconcat
              ["Enabled" @= _lcEnabled,
               "IncludeCookies" @= _lcIncludeCookies,
               "Bucket" @= _lcBucket, "Prefix" @= _lcPrefix]

-- | A complex type that describes the Amazon S3 bucket or the HTTP server
-- (for example, a web server) from which CloudFront gets your files.You
-- must create at least one origin.
--
-- /See:/ 'origin' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oriCustomOriginConfig'
--
-- * 'oriS3OriginConfig'
--
-- * 'oriOriginPath'
--
-- * 'oriId'
--
-- * 'oriDomainName'
data Origin = Origin'
    { _oriCustomOriginConfig :: !(Maybe CustomOriginConfig)
    , _oriS3OriginConfig     :: !(Maybe S3OriginConfig)
    , _oriOriginPath         :: !(Maybe Text)
    , _oriId                 :: !Text
    , _oriDomainName         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Origin' smart constructor.
origin :: Text -> Text -> Origin
origin pId pDomainName =
    Origin'
    { _oriCustomOriginConfig = Nothing
    , _oriS3OriginConfig = Nothing
    , _oriOriginPath = Nothing
    , _oriId = pId
    , _oriDomainName = pDomainName
    }

-- | A complex type that contains information about a custom origin. If the
-- origin is an Amazon S3 bucket, use the S3OriginConfig element instead.
oriCustomOriginConfig :: Lens' Origin (Maybe CustomOriginConfig)
oriCustomOriginConfig = lens _oriCustomOriginConfig (\ s a -> s{_oriCustomOriginConfig = a});

-- | A complex type that contains information about the Amazon S3 origin. If
-- the origin is a custom origin, use the CustomOriginConfig element
-- instead.
oriS3OriginConfig :: Lens' Origin (Maybe S3OriginConfig)
oriS3OriginConfig = lens _oriS3OriginConfig (\ s a -> s{_oriS3OriginConfig = a});

-- | An optional element that causes CloudFront to request your content from
-- a directory in your Amazon S3 bucket or your custom origin. When you
-- include the OriginPath element, specify the directory name, beginning
-- with a \/. CloudFront appends the directory name to the value of
-- DomainName.
oriOriginPath :: Lens' Origin (Maybe Text)
oriOriginPath = lens _oriOriginPath (\ s a -> s{_oriOriginPath = a});

-- | A unique identifier for the origin. The value of Id must be unique
-- within the distribution. You use the value of Id when you create a cache
-- behavior. The Id identifies the origin that CloudFront routes a request
-- to when the request matches the path pattern for that cache behavior.
oriId :: Lens' Origin Text
oriId = lens _oriId (\ s a -> s{_oriId = a});

-- | Amazon S3 origins: The DNS name of the Amazon S3 bucket from which you
-- want CloudFront to get objects for this origin, for example,
-- myawsbucket.s3.amazonaws.com. Custom origins: The DNS domain name for
-- the HTTP server from which you want CloudFront to get objects for this
-- origin, for example, www.example.com.
oriDomainName :: Lens' Origin Text
oriDomainName = lens _oriDomainName (\ s a -> s{_oriDomainName = a});

instance FromXML Origin where
        parseXML x
          = Origin' <$>
              (x .@? "CustomOriginConfig") <*>
                (x .@? "S3OriginConfig")
                <*> (x .@? "OriginPath")
                <*> (x .@ "Id")
                <*> (x .@ "DomainName")

instance ToXML Origin where
        toXML Origin'{..}
          = mconcat
              ["CustomOriginConfig" @= _oriCustomOriginConfig,
               "S3OriginConfig" @= _oriS3OriginConfig,
               "OriginPath" @= _oriOriginPath, "Id" @= _oriId,
               "DomainName" @= _oriDomainName]

-- | A complex type that contains information about origins for this
-- distribution.
--
-- /See:/ 'origins' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oriItems'
--
-- * 'oriQuantity'
data Origins = Origins'
    { _oriItems    :: !(Maybe (List1 Origin))
    , _oriQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Origins' smart constructor.
origins :: Int -> Origins
origins pQuantity =
    Origins'
    { _oriItems = Nothing
    , _oriQuantity = pQuantity
    }

-- | A complex type that contains origins for this distribution.
oriItems :: Lens' Origins (Maybe (NonEmpty Origin))
oriItems = lens _oriItems (\ s a -> s{_oriItems = a}) . mapping _List1;

-- | The number of origins for this distribution.
oriQuantity :: Lens' Origins Int
oriQuantity = lens _oriQuantity (\ s a -> s{_oriQuantity = a});

instance FromXML Origins where
        parseXML x
          = Origins' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList1 "Origin"))
                <*> (x .@ "Quantity")

instance ToXML Origins where
        toXML Origins'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Origin" <$> _oriItems),
               "Quantity" @= _oriQuantity]

-- | A complex type that contains information about the objects that you want
-- to invalidate.
--
-- /See:/ 'paths' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'patItems'
--
-- * 'patQuantity'
data Paths = Paths'
    { _patItems    :: !(Maybe [Text])
    , _patQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Paths' smart constructor.
paths :: Int -> Paths
paths pQuantity =
    Paths'
    { _patItems = Nothing
    , _patQuantity = pQuantity
    }

-- | A complex type that contains a list of the objects that you want to
-- invalidate.
patItems :: Lens' Paths [Text]
patItems = lens _patItems (\ s a -> s{_patItems = a}) . _Default;

-- | The number of objects that you want to invalidate.
patQuantity :: Lens' Paths Int
patQuantity = lens _patQuantity (\ s a -> s{_patQuantity = a});

instance FromXML Paths where
        parseXML x
          = Paths' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "Path"))
                <*> (x .@ "Quantity")

instance ToXML Paths where
        toXML Paths'{..}
          = mconcat
              ["Items" @= toXML (toXMLList "Path" <$> _patItems),
               "Quantity" @= _patQuantity]

-- | A complex type that identifies ways in which you want to restrict
-- distribution of your content.
--
-- /See:/ 'restrictions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'resGeoRestriction'
newtype Restrictions = Restrictions'
    { _resGeoRestriction :: GeoRestriction
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Restrictions' smart constructor.
restrictions :: GeoRestriction -> Restrictions
restrictions pGeoRestriction =
    Restrictions'
    { _resGeoRestriction = pGeoRestriction
    }

-- | FIXME: Undocumented member.
resGeoRestriction :: Lens' Restrictions GeoRestriction
resGeoRestriction = lens _resGeoRestriction (\ s a -> s{_resGeoRestriction = a});

instance FromXML Restrictions where
        parseXML x
          = Restrictions' <$> (x .@ "GeoRestriction")

instance ToXML Restrictions where
        toXML Restrictions'{..}
          = mconcat ["GeoRestriction" @= _resGeoRestriction]

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
--
-- /See:/ 's3Origin' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'soDomainName'
--
-- * 'soOriginAccessIdentity'
data S3Origin = S3Origin'
    { _soDomainName           :: !Text
    , _soOriginAccessIdentity :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'S3Origin' smart constructor.
s3Origin :: Text -> Text -> S3Origin
s3Origin pDomainName pOriginAccessIdentity =
    S3Origin'
    { _soDomainName = pDomainName
    , _soOriginAccessIdentity = pOriginAccessIdentity
    }

-- | The DNS name of the S3 origin.
soDomainName :: Lens' S3Origin Text
soDomainName = lens _soDomainName (\ s a -> s{_soDomainName = a});

-- | Your S3 origin\'s origin access identity.
soOriginAccessIdentity :: Lens' S3Origin Text
soOriginAccessIdentity = lens _soOriginAccessIdentity (\ s a -> s{_soOriginAccessIdentity = a});

instance FromXML S3Origin where
        parseXML x
          = S3Origin' <$>
              (x .@ "DomainName") <*> (x .@ "OriginAccessIdentity")

instance ToXML S3Origin where
        toXML S3Origin'{..}
          = mconcat
              ["DomainName" @= _soDomainName,
               "OriginAccessIdentity" @= _soOriginAccessIdentity]

-- | A complex type that contains information about the Amazon S3 origin. If
-- the origin is a custom origin, use the CustomOriginConfig element
-- instead.
--
-- /See:/ 's3OriginConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'socOriginAccessIdentity'
newtype S3OriginConfig = S3OriginConfig'
    { _socOriginAccessIdentity :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'S3OriginConfig' smart constructor.
s3OriginConfig :: Text -> S3OriginConfig
s3OriginConfig pOriginAccessIdentity =
    S3OriginConfig'
    { _socOriginAccessIdentity = pOriginAccessIdentity
    }

-- | The CloudFront origin access identity to associate with the origin. Use
-- an origin access identity to configure the origin so that end users can
-- only access objects in an Amazon S3 bucket through CloudFront. If you
-- want end users to be able to access objects using either the CloudFront
-- URL or the Amazon S3 URL, specify an empty OriginAccessIdentity element.
-- To delete the origin access identity from an existing distribution,
-- update the distribution configuration and include an empty
-- OriginAccessIdentity element. To replace the origin access identity,
-- update the distribution configuration and specify the new origin access
-- identity. Use the format origin-access-identity\/cloudfront\/Id where Id
-- is the value that CloudFront returned in the Id element when you created
-- the origin access identity.
socOriginAccessIdentity :: Lens' S3OriginConfig Text
socOriginAccessIdentity = lens _socOriginAccessIdentity (\ s a -> s{_socOriginAccessIdentity = a});

instance FromXML S3OriginConfig where
        parseXML x
          = S3OriginConfig' <$> (x .@ "OriginAccessIdentity")

instance ToXML S3OriginConfig where
        toXML S3OriginConfig'{..}
          = mconcat
              ["OriginAccessIdentity" @= _socOriginAccessIdentity]

-- | A complex type that lists the AWS accounts that were included in the
-- TrustedSigners complex type, as well as their active CloudFront key pair
-- IDs, if any.
--
-- /See:/ 'signer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sigAWSAccountNumber'
--
-- * 'sigKeyPairIds'
data Signer = Signer'
    { _sigAWSAccountNumber :: !(Maybe Text)
    , _sigKeyPairIds       :: !(Maybe KeyPairIds)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Signer' smart constructor.
signer :: Signer
signer =
    Signer'
    { _sigAWSAccountNumber = Nothing
    , _sigKeyPairIds = Nothing
    }

-- | Specifies an AWS account that can create signed URLs. Values: self,
-- which indicates that the AWS account that was used to create the
-- distribution can created signed URLs, or an AWS account number. Omit the
-- dashes in the account number.
sigAWSAccountNumber :: Lens' Signer (Maybe Text)
sigAWSAccountNumber = lens _sigAWSAccountNumber (\ s a -> s{_sigAWSAccountNumber = a});

-- | A complex type that lists the active CloudFront key pairs, if any, that
-- are associated with AwsAccountNumber.
sigKeyPairIds :: Lens' Signer (Maybe KeyPairIds)
sigKeyPairIds = lens _sigKeyPairIds (\ s a -> s{_sigKeyPairIds = a});

instance FromXML Signer where
        parseXML x
          = Signer' <$>
              (x .@? "AwsAccountNumber") <*> (x .@? "KeyPairIds")

-- | A streaming distribution.
--
-- /See:/ 'streamingDistribution' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdLastModifiedTime'
--
-- * 'sdId'
--
-- * 'sdStatus'
--
-- * 'sdDomainName'
--
-- * 'sdActiveTrustedSigners'
--
-- * 'sdStreamingDistributionConfig'
data StreamingDistribution = StreamingDistribution'
    { _sdLastModifiedTime            :: !(Maybe ISO8601)
    , _sdId                          :: !Text
    , _sdStatus                      :: !Text
    , _sdDomainName                  :: !Text
    , _sdActiveTrustedSigners        :: !ActiveTrustedSigners
    , _sdStreamingDistributionConfig :: !StreamingDistributionConfig
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StreamingDistribution' smart constructor.
streamingDistribution :: Text -> Text -> Text -> ActiveTrustedSigners -> StreamingDistributionConfig -> StreamingDistribution
streamingDistribution pId pStatus pDomainName pActiveTrustedSigners pStreamingDistributionConfig =
    StreamingDistribution'
    { _sdLastModifiedTime = Nothing
    , _sdId = pId
    , _sdStatus = pStatus
    , _sdDomainName = pDomainName
    , _sdActiveTrustedSigners = pActiveTrustedSigners
    , _sdStreamingDistributionConfig = pStreamingDistributionConfig
    }

-- | The date and time the distribution was last modified.
sdLastModifiedTime :: Lens' StreamingDistribution (Maybe UTCTime)
sdLastModifiedTime = lens _sdLastModifiedTime (\ s a -> s{_sdLastModifiedTime = a}) . mapping _Time;

-- | The identifier for the streaming distribution. For example:
-- EGTXBD79H29TRA8.
sdId :: Lens' StreamingDistribution Text
sdId = lens _sdId (\ s a -> s{_sdId = a});

-- | The current status of the streaming distribution. When the status is
-- Deployed, the distribution\'s information is fully propagated throughout
-- the Amazon CloudFront system.
sdStatus :: Lens' StreamingDistribution Text
sdStatus = lens _sdStatus (\ s a -> s{_sdStatus = a});

-- | The domain name corresponding to the streaming distribution. For
-- example: s5c39gqb8ow64r.cloudfront.net.
sdDomainName :: Lens' StreamingDistribution Text
sdDomainName = lens _sdDomainName (\ s a -> s{_sdDomainName = a});

-- | CloudFront automatically adds this element to the response only if
-- you\'ve set up the distribution to serve private content with signed
-- URLs. The element lists the key pair IDs that CloudFront is aware of for
-- each trusted signer. The Signer child element lists the AWS account
-- number of the trusted signer (or an empty Self element if the signer is
-- you). The Signer element also includes the IDs of any active key pairs
-- associated with the trusted signer\'s AWS account. If no KeyPairId
-- element appears for a Signer, that signer can\'t create working signed
-- URLs.
sdActiveTrustedSigners :: Lens' StreamingDistribution ActiveTrustedSigners
sdActiveTrustedSigners = lens _sdActiveTrustedSigners (\ s a -> s{_sdActiveTrustedSigners = a});

-- | The current configuration information for the streaming distribution.
sdStreamingDistributionConfig :: Lens' StreamingDistribution StreamingDistributionConfig
sdStreamingDistributionConfig = lens _sdStreamingDistributionConfig (\ s a -> s{_sdStreamingDistributionConfig = a});

instance FromXML StreamingDistribution where
        parseXML x
          = StreamingDistribution' <$>
              (x .@? "LastModifiedTime") <*> (x .@ "Id") <*>
                (x .@ "Status")
                <*> (x .@ "DomainName")
                <*> (x .@ "ActiveTrustedSigners")
                <*> (x .@ "StreamingDistributionConfig")

-- | The configuration for the streaming distribution.
--
-- /See:/ 'streamingDistributionConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdcAliases'
--
-- * 'sdcPriceClass'
--
-- * 'sdcLogging'
--
-- * 'sdcCallerReference'
--
-- * 'sdcS3Origin'
--
-- * 'sdcComment'
--
-- * 'sdcTrustedSigners'
--
-- * 'sdcEnabled'
data StreamingDistributionConfig = StreamingDistributionConfig'
    { _sdcAliases         :: !(Maybe Aliases)
    , _sdcPriceClass      :: !(Maybe PriceClass)
    , _sdcLogging         :: !(Maybe StreamingLoggingConfig)
    , _sdcCallerReference :: !Text
    , _sdcS3Origin        :: !S3Origin
    , _sdcComment         :: !Text
    , _sdcTrustedSigners  :: !TrustedSigners
    , _sdcEnabled         :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StreamingDistributionConfig' smart constructor.
streamingDistributionConfig :: Text -> S3Origin -> Text -> TrustedSigners -> Bool -> StreamingDistributionConfig
streamingDistributionConfig pCallerReference pS3Origin pComment pTrustedSigners pEnabled =
    StreamingDistributionConfig'
    { _sdcAliases = Nothing
    , _sdcPriceClass = Nothing
    , _sdcLogging = Nothing
    , _sdcCallerReference = pCallerReference
    , _sdcS3Origin = pS3Origin
    , _sdcComment = pComment
    , _sdcTrustedSigners = pTrustedSigners
    , _sdcEnabled = pEnabled
    }

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdcAliases :: Lens' StreamingDistributionConfig (Maybe Aliases)
sdcAliases = lens _sdcAliases (\ s a -> s{_sdcAliases = a});

-- | A complex type that contains information about price class for this
-- streaming distribution.
sdcPriceClass :: Lens' StreamingDistributionConfig (Maybe PriceClass)
sdcPriceClass = lens _sdcPriceClass (\ s a -> s{_sdcPriceClass = a});

-- | A complex type that controls whether access logs are written for the
-- streaming distribution.
sdcLogging :: Lens' StreamingDistributionConfig (Maybe StreamingLoggingConfig)
sdcLogging = lens _sdcLogging (\ s a -> s{_sdcLogging = a});

-- | A unique number that ensures the request can\'t be replayed. If the
-- CallerReference is new (no matter the content of the
-- StreamingDistributionConfig object), a new streaming distribution is
-- created. If the CallerReference is a value you already sent in a
-- previous request to create a streaming distribution, and the content of
-- the StreamingDistributionConfig is identical to the original request
-- (ignoring white space), the response includes the same information
-- returned to the original request. If the CallerReference is a value you
-- already sent in a previous request to create a streaming distribution
-- but the content of the StreamingDistributionConfig is different from the
-- original request, CloudFront returns a DistributionAlreadyExists error.
sdcCallerReference :: Lens' StreamingDistributionConfig Text
sdcCallerReference = lens _sdcCallerReference (\ s a -> s{_sdcCallerReference = a});

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdcS3Origin :: Lens' StreamingDistributionConfig S3Origin
sdcS3Origin = lens _sdcS3Origin (\ s a -> s{_sdcS3Origin = a});

-- | Any comments you want to include about the streaming distribution.
sdcComment :: Lens' StreamingDistributionConfig Text
sdcComment = lens _sdcComment (\ s a -> s{_sdcComment = a});

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
sdcTrustedSigners :: Lens' StreamingDistributionConfig TrustedSigners
sdcTrustedSigners = lens _sdcTrustedSigners (\ s a -> s{_sdcTrustedSigners = a});

-- | Whether the streaming distribution is enabled to accept end user
-- requests for content.
sdcEnabled :: Lens' StreamingDistributionConfig Bool
sdcEnabled = lens _sdcEnabled (\ s a -> s{_sdcEnabled = a});

instance FromXML StreamingDistributionConfig where
        parseXML x
          = StreamingDistributionConfig' <$>
              (x .@? "Aliases") <*> (x .@? "PriceClass") <*>
                (x .@? "Logging")
                <*> (x .@ "CallerReference")
                <*> (x .@ "S3Origin")
                <*> (x .@ "Comment")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "Enabled")

instance ToXML StreamingDistributionConfig where
        toXML StreamingDistributionConfig'{..}
          = mconcat
              ["Aliases" @= _sdcAliases,
               "PriceClass" @= _sdcPriceClass,
               "Logging" @= _sdcLogging,
               "CallerReference" @= _sdcCallerReference,
               "S3Origin" @= _sdcS3Origin, "Comment" @= _sdcComment,
               "TrustedSigners" @= _sdcTrustedSigners,
               "Enabled" @= _sdcEnabled]

-- | A streaming distribution list.
--
-- /See:/ 'streamingDistributionList' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdlItems'
--
-- * 'sdlNextMarker'
--
-- * 'sdlMarker'
--
-- * 'sdlMaxItems'
--
-- * 'sdlIsTruncated'
--
-- * 'sdlQuantity'
data StreamingDistributionList = StreamingDistributionList'
    { _sdlItems       :: !(Maybe [StreamingDistributionSummary])
    , _sdlNextMarker  :: !(Maybe Text)
    , _sdlMarker      :: !Text
    , _sdlMaxItems    :: !Int
    , _sdlIsTruncated :: !Bool
    , _sdlQuantity    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StreamingDistributionList' smart constructor.
streamingDistributionList :: Text -> Int -> Bool -> Int -> StreamingDistributionList
streamingDistributionList pMarker pMaxItems pIsTruncated pQuantity =
    StreamingDistributionList'
    { _sdlItems = Nothing
    , _sdlNextMarker = Nothing
    , _sdlMarker = pMarker
    , _sdlMaxItems = pMaxItems
    , _sdlIsTruncated = pIsTruncated
    , _sdlQuantity = pQuantity
    }

-- | A complex type that contains one StreamingDistributionSummary element
-- for each distribution that was created by the current AWS account.
sdlItems :: Lens' StreamingDistributionList [StreamingDistributionSummary]
sdlItems = lens _sdlItems (\ s a -> s{_sdlItems = a}) . _Default;

-- | If IsTruncated is true, this element is present and contains the value
-- you can use for the Marker request parameter to continue listing your
-- streaming distributions where they left off.
sdlNextMarker :: Lens' StreamingDistributionList (Maybe Text)
sdlNextMarker = lens _sdlNextMarker (\ s a -> s{_sdlNextMarker = a});

-- | The value you provided for the Marker request parameter.
sdlMarker :: Lens' StreamingDistributionList Text
sdlMarker = lens _sdlMarker (\ s a -> s{_sdlMarker = a});

-- | The value you provided for the MaxItems request parameter.
sdlMaxItems :: Lens' StreamingDistributionList Int
sdlMaxItems = lens _sdlMaxItems (\ s a -> s{_sdlMaxItems = a});

-- | A flag that indicates whether more streaming distributions remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request using the Marker request parameter to retrieve more
-- distributions in the list.
sdlIsTruncated :: Lens' StreamingDistributionList Bool
sdlIsTruncated = lens _sdlIsTruncated (\ s a -> s{_sdlIsTruncated = a});

-- | The number of streaming distributions that were created by the current
-- AWS account.
sdlQuantity :: Lens' StreamingDistributionList Int
sdlQuantity = lens _sdlQuantity (\ s a -> s{_sdlQuantity = a});

instance FromXML StreamingDistributionList where
        parseXML x
          = StreamingDistributionList' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "StreamingDistributionSummary"))
                <*> (x .@? "NextMarker")
                <*> (x .@ "Marker")
                <*> (x .@ "MaxItems")
                <*> (x .@ "IsTruncated")
                <*> (x .@ "Quantity")

-- | A summary of the information for an Amazon CloudFront streaming
-- distribution.
--
-- /See:/ 'streamingDistributionSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdsId'
--
-- * 'sdsStatus'
--
-- * 'sdsLastModifiedTime'
--
-- * 'sdsDomainName'
--
-- * 'sdsS3Origin'
--
-- * 'sdsAliases'
--
-- * 'sdsTrustedSigners'
--
-- * 'sdsComment'
--
-- * 'sdsPriceClass'
--
-- * 'sdsEnabled'
data StreamingDistributionSummary = StreamingDistributionSummary'
    { _sdsId               :: !Text
    , _sdsStatus           :: !Text
    , _sdsLastModifiedTime :: !ISO8601
    , _sdsDomainName       :: !Text
    , _sdsS3Origin         :: !S3Origin
    , _sdsAliases          :: !Aliases
    , _sdsTrustedSigners   :: !TrustedSigners
    , _sdsComment          :: !Text
    , _sdsPriceClass       :: !PriceClass
    , _sdsEnabled          :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StreamingDistributionSummary' smart constructor.
streamingDistributionSummary :: Text -> Text -> UTCTime -> Text -> S3Origin -> Aliases -> TrustedSigners -> Text -> PriceClass -> Bool -> StreamingDistributionSummary
streamingDistributionSummary pId pStatus pLastModifiedTime pDomainName pS3Origin pAliases pTrustedSigners pComment pPriceClass pEnabled =
    StreamingDistributionSummary'
    { _sdsId = pId
    , _sdsStatus = pStatus
    , _sdsLastModifiedTime = _Time # pLastModifiedTime
    , _sdsDomainName = pDomainName
    , _sdsS3Origin = pS3Origin
    , _sdsAliases = pAliases
    , _sdsTrustedSigners = pTrustedSigners
    , _sdsComment = pComment
    , _sdsPriceClass = pPriceClass
    , _sdsEnabled = pEnabled
    }

-- | The identifier for the distribution. For example: EDFDVBD632BHDS5.
sdsId :: Lens' StreamingDistributionSummary Text
sdsId = lens _sdsId (\ s a -> s{_sdsId = a});

-- | Indicates the current status of the distribution. When the status is
-- Deployed, the distribution\'s information is fully propagated throughout
-- the Amazon CloudFront system.
sdsStatus :: Lens' StreamingDistributionSummary Text
sdsStatus = lens _sdsStatus (\ s a -> s{_sdsStatus = a});

-- | The date and time the distribution was last modified.
sdsLastModifiedTime :: Lens' StreamingDistributionSummary UTCTime
sdsLastModifiedTime = lens _sdsLastModifiedTime (\ s a -> s{_sdsLastModifiedTime = a}) . _Time;

-- | The domain name corresponding to the distribution. For example:
-- d604721fxaaqy9.cloudfront.net.
sdsDomainName :: Lens' StreamingDistributionSummary Text
sdsDomainName = lens _sdsDomainName (\ s a -> s{_sdsDomainName = a});

-- | A complex type that contains information about the Amazon S3 bucket from
-- which you want CloudFront to get your media files for distribution.
sdsS3Origin :: Lens' StreamingDistributionSummary S3Origin
sdsS3Origin = lens _sdsS3Origin (\ s a -> s{_sdsS3Origin = a});

-- | A complex type that contains information about CNAMEs (alternate domain
-- names), if any, for this streaming distribution.
sdsAliases :: Lens' StreamingDistributionSummary Aliases
sdsAliases = lens _sdsAliases (\ s a -> s{_sdsAliases = a});

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
sdsTrustedSigners :: Lens' StreamingDistributionSummary TrustedSigners
sdsTrustedSigners = lens _sdsTrustedSigners (\ s a -> s{_sdsTrustedSigners = a});

-- | The comment originally specified when this distribution was created.
sdsComment :: Lens' StreamingDistributionSummary Text
sdsComment = lens _sdsComment (\ s a -> s{_sdsComment = a});

-- | FIXME: Undocumented member.
sdsPriceClass :: Lens' StreamingDistributionSummary PriceClass
sdsPriceClass = lens _sdsPriceClass (\ s a -> s{_sdsPriceClass = a});

-- | Whether the distribution is enabled to accept end user requests for
-- content.
sdsEnabled :: Lens' StreamingDistributionSummary Bool
sdsEnabled = lens _sdsEnabled (\ s a -> s{_sdsEnabled = a});

instance FromXML StreamingDistributionSummary where
        parseXML x
          = StreamingDistributionSummary' <$>
              (x .@ "Id") <*> (x .@ "Status") <*>
                (x .@ "LastModifiedTime")
                <*> (x .@ "DomainName")
                <*> (x .@ "S3Origin")
                <*> (x .@ "Aliases")
                <*> (x .@ "TrustedSigners")
                <*> (x .@ "Comment")
                <*> (x .@ "PriceClass")
                <*> (x .@ "Enabled")

-- | A complex type that controls whether access logs are written for this
-- streaming distribution.
--
-- /See:/ 'streamingLoggingConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slcEnabled'
--
-- * 'slcBucket'
--
-- * 'slcPrefix'
data StreamingLoggingConfig = StreamingLoggingConfig'
    { _slcEnabled :: !Bool
    , _slcBucket  :: !Text
    , _slcPrefix  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StreamingLoggingConfig' smart constructor.
streamingLoggingConfig :: Bool -> Text -> Text -> StreamingLoggingConfig
streamingLoggingConfig pEnabled pBucket pPrefix =
    StreamingLoggingConfig'
    { _slcEnabled = pEnabled
    , _slcBucket = pBucket
    , _slcPrefix = pPrefix
    }

-- | Specifies whether you want CloudFront to save access logs to an Amazon
-- S3 bucket. If you do not want to enable logging when you create a
-- streaming distribution or if you want to disable logging for an existing
-- streaming distribution, specify false for Enabled, and specify empty
-- Bucket and Prefix elements. If you specify false for Enabled but you
-- specify values for Bucket and Prefix, the values are automatically
-- deleted.
slcEnabled :: Lens' StreamingLoggingConfig Bool
slcEnabled = lens _slcEnabled (\ s a -> s{_slcEnabled = a});

-- | The Amazon S3 bucket to store the access logs in, for example,
-- myawslogbucket.s3.amazonaws.com.
slcBucket :: Lens' StreamingLoggingConfig Text
slcBucket = lens _slcBucket (\ s a -> s{_slcBucket = a});

-- | An optional string that you want CloudFront to prefix to the access log
-- filenames for this streaming distribution, for example, myprefix\/. If
-- you want to enable logging, but you do not want to specify a prefix, you
-- still must include an empty Prefix element in the Logging element.
slcPrefix :: Lens' StreamingLoggingConfig Text
slcPrefix = lens _slcPrefix (\ s a -> s{_slcPrefix = a});

instance FromXML StreamingLoggingConfig where
        parseXML x
          = StreamingLoggingConfig' <$>
              (x .@ "Enabled") <*> (x .@ "Bucket") <*>
                (x .@ "Prefix")

instance ToXML StreamingLoggingConfig where
        toXML StreamingLoggingConfig'{..}
          = mconcat
              ["Enabled" @= _slcEnabled, "Bucket" @= _slcBucket,
               "Prefix" @= _slcPrefix]

-- | A complex type that specifies the AWS accounts, if any, that you want to
-- allow to create signed URLs for private content. If you want to require
-- signed URLs in requests for objects in the target origin that match the
-- PathPattern for this cache behavior, specify true for Enabled, and
-- specify the applicable values for Quantity and Items. For more
-- information, go to Using a Signed URL to Serve Private Content in the
-- Amazon CloudFront Developer Guide. If you don\'t want to require signed
-- URLs in requests for objects that match PathPattern, specify false for
-- Enabled and 0 for Quantity. Omit Items. To add, change, or remove one or
-- more trusted signers, change Enabled to true (if it\'s currently false),
-- change Quantity as applicable, and specify all of the trusted signers
-- that you want to include in the updated distribution.
--
-- /See:/ 'trustedSigners' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tsItems'
--
-- * 'tsEnabled'
--
-- * 'tsQuantity'
data TrustedSigners = TrustedSigners'
    { _tsItems    :: !(Maybe [Text])
    , _tsEnabled  :: !Bool
    , _tsQuantity :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TrustedSigners' smart constructor.
trustedSigners :: Bool -> Int -> TrustedSigners
trustedSigners pEnabled pQuantity =
    TrustedSigners'
    { _tsItems = Nothing
    , _tsEnabled = pEnabled
    , _tsQuantity = pQuantity
    }

-- | Optional: A complex type that contains trusted signers for this cache
-- behavior. If Quantity is 0, you can omit Items.
tsItems :: Lens' TrustedSigners [Text]
tsItems = lens _tsItems (\ s a -> s{_tsItems = a}) . _Default;

-- | Specifies whether you want to require end users to use signed URLs to
-- access the files specified by PathPattern and TargetOriginId.
tsEnabled :: Lens' TrustedSigners Bool
tsEnabled = lens _tsEnabled (\ s a -> s{_tsEnabled = a});

-- | The number of trusted signers for this cache behavior.
tsQuantity :: Lens' TrustedSigners Int
tsQuantity = lens _tsQuantity (\ s a -> s{_tsQuantity = a});

instance FromXML TrustedSigners where
        parseXML x
          = TrustedSigners' <$>
              (x .@? "Items" .!@ mempty >>=
                 may (parseXMLList "AwsAccountNumber"))
                <*> (x .@ "Enabled")
                <*> (x .@ "Quantity")

instance ToXML TrustedSigners where
        toXML TrustedSigners'{..}
          = mconcat
              ["Items" @=
                 toXML (toXMLList "AwsAccountNumber" <$> _tsItems),
               "Enabled" @= _tsEnabled, "Quantity" @= _tsQuantity]

-- | A complex type that contains information about viewer certificates for
-- this distribution.
--
-- /See:/ 'viewerCertificate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcSSLSupportMethod'
--
-- * 'vcMinimumProtocolVersion'
--
-- * 'vcIAMCertificateId'
--
-- * 'vcCloudFrontDefaultCertificate'
data ViewerCertificate = ViewerCertificate'
    { _vcSSLSupportMethod             :: !(Maybe SSLSupportMethod)
    , _vcMinimumProtocolVersion       :: !(Maybe MinimumProtocolVersion)
    , _vcIAMCertificateId             :: !(Maybe Text)
    , _vcCloudFrontDefaultCertificate :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ViewerCertificate' smart constructor.
viewerCertificate :: ViewerCertificate
viewerCertificate =
    ViewerCertificate'
    { _vcSSLSupportMethod = Nothing
    , _vcMinimumProtocolVersion = Nothing
    , _vcIAMCertificateId = Nothing
    , _vcCloudFrontDefaultCertificate = Nothing
    }

-- | If you specify a value for IAMCertificateId, you must also specify how
-- you want CloudFront to serve HTTPS requests. Valid values are vip and
-- sni-only. If you specify vip, CloudFront uses dedicated IP addresses for
-- your content and can respond to HTTPS requests from any viewer. However,
-- you must request permission to use this feature, and you incur
-- additional monthly charges. If you specify sni-only, CloudFront can only
-- respond to HTTPS requests from viewers that support Server Name
-- Indication (SNI). All modern browsers support SNI, but some browsers
-- still in use don\'t support SNI. Do not specify a value for
-- SSLSupportMethod if you specified true for CloudFrontDefaultCertificate.
vcSSLSupportMethod :: Lens' ViewerCertificate (Maybe SSLSupportMethod)
vcSSLSupportMethod = lens _vcSSLSupportMethod (\ s a -> s{_vcSSLSupportMethod = a});

-- | Specify the minimum version of the SSL protocol that you want CloudFront
-- to use, SSLv3 or TLSv1, for HTTPS connections. CloudFront will serve
-- your objects only to browsers or devices that support at least the SSL
-- version that you specify. The TLSv1 protocol is more secure, so we
-- recommend that you specify SSLv3 only if your users are using browsers
-- or devices that don\'t support TLSv1. If you\'re using a custom
-- certificate (if you specify a value for IAMCertificateId) and if you\'re
-- using dedicated IP (if you specify vip for SSLSupportMethod), you can
-- choose SSLv3 or TLSv1 as the MinimumProtocolVersion. If you\'re using a
-- custom certificate (if you specify a value for IAMCertificateId) and if
-- you\'re using SNI (if you specify sni-only for SSLSupportMethod), you
-- must specify TLSv1 for MinimumProtocolVersion.
vcMinimumProtocolVersion :: Lens' ViewerCertificate (Maybe MinimumProtocolVersion)
vcMinimumProtocolVersion = lens _vcMinimumProtocolVersion (\ s a -> s{_vcMinimumProtocolVersion = a});

-- | If you want viewers to use HTTPS to request your objects and you\'re
-- using an alternate domain name in your object URLs (for example,
-- https:\/\/example.com\/logo.jpg), specify the IAM certificate identifier
-- of the custom viewer certificate for this distribution. Specify either
-- this value or CloudFrontDefaultCertificate.
vcIAMCertificateId :: Lens' ViewerCertificate (Maybe Text)
vcIAMCertificateId = lens _vcIAMCertificateId (\ s a -> s{_vcIAMCertificateId = a});

-- | If you want viewers to use HTTPS to request your objects and you\'re
-- using the CloudFront domain name of your distribution in your object
-- URLs (for example, https:\/\/d111111abcdef8.cloudfront.net\/logo.jpg),
-- set to true. Omit this value if you are setting an IAMCertificateId.
vcCloudFrontDefaultCertificate :: Lens' ViewerCertificate (Maybe Bool)
vcCloudFrontDefaultCertificate = lens _vcCloudFrontDefaultCertificate (\ s a -> s{_vcCloudFrontDefaultCertificate = a});

instance FromXML ViewerCertificate where
        parseXML x
          = ViewerCertificate' <$>
              (x .@? "SSLSupportMethod") <*>
                (x .@? "MinimumProtocolVersion")
                <*> (x .@? "IAMCertificateId")
                <*> (x .@? "CloudFrontDefaultCertificate")

instance ToXML ViewerCertificate where
        toXML ViewerCertificate'{..}
          = mconcat
              ["SSLSupportMethod" @= _vcSSLSupportMethod,
               "MinimumProtocolVersion" @=
                 _vcMinimumProtocolVersion,
               "IAMCertificateId" @= _vcIAMCertificateId,
               "CloudFrontDefaultCertificate" @=
                 _vcCloudFrontDefaultCertificate]
