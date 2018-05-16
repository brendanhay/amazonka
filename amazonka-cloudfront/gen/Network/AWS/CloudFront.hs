{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon CloudFront is a web service that speeds up distribution of your static and dynamic web content, for example, .html, .css, .php, image, and media files, to end users. CloudFront delivers your content through a worldwide network of edge locations. When an end user requests content that you're serving with CloudFront, the user is routed to the edge location that provides the lowest latency, so content is delivered with the best possible performance. If the content is already in that edge location, CloudFront delivers it immediately. If the content is not currently in that edge location, CloudFront retrieves it from an Amazon S3 bucket or an HTTP server (for example, a web server) that you have identified as the source for the definitive version of your content.
module Network.AWS.CloudFront
    (
    -- * Service Configuration
      cloudFront

    -- * Errors
    -- $errors

    -- ** TooManyOriginCustomHeaders
    , _TooManyOriginCustomHeaders

    -- ** InvalidTagging
    , _InvalidTagging

    -- ** InvalidErrorCode
    , _InvalidErrorCode

    -- ** NoSuchFieldLevelEncryptionProfile
    , _NoSuchFieldLevelEncryptionProfile

    -- ** FieldLevelEncryptionProfileInUse
    , _FieldLevelEncryptionProfileInUse

    -- ** InvalidOriginReadTimeout
    , _InvalidOriginReadTimeout

    -- ** TooManyFieldLevelEncryptionProfiles
    , _TooManyFieldLevelEncryptionProfiles

    -- ** TooManyCacheBehaviors
    , _TooManyCacheBehaviors

    -- ** TooManyCloudFrontOriginAccessIdentities
    , _TooManyCloudFrontOriginAccessIdentities

    -- ** InvalidOriginAccessIdentity
    , _InvalidOriginAccessIdentity

    -- ** DistributionNotDisabled
    , _DistributionNotDisabled

    -- ** NoSuchStreamingDistribution
    , _NoSuchStreamingDistribution

    -- ** InconsistentQuantities
    , _InconsistentQuantities

    -- ** InvalidArgument
    , _InvalidArgument

    -- ** InvalidOriginKeepaliveTimeout
    , _InvalidOriginKeepaliveTimeout

    -- ** TooManyInvalidationsInProgress
    , _TooManyInvalidationsInProgress

    -- ** InvalidWebACLId
    , _InvalidWebACLId

    -- ** TooManyQueryStringParameters
    , _TooManyQueryStringParameters

    -- ** TooManyFieldLevelEncryptionQueryArgProfiles
    , _TooManyFieldLevelEncryptionQueryArgProfiles

    -- ** TooManyDistributionCNAMEs
    , _TooManyDistributionCNAMEs

    -- ** NoSuchCloudFrontOriginAccessIdentity
    , _NoSuchCloudFrontOriginAccessIdentity

    -- ** CloudFrontOriginAccessIdentityInUse
    , _CloudFrontOriginAccessIdentityInUse

    -- ** TooManyStreamingDistributions
    , _TooManyStreamingDistributions

    -- ** CannotChangeImmutablePublicKeyFields
    , _CannotChangeImmutablePublicKeyFields

    -- ** BatchTooLarge
    , _BatchTooLarge

    -- ** TooManyCookieNamesInWhiteList
    , _TooManyCookieNamesInWhiteList

    -- ** InvalidLambdaFunctionAssociation
    , _InvalidLambdaFunctionAssociation

    -- ** InvalidForwardCookies
    , _InvalidForwardCookies

    -- ** FieldLevelEncryptionConfigInUse
    , _FieldLevelEncryptionConfigInUse

    -- ** TooManyTrustedSigners
    , _TooManyTrustedSigners

    -- ** InvalidOrigin
    , _InvalidOrigin

    -- ** NoSuchInvalidation
    , _NoSuchInvalidation

    -- ** PublicKeyAlreadyExists
    , _PublicKeyAlreadyExists

    -- ** NoSuchOrigin
    , _NoSuchOrigin

    -- ** InvalidTTLOrder
    , _InvalidTTLOrder

    -- ** StreamingDistributionNotDisabled
    , _StreamingDistributionNotDisabled

    -- ** TooManyHeadersInForwardedValues
    , _TooManyHeadersInForwardedValues

    -- ** NoSuchResource
    , _NoSuchResource

    -- ** TooManyFieldLevelEncryptionEncryptionEntities
    , _TooManyFieldLevelEncryptionEncryptionEntities

    -- ** TooManyStreamingDistributionCNAMEs
    , _TooManyStreamingDistributionCNAMEs

    -- ** FieldLevelEncryptionProfileAlreadyExists
    , _FieldLevelEncryptionProfileAlreadyExists

    -- ** ResourceInUse
    , _ResourceInUse

    -- ** InvalidRequiredProtocol
    , _InvalidRequiredProtocol

    -- ** TooManyDistributions
    , _TooManyDistributions

    -- ** TooManyCertificates
    , _TooManyCertificates

    -- ** DistributionAlreadyExists
    , _DistributionAlreadyExists

    -- ** TooManyDistributionsAssociatedToFieldLevelEncryptionConfig
    , _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig

    -- ** InvalidQueryStringParameters
    , _InvalidQueryStringParameters

    -- ** MissingBody
    , _MissingBody

    -- ** IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior
    , _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior

    -- ** IllegalUpdate
    , _IllegalUpdate

    -- ** InvalidIfMatchVersion
    , _InvalidIfMatchVersion

    -- ** FieldLevelEncryptionConfigAlreadyExists
    , _FieldLevelEncryptionConfigAlreadyExists

    -- ** PreconditionFailed
    , _PreconditionFailed

    -- ** InvalidResponseCode
    , _InvalidResponseCode

    -- ** InvalidHeadersForS3Origin
    , _InvalidHeadersForS3Origin

    -- ** CNAMEAlreadyExists
    , _CNAMEAlreadyExists

    -- ** NoSuchPublicKey
    , _NoSuchPublicKey

    -- ** PublicKeyInUse
    , _PublicKeyInUse

    -- ** TrustedSignerDoesNotExist
    , _TrustedSignerDoesNotExist

    -- ** InvalidProtocolSettings
    , _InvalidProtocolSettings

    -- ** TooManyPublicKeys
    , _TooManyPublicKeys

    -- ** NoSuchFieldLevelEncryptionConfig
    , _NoSuchFieldLevelEncryptionConfig

    -- ** TooManyFieldLevelEncryptionContentTypeProfiles
    , _TooManyFieldLevelEncryptionContentTypeProfiles

    -- ** TooManyFieldLevelEncryptionFieldPatterns
    , _TooManyFieldLevelEncryptionFieldPatterns

    -- ** TooManyFieldLevelEncryptionConfigs
    , _TooManyFieldLevelEncryptionConfigs

    -- ** TooManyLambdaFunctionAssociations
    , _TooManyLambdaFunctionAssociations

    -- ** CloudFrontOriginAccessIdentityAlreadyExists
    , _CloudFrontOriginAccessIdentityAlreadyExists

    -- ** TooManyOrigins
    , _TooManyOrigins

    -- ** InvalidRelativePath
    , _InvalidRelativePath

    -- ** StreamingDistributionAlreadyExists
    , _StreamingDistributionAlreadyExists

    -- ** QueryArgProfileEmpty
    , _QueryArgProfileEmpty

    -- ** InvalidMinimumProtocolVersion
    , _InvalidMinimumProtocolVersion

    -- ** AccessDenied
    , _AccessDenied

    -- ** InvalidViewerCertificate
    , _InvalidViewerCertificate

    -- ** NoSuchDistribution
    , _NoSuchDistribution

    -- ** FieldLevelEncryptionProfileSizeExceeded
    , _FieldLevelEncryptionProfileSizeExceeded

    -- ** InvalidDefaultRootObject
    , _InvalidDefaultRootObject

    -- ** TooManyDistributionsWithLambdaAssociations
    , _TooManyDistributionsWithLambdaAssociations

    -- ** InvalidGeoRestrictionParameter
    , _InvalidGeoRestrictionParameter

    -- ** InvalidLocationCode
    , _InvalidLocationCode

    -- * Waiters
    -- $waiters

    -- ** StreamingDistributionDeployed
    , streamingDistributionDeployed

    -- ** DistributionDeployed
    , distributionDeployed

    -- ** InvalidationCompleted
    , invalidationCompleted

    -- * Operations
    -- $operations

    -- ** DeleteStreamingDistribution
    , module Network.AWS.CloudFront.DeleteStreamingDistribution

    -- ** UpdateStreamingDistribution
    , module Network.AWS.CloudFront.UpdateStreamingDistribution

    -- ** ListPublicKeys
    , module Network.AWS.CloudFront.ListPublicKeys

    -- ** GetFieldLevelEncryptionConfig
    , module Network.AWS.CloudFront.GetFieldLevelEncryptionConfig

    -- ** ListTagsForResource
    , module Network.AWS.CloudFront.ListTagsForResource

    -- ** CreatePublicKey
    , module Network.AWS.CloudFront.CreatePublicKey

    -- ** GetPublicKeyConfig
    , module Network.AWS.CloudFront.GetPublicKeyConfig

    -- ** CreateDistributionWithTags
    , module Network.AWS.CloudFront.CreateDistributionWithTags

    -- ** CreateFieldLevelEncryptionConfig
    , module Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig

    -- ** GetFieldLevelEncryption
    , module Network.AWS.CloudFront.GetFieldLevelEncryption

    -- ** GetPublicKey
    , module Network.AWS.CloudFront.GetPublicKey

    -- ** DeleteFieldLevelEncryptionConfig
    , module Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig

    -- ** UpdateFieldLevelEncryptionConfig
    , module Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig

    -- ** CreateDistribution
    , module Network.AWS.CloudFront.CreateDistribution

    -- ** GetFieldLevelEncryptionProfile
    , module Network.AWS.CloudFront.GetFieldLevelEncryptionProfile

    -- ** GetDistributionConfig
    , module Network.AWS.CloudFront.GetDistributionConfig

    -- ** CreateStreamingDistributionWithTags
    , module Network.AWS.CloudFront.CreateStreamingDistributionWithTags

    -- ** DeleteFieldLevelEncryptionProfile
    , module Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile

    -- ** UpdateFieldLevelEncryptionProfile
    , module Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile

    -- ** DeleteServiceLinkedRole
    , module Network.AWS.CloudFront.DeleteServiceLinkedRole

    -- ** CreateFieldLevelEncryptionProfile
    , module Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile

    -- ** GetDistribution
    , module Network.AWS.CloudFront.GetDistribution

    -- ** GetFieldLevelEncryptionProfileConfig
    , module Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig

    -- ** UpdateCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity

    -- ** DeleteCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity

    -- ** ListStreamingDistributions (Paginated)
    , module Network.AWS.CloudFront.ListStreamingDistributions

    -- ** DeletePublicKey
    , module Network.AWS.CloudFront.DeletePublicKey

    -- ** UpdatePublicKey
    , module Network.AWS.CloudFront.UpdatePublicKey

    -- ** GetStreamingDistributionConfig
    , module Network.AWS.CloudFront.GetStreamingDistributionConfig

    -- ** GetCloudFrontOriginAccessIdentityConfig
    , module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig

    -- ** CreateStreamingDistribution
    , module Network.AWS.CloudFront.CreateStreamingDistribution

    -- ** CreateCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity

    -- ** ListCloudFrontOriginAccessIdentities (Paginated)
    , module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities

    -- ** GetInvalidation
    , module Network.AWS.CloudFront.GetInvalidation

    -- ** ListInvalidations (Paginated)
    , module Network.AWS.CloudFront.ListInvalidations

    -- ** CreateInvalidation
    , module Network.AWS.CloudFront.CreateInvalidation

    -- ** GetCloudFrontOriginAccessIdentity
    , module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity

    -- ** ListFieldLevelEncryptionConfigs
    , module Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs

    -- ** TagResource
    , module Network.AWS.CloudFront.TagResource

    -- ** GetStreamingDistribution
    , module Network.AWS.CloudFront.GetStreamingDistribution

    -- ** UpdateDistribution
    , module Network.AWS.CloudFront.UpdateDistribution

    -- ** DeleteDistribution
    , module Network.AWS.CloudFront.DeleteDistribution

    -- ** UntagResource
    , module Network.AWS.CloudFront.UntagResource

    -- ** ListDistributionsByWebACLId
    , module Network.AWS.CloudFront.ListDistributionsByWebACLId

    -- ** ListDistributions (Paginated)
    , module Network.AWS.CloudFront.ListDistributions

    -- ** ListFieldLevelEncryptionProfiles
    , module Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles

    -- * Types

    -- ** CertificateSource
    , CertificateSource (..)

    -- ** EventType
    , EventType (..)

    -- ** Format
    , Format (..)

    -- ** GeoRestrictionType
    , GeoRestrictionType (..)

    -- ** HTTPVersion
    , HTTPVersion (..)

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

    -- ** SSLProtocol
    , SSLProtocol (..)

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
    , cbLambdaFunctionAssociations
    , cbMaxTTL
    , cbCompress
    , cbSmoothStreaming
    , cbDefaultTTL
    , cbFieldLevelEncryptionId
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

    -- ** ContentTypeProfile
    , ContentTypeProfile
    , contentTypeProfile
    , ctpProfileId
    , ctpFormat
    , ctpContentType

    -- ** ContentTypeProfileConfig
    , ContentTypeProfileConfig
    , contentTypeProfileConfig
    , ctpcContentTypeProfiles
    , ctpcForwardWhenContentTypeIsUnknown

    -- ** ContentTypeProfiles
    , ContentTypeProfiles
    , contentTypeProfiles
    , ctpItems
    , ctpQuantity

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

    -- ** CustomHeaders
    , CustomHeaders
    , customHeaders
    , chItems
    , chQuantity

    -- ** CustomOriginConfig
    , CustomOriginConfig
    , customOriginConfig
    , cocOriginKeepaliveTimeout
    , cocOriginReadTimeout
    , cocOriginSSLProtocols
    , cocHTTPPort
    , cocHTTPSPort
    , cocOriginProtocolPolicy

    -- ** DefaultCacheBehavior
    , DefaultCacheBehavior
    , defaultCacheBehavior
    , dcbAllowedMethods
    , dcbLambdaFunctionAssociations
    , dcbMaxTTL
    , dcbCompress
    , dcbSmoothStreaming
    , dcbDefaultTTL
    , dcbFieldLevelEncryptionId
    , dcbTargetOriginId
    , dcbForwardedValues
    , dcbTrustedSigners
    , dcbViewerProtocolPolicy
    , dcbMinTTL

    -- ** Distribution
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

    -- ** DistributionConfig
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
    , dcIsIPV6Enabled
    , dcCallerReference
    , dcOrigins
    , dcDefaultCacheBehavior
    , dcComment
    , dcEnabled

    -- ** DistributionConfigWithTags
    , DistributionConfigWithTags
    , distributionConfigWithTags
    , dcwtDistributionConfig
    , dcwtTags

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
    , dsIsIPV6Enabled

    -- ** EncryptionEntities
    , EncryptionEntities
    , encryptionEntities
    , eeItems
    , eeQuantity

    -- ** EncryptionEntity
    , EncryptionEntity
    , encryptionEntity
    , eePublicKeyId
    , eeProviderId
    , eeFieldPatterns

    -- ** FieldLevelEncryption
    , FieldLevelEncryption
    , fieldLevelEncryption
    , fleId
    , fleLastModifiedTime
    , fleFieldLevelEncryptionConfig

    -- ** FieldLevelEncryptionConfig
    , FieldLevelEncryptionConfig
    , fieldLevelEncryptionConfig
    , flecQueryArgProfileConfig
    , flecContentTypeProfileConfig
    , flecComment
    , flecCallerReference

    -- ** FieldLevelEncryptionList
    , FieldLevelEncryptionList
    , fieldLevelEncryptionList
    , flelItems
    , flelNextMarker
    , flelMaxItems
    , flelQuantity

    -- ** FieldLevelEncryptionProfile
    , FieldLevelEncryptionProfile
    , fieldLevelEncryptionProfile
    , flepId
    , flepLastModifiedTime
    , flepFieldLevelEncryptionProfileConfig

    -- ** FieldLevelEncryptionProfileConfig
    , FieldLevelEncryptionProfileConfig
    , fieldLevelEncryptionProfileConfig
    , flepcComment
    , flepcName
    , flepcCallerReference
    , flepcEncryptionEntities

    -- ** FieldLevelEncryptionProfileList
    , FieldLevelEncryptionProfileList
    , fieldLevelEncryptionProfileList
    , fleplItems
    , fleplNextMarker
    , fleplMaxItems
    , fleplQuantity

    -- ** FieldLevelEncryptionProfileSummary
    , FieldLevelEncryptionProfileSummary
    , fieldLevelEncryptionProfileSummary
    , flepsComment
    , flepsId
    , flepsLastModifiedTime
    , flepsName
    , flepsEncryptionEntities

    -- ** FieldLevelEncryptionSummary
    , FieldLevelEncryptionSummary
    , fieldLevelEncryptionSummary
    , flesQueryArgProfileConfig
    , flesContentTypeProfileConfig
    , flesComment
    , flesId
    , flesLastModifiedTime

    -- ** FieldPatterns
    , FieldPatterns
    , fieldPatterns
    , fpItems
    , fpQuantity

    -- ** ForwardedValues
    , ForwardedValues
    , forwardedValues
    , fvQueryStringCacheKeys
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

    -- ** LambdaFunctionAssociation
    , LambdaFunctionAssociation
    , lambdaFunctionAssociation
    , lfaLambdaFunctionARN
    , lfaEventType

    -- ** LambdaFunctionAssociations
    , LambdaFunctionAssociations
    , lambdaFunctionAssociations
    , lfaItems
    , lfaQuantity

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
    , oCustomHeaders
    , oCustomOriginConfig
    , oS3OriginConfig
    , oOriginPath
    , oId
    , oDomainName

    -- ** OriginCustomHeader
    , OriginCustomHeader
    , originCustomHeader
    , ochHeaderName
    , ochHeaderValue

    -- ** OriginSSLProtocols
    , OriginSSLProtocols
    , originSSLProtocols
    , ospQuantity
    , ospItems

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

    -- ** PublicKey
    , PublicKey
    , publicKey
    , pkId
    , pkCreatedTime
    , pkPublicKeyConfig

    -- ** PublicKeyConfig
    , PublicKeyConfig
    , publicKeyConfig
    , pkcComment
    , pkcCallerReference
    , pkcName
    , pkcEncodedKey

    -- ** PublicKeyList
    , PublicKeyList
    , publicKeyList
    , pklItems
    , pklNextMarker
    , pklMaxItems
    , pklQuantity

    -- ** PublicKeySummary
    , PublicKeySummary
    , publicKeySummary
    , pksComment
    , pksId
    , pksName
    , pksCreatedTime
    , pksEncodedKey

    -- ** QueryArgProfile
    , QueryArgProfile
    , queryArgProfile
    , qapQueryArg
    , qapProfileId

    -- ** QueryArgProfileConfig
    , QueryArgProfileConfig
    , queryArgProfileConfig
    , qapcQueryArgProfiles
    , qapcForwardWhenQueryArgProfileIsUnknown

    -- ** QueryArgProfiles
    , QueryArgProfiles
    , queryArgProfiles
    , qapItems
    , qapQuantity

    -- ** QueryStringCacheKeys
    , QueryStringCacheKeys
    , queryStringCacheKeys
    , qsckItems
    , qsckQuantity

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
    , sdARN
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

    -- ** StreamingDistributionConfigWithTags
    , StreamingDistributionConfigWithTags
    , streamingDistributionConfigWithTags
    , sdcwtStreamingDistributionConfig
    , sdcwtTags

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

    -- ** StreamingLoggingConfig
    , StreamingLoggingConfig
    , streamingLoggingConfig
    , slcEnabled
    , slcBucket
    , slcPrefix

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TagKeys
    , TagKeys
    , tagKeys
    , tkItems

    -- ** Tags
    , Tags
    , tags
    , tItems

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
    , vcACMCertificateARN
    , vcCertificateSource
    , vcMinimumProtocolVersion
    , vcCertificate
    , vcIAMCertificateId
    , vcCloudFrontDefaultCertificate
    ) where

import Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.CreateDistribution
import Network.AWS.CloudFront.CreateDistributionWithTags
import Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
import Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
import Network.AWS.CloudFront.CreateInvalidation
import Network.AWS.CloudFront.CreatePublicKey
import Network.AWS.CloudFront.CreateStreamingDistribution
import Network.AWS.CloudFront.CreateStreamingDistributionWithTags
import Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.DeleteDistribution
import Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig
import Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
import Network.AWS.CloudFront.DeletePublicKey
import Network.AWS.CloudFront.DeleteServiceLinkedRole
import Network.AWS.CloudFront.DeleteStreamingDistribution
import Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
import Network.AWS.CloudFront.GetDistribution
import Network.AWS.CloudFront.GetDistributionConfig
import Network.AWS.CloudFront.GetFieldLevelEncryption
import Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
import Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
import Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
import Network.AWS.CloudFront.GetInvalidation
import Network.AWS.CloudFront.GetPublicKey
import Network.AWS.CloudFront.GetPublicKeyConfig
import Network.AWS.CloudFront.GetStreamingDistribution
import Network.AWS.CloudFront.GetStreamingDistributionConfig
import Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
import Network.AWS.CloudFront.ListDistributions
import Network.AWS.CloudFront.ListDistributionsByWebACLId
import Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
import Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
import Network.AWS.CloudFront.ListInvalidations
import Network.AWS.CloudFront.ListPublicKeys
import Network.AWS.CloudFront.ListStreamingDistributions
import Network.AWS.CloudFront.ListTagsForResource
import Network.AWS.CloudFront.TagResource
import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.UntagResource
import Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.UpdateDistribution
import Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig
import Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile
import Network.AWS.CloudFront.UpdatePublicKey
import Network.AWS.CloudFront.UpdateStreamingDistribution
import Network.AWS.CloudFront.Waiters

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
