{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon CloudFront is a web service that speeds up distribution of your static and dynamic web content, for example, .html, .css, .php, image, and media files, to end users. CloudFront delivers your content through a worldwide network of edge locations. When an end user requests content that you're serving with CloudFront, the user is routed to the edge location that provides the lowest latency, so content is delivered with the best possible performance. If the content is already in that edge location, CloudFront delivers it immediately. If the content is not currently in that edge location, CloudFront retrieves it from an Amazon S3 bucket or an HTTP server (for example, a web server) that you have identified as the source for the definitive version of your content.
module Network.AWS.CloudFront
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** TooManyOriginCustomHeaders
    _TooManyOriginCustomHeaders,

    -- ** InvalidTagging
    _InvalidTagging,

    -- ** InvalidErrorCode
    _InvalidErrorCode,

    -- ** NoSuchFieldLevelEncryptionProfile
    _NoSuchFieldLevelEncryptionProfile,

    -- ** FieldLevelEncryptionProfileInUse
    _FieldLevelEncryptionProfileInUse,

    -- ** InvalidOriginReadTimeout
    _InvalidOriginReadTimeout,

    -- ** TooManyFieldLevelEncryptionProfiles
    _TooManyFieldLevelEncryptionProfiles,

    -- ** TooManyCacheBehaviors
    _TooManyCacheBehaviors,

    -- ** TooManyCloudFrontOriginAccessIdentities
    _TooManyCloudFrontOriginAccessIdentities,

    -- ** RealtimeLogConfigAlreadyExists
    _RealtimeLogConfigAlreadyExists,

    -- ** InvalidOriginAccessIdentity
    _InvalidOriginAccessIdentity,

    -- ** DistributionNotDisabled
    _DistributionNotDisabled,

    -- ** NoSuchStreamingDistribution
    _NoSuchStreamingDistribution,

    -- ** InconsistentQuantities
    _InconsistentQuantities,

    -- ** InvalidArgument
    _InvalidArgument,

    -- ** InvalidOriginKeepaliveTimeout
    _InvalidOriginKeepaliveTimeout,

    -- ** TooManyInvalidationsInProgress
    _TooManyInvalidationsInProgress,

    -- ** InvalidWebACLId
    _InvalidWebACLId,

    -- ** TooManyQueryStringParameters
    _TooManyQueryStringParameters,

    -- ** TooManyFieldLevelEncryptionQueryArgProfiles
    _TooManyFieldLevelEncryptionQueryArgProfiles,

    -- ** TooManyDistributionCNAMEs
    _TooManyDistributionCNAMEs,

    -- ** NoSuchCloudFrontOriginAccessIdentity
    _NoSuchCloudFrontOriginAccessIdentity,

    -- ** CloudFrontOriginAccessIdentityInUse
    _CloudFrontOriginAccessIdentityInUse,

    -- ** TooManyStreamingDistributions
    _TooManyStreamingDistributions,

    -- ** CannotChangeImmutablePublicKeyFields
    _CannotChangeImmutablePublicKeyFields,

    -- ** BatchTooLarge
    _BatchTooLarge,

    -- ** TooManyCookieNamesInWhiteList
    _TooManyCookieNamesInWhiteList,

    -- ** TooManyPublicKeysInKeyGroup
    _TooManyPublicKeysInKeyGroup,

    -- ** InvalidLambdaFunctionAssociation
    _InvalidLambdaFunctionAssociation,

    -- ** TooManyKeyGroupsAssociatedToDistribution
    _TooManyKeyGroupsAssociatedToDistribution,

    -- ** NoSuchRealtimeLogConfig
    _NoSuchRealtimeLogConfig,

    -- ** InvalidForwardCookies
    _InvalidForwardCookies,

    -- ** FieldLevelEncryptionConfigInUse
    _FieldLevelEncryptionConfigInUse,

    -- ** TooManyTrustedSigners
    _TooManyTrustedSigners,

    -- ** TooManyDistributionsAssociatedToKeyGroup
    _TooManyDistributionsAssociatedToKeyGroup,

    -- ** InvalidOrigin
    _InvalidOrigin,

    -- ** CachePolicyInUse
    _CachePolicyInUse,

    -- ** NoSuchInvalidation
    _NoSuchInvalidation,

    -- ** PublicKeyAlreadyExists
    _PublicKeyAlreadyExists,

    -- ** NoSuchOrigin
    _NoSuchOrigin,

    -- ** TooManyHeadersInCachePolicy
    _TooManyHeadersInCachePolicy,

    -- ** NoSuchCachePolicy
    _NoSuchCachePolicy,

    -- ** InvalidTTLOrder
    _InvalidTTLOrder,

    -- ** StreamingDistributionNotDisabled
    _StreamingDistributionNotDisabled,

    -- ** OriginRequestPolicyAlreadyExists
    _OriginRequestPolicyAlreadyExists,

    -- ** TooManyHeadersInForwardedValues
    _TooManyHeadersInForwardedValues,

    -- ** NoSuchResource
    _NoSuchResource,

    -- ** TooManyFieldLevelEncryptionEncryptionEntities
    _TooManyFieldLevelEncryptionEncryptionEntities,

    -- ** TooManyStreamingDistributionCNAMEs
    _TooManyStreamingDistributionCNAMEs,

    -- ** FieldLevelEncryptionProfileAlreadyExists
    _FieldLevelEncryptionProfileAlreadyExists,

    -- ** KeyGroupAlreadyExists
    _KeyGroupAlreadyExists,

    -- ** TrustedKeyGroupDoesNotExist
    _TrustedKeyGroupDoesNotExist,

    -- ** ResourceInUse
    _ResourceInUse,

    -- ** InvalidRequiredProtocol
    _InvalidRequiredProtocol,

    -- ** TooManyDistributions
    _TooManyDistributions,

    -- ** TooManyDistributionsWithSingleFunctionARN
    _TooManyDistributionsWithSingleFunctionARN,

    -- ** TooManyHeadersInOriginRequestPolicy
    _TooManyHeadersInOriginRequestPolicy,

    -- ** TooManyCertificates
    _TooManyCertificates,

    -- ** NoSuchOriginRequestPolicy
    _NoSuchOriginRequestPolicy,

    -- ** DistributionAlreadyExists
    _DistributionAlreadyExists,

    -- ** TooManyDistributionsAssociatedToFieldLevelEncryptionConfig
    _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig,

    -- ** TooManyKeyGroups
    _TooManyKeyGroups,

    -- ** InvalidQueryStringParameters
    _InvalidQueryStringParameters,

    -- ** MissingBody
    _MissingBody,

    -- ** IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior
    _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior,

    -- ** TooManyOriginRequestPolicies
    _TooManyOriginRequestPolicies,

    -- ** IllegalDelete
    _IllegalDelete,

    -- ** IllegalUpdate
    _IllegalUpdate,

    -- ** InvalidIfMatchVersion
    _InvalidIfMatchVersion,

    -- ** FieldLevelEncryptionConfigAlreadyExists
    _FieldLevelEncryptionConfigAlreadyExists,

    -- ** PreconditionFailed
    _PreconditionFailed,

    -- ** InvalidResponseCode
    _InvalidResponseCode,

    -- ** InvalidHeadersForS3Origin
    _InvalidHeadersForS3Origin,

    -- ** CNAMEAlreadyExists
    _CNAMEAlreadyExists,

    -- ** NoSuchPublicKey
    _NoSuchPublicKey,

    -- ** PublicKeyInUse
    _PublicKeyInUse,

    -- ** TrustedSignerDoesNotExist
    _TrustedSignerDoesNotExist,

    -- ** InvalidProtocolSettings
    _InvalidProtocolSettings,

    -- ** CachePolicyAlreadyExists
    _CachePolicyAlreadyExists,

    -- ** TooManyCookiesInOriginRequestPolicy
    _TooManyCookiesInOriginRequestPolicy,

    -- ** TooManyOriginGroupsPerDistribution
    _TooManyOriginGroupsPerDistribution,

    -- ** TooManyPublicKeys
    _TooManyPublicKeys,

    -- ** NoSuchFieldLevelEncryptionConfig
    _NoSuchFieldLevelEncryptionConfig,

    -- ** TooManyRealtimeLogConfigs
    _TooManyRealtimeLogConfigs,

    -- ** RealtimeLogConfigInUse
    _RealtimeLogConfigInUse,

    -- ** TooManyCachePolicies
    _TooManyCachePolicies,

    -- ** TooManyFieldLevelEncryptionContentTypeProfiles
    _TooManyFieldLevelEncryptionContentTypeProfiles,

    -- ** TooManyFieldLevelEncryptionFieldPatterns
    _TooManyFieldLevelEncryptionFieldPatterns,

    -- ** TooManyFieldLevelEncryptionConfigs
    _TooManyFieldLevelEncryptionConfigs,

    -- ** TooManyLambdaFunctionAssociations
    _TooManyLambdaFunctionAssociations,

    -- ** CloudFrontOriginAccessIdentityAlreadyExists
    _CloudFrontOriginAccessIdentityAlreadyExists,

    -- ** TooManyQueryStringsInCachePolicy
    _TooManyQueryStringsInCachePolicy,

    -- ** TooManyOrigins
    _TooManyOrigins,

    -- ** InvalidRelativePath
    _InvalidRelativePath,

    -- ** StreamingDistributionAlreadyExists
    _StreamingDistributionAlreadyExists,

    -- ** TooManyDistributionsAssociatedToOriginRequestPolicy
    _TooManyDistributionsAssociatedToOriginRequestPolicy,

    -- ** QueryArgProfileEmpty
    _QueryArgProfileEmpty,

    -- ** TooManyCookiesInCachePolicy
    _TooManyCookiesInCachePolicy,

    -- ** InvalidMinimumProtocolVersion
    _InvalidMinimumProtocolVersion,

    -- ** AccessDenied
    _AccessDenied,

    -- ** InvalidViewerCertificate
    _InvalidViewerCertificate,

    -- ** NoSuchDistribution
    _NoSuchDistribution,

    -- ** FieldLevelEncryptionProfileSizeExceeded
    _FieldLevelEncryptionProfileSizeExceeded,

    -- ** TooManyQueryStringsInOriginRequestPolicy
    _TooManyQueryStringsInOriginRequestPolicy,

    -- ** InvalidDefaultRootObject
    _InvalidDefaultRootObject,

    -- ** TooManyDistributionsWithLambdaAssociations
    _TooManyDistributionsWithLambdaAssociations,

    -- ** TooManyDistributionsAssociatedToCachePolicy
    _TooManyDistributionsAssociatedToCachePolicy,

    -- ** InvalidGeoRestrictionParameter
    _InvalidGeoRestrictionParameter,

    -- ** OriginRequestPolicyInUse
    _OriginRequestPolicyInUse,

    -- ** InvalidLocationCode
    _InvalidLocationCode,

    -- * Waiters
    -- $waiters

    -- ** StreamingDistributionDeployed
    mkStreamingDistributionDeployed,

    -- ** DistributionDeployed
    mkDistributionDeployed,

    -- ** InvalidationCompleted
    mkInvalidationCompleted,

    -- * Operations
    -- $operations

    -- ** DeleteOriginRequestPolicy
    module Network.AWS.CloudFront.DeleteOriginRequestPolicy,

    -- ** UpdateOriginRequestPolicy
    module Network.AWS.CloudFront.UpdateOriginRequestPolicy,

    -- ** DeleteStreamingDistribution
    module Network.AWS.CloudFront.DeleteStreamingDistribution,

    -- ** UpdateStreamingDistribution
    module Network.AWS.CloudFront.UpdateStreamingDistribution,

    -- ** ListPublicKeys
    module Network.AWS.CloudFront.ListPublicKeys,

    -- ** GetFieldLevelEncryptionConfig
    module Network.AWS.CloudFront.GetFieldLevelEncryptionConfig,

    -- ** ListTagsForResource
    module Network.AWS.CloudFront.ListTagsForResource,

    -- ** CreatePublicKey
    module Network.AWS.CloudFront.CreatePublicKey,

    -- ** GetPublicKeyConfig
    module Network.AWS.CloudFront.GetPublicKeyConfig,

    -- ** CreateDistributionWithTags
    module Network.AWS.CloudFront.CreateDistributionWithTags,

    -- ** CreateFieldLevelEncryptionConfig
    module Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig,

    -- ** DeleteCachePolicy
    module Network.AWS.CloudFront.DeleteCachePolicy,

    -- ** UpdateCachePolicy
    module Network.AWS.CloudFront.UpdateCachePolicy,

    -- ** GetFieldLevelEncryption
    module Network.AWS.CloudFront.GetFieldLevelEncryption,

    -- ** ListRealtimeLogConfigs
    module Network.AWS.CloudFront.ListRealtimeLogConfigs,

    -- ** GetPublicKey
    module Network.AWS.CloudFront.GetPublicKey,

    -- ** DeleteRealtimeLogConfig
    module Network.AWS.CloudFront.DeleteRealtimeLogConfig,

    -- ** UpdateRealtimeLogConfig
    module Network.AWS.CloudFront.UpdateRealtimeLogConfig,

    -- ** ListDistributionsByOriginRequestPolicyId
    module Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId,

    -- ** DeleteFieldLevelEncryptionConfig
    module Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig,

    -- ** UpdateFieldLevelEncryptionConfig
    module Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig,

    -- ** GetKeyGroup
    module Network.AWS.CloudFront.GetKeyGroup,

    -- ** CreateDistribution
    module Network.AWS.CloudFront.CreateDistribution,

    -- ** GetFieldLevelEncryptionProfile
    module Network.AWS.CloudFront.GetFieldLevelEncryptionProfile,

    -- ** DeleteMonitoringSubscription
    module Network.AWS.CloudFront.DeleteMonitoringSubscription,

    -- ** GetDistributionConfig
    module Network.AWS.CloudFront.GetDistributionConfig,

    -- ** CreateStreamingDistributionWithTags
    module Network.AWS.CloudFront.CreateStreamingDistributionWithTags,

    -- ** DeleteFieldLevelEncryptionProfile
    module Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile,

    -- ** UpdateFieldLevelEncryptionProfile
    module Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile,

    -- ** ListDistributionsByCachePolicyId
    module Network.AWS.CloudFront.ListDistributionsByCachePolicyId,

    -- ** CreateFieldLevelEncryptionProfile
    module Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile,

    -- ** GetKeyGroupConfig
    module Network.AWS.CloudFront.GetKeyGroupConfig,

    -- ** GetDistribution
    module Network.AWS.CloudFront.GetDistribution,

    -- ** GetFieldLevelEncryptionProfileConfig
    module Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig,

    -- ** CreateKeyGroup
    module Network.AWS.CloudFront.CreateKeyGroup,

    -- ** UpdateCloudFrontOriginAccessIdentity
    module Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity,

    -- ** DeleteCloudFrontOriginAccessIdentity
    module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity,

    -- ** ListStreamingDistributions (Paginated)
    module Network.AWS.CloudFront.ListStreamingDistributions,

    -- ** DeletePublicKey
    module Network.AWS.CloudFront.DeletePublicKey,

    -- ** UpdatePublicKey
    module Network.AWS.CloudFront.UpdatePublicKey,

    -- ** GetRealtimeLogConfig
    module Network.AWS.CloudFront.GetRealtimeLogConfig,

    -- ** GetStreamingDistributionConfig
    module Network.AWS.CloudFront.GetStreamingDistributionConfig,

    -- ** GetCloudFrontOriginAccessIdentityConfig
    module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig,

    -- ** CreateStreamingDistribution
    module Network.AWS.CloudFront.CreateStreamingDistribution,

    -- ** CreateCloudFrontOriginAccessIdentity
    module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity,

    -- ** ListCloudFrontOriginAccessIdentities (Paginated)
    module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities,

    -- ** GetInvalidation
    module Network.AWS.CloudFront.GetInvalidation,

    -- ** GetCachePolicy
    module Network.AWS.CloudFront.GetCachePolicy,

    -- ** CreateRealtimeLogConfig
    module Network.AWS.CloudFront.CreateRealtimeLogConfig,

    -- ** ListInvalidations (Paginated)
    module Network.AWS.CloudFront.ListInvalidations,

    -- ** CreateInvalidation
    module Network.AWS.CloudFront.CreateInvalidation,

    -- ** GetCloudFrontOriginAccessIdentity
    module Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity,

    -- ** ListCachePolicies
    module Network.AWS.CloudFront.ListCachePolicies,

    -- ** CreateCachePolicy
    module Network.AWS.CloudFront.CreateCachePolicy,

    -- ** GetCachePolicyConfig
    module Network.AWS.CloudFront.GetCachePolicyConfig,

    -- ** ListFieldLevelEncryptionConfigs
    module Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs,

    -- ** ListDistributionsByKeyGroup
    module Network.AWS.CloudFront.ListDistributionsByKeyGroup,

    -- ** TagResource
    module Network.AWS.CloudFront.TagResource,

    -- ** GetStreamingDistribution
    module Network.AWS.CloudFront.GetStreamingDistribution,

    -- ** UpdateDistribution
    module Network.AWS.CloudFront.UpdateDistribution,

    -- ** DeleteDistribution
    module Network.AWS.CloudFront.DeleteDistribution,

    -- ** GetOriginRequestPolicy
    module Network.AWS.CloudFront.GetOriginRequestPolicy,

    -- ** UntagResource
    module Network.AWS.CloudFront.UntagResource,

    -- ** CreateMonitoringSubscription
    module Network.AWS.CloudFront.CreateMonitoringSubscription,

    -- ** ListDistributionsByWebACLId
    module Network.AWS.CloudFront.ListDistributionsByWebACLId,

    -- ** ListDistributions (Paginated)
    module Network.AWS.CloudFront.ListDistributions,

    -- ** ListDistributionsByRealtimeLogConfig
    module Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig,

    -- ** CreateOriginRequestPolicy
    module Network.AWS.CloudFront.CreateOriginRequestPolicy,

    -- ** ListKeyGroups
    module Network.AWS.CloudFront.ListKeyGroups,

    -- ** ListFieldLevelEncryptionProfiles
    module Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles,

    -- ** GetMonitoringSubscription
    module Network.AWS.CloudFront.GetMonitoringSubscription,

    -- ** UpdateKeyGroup
    module Network.AWS.CloudFront.UpdateKeyGroup,

    -- ** DeleteKeyGroup
    module Network.AWS.CloudFront.DeleteKeyGroup,

    -- ** ListOriginRequestPolicies
    module Network.AWS.CloudFront.ListOriginRequestPolicies,

    -- ** GetOriginRequestPolicyConfig
    module Network.AWS.CloudFront.GetOriginRequestPolicyConfig,

    -- * Types

    -- ** EncryptionEntity
    EncryptionEntity (..),
    mkEncryptionEntity,
    eePublicKeyId,
    eeProviderId,
    eeFieldPatterns,

    -- ** CloudFrontOriginAccessIdentityList
    CloudFrontOriginAccessIdentityList (..),
    mkCloudFrontOriginAccessIdentityList,
    cfoailMarker,
    cfoailMaxItems,
    cfoailIsTruncated,
    cfoailQuantity,
    cfoailItems,
    cfoailNextMarker,

    -- ** CachePolicy
    CachePolicy (..),
    mkCachePolicy,
    cpId,
    cpLastModifiedTime,
    cpCachePolicyConfig,

    -- ** Invalidation
    Invalidation (..),
    mkInvalidation,
    iId,
    iStatus,
    iCreateTime,
    iInvalidationBatch,

    -- ** SSLSupportMethod
    SSLSupportMethod (..),

    -- ** AllowedMethods
    AllowedMethods (..),
    mkAllowedMethods,
    amQuantity,
    amItems,
    amCachedMethods,

    -- ** OriginGroupFailoverCriteria
    OriginGroupFailoverCriteria (..),
    mkOriginGroupFailoverCriteria,
    ogfcStatusCodes,

    -- ** AliasICPRecordal
    AliasICPRecordal (..),
    mkAliasICPRecordal,
    aicprCNAME,
    aicprICPRecordalStatus,

    -- ** CloudFrontOriginAccessIdentityConfig
    CloudFrontOriginAccessIdentityConfig (..),
    mkCloudFrontOriginAccessIdentityConfig,
    cfoaicCallerReference,
    cfoaicComment,

    -- ** Origin
    Origin (..),
    mkOrigin,
    oId,
    oDomainName,
    oConnectionAttempts,
    oConnectionTimeout,
    oCustomHeaders,
    oCustomOriginConfig,
    oOriginPath,
    oOriginShield,
    oS3OriginConfig,

    -- ** FieldLevelEncryptionProfileSummary
    FieldLevelEncryptionProfileSummary (..),
    mkFieldLevelEncryptionProfileSummary,
    flepsId,
    flepsLastModifiedTime,
    flepsName,
    flepsEncryptionEntities,
    flepsComment,

    -- ** ViewerProtocolPolicy
    ViewerProtocolPolicy (..),

    -- ** StreamingDistributionList
    StreamingDistributionList (..),
    mkStreamingDistributionList,
    sdlMarker,
    sdlMaxItems,
    sdlIsTruncated,
    sdlQuantity,
    sdlItems,
    sdlNextMarker,

    -- ** KeyGroupSummary
    KeyGroupSummary (..),
    mkKeyGroupSummary,
    kgsKeyGroup,

    -- ** StreamingDistributionConfig
    StreamingDistributionConfig (..),
    mkStreamingDistributionConfig,
    sdcCallerReference,
    sdcS3Origin,
    sdcComment,
    sdcTrustedSigners,
    sdcEnabled,
    sdcAliases,
    sdcLogging,
    sdcPriceClass,

    -- ** Signer
    Signer (..),
    mkSigner,
    sAwsAccountNumber,
    sKeyPairIds,

    -- ** CookiePreference
    CookiePreference (..),
    mkCookiePreference,
    cpForward,
    cpWhitelistedNames,

    -- ** QueryArgProfileConfig
    QueryArgProfileConfig (..),
    mkQueryArgProfileConfig,
    qapcForwardWhenQueryArgProfileIsUnknown,
    qapcQueryArgProfiles,

    -- ** CustomHeaders
    CustomHeaders (..),
    mkCustomHeaders,
    chQuantity,
    chItems,

    -- ** CachePolicyHeaderBehavior
    CachePolicyHeaderBehavior (..),

    -- ** LambdaFunctionAssociations
    LambdaFunctionAssociations (..),
    mkLambdaFunctionAssociations,
    lfaQuantity,
    lfaItems,

    -- ** QueryStringCacheKeys
    QueryStringCacheKeys (..),
    mkQueryStringCacheKeys,
    qsckQuantity,
    qsckItems,

    -- ** CachePolicyQueryStringBehavior
    CachePolicyQueryStringBehavior (..),

    -- ** OriginRequestPolicyList
    OriginRequestPolicyList (..),
    mkOriginRequestPolicyList,
    orplMaxItems,
    orplQuantity,
    orplItems,
    orplNextMarker,

    -- ** FieldLevelEncryptionSummary
    FieldLevelEncryptionSummary (..),
    mkFieldLevelEncryptionSummary,
    flesId,
    flesLastModifiedTime,
    flesComment,
    flesContentTypeProfileConfig,
    flesQueryArgProfileConfig,

    -- ** OriginRequestPolicyConfig
    OriginRequestPolicyConfig (..),
    mkOriginRequestPolicyConfig,
    orpcName,
    orpcHeadersConfig,
    orpcCookiesConfig,
    orpcQueryStringsConfig,
    orpcComment,

    -- ** MonitoringSubscription
    MonitoringSubscription (..),
    mkMonitoringSubscription,
    msRealtimeMetricsSubscriptionConfig,

    -- ** ActiveTrustedKeyGroups
    ActiveTrustedKeyGroups (..),
    mkActiveTrustedKeyGroups,
    atkgEnabled,
    atkgQuantity,
    atkgItems,

    -- ** OriginProtocolPolicy
    OriginProtocolPolicy (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** ContentTypeProfile
    ContentTypeProfile (..),
    mkContentTypeProfile,
    ctpFormat,
    ctpContentType,
    ctpProfileId,

    -- ** Distribution
    Distribution (..),
    mkDistribution,
    dId,
    dARN,
    dStatus,
    dLastModifiedTime,
    dInProgressInvalidationBatches,
    dDomainName,
    dDistributionConfig,
    dActiveTrustedKeyGroups,
    dActiveTrustedSigners,
    dAliasICPRecordals,

    -- ** HttpVersion
    HttpVersion (..),

    -- ** FieldLevelEncryptionProfileList
    FieldLevelEncryptionProfileList (..),
    mkFieldLevelEncryptionProfileList,
    fleplMaxItems,
    fleplQuantity,
    fleplItems,
    fleplNextMarker,

    -- ** SslProtocol
    SslProtocol (..),

    -- ** KeyGroupConfig
    KeyGroupConfig (..),
    mkKeyGroupConfig,
    kgcName,
    kgcItems,
    kgcComment,

    -- ** RealtimeLogConfigs
    RealtimeLogConfigs (..),
    mkRealtimeLogConfigs,
    rlcMaxItems,
    rlcIsTruncated,
    rlcMarker,
    rlcItems,
    rlcNextMarker,

    -- ** FieldLevelEncryptionProfileConfig
    FieldLevelEncryptionProfileConfig (..),
    mkFieldLevelEncryptionProfileConfig,
    flepcName,
    flepcCallerReference,
    flepcEncryptionEntities,
    flepcComment,

    -- ** CloudFrontOriginAccessIdentitySummary
    CloudFrontOriginAccessIdentitySummary (..),
    mkCloudFrontOriginAccessIdentitySummary,
    cfoaisId,
    cfoaisS3CanonicalUserId,
    cfoaisComment,

    -- ** KeyGroupList
    KeyGroupList (..),
    mkKeyGroupList,
    kglMaxItems,
    kglQuantity,
    kglItems,
    kglNextMarker,

    -- ** StreamingDistributionSummary
    StreamingDistributionSummary (..),
    mkStreamingDistributionSummary,
    sdsId,
    sdsARN,
    sdsStatus,
    sdsLastModifiedTime,
    sdsDomainName,
    sdsS3Origin,
    sdsAliases,
    sdsTrustedSigners,
    sdsComment,
    sdsPriceClass,
    sdsEnabled,

    -- ** OriginRequestPolicy
    OriginRequestPolicy (..),
    mkOriginRequestPolicy,
    orpId,
    orpLastModifiedTime,
    orpOriginRequestPolicyConfig,

    -- ** CustomOriginConfig
    CustomOriginConfig (..),
    mkCustomOriginConfig,
    cocHTTPPort,
    cocHTTPSPort,
    cocOriginProtocolPolicy,
    cocOriginKeepaliveTimeout,
    cocOriginReadTimeout,
    cocOriginSslProtocols,

    -- ** String
    String (..),

    -- ** QueryArgProfile
    QueryArgProfile (..),
    mkQueryArgProfile,
    qapQueryArg,
    qapProfileId,

    -- ** FieldLevelEncryptionProfile
    FieldLevelEncryptionProfile (..),
    mkFieldLevelEncryptionProfile,
    flepId,
    flepLastModifiedTime,
    flepFieldLevelEncryptionProfileConfig,

    -- ** OriginRequestPolicyCookiesConfig
    OriginRequestPolicyCookiesConfig (..),
    mkOriginRequestPolicyCookiesConfig,
    orpccCookieBehavior,
    orpccCookies,

    -- ** OriginGroups
    OriginGroups (..),
    mkOriginGroups,
    ogQuantity,
    ogItems,

    -- ** Aliases
    Aliases (..),
    mkAliases,
    aQuantity,
    aItems,

    -- ** InvalidationBatch
    InvalidationBatch (..),
    mkInvalidationBatch,
    ibPaths,
    ibCallerReference,

    -- ** CertificateSource
    CertificateSource (..),

    -- ** InvalidationSummary
    InvalidationSummary (..),
    mkInvalidationSummary,
    isId,
    isCreateTime,
    isStatus,

    -- ** LambdaFunctionARN
    LambdaFunctionARN (..),

    -- ** DistributionConfig
    DistributionConfig (..),
    mkDistributionConfig,
    dcCallerReference,
    dcOrigins,
    dcDefaultCacheBehavior,
    dcComment,
    dcEnabled,
    dcAliases,
    dcCacheBehaviors,
    dcCustomErrorResponses,
    dcDefaultRootObject,
    dcHttpVersion,
    dcIsIPV6Enabled,
    dcLogging,
    dcOriginGroups,
    dcPriceClass,
    dcRestrictions,
    dcViewerCertificate,
    dcWebACLId,

    -- ** CachePolicySummary
    CachePolicySummary (..),
    mkCachePolicySummary,
    cpsType,
    cpsCachePolicy,

    -- ** CacheBehavior
    CacheBehavior (..),
    mkCacheBehavior,
    cbPathPattern,
    cbTargetOriginId,
    cbViewerProtocolPolicy,
    cbAllowedMethods,
    cbCachePolicyId,
    cbCompress,
    cbDefaultTTL,
    cbFieldLevelEncryptionId,
    cbForwardedValues,
    cbLambdaFunctionAssociations,
    cbMaxTTL,
    cbMinTTL,
    cbOriginRequestPolicyId,
    cbRealtimeLogConfigArn,
    cbSmoothStreaming,
    cbTrustedKeyGroups,
    cbTrustedSigners,

    -- ** KeyGroup
    KeyGroup (..),
    mkKeyGroup,
    kgId,
    kgLastModifiedTime,
    kgKeyGroupConfig,

    -- ** ContentTypeProfileConfig
    ContentTypeProfileConfig (..),
    mkContentTypeProfileConfig,
    ctpcForwardWhenContentTypeIsUnknown,
    ctpcContentTypeProfiles,

    -- ** ParametersInCacheKeyAndForwardedToOrigin
    ParametersInCacheKeyAndForwardedToOrigin (..),
    mkParametersInCacheKeyAndForwardedToOrigin,
    pickaftoEnableAcceptEncodingGzip,
    pickaftoHeadersConfig,
    pickaftoCookiesConfig,
    pickaftoQueryStringsConfig,
    pickaftoEnableAcceptEncodingBrotli,

    -- ** DistributionList
    DistributionList (..),
    mkDistributionList,
    dlMarker,
    dlMaxItems,
    dlIsTruncated,
    dlQuantity,
    dlItems,
    dlNextMarker,

    -- ** Format
    Format (..),

    -- ** KeyPairIds
    KeyPairIds (..),
    mkKeyPairIds,
    kpiQuantity,
    kpiItems,

    -- ** PriceClass
    PriceClass (..),

    -- ** OriginShieldRegion
    OriginShieldRegion (..),

    -- ** OriginGroup
    OriginGroup (..),
    mkOriginGroup,
    ogId,
    ogFailoverCriteria,
    ogMembers,

    -- ** CustomErrorResponses
    CustomErrorResponses (..),
    mkCustomErrorResponses,
    cerQuantity,
    cerItems,

    -- ** OriginRequestPolicyHeadersConfig
    OriginRequestPolicyHeadersConfig (..),
    mkOriginRequestPolicyHeadersConfig,
    orphcHeaderBehavior,
    orphcHeaders,

    -- ** ICPRecordalStatus
    ICPRecordalStatus (..),

    -- ** TagKeys
    TagKeys (..),
    mkTagKeys,
    tkItems,

    -- ** PublicKey
    PublicKey (..),
    mkPublicKey,
    pkId,
    pkCreatedTime,
    pkPublicKeyConfig,

    -- ** S3OriginConfig
    S3OriginConfig (..),
    mkS3OriginConfig,
    socOriginAccessIdentity,

    -- ** OriginRequestPolicyQueryStringBehavior
    OriginRequestPolicyQueryStringBehavior (..),

    -- ** KinesisStreamConfig
    KinesisStreamConfig (..),
    mkKinesisStreamConfig,
    kscRoleARN,
    kscStreamARN,

    -- ** TrustedKeyGroups
    TrustedKeyGroups (..),
    mkTrustedKeyGroups,
    tkgEnabled,
    tkgQuantity,
    tkgItems,

    -- ** GeoRestriction
    GeoRestriction (..),
    mkGeoRestriction,
    grRestrictionType,
    grQuantity,
    grItems,

    -- ** RealtimeLogConfig
    RealtimeLogConfig (..),
    mkRealtimeLogConfig,
    rlcARN,
    rlcName,
    rlcSamplingRate,
    rlcEndPoints,
    rlcFields,

    -- ** S3Origin
    S3Origin (..),
    mkS3Origin,
    soDomainName,
    soOriginAccessIdentity,

    -- ** PublicKeyList
    PublicKeyList (..),
    mkPublicKeyList,
    pklMaxItems,
    pklQuantity,
    pklItems,
    pklNextMarker,

    -- ** Headers
    Headers (..),
    mkHeaders,
    hQuantity,
    hItems,

    -- ** CachePolicyCookieBehavior
    CachePolicyCookieBehavior (..),

    -- ** PublicKeyConfig
    PublicKeyConfig (..),
    mkPublicKeyConfig,
    pkcCallerReference,
    pkcName,
    pkcEncodedKey,
    pkcComment,

    -- ** LambdaFunctionAssociation
    LambdaFunctionAssociation (..),
    mkLambdaFunctionAssociation,
    lfaLambdaFunctionARN,
    lfaEventType,
    lfaIncludeBody,

    -- ** EventType
    EventType (..),

    -- ** CachedMethods
    CachedMethods (..),
    mkCachedMethods,
    cmQuantity,
    cmItems,

    -- ** CachePolicyType
    CachePolicyType (..),

    -- ** ViewerCertificate
    ViewerCertificate (..),
    mkViewerCertificate,
    vcACMCertificateArn,
    vcCertificate,
    vcCertificateSource,
    vcCloudFrontDefaultCertificate,
    vcIAMCertificateId,
    vcMinimumProtocolVersion,
    vcSSLSupportMethod,

    -- ** RealtimeMetricsSubscriptionStatus
    RealtimeMetricsSubscriptionStatus (..),

    -- ** ResourceARN
    ResourceARN (..),

    -- ** Restrictions
    Restrictions (..),
    mkRestrictions,
    rGeoRestriction,

    -- ** KGKeyPairIds
    KGKeyPairIds (..),
    mkKGKeyPairIds,
    kgkpiKeyGroupId,
    kgkpiKeyPairIds,

    -- ** Origins
    Origins (..),
    mkOrigins,
    oQuantity,
    oItems,

    -- ** Method
    Method (..),

    -- ** StreamingDistributionConfigWithTags
    StreamingDistributionConfigWithTags (..),
    mkStreamingDistributionConfigWithTags,
    sdcwtStreamingDistributionConfig,
    sdcwtTags,

    -- ** MinimumProtocolVersion
    MinimumProtocolVersion (..),

    -- ** ForwardedValues
    ForwardedValues (..),
    mkForwardedValues,
    fvQueryString,
    fvCookies,
    fvHeaders,
    fvQueryStringCacheKeys,

    -- ** EncryptionEntities
    EncryptionEntities (..),
    mkEncryptionEntities,
    eeQuantity,
    eeItems,

    -- ** StatusCodes
    StatusCodes (..),
    mkStatusCodes,
    scQuantity,
    scItems,

    -- ** TrustedSigners
    TrustedSigners (..),
    mkTrustedSigners,
    tsEnabled,
    tsQuantity,
    tsItems,

    -- ** PublicKeySummary
    PublicKeySummary (..),
    mkPublicKeySummary,
    pksId,
    pksName,
    pksCreatedTime,
    pksEncodedKey,
    pksComment,

    -- ** CachePolicyQueryStringsConfig
    CachePolicyQueryStringsConfig (..),
    mkCachePolicyQueryStringsConfig,
    cpqscQueryStringBehavior,
    cpqscQueryStrings,

    -- ** CachePolicyHeadersConfig
    CachePolicyHeadersConfig (..),
    mkCachePolicyHeadersConfig,
    cphcHeaderBehavior,
    cphcHeaders,

    -- ** ItemSelection
    ItemSelection (..),

    -- ** OriginRequestPolicyCookieBehavior
    OriginRequestPolicyCookieBehavior (..),

    -- ** OriginSslProtocols
    OriginSslProtocols (..),
    mkOriginSslProtocols,
    ospQuantity,
    ospItems,

    -- ** DistributionIdList
    DistributionIdList (..),
    mkDistributionIdList,
    dilMarker,
    dilMaxItems,
    dilIsTruncated,
    dilQuantity,
    dilItems,
    dilNextMarker,

    -- ** OriginRequestPolicyType
    OriginRequestPolicyType (..),

    -- ** TagKey
    TagKey (..),

    -- ** StreamingLoggingConfig
    StreamingLoggingConfig (..),
    mkStreamingLoggingConfig,
    slcEnabled,
    slcBucket,
    slcPrefix,

    -- ** OriginGroupMembers
    OriginGroupMembers (..),
    mkOriginGroupMembers,
    ogmQuantity,
    ogmItems,

    -- ** OriginCustomHeader
    OriginCustomHeader (..),
    mkOriginCustomHeader,
    ochHeaderName,
    ochHeaderValue,

    -- ** CookieNames
    CookieNames (..),
    mkCookieNames,
    cnQuantity,
    cnItems,

    -- ** CustomErrorResponse
    CustomErrorResponse (..),
    mkCustomErrorResponse,
    cerErrorCode,
    cerErrorCachingMinTTL,
    cerResponseCode,
    cerResponsePagePath,

    -- ** EndPoint
    EndPoint (..),
    mkEndPoint,
    epStreamType,
    epKinesisStreamConfig,

    -- ** QueryStringNames
    QueryStringNames (..),
    mkQueryStringNames,
    qsnQuantity,
    qsnItems,

    -- ** FieldLevelEncryption
    FieldLevelEncryption (..),
    mkFieldLevelEncryption,
    fleId,
    fleLastModifiedTime,
    fleFieldLevelEncryptionConfig,

    -- ** DistributionConfigWithTags
    DistributionConfigWithTags (..),
    mkDistributionConfigWithTags,
    dcwtDistributionConfig,
    dcwtTags,

    -- ** OriginRequestPolicyQueryStringsConfig
    OriginRequestPolicyQueryStringsConfig (..),
    mkOriginRequestPolicyQueryStringsConfig,
    orpqscQueryStringBehavior,
    orpqscQueryStrings,

    -- ** OriginGroupMember
    OriginGroupMember (..),
    mkOriginGroupMember,
    ogmOriginId,

    -- ** OriginShield
    OriginShield (..),
    mkOriginShield,
    osEnabled,
    osOriginShieldRegion,

    -- ** CacheBehaviors
    CacheBehaviors (..),
    mkCacheBehaviors,
    cbQuantity,
    cbItems,

    -- ** DefaultCacheBehavior
    DefaultCacheBehavior (..),
    mkDefaultCacheBehavior,
    dcbTargetOriginId,
    dcbViewerProtocolPolicy,
    dcbAllowedMethods,
    dcbCachePolicyId,
    dcbCompress,
    dcbDefaultTTL,
    dcbFieldLevelEncryptionId,
    dcbForwardedValues,
    dcbLambdaFunctionAssociations,
    dcbMaxTTL,
    dcbMinTTL,
    dcbOriginRequestPolicyId,
    dcbRealtimeLogConfigArn,
    dcbSmoothStreaming,
    dcbTrustedKeyGroups,
    dcbTrustedSigners,

    -- ** InvalidationList
    InvalidationList (..),
    mkInvalidationList,
    ilMarker,
    ilMaxItems,
    ilIsTruncated,
    ilQuantity,
    ilItems,
    ilNextMarker,

    -- ** QueryArgProfiles
    QueryArgProfiles (..),
    mkQueryArgProfiles,
    qapQuantity,
    qapItems,

    -- ** CachePolicyConfig
    CachePolicyConfig (..),
    mkCachePolicyConfig,
    cpcName,
    cpcMinTTL,
    cpcComment,
    cpcDefaultTTL,
    cpcMaxTTL,
    cpcParametersInCacheKeyAndForwardedToOrigin,

    -- ** StreamingDistribution
    StreamingDistribution (..),
    mkStreamingDistribution,
    sdId,
    sdARN,
    sdStatus,
    sdDomainName,
    sdActiveTrustedSigners,
    sdStreamingDistributionConfig,
    sdLastModifiedTime,

    -- ** CachePolicyList
    CachePolicyList (..),
    mkCachePolicyList,
    cplMaxItems,
    cplQuantity,
    cplItems,
    cplNextMarker,

    -- ** Paths
    Paths (..),
    mkPaths,
    pQuantity,
    pItems,

    -- ** CloudFrontOriginAccessIdentity
    CloudFrontOriginAccessIdentity (..),
    mkCloudFrontOriginAccessIdentity,
    cfoaiId,
    cfoaiS3CanonicalUserId,
    cfoaiCloudFrontOriginAccessIdentityConfig,

    -- ** OriginRequestPolicyHeaderBehavior
    OriginRequestPolicyHeaderBehavior (..),

    -- ** ActiveTrustedSigners
    ActiveTrustedSigners (..),
    mkActiveTrustedSigners,
    atsEnabled,
    atsQuantity,
    atsItems,

    -- ** DistributionSummary
    DistributionSummary (..),
    mkDistributionSummary,
    dsId,
    dsARN,
    dsStatus,
    dsLastModifiedTime,
    dsDomainName,
    dsAliases,
    dsOrigins,
    dsDefaultCacheBehavior,
    dsCacheBehaviors,
    dsCustomErrorResponses,
    dsComment,
    dsPriceClass,
    dsEnabled,
    dsViewerCertificate,
    dsRestrictions,
    dsWebACLId,
    dsHttpVersion,
    dsIsIPV6Enabled,
    dsAliasICPRecordals,
    dsOriginGroups,

    -- ** OriginRequestPolicySummary
    OriginRequestPolicySummary (..),
    mkOriginRequestPolicySummary,
    orpsType,
    orpsOriginRequestPolicy,

    -- ** ContentTypeProfiles
    ContentTypeProfiles (..),
    mkContentTypeProfiles,
    ctpQuantity,
    ctpItems,

    -- ** Tags
    Tags (..),
    mkTags,
    tItems,

    -- ** CachePolicyCookiesConfig
    CachePolicyCookiesConfig (..),
    mkCachePolicyCookiesConfig,
    cpccCookieBehavior,
    cpccCookies,

    -- ** GeoRestrictionType
    GeoRestrictionType (..),

    -- ** LoggingConfig
    LoggingConfig (..),
    mkLoggingConfig,
    lcEnabled,
    lcIncludeCookies,
    lcBucket,
    lcPrefix,

    -- ** FieldLevelEncryptionConfig
    FieldLevelEncryptionConfig (..),
    mkFieldLevelEncryptionConfig,
    flecCallerReference,
    flecComment,
    flecContentTypeProfileConfig,
    flecQueryArgProfileConfig,

    -- ** FieldLevelEncryptionList
    FieldLevelEncryptionList (..),
    mkFieldLevelEncryptionList,
    flelMaxItems,
    flelQuantity,
    flelItems,
    flelNextMarker,

    -- ** FieldPatterns
    FieldPatterns (..),
    mkFieldPatterns,
    fpQuantity,
    fpItems,

    -- ** RealtimeMetricsSubscriptionConfig
    RealtimeMetricsSubscriptionConfig (..),
    mkRealtimeMetricsSubscriptionConfig,
    rmscRealtimeMetricsSubscriptionStatus,

    -- ** PublicKeyId
    PublicKeyId (..),

    -- ** ProviderId
    ProviderId (..),

    -- ** Id
    Id (..),

    -- ** ETag
    ETag (..),

    -- ** Location
    Location (..),

    -- ** Marker
    Marker (..),

    -- ** NextMarker
    NextMarker (..),

    -- ** Status
    Status (..),

    -- ** CNAME
    CNAME (..),

    -- ** CallerReference
    CallerReference (..),

    -- ** Comment
    Comment (..),

    -- ** DomainName
    DomainName (..),

    -- ** OriginPath
    OriginPath (..),

    -- ** Name
    Name (..),

    -- ** IfMatch
    IfMatch (..),

    -- ** AwsAccountNumber
    AwsAccountNumber (..),

    -- ** DistributionId
    DistributionId (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** ContentType
    ContentType (..),

    -- ** ProfileId
    ProfileId (..),

    -- ** ARN
    ARN (..),

    -- ** S3CanonicalUserId
    S3CanonicalUserId (..),

    -- ** Resource
    Resource (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.CloudFront.CreateCachePolicy
import Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.CreateDistribution
import Network.AWS.CloudFront.CreateDistributionWithTags
import Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
import Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
import Network.AWS.CloudFront.CreateInvalidation
import Network.AWS.CloudFront.CreateKeyGroup
import Network.AWS.CloudFront.CreateMonitoringSubscription
import Network.AWS.CloudFront.CreateOriginRequestPolicy
import Network.AWS.CloudFront.CreatePublicKey
import Network.AWS.CloudFront.CreateRealtimeLogConfig
import Network.AWS.CloudFront.CreateStreamingDistribution
import Network.AWS.CloudFront.CreateStreamingDistributionWithTags
import Network.AWS.CloudFront.DeleteCachePolicy
import Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.DeleteDistribution
import Network.AWS.CloudFront.DeleteFieldLevelEncryptionConfig
import Network.AWS.CloudFront.DeleteFieldLevelEncryptionProfile
import Network.AWS.CloudFront.DeleteKeyGroup
import Network.AWS.CloudFront.DeleteMonitoringSubscription
import Network.AWS.CloudFront.DeleteOriginRequestPolicy
import Network.AWS.CloudFront.DeletePublicKey
import Network.AWS.CloudFront.DeleteRealtimeLogConfig
import Network.AWS.CloudFront.DeleteStreamingDistribution
import Network.AWS.CloudFront.GetCachePolicy
import Network.AWS.CloudFront.GetCachePolicyConfig
import Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.GetCloudFrontOriginAccessIdentityConfig
import Network.AWS.CloudFront.GetDistribution
import Network.AWS.CloudFront.GetDistributionConfig
import Network.AWS.CloudFront.GetFieldLevelEncryption
import Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
import Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
import Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
import Network.AWS.CloudFront.GetInvalidation
import Network.AWS.CloudFront.GetKeyGroup
import Network.AWS.CloudFront.GetKeyGroupConfig
import Network.AWS.CloudFront.GetMonitoringSubscription
import Network.AWS.CloudFront.GetOriginRequestPolicy
import Network.AWS.CloudFront.GetOriginRequestPolicyConfig
import Network.AWS.CloudFront.GetPublicKey
import Network.AWS.CloudFront.GetPublicKeyConfig
import Network.AWS.CloudFront.GetRealtimeLogConfig
import Network.AWS.CloudFront.GetStreamingDistribution
import Network.AWS.CloudFront.GetStreamingDistributionConfig
import Network.AWS.CloudFront.ListCachePolicies
import Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
import Network.AWS.CloudFront.ListDistributions
import Network.AWS.CloudFront.ListDistributionsByCachePolicyId
import Network.AWS.CloudFront.ListDistributionsByKeyGroup
import Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
import Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
import Network.AWS.CloudFront.ListDistributionsByWebACLId
import Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
import Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
import Network.AWS.CloudFront.ListInvalidations
import Network.AWS.CloudFront.ListKeyGroups
import Network.AWS.CloudFront.ListOriginRequestPolicies
import Network.AWS.CloudFront.ListPublicKeys
import Network.AWS.CloudFront.ListRealtimeLogConfigs
import Network.AWS.CloudFront.ListStreamingDistributions
import Network.AWS.CloudFront.ListTagsForResource
import Network.AWS.CloudFront.TagResource
import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.UntagResource
import Network.AWS.CloudFront.UpdateCachePolicy
import Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.UpdateDistribution
import Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig
import Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile
import Network.AWS.CloudFront.UpdateKeyGroup
import Network.AWS.CloudFront.UpdateOriginRequestPolicy
import Network.AWS.CloudFront.UpdatePublicKey
import Network.AWS.CloudFront.UpdateRealtimeLogConfig
import Network.AWS.CloudFront.UpdateStreamingDistribution
import Network.AWS.CloudFront.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudFront'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
