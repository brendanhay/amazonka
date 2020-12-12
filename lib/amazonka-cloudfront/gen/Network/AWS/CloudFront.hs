{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
    cloudFrontService,

    -- * Errors
    -- $errors

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

    -- ** CachePolicyCookieBehavior
    CachePolicyCookieBehavior (..),

    -- ** CachePolicyHeaderBehavior
    CachePolicyHeaderBehavior (..),

    -- ** CachePolicyQueryStringBehavior
    CachePolicyQueryStringBehavior (..),

    -- ** CachePolicyType
    CachePolicyType (..),

    -- ** CertificateSource
    CertificateSource (..),

    -- ** EventType
    EventType (..),

    -- ** Format
    Format (..),

    -- ** GeoRestrictionType
    GeoRestrictionType (..),

    -- ** HTTPVersion
    HTTPVersion (..),

    -- ** ICPRecordalStatus
    ICPRecordalStatus (..),

    -- ** ItemSelection
    ItemSelection (..),

    -- ** Method
    Method (..),

    -- ** MinimumProtocolVersion
    MinimumProtocolVersion (..),

    -- ** OriginProtocolPolicy
    OriginProtocolPolicy (..),

    -- ** OriginRequestPolicyCookieBehavior
    OriginRequestPolicyCookieBehavior (..),

    -- ** OriginRequestPolicyHeaderBehavior
    OriginRequestPolicyHeaderBehavior (..),

    -- ** OriginRequestPolicyQueryStringBehavior
    OriginRequestPolicyQueryStringBehavior (..),

    -- ** OriginRequestPolicyType
    OriginRequestPolicyType (..),

    -- ** PriceClass
    PriceClass (..),

    -- ** RealtimeMetricsSubscriptionStatus
    RealtimeMetricsSubscriptionStatus (..),

    -- ** SSLProtocol
    SSLProtocol (..),

    -- ** SSLSupportMethod
    SSLSupportMethod (..),

    -- ** ViewerProtocolPolicy
    ViewerProtocolPolicy (..),

    -- ** ActiveTrustedKeyGroups
    ActiveTrustedKeyGroups (..),
    mkActiveTrustedKeyGroups,
    atkgItems,
    atkgEnabled,
    atkgQuantity,

    -- ** ActiveTrustedSigners
    ActiveTrustedSigners (..),
    mkActiveTrustedSigners,
    atsItems,
    atsEnabled,
    atsQuantity,

    -- ** AliasICPRecordal
    AliasICPRecordal (..),
    mkAliasICPRecordal,
    aicprCNAME,
    aicprICPRecordalStatus,

    -- ** Aliases
    Aliases (..),
    mkAliases,
    aItems,
    aQuantity,

    -- ** AllowedMethods
    AllowedMethods (..),
    mkAllowedMethods,
    amCachedMethods,
    amQuantity,
    amItems,

    -- ** CacheBehavior
    CacheBehavior (..),
    mkCacheBehavior,
    cbAllowedMethods,
    cbLambdaFunctionAssociations,
    cbMaxTTL,
    cbMinTTL,
    cbCompress,
    cbSmoothStreaming,
    cbTrustedKeyGroups,
    cbRealtimeLogConfigARN,
    cbDefaultTTL,
    cbForwardedValues,
    cbTrustedSigners,
    cbOriginRequestPolicyId,
    cbFieldLevelEncryptionId,
    cbCachePolicyId,
    cbPathPattern,
    cbTargetOriginId,
    cbViewerProtocolPolicy,

    -- ** CacheBehaviors
    CacheBehaviors (..),
    mkCacheBehaviors,
    cbItems,
    cbQuantity,

    -- ** CachePolicy
    CachePolicy (..),
    mkCachePolicy,
    cpId,
    cpLastModifiedTime,
    cpCachePolicyConfig,

    -- ** CachePolicyConfig
    CachePolicyConfig (..),
    mkCachePolicyConfig,
    cpcMaxTTL,
    cpcParametersInCacheKeyAndForwardedToOrigin,
    cpcDefaultTTL,
    cpcComment,
    cpcName,
    cpcMinTTL,

    -- ** CachePolicyCookiesConfig
    CachePolicyCookiesConfig (..),
    mkCachePolicyCookiesConfig,
    cpccCookies,
    cpccCookieBehavior,

    -- ** CachePolicyHeadersConfig
    CachePolicyHeadersConfig (..),
    mkCachePolicyHeadersConfig,
    cphcHeaders,
    cphcHeaderBehavior,

    -- ** CachePolicyList
    CachePolicyList (..),
    mkCachePolicyList,
    cplItems,
    cplNextMarker,
    cplMaxItems,
    cplQuantity,

    -- ** CachePolicyQueryStringsConfig
    CachePolicyQueryStringsConfig (..),
    mkCachePolicyQueryStringsConfig,
    cpqscQueryStrings,
    cpqscQueryStringBehavior,

    -- ** CachePolicySummary
    CachePolicySummary (..),
    mkCachePolicySummary,
    cpsType,
    cpsCachePolicy,

    -- ** CachedMethods
    CachedMethods (..),
    mkCachedMethods,
    cmQuantity,
    cmItems,

    -- ** CloudFrontOriginAccessIdentity
    CloudFrontOriginAccessIdentity (..),
    mkCloudFrontOriginAccessIdentity,
    cfoaiCloudFrontOriginAccessIdentityConfig,
    cfoaiId,
    cfoaiS3CanonicalUserId,

    -- ** CloudFrontOriginAccessIdentityConfig
    CloudFrontOriginAccessIdentityConfig (..),
    mkCloudFrontOriginAccessIdentityConfig,
    cfoaicCallerReference,
    cfoaicComment,

    -- ** CloudFrontOriginAccessIdentityList
    CloudFrontOriginAccessIdentityList (..),
    mkCloudFrontOriginAccessIdentityList,
    cfoailItems,
    cfoailNextMarker,
    cfoailMarker,
    cfoailMaxItems,
    cfoailIsTruncated,
    cfoailQuantity,

    -- ** CloudFrontOriginAccessIdentitySummary
    CloudFrontOriginAccessIdentitySummary (..),
    mkCloudFrontOriginAccessIdentitySummary,
    cfoaisId,
    cfoaisS3CanonicalUserId,
    cfoaisComment,

    -- ** ContentTypeProfile
    ContentTypeProfile (..),
    mkContentTypeProfile,
    ctpProfileId,
    ctpFormat,
    ctpContentType,

    -- ** ContentTypeProfileConfig
    ContentTypeProfileConfig (..),
    mkContentTypeProfileConfig,
    ctpcContentTypeProfiles,
    ctpcForwardWhenContentTypeIsUnknown,

    -- ** ContentTypeProfiles
    ContentTypeProfiles (..),
    mkContentTypeProfiles,
    ctpItems,
    ctpQuantity,

    -- ** CookieNames
    CookieNames (..),
    mkCookieNames,
    cnItems,
    cnQuantity,

    -- ** CookiePreference
    CookiePreference (..),
    mkCookiePreference,
    cpWhitelistedNames,
    cpForward,

    -- ** CustomErrorResponse
    CustomErrorResponse (..),
    mkCustomErrorResponse,
    ceResponsePagePath,
    ceResponseCode,
    ceErrorCachingMinTTL,
    ceErrorCode,

    -- ** CustomErrorResponses
    CustomErrorResponses (..),
    mkCustomErrorResponses,
    cerItems,
    cerQuantity,

    -- ** CustomHeaders
    CustomHeaders (..),
    mkCustomHeaders,
    chItems,
    chQuantity,

    -- ** CustomOriginConfig
    CustomOriginConfig (..),
    mkCustomOriginConfig,
    cocOriginKeepaliveTimeout,
    cocOriginReadTimeout,
    cocOriginSSLProtocols,
    cocHTTPPort,
    cocHTTPSPort,
    cocOriginProtocolPolicy,

    -- ** DefaultCacheBehavior
    DefaultCacheBehavior (..),
    mkDefaultCacheBehavior,
    dcbAllowedMethods,
    dcbLambdaFunctionAssociations,
    dcbMaxTTL,
    dcbMinTTL,
    dcbCompress,
    dcbSmoothStreaming,
    dcbTrustedKeyGroups,
    dcbRealtimeLogConfigARN,
    dcbDefaultTTL,
    dcbForwardedValues,
    dcbTrustedSigners,
    dcbOriginRequestPolicyId,
    dcbFieldLevelEncryptionId,
    dcbCachePolicyId,
    dcbTargetOriginId,
    dcbViewerProtocolPolicy,

    -- ** Distribution
    Distribution (..),
    mkDistribution,
    dActiveTrustedKeyGroups,
    dAliasICPRecordals,
    dActiveTrustedSigners,
    dId,
    dARN,
    dStatus,
    dLastModifiedTime,
    dInProgressInvalidationBatches,
    dDomainName,
    dDistributionConfig,

    -- ** DistributionConfig
    DistributionConfig (..),
    mkDistributionConfig,
    dcHTTPVersion,
    dcOriginGroups,
    dcAliases,
    dcDefaultRootObject,
    dcPriceClass,
    dcCustomErrorResponses,
    dcWebACLId,
    dcViewerCertificate,
    dcRestrictions,
    dcLogging,
    dcCacheBehaviors,
    dcIsIPV6Enabled,
    dcCallerReference,
    dcOrigins,
    dcDefaultCacheBehavior,
    dcComment,
    dcEnabled,

    -- ** DistributionConfigWithTags
    DistributionConfigWithTags (..),
    mkDistributionConfigWithTags,
    dcwtDistributionConfig,
    dcwtTags,

    -- ** DistributionIdList
    DistributionIdList (..),
    mkDistributionIdList,
    dilItems,
    dilNextMarker,
    dilMarker,
    dilMaxItems,
    dilIsTruncated,
    dilQuantity,

    -- ** DistributionList
    DistributionList (..),
    mkDistributionList,
    dlItems,
    dlNextMarker,
    dlMarker,
    dlMaxItems,
    dlIsTruncated,
    dlQuantity,

    -- ** DistributionSummary
    DistributionSummary (..),
    mkDistributionSummary,
    dsOriginGroups,
    dsAliasICPRecordals,
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
    dsHTTPVersion,
    dsIsIPV6Enabled,

    -- ** EncryptionEntities
    EncryptionEntities (..),
    mkEncryptionEntities,
    eeItems,
    eeQuantity,

    -- ** EncryptionEntity
    EncryptionEntity (..),
    mkEncryptionEntity,
    eePublicKeyId,
    eeProviderId,
    eeFieldPatterns,

    -- ** EndPoint
    EndPoint (..),
    mkEndPoint,
    epKinesisStreamConfig,
    epStreamType,

    -- ** FieldLevelEncryption
    FieldLevelEncryption (..),
    mkFieldLevelEncryption,
    fleId,
    fleLastModifiedTime,
    fleFieldLevelEncryptionConfig,

    -- ** FieldLevelEncryptionConfig
    FieldLevelEncryptionConfig (..),
    mkFieldLevelEncryptionConfig,
    flecQueryArgProfileConfig,
    flecContentTypeProfileConfig,
    flecComment,
    flecCallerReference,

    -- ** FieldLevelEncryptionList
    FieldLevelEncryptionList (..),
    mkFieldLevelEncryptionList,
    flelItems,
    flelNextMarker,
    flelMaxItems,
    flelQuantity,

    -- ** FieldLevelEncryptionProfile
    FieldLevelEncryptionProfile (..),
    mkFieldLevelEncryptionProfile,
    flepId,
    flepLastModifiedTime,
    flepFieldLevelEncryptionProfileConfig,

    -- ** FieldLevelEncryptionProfileConfig
    FieldLevelEncryptionProfileConfig (..),
    mkFieldLevelEncryptionProfileConfig,
    flepcComment,
    flepcName,
    flepcCallerReference,
    flepcEncryptionEntities,

    -- ** FieldLevelEncryptionProfileList
    FieldLevelEncryptionProfileList (..),
    mkFieldLevelEncryptionProfileList,
    fleplItems,
    fleplNextMarker,
    fleplMaxItems,
    fleplQuantity,

    -- ** FieldLevelEncryptionProfileSummary
    FieldLevelEncryptionProfileSummary (..),
    mkFieldLevelEncryptionProfileSummary,
    flepsComment,
    flepsId,
    flepsLastModifiedTime,
    flepsName,
    flepsEncryptionEntities,

    -- ** FieldLevelEncryptionSummary
    FieldLevelEncryptionSummary (..),
    mkFieldLevelEncryptionSummary,
    flesQueryArgProfileConfig,
    flesContentTypeProfileConfig,
    flesComment,
    flesId,
    flesLastModifiedTime,

    -- ** FieldPatterns
    FieldPatterns (..),
    mkFieldPatterns,
    fpItems,
    fpQuantity,

    -- ** ForwardedValues
    ForwardedValues (..),
    mkForwardedValues,
    fvQueryStringCacheKeys,
    fvHeaders,
    fvQueryString,
    fvCookies,

    -- ** GeoRestriction
    GeoRestriction (..),
    mkGeoRestriction,
    grItems,
    grRestrictionType,
    grQuantity,

    -- ** Headers
    Headers (..),
    mkHeaders,
    hItems,
    hQuantity,

    -- ** Invalidation
    Invalidation (..),
    mkInvalidation,
    iId,
    iStatus,
    iCreateTime,
    iInvalidationBatch,

    -- ** InvalidationBatch
    InvalidationBatch (..),
    mkInvalidationBatch,
    ibPaths,
    ibCallerReference,

    -- ** InvalidationList
    InvalidationList (..),
    mkInvalidationList,
    ilItems,
    ilNextMarker,
    ilMarker,
    ilMaxItems,
    ilIsTruncated,
    ilQuantity,

    -- ** InvalidationSummary
    InvalidationSummary (..),
    mkInvalidationSummary,
    isId,
    isCreateTime,
    isStatus,

    -- ** KGKeyPairIds
    KGKeyPairIds (..),
    mkKGKeyPairIds,
    kgkpiKeyPairIds,
    kgkpiKeyGroupId,

    -- ** KeyGroup
    KeyGroup (..),
    mkKeyGroup,
    kgId,
    kgLastModifiedTime,
    kgKeyGroupConfig,

    -- ** KeyGroupConfig
    KeyGroupConfig (..),
    mkKeyGroupConfig,
    kgcComment,
    kgcName,
    kgcItems,

    -- ** KeyGroupList
    KeyGroupList (..),
    mkKeyGroupList,
    kglItems,
    kglNextMarker,
    kglMaxItems,
    kglQuantity,

    -- ** KeyGroupSummary
    KeyGroupSummary (..),
    mkKeyGroupSummary,
    kgsKeyGroup,

    -- ** KeyPairIds
    KeyPairIds (..),
    mkKeyPairIds,
    kpiItems,
    kpiQuantity,

    -- ** KinesisStreamConfig
    KinesisStreamConfig (..),
    mkKinesisStreamConfig,
    kscRoleARN,
    kscStreamARN,

    -- ** LambdaFunctionAssociation
    LambdaFunctionAssociation (..),
    mkLambdaFunctionAssociation,
    lfaIncludeBody,
    lfaLambdaFunctionARN,
    lfaEventType,

    -- ** LambdaFunctionAssociations
    LambdaFunctionAssociations (..),
    mkLambdaFunctionAssociations,
    lfaItems,
    lfaQuantity,

    -- ** LoggingConfig
    LoggingConfig (..),
    mkLoggingConfig,
    lcEnabled,
    lcIncludeCookies,
    lcBucket,
    lcPrefix,

    -- ** MonitoringSubscription
    MonitoringSubscription (..),
    mkMonitoringSubscription,
    msRealtimeMetricsSubscriptionConfig,

    -- ** Origin
    Origin (..),
    mkOrigin,
    oCustomHeaders,
    oCustomOriginConfig,
    oConnectionTimeout,
    oConnectionAttempts,
    oS3OriginConfig,
    oOriginPath,
    oOriginShield,
    oId,
    oDomainName,

    -- ** OriginCustomHeader
    OriginCustomHeader (..),
    mkOriginCustomHeader,
    ochHeaderName,
    ochHeaderValue,

    -- ** OriginGroup
    OriginGroup (..),
    mkOriginGroup,
    ogId,
    ogFailoverCriteria,
    ogMembers,

    -- ** OriginGroupFailoverCriteria
    OriginGroupFailoverCriteria (..),
    mkOriginGroupFailoverCriteria,
    ogfcStatusCodes,

    -- ** OriginGroupMember
    OriginGroupMember (..),
    mkOriginGroupMember,
    ogmOriginId,

    -- ** OriginGroupMembers
    OriginGroupMembers (..),
    mkOriginGroupMembers,
    ogmQuantity,
    ogmItems,

    -- ** OriginGroups
    OriginGroups (..),
    mkOriginGroups,
    ogItems,
    ogQuantity,

    -- ** OriginRequestPolicy
    OriginRequestPolicy (..),
    mkOriginRequestPolicy,
    orpId,
    orpLastModifiedTime,
    orpOriginRequestPolicyConfig,

    -- ** OriginRequestPolicyConfig
    OriginRequestPolicyConfig (..),
    mkOriginRequestPolicyConfig,
    orpcComment,
    orpcName,
    orpcHeadersConfig,
    orpcCookiesConfig,
    orpcQueryStringsConfig,

    -- ** OriginRequestPolicyCookiesConfig
    OriginRequestPolicyCookiesConfig (..),
    mkOriginRequestPolicyCookiesConfig,
    orpccCookies,
    orpccCookieBehavior,

    -- ** OriginRequestPolicyHeadersConfig
    OriginRequestPolicyHeadersConfig (..),
    mkOriginRequestPolicyHeadersConfig,
    orphcHeaders,
    orphcHeaderBehavior,

    -- ** OriginRequestPolicyList
    OriginRequestPolicyList (..),
    mkOriginRequestPolicyList,
    orplItems,
    orplNextMarker,
    orplMaxItems,
    orplQuantity,

    -- ** OriginRequestPolicyQueryStringsConfig
    OriginRequestPolicyQueryStringsConfig (..),
    mkOriginRequestPolicyQueryStringsConfig,
    orpqscQueryStrings,
    orpqscQueryStringBehavior,

    -- ** OriginRequestPolicySummary
    OriginRequestPolicySummary (..),
    mkOriginRequestPolicySummary,
    orpsType,
    orpsOriginRequestPolicy,

    -- ** OriginSSLProtocols
    OriginSSLProtocols (..),
    mkOriginSSLProtocols,
    ospQuantity,
    ospItems,

    -- ** OriginShield
    OriginShield (..),
    mkOriginShield,
    osOriginShieldRegion,
    osEnabled,

    -- ** Origins
    Origins (..),
    mkOrigins,
    oQuantity,
    oItems,

    -- ** ParametersInCacheKeyAndForwardedToOrigin
    ParametersInCacheKeyAndForwardedToOrigin (..),
    mkParametersInCacheKeyAndForwardedToOrigin,
    pickaftoEnableAcceptEncodingBrotli,
    pickaftoEnableAcceptEncodingGzip,
    pickaftoHeadersConfig,
    pickaftoCookiesConfig,
    pickaftoQueryStringsConfig,

    -- ** Paths
    Paths (..),
    mkPaths,
    pItems,
    pQuantity,

    -- ** PublicKey
    PublicKey (..),
    mkPublicKey,
    pkId,
    pkCreatedTime,
    pkPublicKeyConfig,

    -- ** PublicKeyConfig
    PublicKeyConfig (..),
    mkPublicKeyConfig,
    pkcComment,
    pkcCallerReference,
    pkcName,
    pkcEncodedKey,

    -- ** PublicKeyList
    PublicKeyList (..),
    mkPublicKeyList,
    pklItems,
    pklNextMarker,
    pklMaxItems,
    pklQuantity,

    -- ** PublicKeySummary
    PublicKeySummary (..),
    mkPublicKeySummary,
    pksComment,
    pksId,
    pksName,
    pksCreatedTime,
    pksEncodedKey,

    -- ** QueryArgProfile
    QueryArgProfile (..),
    mkQueryArgProfile,
    qapQueryArg,
    qapProfileId,

    -- ** QueryArgProfileConfig
    QueryArgProfileConfig (..),
    mkQueryArgProfileConfig,
    qapcQueryArgProfiles,
    qapcForwardWhenQueryArgProfileIsUnknown,

    -- ** QueryArgProfiles
    QueryArgProfiles (..),
    mkQueryArgProfiles,
    qapItems,
    qapQuantity,

    -- ** QueryStringCacheKeys
    QueryStringCacheKeys (..),
    mkQueryStringCacheKeys,
    qsckItems,
    qsckQuantity,

    -- ** QueryStringNames
    QueryStringNames (..),
    mkQueryStringNames,
    qsnItems,
    qsnQuantity,

    -- ** RealtimeLogConfig
    RealtimeLogConfig (..),
    mkRealtimeLogConfig,
    rlcARN,
    rlcName,
    rlcSamplingRate,
    rlcEndPoints,
    rlcFields,

    -- ** RealtimeLogConfigs
    RealtimeLogConfigs (..),
    mkRealtimeLogConfigs,
    rlcItems,
    rlcNextMarker,
    rlcMaxItems,
    rlcIsTruncated,
    rlcMarker,

    -- ** RealtimeMetricsSubscriptionConfig
    RealtimeMetricsSubscriptionConfig (..),
    mkRealtimeMetricsSubscriptionConfig,
    rmscRealtimeMetricsSubscriptionStatus,

    -- ** Restrictions
    Restrictions (..),
    mkRestrictions,
    rGeoRestriction,

    -- ** S3Origin
    S3Origin (..),
    mkS3Origin,
    soDomainName,
    soOriginAccessIdentity,

    -- ** S3OriginConfig
    S3OriginConfig (..),
    mkS3OriginConfig,
    socOriginAccessIdentity,

    -- ** Signer
    Signer (..),
    mkSigner,
    sAWSAccountNumber,
    sKeyPairIds,

    -- ** StatusCodes
    StatusCodes (..),
    mkStatusCodes,
    scQuantity,
    scItems,

    -- ** StreamingDistribution
    StreamingDistribution (..),
    mkStreamingDistribution,
    sdLastModifiedTime,
    sdId,
    sdARN,
    sdStatus,
    sdDomainName,
    sdActiveTrustedSigners,
    sdStreamingDistributionConfig,

    -- ** StreamingDistributionConfig
    StreamingDistributionConfig (..),
    mkStreamingDistributionConfig,
    sdcAliases,
    sdcPriceClass,
    sdcLogging,
    sdcCallerReference,
    sdcS3Origin,
    sdcComment,
    sdcTrustedSigners,
    sdcEnabled,

    -- ** StreamingDistributionConfigWithTags
    StreamingDistributionConfigWithTags (..),
    mkStreamingDistributionConfigWithTags,
    sdcwtStreamingDistributionConfig,
    sdcwtTags,

    -- ** StreamingDistributionList
    StreamingDistributionList (..),
    mkStreamingDistributionList,
    sdlItems,
    sdlNextMarker,
    sdlMarker,
    sdlMaxItems,
    sdlIsTruncated,
    sdlQuantity,

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

    -- ** StreamingLoggingConfig
    StreamingLoggingConfig (..),
    mkStreamingLoggingConfig,
    slcEnabled,
    slcBucket,
    slcPrefix,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TagKeys
    TagKeys (..),
    mkTagKeys,
    tkItems,

    -- ** Tags
    Tags (..),
    mkTags,
    tItems,

    -- ** TrustedKeyGroups
    TrustedKeyGroups (..),
    mkTrustedKeyGroups,
    tkgItems,
    tkgEnabled,
    tkgQuantity,

    -- ** TrustedSigners
    TrustedSigners (..),
    mkTrustedSigners,
    tsItems,
    tsEnabled,
    tsQuantity,

    -- ** ViewerCertificate
    ViewerCertificate (..),
    mkViewerCertificate,
    vcSSLSupportMethod,
    vcACMCertificateARN,
    vcCertificateSource,
    vcMinimumProtocolVersion,
    vcCertificate,
    vcIAMCertificateId,
    vcCloudFrontDefaultCertificate,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
