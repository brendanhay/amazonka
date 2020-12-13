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
    atkgEnabled,
    atkgQuantity,
    atkgItems,

    -- ** ActiveTrustedSigners
    ActiveTrustedSigners (..),
    mkActiveTrustedSigners,
    atsEnabled,
    atsQuantity,
    atsItems,

    -- ** AliasICPRecordal
    AliasICPRecordal (..),
    mkAliasICPRecordal,
    aicprCNAME,
    aicprICPRecordalStatus,

    -- ** Aliases
    Aliases (..),
    mkAliases,
    aQuantity,
    aItems,

    -- ** AllowedMethods
    AllowedMethods (..),
    mkAllowedMethods,
    amQuantity,
    amItems,
    amCachedMethods,

    -- ** CacheBehavior
    CacheBehavior (..),
    mkCacheBehavior,
    cbAllowedMethods,
    cbViewerProtocolPolicy,
    cbLambdaFunctionAssociations,
    cbMaxTTL,
    cbTargetOriginId,
    cbMinTTL,
    cbCompress,
    cbSmoothStreaming,
    cbTrustedKeyGroups,
    cbRealtimeLogConfigARN,
    cbDefaultTTL,
    cbForwardedValues,
    cbTrustedSigners,
    cbPathPattern,
    cbOriginRequestPolicyId,
    cbFieldLevelEncryptionId,
    cbCachePolicyId,

    -- ** CacheBehaviors
    CacheBehaviors (..),
    mkCacheBehaviors,
    cbQuantity,
    cbItems,

    -- ** CachePolicy
    CachePolicy (..),
    mkCachePolicy,
    cpLastModifiedTime,
    cpId,
    cpCachePolicyConfig,

    -- ** CachePolicyConfig
    CachePolicyConfig (..),
    mkCachePolicyConfig,
    cpcMaxTTL,
    cpcParametersInCacheKeyAndForwardedToOrigin,
    cpcMinTTL,
    cpcName,
    cpcDefaultTTL,
    cpcComment,

    -- ** CachePolicyCookiesConfig
    CachePolicyCookiesConfig (..),
    mkCachePolicyCookiesConfig,
    cpccCookies,
    cpccCookieBehavior,

    -- ** CachePolicyHeadersConfig
    CachePolicyHeadersConfig (..),
    mkCachePolicyHeadersConfig,
    cphcHeaderBehavior,
    cphcHeaders,

    -- ** CachePolicyList
    CachePolicyList (..),
    mkCachePolicyList,
    cplQuantity,
    cplItems,
    cplMaxItems,
    cplNextMarker,

    -- ** CachePolicyQueryStringsConfig
    CachePolicyQueryStringsConfig (..),
    mkCachePolicyQueryStringsConfig,
    cpqscQueryStringBehavior,
    cpqscQueryStrings,

    -- ** CachePolicySummary
    CachePolicySummary (..),
    mkCachePolicySummary,
    cpsCachePolicy,
    cpsType,

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
    cfoaicComment,
    cfoaicCallerReference,

    -- ** CloudFrontOriginAccessIdentityList
    CloudFrontOriginAccessIdentityList (..),
    mkCloudFrontOriginAccessIdentityList,
    cfoailQuantity,
    cfoailItems,
    cfoailMarker,
    cfoailMaxItems,
    cfoailNextMarker,
    cfoailIsTruncated,

    -- ** CloudFrontOriginAccessIdentitySummary
    CloudFrontOriginAccessIdentitySummary (..),
    mkCloudFrontOriginAccessIdentitySummary,
    cfoaisId,
    cfoaisComment,
    cfoaisS3CanonicalUserId,

    -- ** ContentTypeProfile
    ContentTypeProfile (..),
    mkContentTypeProfile,
    ctpFormat,
    ctpProfileId,
    ctpContentType,

    -- ** ContentTypeProfileConfig
    ContentTypeProfileConfig (..),
    mkContentTypeProfileConfig,
    ctpcForwardWhenContentTypeIsUnknown,
    ctpcContentTypeProfiles,

    -- ** ContentTypeProfiles
    ContentTypeProfiles (..),
    mkContentTypeProfiles,
    ctpQuantity,
    ctpItems,

    -- ** CookieNames
    CookieNames (..),
    mkCookieNames,
    cnQuantity,
    cnItems,

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
    cerQuantity,
    cerItems,

    -- ** CustomHeaders
    CustomHeaders (..),
    mkCustomHeaders,
    chQuantity,
    chItems,

    -- ** CustomOriginConfig
    CustomOriginConfig (..),
    mkCustomOriginConfig,
    cocOriginProtocolPolicy,
    cocOriginKeepaliveTimeout,
    cocHTTPPort,
    cocHTTPSPort,
    cocOriginReadTimeout,
    cocOriginSSLProtocols,

    -- ** DefaultCacheBehavior
    DefaultCacheBehavior (..),
    mkDefaultCacheBehavior,
    dcbAllowedMethods,
    dcbViewerProtocolPolicy,
    dcbLambdaFunctionAssociations,
    dcbMaxTTL,
    dcbTargetOriginId,
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

    -- ** Distribution
    Distribution (..),
    mkDistribution,
    dInProgressInvalidationBatches,
    dStatus,
    dActiveTrustedKeyGroups,
    dARN,
    dDistributionConfig,
    dLastModifiedTime,
    dDomainName,
    dAliasICPRecordals,
    dId,
    dActiveTrustedSigners,

    -- ** DistributionConfig
    DistributionConfig (..),
    mkDistributionConfig,
    dcHTTPVersion,
    dcEnabled,
    dcOriginGroups,
    dcAliases,
    dcDefaultRootObject,
    dcPriceClass,
    dcCustomErrorResponses,
    dcWebACLId,
    dcViewerCertificate,
    dcRestrictions,
    dcOrigins,
    dcLogging,
    dcCacheBehaviors,
    dcDefaultCacheBehavior,
    dcComment,
    dcCallerReference,
    dcIsIPV6Enabled,

    -- ** DistributionConfigWithTags
    DistributionConfigWithTags (..),
    mkDistributionConfigWithTags,
    dcwtDistributionConfig,
    dcwtTags,

    -- ** DistributionIdList
    DistributionIdList (..),
    mkDistributionIdList,
    dilQuantity,
    dilItems,
    dilMarker,
    dilMaxItems,
    dilNextMarker,
    dilIsTruncated,

    -- ** DistributionList
    DistributionList (..),
    mkDistributionList,
    dlQuantity,
    dlItems,
    dlMarker,
    dlMaxItems,
    dlNextMarker,
    dlIsTruncated,

    -- ** DistributionSummary
    DistributionSummary (..),
    mkDistributionSummary,
    dsStatus,
    dsHTTPVersion,
    dsEnabled,
    dsARN,
    dsOriginGroups,
    dsAliases,
    dsPriceClass,
    dsCustomErrorResponses,
    dsWebACLId,
    dsLastModifiedTime,
    dsViewerCertificate,
    dsDomainName,
    dsRestrictions,
    dsOrigins,
    dsAliasICPRecordals,
    dsId,
    dsCacheBehaviors,
    dsDefaultCacheBehavior,
    dsComment,
    dsIsIPV6Enabled,

    -- ** EncryptionEntities
    EncryptionEntities (..),
    mkEncryptionEntities,
    eeQuantity,
    eeItems,

    -- ** EncryptionEntity
    EncryptionEntity (..),
    mkEncryptionEntity,
    eeProviderId,
    eePublicKeyId,
    eeFieldPatterns,

    -- ** EndPoint
    EndPoint (..),
    mkEndPoint,
    epKinesisStreamConfig,
    epStreamType,

    -- ** FieldLevelEncryption
    FieldLevelEncryption (..),
    mkFieldLevelEncryption,
    fleLastModifiedTime,
    fleId,
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
    flelQuantity,
    flelItems,
    flelMaxItems,
    flelNextMarker,

    -- ** FieldLevelEncryptionProfile
    FieldLevelEncryptionProfile (..),
    mkFieldLevelEncryptionProfile,
    flepFieldLevelEncryptionProfileConfig,
    flepLastModifiedTime,
    flepId,

    -- ** FieldLevelEncryptionProfileConfig
    FieldLevelEncryptionProfileConfig (..),
    mkFieldLevelEncryptionProfileConfig,
    flepcName,
    flepcEncryptionEntities,
    flepcComment,
    flepcCallerReference,

    -- ** FieldLevelEncryptionProfileList
    FieldLevelEncryptionProfileList (..),
    mkFieldLevelEncryptionProfileList,
    fleplQuantity,
    fleplItems,
    fleplMaxItems,
    fleplNextMarker,

    -- ** FieldLevelEncryptionProfileSummary
    FieldLevelEncryptionProfileSummary (..),
    mkFieldLevelEncryptionProfileSummary,
    flepsLastModifiedTime,
    flepsName,
    flepsEncryptionEntities,
    flepsId,
    flepsComment,

    -- ** FieldLevelEncryptionSummary
    FieldLevelEncryptionSummary (..),
    mkFieldLevelEncryptionSummary,
    flesQueryArgProfileConfig,
    flesContentTypeProfileConfig,
    flesLastModifiedTime,
    flesId,
    flesComment,

    -- ** FieldPatterns
    FieldPatterns (..),
    mkFieldPatterns,
    fpQuantity,
    fpItems,

    -- ** ForwardedValues
    ForwardedValues (..),
    mkForwardedValues,
    fvQueryStringCacheKeys,
    fvHeaders,
    fvCookies,
    fvQueryString,

    -- ** GeoRestriction
    GeoRestriction (..),
    mkGeoRestriction,
    grQuantity,
    grItems,
    grRestrictionType,

    -- ** Headers
    Headers (..),
    mkHeaders,
    hQuantity,
    hItems,

    -- ** Invalidation
    Invalidation (..),
    mkInvalidation,
    iStatus,
    iInvalidationBatch,
    iId,
    iCreateTime,

    -- ** InvalidationBatch
    InvalidationBatch (..),
    mkInvalidationBatch,
    ibCallerReference,
    ibPaths,

    -- ** InvalidationList
    InvalidationList (..),
    mkInvalidationList,
    ilQuantity,
    ilItems,
    ilMarker,
    ilMaxItems,
    ilNextMarker,
    ilIsTruncated,

    -- ** InvalidationSummary
    InvalidationSummary (..),
    mkInvalidationSummary,
    isStatus,
    isId,
    isCreateTime,

    -- ** KGKeyPairIds
    KGKeyPairIds (..),
    mkKGKeyPairIds,
    kgkpiKeyPairIds,
    kgkpiKeyGroupId,

    -- ** KeyGroup
    KeyGroup (..),
    mkKeyGroup,
    kgKeyGroupConfig,
    kgLastModifiedTime,
    kgId,

    -- ** KeyGroupConfig
    KeyGroupConfig (..),
    mkKeyGroupConfig,
    kgcItems,
    kgcName,
    kgcComment,

    -- ** KeyGroupList
    KeyGroupList (..),
    mkKeyGroupList,
    kglQuantity,
    kglItems,
    kglMaxItems,
    kglNextMarker,

    -- ** KeyGroupSummary
    KeyGroupSummary (..),
    mkKeyGroupSummary,
    kgsKeyGroup,

    -- ** KeyPairIds
    KeyPairIds (..),
    mkKeyPairIds,
    kpiQuantity,
    kpiItems,

    -- ** KinesisStreamConfig
    KinesisStreamConfig (..),
    mkKinesisStreamConfig,
    kscStreamARN,
    kscRoleARN,

    -- ** LambdaFunctionAssociation
    LambdaFunctionAssociation (..),
    mkLambdaFunctionAssociation,
    lfaIncludeBody,
    lfaLambdaFunctionARN,
    lfaEventType,

    -- ** LambdaFunctionAssociations
    LambdaFunctionAssociations (..),
    mkLambdaFunctionAssociations,
    lfaQuantity,
    lfaItems,

    -- ** LoggingConfig
    LoggingConfig (..),
    mkLoggingConfig,
    lcEnabled,
    lcPrefix,
    lcBucket,
    lcIncludeCookies,

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
    oDomainName,
    oId,
    oOriginShield,

    -- ** OriginCustomHeader
    OriginCustomHeader (..),
    mkOriginCustomHeader,
    ochHeaderValue,
    ochHeaderName,

    -- ** OriginGroup
    OriginGroup (..),
    mkOriginGroup,
    ogFailoverCriteria,
    ogMembers,
    ogId,

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
    ogQuantity,
    ogItems,

    -- ** OriginRequestPolicy
    OriginRequestPolicy (..),
    mkOriginRequestPolicy,
    orpOriginRequestPolicyConfig,
    orpLastModifiedTime,
    orpId,

    -- ** OriginRequestPolicyConfig
    OriginRequestPolicyConfig (..),
    mkOriginRequestPolicyConfig,
    orpcQueryStringsConfig,
    orpcHeadersConfig,
    orpcName,
    orpcCookiesConfig,
    orpcComment,

    -- ** OriginRequestPolicyCookiesConfig
    OriginRequestPolicyCookiesConfig (..),
    mkOriginRequestPolicyCookiesConfig,
    orpccCookies,
    orpccCookieBehavior,

    -- ** OriginRequestPolicyHeadersConfig
    OriginRequestPolicyHeadersConfig (..),
    mkOriginRequestPolicyHeadersConfig,
    orphcHeaderBehavior,
    orphcHeaders,

    -- ** OriginRequestPolicyList
    OriginRequestPolicyList (..),
    mkOriginRequestPolicyList,
    orplQuantity,
    orplItems,
    orplMaxItems,
    orplNextMarker,

    -- ** OriginRequestPolicyQueryStringsConfig
    OriginRequestPolicyQueryStringsConfig (..),
    mkOriginRequestPolicyQueryStringsConfig,
    orpqscQueryStringBehavior,
    orpqscQueryStrings,

    -- ** OriginRequestPolicySummary
    OriginRequestPolicySummary (..),
    mkOriginRequestPolicySummary,
    orpsOriginRequestPolicy,
    orpsType,

    -- ** OriginSSLProtocols
    OriginSSLProtocols (..),
    mkOriginSSLProtocols,
    ospQuantity,
    ospItems,

    -- ** OriginShield
    OriginShield (..),
    mkOriginShield,
    osEnabled,
    osOriginShieldRegion,

    -- ** Origins
    Origins (..),
    mkOrigins,
    oQuantity,
    oItems,

    -- ** ParametersInCacheKeyAndForwardedToOrigin
    ParametersInCacheKeyAndForwardedToOrigin (..),
    mkParametersInCacheKeyAndForwardedToOrigin,
    pickaftoQueryStringsConfig,
    pickaftoHeadersConfig,
    pickaftoEnableAcceptEncodingBrotli,
    pickaftoCookiesConfig,
    pickaftoEnableAcceptEncodingGzip,

    -- ** Paths
    Paths (..),
    mkPaths,
    pQuantity,
    pItems,

    -- ** PublicKey
    PublicKey (..),
    mkPublicKey,
    pkCreatedTime,
    pkPublicKeyConfig,
    pkId,

    -- ** PublicKeyConfig
    PublicKeyConfig (..),
    mkPublicKeyConfig,
    pkcEncodedKey,
    pkcName,
    pkcComment,
    pkcCallerReference,

    -- ** PublicKeyList
    PublicKeyList (..),
    mkPublicKeyList,
    pklQuantity,
    pklItems,
    pklMaxItems,
    pklNextMarker,

    -- ** PublicKeySummary
    PublicKeySummary (..),
    mkPublicKeySummary,
    pksEncodedKey,
    pksCreatedTime,
    pksName,
    pksId,
    pksComment,

    -- ** QueryArgProfile
    QueryArgProfile (..),
    mkQueryArgProfile,
    qapProfileId,
    qapQueryArg,

    -- ** QueryArgProfileConfig
    QueryArgProfileConfig (..),
    mkQueryArgProfileConfig,
    qapcForwardWhenQueryArgProfileIsUnknown,
    qapcQueryArgProfiles,

    -- ** QueryArgProfiles
    QueryArgProfiles (..),
    mkQueryArgProfiles,
    qapQuantity,
    qapItems,

    -- ** QueryStringCacheKeys
    QueryStringCacheKeys (..),
    mkQueryStringCacheKeys,
    qsckQuantity,
    qsckItems,

    -- ** QueryStringNames
    QueryStringNames (..),
    mkQueryStringNames,
    qsnQuantity,
    qsnItems,

    -- ** RealtimeLogConfig
    RealtimeLogConfig (..),
    mkRealtimeLogConfig,
    rlcARN,
    rlcSamplingRate,
    rlcName,
    rlcEndPoints,
    rlcFields,

    -- ** RealtimeLogConfigs
    RealtimeLogConfigs (..),
    mkRealtimeLogConfigs,
    rlcItems,
    rlcMarker,
    rlcMaxItems,
    rlcNextMarker,
    rlcIsTruncated,

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
    sdStatus,
    sdStreamingDistributionConfig,
    sdARN,
    sdLastModifiedTime,
    sdDomainName,
    sdId,
    sdActiveTrustedSigners,

    -- ** StreamingDistributionConfig
    StreamingDistributionConfig (..),
    mkStreamingDistributionConfig,
    sdcEnabled,
    sdcAliases,
    sdcPriceClass,
    sdcS3Origin,
    sdcTrustedSigners,
    sdcLogging,
    sdcComment,
    sdcCallerReference,

    -- ** StreamingDistributionConfigWithTags
    StreamingDistributionConfigWithTags (..),
    mkStreamingDistributionConfigWithTags,
    sdcwtStreamingDistributionConfig,
    sdcwtTags,

    -- ** StreamingDistributionList
    StreamingDistributionList (..),
    mkStreamingDistributionList,
    sdlQuantity,
    sdlItems,
    sdlMarker,
    sdlMaxItems,
    sdlNextMarker,
    sdlIsTruncated,

    -- ** StreamingDistributionSummary
    StreamingDistributionSummary (..),
    mkStreamingDistributionSummary,
    sdsStatus,
    sdsEnabled,
    sdsARN,
    sdsAliases,
    sdsPriceClass,
    sdsLastModifiedTime,
    sdsS3Origin,
    sdsDomainName,
    sdsTrustedSigners,
    sdsId,
    sdsComment,

    -- ** StreamingLoggingConfig
    StreamingLoggingConfig (..),
    mkStreamingLoggingConfig,
    slcEnabled,
    slcPrefix,
    slcBucket,

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
    tkgEnabled,
    tkgQuantity,
    tkgItems,

    -- ** TrustedSigners
    TrustedSigners (..),
    mkTrustedSigners,
    tsEnabled,
    tsQuantity,
    tsItems,

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
