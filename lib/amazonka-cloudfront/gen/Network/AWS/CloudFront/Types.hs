-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types
  ( -- * Service configuration
    cloudFrontService,

    -- * Errors

    -- * CachePolicyCookieBehavior
    CachePolicyCookieBehavior (..),

    -- * CachePolicyHeaderBehavior
    CachePolicyHeaderBehavior (..),

    -- * CachePolicyQueryStringBehavior
    CachePolicyQueryStringBehavior (..),

    -- * CachePolicyType
    CachePolicyType (..),

    -- * CertificateSource
    CertificateSource (..),

    -- * EventType
    EventType (..),

    -- * Format
    Format (..),

    -- * GeoRestrictionType
    GeoRestrictionType (..),

    -- * HTTPVersion
    HTTPVersion (..),

    -- * ICPRecordalStatus
    ICPRecordalStatus (..),

    -- * ItemSelection
    ItemSelection (..),

    -- * Method
    Method (..),

    -- * MinimumProtocolVersion
    MinimumProtocolVersion (..),

    -- * OriginProtocolPolicy
    OriginProtocolPolicy (..),

    -- * OriginRequestPolicyCookieBehavior
    OriginRequestPolicyCookieBehavior (..),

    -- * OriginRequestPolicyHeaderBehavior
    OriginRequestPolicyHeaderBehavior (..),

    -- * OriginRequestPolicyQueryStringBehavior
    OriginRequestPolicyQueryStringBehavior (..),

    -- * OriginRequestPolicyType
    OriginRequestPolicyType (..),

    -- * PriceClass
    PriceClass (..),

    -- * RealtimeMetricsSubscriptionStatus
    RealtimeMetricsSubscriptionStatus (..),

    -- * SSLProtocol
    SSLProtocol (..),

    -- * SSLSupportMethod
    SSLSupportMethod (..),

    -- * ViewerProtocolPolicy
    ViewerProtocolPolicy (..),

    -- * ActiveTrustedKeyGroups
    ActiveTrustedKeyGroups (..),
    mkActiveTrustedKeyGroups,
    atkgEnabled,
    atkgQuantity,
    atkgItems,

    -- * ActiveTrustedSigners
    ActiveTrustedSigners (..),
    mkActiveTrustedSigners,
    atsEnabled,
    atsQuantity,
    atsItems,

    -- * AliasICPRecordal
    AliasICPRecordal (..),
    mkAliasICPRecordal,
    aicprCNAME,
    aicprICPRecordalStatus,

    -- * Aliases
    Aliases (..),
    mkAliases,
    aQuantity,
    aItems,

    -- * AllowedMethods
    AllowedMethods (..),
    mkAllowedMethods,
    amQuantity,
    amItems,
    amCachedMethods,

    -- * CacheBehavior
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

    -- * CacheBehaviors
    CacheBehaviors (..),
    mkCacheBehaviors,
    cbQuantity,
    cbItems,

    -- * CachePolicy
    CachePolicy (..),
    mkCachePolicy,
    cpLastModifiedTime,
    cpId,
    cpCachePolicyConfig,

    -- * CachePolicyConfig
    CachePolicyConfig (..),
    mkCachePolicyConfig,
    cpcMaxTTL,
    cpcParametersInCacheKeyAndForwardedToOrigin,
    cpcMinTTL,
    cpcName,
    cpcDefaultTTL,
    cpcComment,

    -- * CachePolicyCookiesConfig
    CachePolicyCookiesConfig (..),
    mkCachePolicyCookiesConfig,
    cpccCookies,
    cpccCookieBehavior,

    -- * CachePolicyHeadersConfig
    CachePolicyHeadersConfig (..),
    mkCachePolicyHeadersConfig,
    cphcHeaderBehavior,
    cphcHeaders,

    -- * CachePolicyList
    CachePolicyList (..),
    mkCachePolicyList,
    cplQuantity,
    cplItems,
    cplMaxItems,
    cplNextMarker,

    -- * CachePolicyQueryStringsConfig
    CachePolicyQueryStringsConfig (..),
    mkCachePolicyQueryStringsConfig,
    cpqscQueryStringBehavior,
    cpqscQueryStrings,

    -- * CachePolicySummary
    CachePolicySummary (..),
    mkCachePolicySummary,
    cpsCachePolicy,
    cpsType,

    -- * CachedMethods
    CachedMethods (..),
    mkCachedMethods,
    cmQuantity,
    cmItems,

    -- * CloudFrontOriginAccessIdentity
    CloudFrontOriginAccessIdentity (..),
    mkCloudFrontOriginAccessIdentity,
    cfoaiCloudFrontOriginAccessIdentityConfig,
    cfoaiId,
    cfoaiS3CanonicalUserId,

    -- * CloudFrontOriginAccessIdentityConfig
    CloudFrontOriginAccessIdentityConfig (..),
    mkCloudFrontOriginAccessIdentityConfig,
    cfoaicComment,
    cfoaicCallerReference,

    -- * CloudFrontOriginAccessIdentityList
    CloudFrontOriginAccessIdentityList (..),
    mkCloudFrontOriginAccessIdentityList,
    cfoailQuantity,
    cfoailItems,
    cfoailMarker,
    cfoailMaxItems,
    cfoailNextMarker,
    cfoailIsTruncated,

    -- * CloudFrontOriginAccessIdentitySummary
    CloudFrontOriginAccessIdentitySummary (..),
    mkCloudFrontOriginAccessIdentitySummary,
    cfoaisId,
    cfoaisComment,
    cfoaisS3CanonicalUserId,

    -- * ContentTypeProfile
    ContentTypeProfile (..),
    mkContentTypeProfile,
    ctpFormat,
    ctpProfileId,
    ctpContentType,

    -- * ContentTypeProfileConfig
    ContentTypeProfileConfig (..),
    mkContentTypeProfileConfig,
    ctpcForwardWhenContentTypeIsUnknown,
    ctpcContentTypeProfiles,

    -- * ContentTypeProfiles
    ContentTypeProfiles (..),
    mkContentTypeProfiles,
    ctpQuantity,
    ctpItems,

    -- * CookieNames
    CookieNames (..),
    mkCookieNames,
    cnQuantity,
    cnItems,

    -- * CookiePreference
    CookiePreference (..),
    mkCookiePreference,
    cpWhitelistedNames,
    cpForward,

    -- * CustomErrorResponse
    CustomErrorResponse (..),
    mkCustomErrorResponse,
    ceResponsePagePath,
    ceResponseCode,
    ceErrorCachingMinTTL,
    ceErrorCode,

    -- * CustomErrorResponses
    CustomErrorResponses (..),
    mkCustomErrorResponses,
    cerQuantity,
    cerItems,

    -- * CustomHeaders
    CustomHeaders (..),
    mkCustomHeaders,
    chQuantity,
    chItems,

    -- * CustomOriginConfig
    CustomOriginConfig (..),
    mkCustomOriginConfig,
    cocOriginProtocolPolicy,
    cocOriginKeepaliveTimeout,
    cocHTTPPort,
    cocHTTPSPort,
    cocOriginReadTimeout,
    cocOriginSSLProtocols,

    -- * DefaultCacheBehavior
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

    -- * Distribution
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

    -- * DistributionConfig
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

    -- * DistributionConfigWithTags
    DistributionConfigWithTags (..),
    mkDistributionConfigWithTags,
    dcwtDistributionConfig,
    dcwtTags,

    -- * DistributionIdList
    DistributionIdList (..),
    mkDistributionIdList,
    dilQuantity,
    dilItems,
    dilMarker,
    dilMaxItems,
    dilNextMarker,
    dilIsTruncated,

    -- * DistributionList
    DistributionList (..),
    mkDistributionList,
    dlQuantity,
    dlItems,
    dlMarker,
    dlMaxItems,
    dlNextMarker,
    dlIsTruncated,

    -- * DistributionSummary
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

    -- * EncryptionEntities
    EncryptionEntities (..),
    mkEncryptionEntities,
    eeQuantity,
    eeItems,

    -- * EncryptionEntity
    EncryptionEntity (..),
    mkEncryptionEntity,
    eeProviderId,
    eePublicKeyId,
    eeFieldPatterns,

    -- * EndPoint
    EndPoint (..),
    mkEndPoint,
    epKinesisStreamConfig,
    epStreamType,

    -- * FieldLevelEncryption
    FieldLevelEncryption (..),
    mkFieldLevelEncryption,
    fleLastModifiedTime,
    fleId,
    fleFieldLevelEncryptionConfig,

    -- * FieldLevelEncryptionConfig
    FieldLevelEncryptionConfig (..),
    mkFieldLevelEncryptionConfig,
    flecQueryArgProfileConfig,
    flecContentTypeProfileConfig,
    flecComment,
    flecCallerReference,

    -- * FieldLevelEncryptionList
    FieldLevelEncryptionList (..),
    mkFieldLevelEncryptionList,
    flelQuantity,
    flelItems,
    flelMaxItems,
    flelNextMarker,

    -- * FieldLevelEncryptionProfile
    FieldLevelEncryptionProfile (..),
    mkFieldLevelEncryptionProfile,
    flepFieldLevelEncryptionProfileConfig,
    flepLastModifiedTime,
    flepId,

    -- * FieldLevelEncryptionProfileConfig
    FieldLevelEncryptionProfileConfig (..),
    mkFieldLevelEncryptionProfileConfig,
    flepcName,
    flepcEncryptionEntities,
    flepcComment,
    flepcCallerReference,

    -- * FieldLevelEncryptionProfileList
    FieldLevelEncryptionProfileList (..),
    mkFieldLevelEncryptionProfileList,
    fleplQuantity,
    fleplItems,
    fleplMaxItems,
    fleplNextMarker,

    -- * FieldLevelEncryptionProfileSummary
    FieldLevelEncryptionProfileSummary (..),
    mkFieldLevelEncryptionProfileSummary,
    flepsLastModifiedTime,
    flepsName,
    flepsEncryptionEntities,
    flepsId,
    flepsComment,

    -- * FieldLevelEncryptionSummary
    FieldLevelEncryptionSummary (..),
    mkFieldLevelEncryptionSummary,
    flesQueryArgProfileConfig,
    flesContentTypeProfileConfig,
    flesLastModifiedTime,
    flesId,
    flesComment,

    -- * FieldPatterns
    FieldPatterns (..),
    mkFieldPatterns,
    fpQuantity,
    fpItems,

    -- * ForwardedValues
    ForwardedValues (..),
    mkForwardedValues,
    fvQueryStringCacheKeys,
    fvHeaders,
    fvCookies,
    fvQueryString,

    -- * GeoRestriction
    GeoRestriction (..),
    mkGeoRestriction,
    grQuantity,
    grItems,
    grRestrictionType,

    -- * Headers
    Headers (..),
    mkHeaders,
    hQuantity,
    hItems,

    -- * Invalidation
    Invalidation (..),
    mkInvalidation,
    iStatus,
    iInvalidationBatch,
    iId,
    iCreateTime,

    -- * InvalidationBatch
    InvalidationBatch (..),
    mkInvalidationBatch,
    ibCallerReference,
    ibPaths,

    -- * InvalidationList
    InvalidationList (..),
    mkInvalidationList,
    ilQuantity,
    ilItems,
    ilMarker,
    ilMaxItems,
    ilNextMarker,
    ilIsTruncated,

    -- * InvalidationSummary
    InvalidationSummary (..),
    mkInvalidationSummary,
    isStatus,
    isId,
    isCreateTime,

    -- * KGKeyPairIds
    KGKeyPairIds (..),
    mkKGKeyPairIds,
    kgkpiKeyPairIds,
    kgkpiKeyGroupId,

    -- * KeyGroup
    KeyGroup (..),
    mkKeyGroup,
    kgKeyGroupConfig,
    kgLastModifiedTime,
    kgId,

    -- * KeyGroupConfig
    KeyGroupConfig (..),
    mkKeyGroupConfig,
    kgcItems,
    kgcName,
    kgcComment,

    -- * KeyGroupList
    KeyGroupList (..),
    mkKeyGroupList,
    kglQuantity,
    kglItems,
    kglMaxItems,
    kglNextMarker,

    -- * KeyGroupSummary
    KeyGroupSummary (..),
    mkKeyGroupSummary,
    kgsKeyGroup,

    -- * KeyPairIds
    KeyPairIds (..),
    mkKeyPairIds,
    kpiQuantity,
    kpiItems,

    -- * KinesisStreamConfig
    KinesisStreamConfig (..),
    mkKinesisStreamConfig,
    kscStreamARN,
    kscRoleARN,

    -- * LambdaFunctionAssociation
    LambdaFunctionAssociation (..),
    mkLambdaFunctionAssociation,
    lfaIncludeBody,
    lfaLambdaFunctionARN,
    lfaEventType,

    -- * LambdaFunctionAssociations
    LambdaFunctionAssociations (..),
    mkLambdaFunctionAssociations,
    lfaQuantity,
    lfaItems,

    -- * LoggingConfig
    LoggingConfig (..),
    mkLoggingConfig,
    lcEnabled,
    lcPrefix,
    lcBucket,
    lcIncludeCookies,

    -- * MonitoringSubscription
    MonitoringSubscription (..),
    mkMonitoringSubscription,
    msRealtimeMetricsSubscriptionConfig,

    -- * Origin
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

    -- * OriginCustomHeader
    OriginCustomHeader (..),
    mkOriginCustomHeader,
    ochHeaderValue,
    ochHeaderName,

    -- * OriginGroup
    OriginGroup (..),
    mkOriginGroup,
    ogFailoverCriteria,
    ogMembers,
    ogId,

    -- * OriginGroupFailoverCriteria
    OriginGroupFailoverCriteria (..),
    mkOriginGroupFailoverCriteria,
    ogfcStatusCodes,

    -- * OriginGroupMember
    OriginGroupMember (..),
    mkOriginGroupMember,
    ogmOriginId,

    -- * OriginGroupMembers
    OriginGroupMembers (..),
    mkOriginGroupMembers,
    ogmQuantity,
    ogmItems,

    -- * OriginGroups
    OriginGroups (..),
    mkOriginGroups,
    ogQuantity,
    ogItems,

    -- * OriginRequestPolicy
    OriginRequestPolicy (..),
    mkOriginRequestPolicy,
    orpOriginRequestPolicyConfig,
    orpLastModifiedTime,
    orpId,

    -- * OriginRequestPolicyConfig
    OriginRequestPolicyConfig (..),
    mkOriginRequestPolicyConfig,
    orpcQueryStringsConfig,
    orpcHeadersConfig,
    orpcName,
    orpcCookiesConfig,
    orpcComment,

    -- * OriginRequestPolicyCookiesConfig
    OriginRequestPolicyCookiesConfig (..),
    mkOriginRequestPolicyCookiesConfig,
    orpccCookies,
    orpccCookieBehavior,

    -- * OriginRequestPolicyHeadersConfig
    OriginRequestPolicyHeadersConfig (..),
    mkOriginRequestPolicyHeadersConfig,
    orphcHeaderBehavior,
    orphcHeaders,

    -- * OriginRequestPolicyList
    OriginRequestPolicyList (..),
    mkOriginRequestPolicyList,
    orplQuantity,
    orplItems,
    orplMaxItems,
    orplNextMarker,

    -- * OriginRequestPolicyQueryStringsConfig
    OriginRequestPolicyQueryStringsConfig (..),
    mkOriginRequestPolicyQueryStringsConfig,
    orpqscQueryStringBehavior,
    orpqscQueryStrings,

    -- * OriginRequestPolicySummary
    OriginRequestPolicySummary (..),
    mkOriginRequestPolicySummary,
    orpsOriginRequestPolicy,
    orpsType,

    -- * OriginSSLProtocols
    OriginSSLProtocols (..),
    mkOriginSSLProtocols,
    ospQuantity,
    ospItems,

    -- * OriginShield
    OriginShield (..),
    mkOriginShield,
    osEnabled,
    osOriginShieldRegion,

    -- * Origins
    Origins (..),
    mkOrigins,
    oQuantity,
    oItems,

    -- * ParametersInCacheKeyAndForwardedToOrigin
    ParametersInCacheKeyAndForwardedToOrigin (..),
    mkParametersInCacheKeyAndForwardedToOrigin,
    pickaftoQueryStringsConfig,
    pickaftoHeadersConfig,
    pickaftoEnableAcceptEncodingBrotli,
    pickaftoCookiesConfig,
    pickaftoEnableAcceptEncodingGzip,

    -- * Paths
    Paths (..),
    mkPaths,
    pQuantity,
    pItems,

    -- * PublicKey
    PublicKey (..),
    mkPublicKey,
    pkCreatedTime,
    pkPublicKeyConfig,
    pkId,

    -- * PublicKeyConfig
    PublicKeyConfig (..),
    mkPublicKeyConfig,
    pkcEncodedKey,
    pkcName,
    pkcComment,
    pkcCallerReference,

    -- * PublicKeyList
    PublicKeyList (..),
    mkPublicKeyList,
    pklQuantity,
    pklItems,
    pklMaxItems,
    pklNextMarker,

    -- * PublicKeySummary
    PublicKeySummary (..),
    mkPublicKeySummary,
    pksEncodedKey,
    pksCreatedTime,
    pksName,
    pksId,
    pksComment,

    -- * QueryArgProfile
    QueryArgProfile (..),
    mkQueryArgProfile,
    qapProfileId,
    qapQueryArg,

    -- * QueryArgProfileConfig
    QueryArgProfileConfig (..),
    mkQueryArgProfileConfig,
    qapcForwardWhenQueryArgProfileIsUnknown,
    qapcQueryArgProfiles,

    -- * QueryArgProfiles
    QueryArgProfiles (..),
    mkQueryArgProfiles,
    qapQuantity,
    qapItems,

    -- * QueryStringCacheKeys
    QueryStringCacheKeys (..),
    mkQueryStringCacheKeys,
    qsckQuantity,
    qsckItems,

    -- * QueryStringNames
    QueryStringNames (..),
    mkQueryStringNames,
    qsnQuantity,
    qsnItems,

    -- * RealtimeLogConfig
    RealtimeLogConfig (..),
    mkRealtimeLogConfig,
    rlcARN,
    rlcSamplingRate,
    rlcName,
    rlcEndPoints,
    rlcFields,

    -- * RealtimeLogConfigs
    RealtimeLogConfigs (..),
    mkRealtimeLogConfigs,
    rlcItems,
    rlcMarker,
    rlcMaxItems,
    rlcNextMarker,
    rlcIsTruncated,

    -- * RealtimeMetricsSubscriptionConfig
    RealtimeMetricsSubscriptionConfig (..),
    mkRealtimeMetricsSubscriptionConfig,
    rmscRealtimeMetricsSubscriptionStatus,

    -- * Restrictions
    Restrictions (..),
    mkRestrictions,
    rGeoRestriction,

    -- * S3Origin
    S3Origin (..),
    mkS3Origin,
    soDomainName,
    soOriginAccessIdentity,

    -- * S3OriginConfig
    S3OriginConfig (..),
    mkS3OriginConfig,
    socOriginAccessIdentity,

    -- * Signer
    Signer (..),
    mkSigner,
    sAWSAccountNumber,
    sKeyPairIds,

    -- * StatusCodes
    StatusCodes (..),
    mkStatusCodes,
    scQuantity,
    scItems,

    -- * StreamingDistribution
    StreamingDistribution (..),
    mkStreamingDistribution,
    sdStatus,
    sdStreamingDistributionConfig,
    sdARN,
    sdLastModifiedTime,
    sdDomainName,
    sdId,
    sdActiveTrustedSigners,

    -- * StreamingDistributionConfig
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

    -- * StreamingDistributionConfigWithTags
    StreamingDistributionConfigWithTags (..),
    mkStreamingDistributionConfigWithTags,
    sdcwtStreamingDistributionConfig,
    sdcwtTags,

    -- * StreamingDistributionList
    StreamingDistributionList (..),
    mkStreamingDistributionList,
    sdlQuantity,
    sdlItems,
    sdlMarker,
    sdlMaxItems,
    sdlNextMarker,
    sdlIsTruncated,

    -- * StreamingDistributionSummary
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

    -- * StreamingLoggingConfig
    StreamingLoggingConfig (..),
    mkStreamingLoggingConfig,
    slcEnabled,
    slcPrefix,
    slcBucket,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TagKeys
    TagKeys (..),
    mkTagKeys,
    tkItems,

    -- * Tags
    Tags (..),
    mkTags,
    tItems,

    -- * TrustedKeyGroups
    TrustedKeyGroups (..),
    mkTrustedKeyGroups,
    tkgEnabled,
    tkgQuantity,
    tkgItems,

    -- * TrustedSigners
    TrustedSigners (..),
    mkTrustedSigners,
    tsEnabled,
    tsQuantity,
    tsItems,

    -- * ViewerCertificate
    ViewerCertificate (..),
    mkViewerCertificate,
    vcSSLSupportMethod,
    vcACMCertificateARN,
    vcCertificateSource,
    vcMinimumProtocolVersion,
    vcCertificate,
    vcIAMCertificateId,
    vcCloudFrontDefaultCertificate,
  )
where

import Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
import Network.AWS.CloudFront.Types.ActiveTrustedSigners
import Network.AWS.CloudFront.Types.AliasICPRecordal
import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.AllowedMethods
import Network.AWS.CloudFront.Types.CacheBehavior
import Network.AWS.CloudFront.Types.CacheBehaviors
import Network.AWS.CloudFront.Types.CachePolicy
import Network.AWS.CloudFront.Types.CachePolicyConfig
import Network.AWS.CloudFront.Types.CachePolicyCookieBehavior
import Network.AWS.CloudFront.Types.CachePolicyCookiesConfig
import Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior
import Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
import Network.AWS.CloudFront.Types.CachePolicyList
import Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
import Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
import Network.AWS.CloudFront.Types.CachePolicySummary
import Network.AWS.CloudFront.Types.CachePolicyType
import Network.AWS.CloudFront.Types.CachedMethods
import Network.AWS.CloudFront.Types.CertificateSource
import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList
import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
import Network.AWS.CloudFront.Types.ContentTypeProfile
import Network.AWS.CloudFront.Types.ContentTypeProfileConfig
import Network.AWS.CloudFront.Types.ContentTypeProfiles
import Network.AWS.CloudFront.Types.CookieNames
import Network.AWS.CloudFront.Types.CookiePreference
import Network.AWS.CloudFront.Types.CustomErrorResponse
import Network.AWS.CloudFront.Types.CustomErrorResponses
import Network.AWS.CloudFront.Types.CustomHeaders
import Network.AWS.CloudFront.Types.CustomOriginConfig
import Network.AWS.CloudFront.Types.DefaultCacheBehavior
import Network.AWS.CloudFront.Types.Distribution
import Network.AWS.CloudFront.Types.DistributionConfig
import Network.AWS.CloudFront.Types.DistributionConfigWithTags
import Network.AWS.CloudFront.Types.DistributionIdList
import Network.AWS.CloudFront.Types.DistributionList
import Network.AWS.CloudFront.Types.DistributionSummary
import Network.AWS.CloudFront.Types.EncryptionEntities
import Network.AWS.CloudFront.Types.EncryptionEntity
import Network.AWS.CloudFront.Types.EndPoint
import Network.AWS.CloudFront.Types.EventType
import Network.AWS.CloudFront.Types.FieldLevelEncryption
import Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
import Network.AWS.CloudFront.Types.FieldLevelEncryptionList
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfile
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary
import Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
import Network.AWS.CloudFront.Types.FieldPatterns
import Network.AWS.CloudFront.Types.Format
import Network.AWS.CloudFront.Types.ForwardedValues
import Network.AWS.CloudFront.Types.GeoRestriction
import Network.AWS.CloudFront.Types.GeoRestrictionType
import Network.AWS.CloudFront.Types.HTTPVersion
import Network.AWS.CloudFront.Types.Headers
import Network.AWS.CloudFront.Types.ICPRecordalStatus
import Network.AWS.CloudFront.Types.Invalidation
import Network.AWS.CloudFront.Types.InvalidationBatch
import Network.AWS.CloudFront.Types.InvalidationList
import Network.AWS.CloudFront.Types.InvalidationSummary
import Network.AWS.CloudFront.Types.ItemSelection
import Network.AWS.CloudFront.Types.KGKeyPairIds
import Network.AWS.CloudFront.Types.KeyGroup
import Network.AWS.CloudFront.Types.KeyGroupConfig
import Network.AWS.CloudFront.Types.KeyGroupList
import Network.AWS.CloudFront.Types.KeyGroupSummary
import Network.AWS.CloudFront.Types.KeyPairIds
import Network.AWS.CloudFront.Types.KinesisStreamConfig
import Network.AWS.CloudFront.Types.LambdaFunctionAssociation
import Network.AWS.CloudFront.Types.LambdaFunctionAssociations
import Network.AWS.CloudFront.Types.LoggingConfig
import Network.AWS.CloudFront.Types.Method
import Network.AWS.CloudFront.Types.MinimumProtocolVersion
import Network.AWS.CloudFront.Types.MonitoringSubscription
import Network.AWS.CloudFront.Types.Origin
import Network.AWS.CloudFront.Types.OriginCustomHeader
import Network.AWS.CloudFront.Types.OriginGroup
import Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
import Network.AWS.CloudFront.Types.OriginGroupMember
import Network.AWS.CloudFront.Types.OriginGroupMembers
import Network.AWS.CloudFront.Types.OriginGroups
import Network.AWS.CloudFront.Types.OriginProtocolPolicy
import Network.AWS.CloudFront.Types.OriginRequestPolicy
import Network.AWS.CloudFront.Types.OriginRequestPolicyConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
import Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyList
import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicySummary
import Network.AWS.CloudFront.Types.OriginRequestPolicyType
import Network.AWS.CloudFront.Types.OriginSSLProtocols
import Network.AWS.CloudFront.Types.OriginShield
import Network.AWS.CloudFront.Types.Origins
import Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
import Network.AWS.CloudFront.Types.Paths
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.PublicKey
import Network.AWS.CloudFront.Types.PublicKeyConfig
import Network.AWS.CloudFront.Types.PublicKeyList
import Network.AWS.CloudFront.Types.PublicKeySummary
import Network.AWS.CloudFront.Types.QueryArgProfile
import Network.AWS.CloudFront.Types.QueryArgProfileConfig
import Network.AWS.CloudFront.Types.QueryArgProfiles
import Network.AWS.CloudFront.Types.QueryStringCacheKeys
import Network.AWS.CloudFront.Types.QueryStringNames
import Network.AWS.CloudFront.Types.RealtimeLogConfig
import Network.AWS.CloudFront.Types.RealtimeLogConfigs
import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionStatus
import Network.AWS.CloudFront.Types.Restrictions
import Network.AWS.CloudFront.Types.S3Origin
import Network.AWS.CloudFront.Types.S3OriginConfig
import Network.AWS.CloudFront.Types.SSLProtocol
import Network.AWS.CloudFront.Types.SSLSupportMethod
import Network.AWS.CloudFront.Types.Signer
import Network.AWS.CloudFront.Types.StatusCodes
import Network.AWS.CloudFront.Types.StreamingDistribution
import Network.AWS.CloudFront.Types.StreamingDistributionConfig
import Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags
import Network.AWS.CloudFront.Types.StreamingDistributionList
import Network.AWS.CloudFront.Types.StreamingDistributionSummary
import Network.AWS.CloudFront.Types.StreamingLoggingConfig
import Network.AWS.CloudFront.Types.Tag
import Network.AWS.CloudFront.Types.TagKeys
import Network.AWS.CloudFront.Types.Tags
import Network.AWS.CloudFront.Types.TrustedKeyGroups
import Network.AWS.CloudFront.Types.TrustedSigners
import Network.AWS.CloudFront.Types.ViewerCertificate
import Network.AWS.CloudFront.Types.ViewerProtocolPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-05-31@ of the Amazon CloudFront SDK configuration.
cloudFrontService :: Lude.Service
cloudFrontService =
  Lude.Service
    { Lude._svcAbbrev = "CloudFront",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cloudfront",
      Lude._svcVersion = "2020-05-31",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudFrontService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "CloudFront",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
