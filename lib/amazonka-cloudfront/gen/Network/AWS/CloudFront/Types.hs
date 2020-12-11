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
    atkgItems,
    atkgEnabled,
    atkgQuantity,

    -- * ActiveTrustedSigners
    ActiveTrustedSigners (..),
    mkActiveTrustedSigners,
    atsItems,
    atsEnabled,
    atsQuantity,

    -- * AliasICPRecordal
    AliasICPRecordal (..),
    mkAliasICPRecordal,
    aicprCNAME,
    aicprICPRecordalStatus,

    -- * Aliases
    Aliases (..),
    mkAliases,
    aItems,
    aQuantity,

    -- * AllowedMethods
    AllowedMethods (..),
    mkAllowedMethods,
    amCachedMethods,
    amQuantity,
    amItems,

    -- * CacheBehavior
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

    -- * CacheBehaviors
    CacheBehaviors (..),
    mkCacheBehaviors,
    cbItems,
    cbQuantity,

    -- * CachePolicy
    CachePolicy (..),
    mkCachePolicy,
    cpId,
    cpLastModifiedTime,
    cpCachePolicyConfig,

    -- * CachePolicyConfig
    CachePolicyConfig (..),
    mkCachePolicyConfig,
    cpcMaxTTL,
    cpcParametersInCacheKeyAndForwardedToOrigin,
    cpcDefaultTTL,
    cpcComment,
    cpcName,
    cpcMinTTL,

    -- * CachePolicyCookiesConfig
    CachePolicyCookiesConfig (..),
    mkCachePolicyCookiesConfig,
    cpccCookies,
    cpccCookieBehavior,

    -- * CachePolicyHeadersConfig
    CachePolicyHeadersConfig (..),
    mkCachePolicyHeadersConfig,
    cphcHeaders,
    cphcHeaderBehavior,

    -- * CachePolicyList
    CachePolicyList (..),
    mkCachePolicyList,
    cplItems,
    cplNextMarker,
    cplMaxItems,
    cplQuantity,

    -- * CachePolicyQueryStringsConfig
    CachePolicyQueryStringsConfig (..),
    mkCachePolicyQueryStringsConfig,
    cpqscQueryStrings,
    cpqscQueryStringBehavior,

    -- * CachePolicySummary
    CachePolicySummary (..),
    mkCachePolicySummary,
    cpsType,
    cpsCachePolicy,

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
    cfoaicCallerReference,
    cfoaicComment,

    -- * CloudFrontOriginAccessIdentityList
    CloudFrontOriginAccessIdentityList (..),
    mkCloudFrontOriginAccessIdentityList,
    cfoailItems,
    cfoailNextMarker,
    cfoailMarker,
    cfoailMaxItems,
    cfoailIsTruncated,
    cfoailQuantity,

    -- * CloudFrontOriginAccessIdentitySummary
    CloudFrontOriginAccessIdentitySummary (..),
    mkCloudFrontOriginAccessIdentitySummary,
    cfoaisId,
    cfoaisS3CanonicalUserId,
    cfoaisComment,

    -- * ContentTypeProfile
    ContentTypeProfile (..),
    mkContentTypeProfile,
    ctpProfileId,
    ctpFormat,
    ctpContentType,

    -- * ContentTypeProfileConfig
    ContentTypeProfileConfig (..),
    mkContentTypeProfileConfig,
    ctpcContentTypeProfiles,
    ctpcForwardWhenContentTypeIsUnknown,

    -- * ContentTypeProfiles
    ContentTypeProfiles (..),
    mkContentTypeProfiles,
    ctpItems,
    ctpQuantity,

    -- * CookieNames
    CookieNames (..),
    mkCookieNames,
    cnItems,
    cnQuantity,

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
    cerItems,
    cerQuantity,

    -- * CustomHeaders
    CustomHeaders (..),
    mkCustomHeaders,
    chItems,
    chQuantity,

    -- * CustomOriginConfig
    CustomOriginConfig (..),
    mkCustomOriginConfig,
    cocOriginKeepaliveTimeout,
    cocOriginReadTimeout,
    cocOriginSSLProtocols,
    cocHTTPPort,
    cocHTTPSPort,
    cocOriginProtocolPolicy,

    -- * DefaultCacheBehavior
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

    -- * Distribution
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

    -- * DistributionConfig
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

    -- * DistributionConfigWithTags
    DistributionConfigWithTags (..),
    mkDistributionConfigWithTags,
    dcwtDistributionConfig,
    dcwtTags,

    -- * DistributionIdList
    DistributionIdList (..),
    mkDistributionIdList,
    dilItems,
    dilNextMarker,
    dilMarker,
    dilMaxItems,
    dilIsTruncated,
    dilQuantity,

    -- * DistributionList
    DistributionList (..),
    mkDistributionList,
    dlItems,
    dlNextMarker,
    dlMarker,
    dlMaxItems,
    dlIsTruncated,
    dlQuantity,

    -- * DistributionSummary
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

    -- * EncryptionEntities
    EncryptionEntities (..),
    mkEncryptionEntities,
    eeItems,
    eeQuantity,

    -- * EncryptionEntity
    EncryptionEntity (..),
    mkEncryptionEntity,
    eePublicKeyId,
    eeProviderId,
    eeFieldPatterns,

    -- * EndPoint
    EndPoint (..),
    mkEndPoint,
    epKinesisStreamConfig,
    epStreamType,

    -- * FieldLevelEncryption
    FieldLevelEncryption (..),
    mkFieldLevelEncryption,
    fleId,
    fleLastModifiedTime,
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
    flelItems,
    flelNextMarker,
    flelMaxItems,
    flelQuantity,

    -- * FieldLevelEncryptionProfile
    FieldLevelEncryptionProfile (..),
    mkFieldLevelEncryptionProfile,
    flepId,
    flepLastModifiedTime,
    flepFieldLevelEncryptionProfileConfig,

    -- * FieldLevelEncryptionProfileConfig
    FieldLevelEncryptionProfileConfig (..),
    mkFieldLevelEncryptionProfileConfig,
    flepcComment,
    flepcName,
    flepcCallerReference,
    flepcEncryptionEntities,

    -- * FieldLevelEncryptionProfileList
    FieldLevelEncryptionProfileList (..),
    mkFieldLevelEncryptionProfileList,
    fleplItems,
    fleplNextMarker,
    fleplMaxItems,
    fleplQuantity,

    -- * FieldLevelEncryptionProfileSummary
    FieldLevelEncryptionProfileSummary (..),
    mkFieldLevelEncryptionProfileSummary,
    flepsComment,
    flepsId,
    flepsLastModifiedTime,
    flepsName,
    flepsEncryptionEntities,

    -- * FieldLevelEncryptionSummary
    FieldLevelEncryptionSummary (..),
    mkFieldLevelEncryptionSummary,
    flesQueryArgProfileConfig,
    flesContentTypeProfileConfig,
    flesComment,
    flesId,
    flesLastModifiedTime,

    -- * FieldPatterns
    FieldPatterns (..),
    mkFieldPatterns,
    fpItems,
    fpQuantity,

    -- * ForwardedValues
    ForwardedValues (..),
    mkForwardedValues,
    fvQueryStringCacheKeys,
    fvHeaders,
    fvQueryString,
    fvCookies,

    -- * GeoRestriction
    GeoRestriction (..),
    mkGeoRestriction,
    grItems,
    grRestrictionType,
    grQuantity,

    -- * Headers
    Headers (..),
    mkHeaders,
    hItems,
    hQuantity,

    -- * Invalidation
    Invalidation (..),
    mkInvalidation,
    iId,
    iStatus,
    iCreateTime,
    iInvalidationBatch,

    -- * InvalidationBatch
    InvalidationBatch (..),
    mkInvalidationBatch,
    ibPaths,
    ibCallerReference,

    -- * InvalidationList
    InvalidationList (..),
    mkInvalidationList,
    ilItems,
    ilNextMarker,
    ilMarker,
    ilMaxItems,
    ilIsTruncated,
    ilQuantity,

    -- * InvalidationSummary
    InvalidationSummary (..),
    mkInvalidationSummary,
    isId,
    isCreateTime,
    isStatus,

    -- * KGKeyPairIds
    KGKeyPairIds (..),
    mkKGKeyPairIds,
    kgkpiKeyPairIds,
    kgkpiKeyGroupId,

    -- * KeyGroup
    KeyGroup (..),
    mkKeyGroup,
    kgId,
    kgLastModifiedTime,
    kgKeyGroupConfig,

    -- * KeyGroupConfig
    KeyGroupConfig (..),
    mkKeyGroupConfig,
    kgcComment,
    kgcName,
    kgcItems,

    -- * KeyGroupList
    KeyGroupList (..),
    mkKeyGroupList,
    kglItems,
    kglNextMarker,
    kglMaxItems,
    kglQuantity,

    -- * KeyGroupSummary
    KeyGroupSummary (..),
    mkKeyGroupSummary,
    kgsKeyGroup,

    -- * KeyPairIds
    KeyPairIds (..),
    mkKeyPairIds,
    kpiItems,
    kpiQuantity,

    -- * KinesisStreamConfig
    KinesisStreamConfig (..),
    mkKinesisStreamConfig,
    kscRoleARN,
    kscStreamARN,

    -- * LambdaFunctionAssociation
    LambdaFunctionAssociation (..),
    mkLambdaFunctionAssociation,
    lfaIncludeBody,
    lfaLambdaFunctionARN,
    lfaEventType,

    -- * LambdaFunctionAssociations
    LambdaFunctionAssociations (..),
    mkLambdaFunctionAssociations,
    lfaItems,
    lfaQuantity,

    -- * LoggingConfig
    LoggingConfig (..),
    mkLoggingConfig,
    lcEnabled,
    lcIncludeCookies,
    lcBucket,
    lcPrefix,

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
    oOriginShield,
    oId,
    oDomainName,

    -- * OriginCustomHeader
    OriginCustomHeader (..),
    mkOriginCustomHeader,
    ochHeaderName,
    ochHeaderValue,

    -- * OriginGroup
    OriginGroup (..),
    mkOriginGroup,
    ogId,
    ogFailoverCriteria,
    ogMembers,

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
    ogItems,
    ogQuantity,

    -- * OriginRequestPolicy
    OriginRequestPolicy (..),
    mkOriginRequestPolicy,
    orpId,
    orpLastModifiedTime,
    orpOriginRequestPolicyConfig,

    -- * OriginRequestPolicyConfig
    OriginRequestPolicyConfig (..),
    mkOriginRequestPolicyConfig,
    orpcComment,
    orpcName,
    orpcHeadersConfig,
    orpcCookiesConfig,
    orpcQueryStringsConfig,

    -- * OriginRequestPolicyCookiesConfig
    OriginRequestPolicyCookiesConfig (..),
    mkOriginRequestPolicyCookiesConfig,
    orpccCookies,
    orpccCookieBehavior,

    -- * OriginRequestPolicyHeadersConfig
    OriginRequestPolicyHeadersConfig (..),
    mkOriginRequestPolicyHeadersConfig,
    orphcHeaders,
    orphcHeaderBehavior,

    -- * OriginRequestPolicyList
    OriginRequestPolicyList (..),
    mkOriginRequestPolicyList,
    orplItems,
    orplNextMarker,
    orplMaxItems,
    orplQuantity,

    -- * OriginRequestPolicyQueryStringsConfig
    OriginRequestPolicyQueryStringsConfig (..),
    mkOriginRequestPolicyQueryStringsConfig,
    orpqscQueryStrings,
    orpqscQueryStringBehavior,

    -- * OriginRequestPolicySummary
    OriginRequestPolicySummary (..),
    mkOriginRequestPolicySummary,
    orpsType,
    orpsOriginRequestPolicy,

    -- * OriginSSLProtocols
    OriginSSLProtocols (..),
    mkOriginSSLProtocols,
    ospQuantity,
    ospItems,

    -- * OriginShield
    OriginShield (..),
    mkOriginShield,
    osOriginShieldRegion,
    osEnabled,

    -- * Origins
    Origins (..),
    mkOrigins,
    oQuantity,
    oItems,

    -- * ParametersInCacheKeyAndForwardedToOrigin
    ParametersInCacheKeyAndForwardedToOrigin (..),
    mkParametersInCacheKeyAndForwardedToOrigin,
    pickaftoEnableAcceptEncodingBrotli,
    pickaftoEnableAcceptEncodingGzip,
    pickaftoHeadersConfig,
    pickaftoCookiesConfig,
    pickaftoQueryStringsConfig,

    -- * Paths
    Paths (..),
    mkPaths,
    pItems,
    pQuantity,

    -- * PublicKey
    PublicKey (..),
    mkPublicKey,
    pkId,
    pkCreatedTime,
    pkPublicKeyConfig,

    -- * PublicKeyConfig
    PublicKeyConfig (..),
    mkPublicKeyConfig,
    pkcComment,
    pkcCallerReference,
    pkcName,
    pkcEncodedKey,

    -- * PublicKeyList
    PublicKeyList (..),
    mkPublicKeyList,
    pklItems,
    pklNextMarker,
    pklMaxItems,
    pklQuantity,

    -- * PublicKeySummary
    PublicKeySummary (..),
    mkPublicKeySummary,
    pksComment,
    pksId,
    pksName,
    pksCreatedTime,
    pksEncodedKey,

    -- * QueryArgProfile
    QueryArgProfile (..),
    mkQueryArgProfile,
    qapQueryArg,
    qapProfileId,

    -- * QueryArgProfileConfig
    QueryArgProfileConfig (..),
    mkQueryArgProfileConfig,
    qapcQueryArgProfiles,
    qapcForwardWhenQueryArgProfileIsUnknown,

    -- * QueryArgProfiles
    QueryArgProfiles (..),
    mkQueryArgProfiles,
    qapItems,
    qapQuantity,

    -- * QueryStringCacheKeys
    QueryStringCacheKeys (..),
    mkQueryStringCacheKeys,
    qsckItems,
    qsckQuantity,

    -- * QueryStringNames
    QueryStringNames (..),
    mkQueryStringNames,
    qsnItems,
    qsnQuantity,

    -- * RealtimeLogConfig
    RealtimeLogConfig (..),
    mkRealtimeLogConfig,
    rlcARN,
    rlcName,
    rlcSamplingRate,
    rlcEndPoints,
    rlcFields,

    -- * RealtimeLogConfigs
    RealtimeLogConfigs (..),
    mkRealtimeLogConfigs,
    rlcItems,
    rlcNextMarker,
    rlcMaxItems,
    rlcIsTruncated,
    rlcMarker,

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
    sdLastModifiedTime,
    sdId,
    sdARN,
    sdStatus,
    sdDomainName,
    sdActiveTrustedSigners,
    sdStreamingDistributionConfig,

    -- * StreamingDistributionConfig
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

    -- * StreamingDistributionConfigWithTags
    StreamingDistributionConfigWithTags (..),
    mkStreamingDistributionConfigWithTags,
    sdcwtStreamingDistributionConfig,
    sdcwtTags,

    -- * StreamingDistributionList
    StreamingDistributionList (..),
    mkStreamingDistributionList,
    sdlItems,
    sdlNextMarker,
    sdlMarker,
    sdlMaxItems,
    sdlIsTruncated,
    sdlQuantity,

    -- * StreamingDistributionSummary
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

    -- * StreamingLoggingConfig
    StreamingLoggingConfig (..),
    mkStreamingLoggingConfig,
    slcEnabled,
    slcBucket,
    slcPrefix,

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
    tkgItems,
    tkgEnabled,
    tkgQuantity,

    -- * TrustedSigners
    TrustedSigners (..),
    mkTrustedSigners,
    tsItems,
    tsEnabled,
    tsQuantity,

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
