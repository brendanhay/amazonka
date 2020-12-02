{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types
  ( -- * Service Configuration
    cloudFront,

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
    ActiveTrustedKeyGroups,
    activeTrustedKeyGroups,
    atkgItems,
    atkgEnabled,
    atkgQuantity,

    -- * ActiveTrustedSigners
    ActiveTrustedSigners,
    activeTrustedSigners,
    atsItems,
    atsEnabled,
    atsQuantity,

    -- * AliasICPRecordal
    AliasICPRecordal,
    aliasICPRecordal,
    aicprCNAME,
    aicprICPRecordalStatus,

    -- * Aliases
    Aliases,
    aliases,
    aItems,
    aQuantity,

    -- * AllowedMethods
    AllowedMethods,
    allowedMethods,
    amCachedMethods,
    amQuantity,
    amItems,

    -- * CacheBehavior
    CacheBehavior,
    cacheBehavior,
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
    CacheBehaviors,
    cacheBehaviors,
    cbItems,
    cbQuantity,

    -- * CachePolicy
    CachePolicy,
    cachePolicy,
    cpId,
    cpLastModifiedTime,
    cpCachePolicyConfig,

    -- * CachePolicyConfig
    CachePolicyConfig,
    cachePolicyConfig,
    cpcMaxTTL,
    cpcParametersInCacheKeyAndForwardedToOrigin,
    cpcDefaultTTL,
    cpcComment,
    cpcName,
    cpcMinTTL,

    -- * CachePolicyCookiesConfig
    CachePolicyCookiesConfig,
    cachePolicyCookiesConfig,
    cpccCookies,
    cpccCookieBehavior,

    -- * CachePolicyHeadersConfig
    CachePolicyHeadersConfig,
    cachePolicyHeadersConfig,
    cphcHeaders,
    cphcHeaderBehavior,

    -- * CachePolicyList
    CachePolicyList,
    cachePolicyList,
    cplItems,
    cplNextMarker,
    cplMaxItems,
    cplQuantity,

    -- * CachePolicyQueryStringsConfig
    CachePolicyQueryStringsConfig,
    cachePolicyQueryStringsConfig,
    cpqscQueryStrings,
    cpqscQueryStringBehavior,

    -- * CachePolicySummary
    CachePolicySummary,
    cachePolicySummary,
    cpsType,
    cpsCachePolicy,

    -- * CachedMethods
    CachedMethods,
    cachedMethods,
    cmQuantity,
    cmItems,

    -- * CloudFrontOriginAccessIdentity
    CloudFrontOriginAccessIdentity,
    cloudFrontOriginAccessIdentity,
    cfoaiCloudFrontOriginAccessIdentityConfig,
    cfoaiId,
    cfoaiS3CanonicalUserId,

    -- * CloudFrontOriginAccessIdentityConfig
    CloudFrontOriginAccessIdentityConfig,
    cloudFrontOriginAccessIdentityConfig,
    cfoaicCallerReference,
    cfoaicComment,

    -- * CloudFrontOriginAccessIdentityList
    CloudFrontOriginAccessIdentityList,
    cloudFrontOriginAccessIdentityList,
    cfoailItems,
    cfoailNextMarker,
    cfoailMarker,
    cfoailMaxItems,
    cfoailIsTruncated,
    cfoailQuantity,

    -- * CloudFrontOriginAccessIdentitySummary
    CloudFrontOriginAccessIdentitySummary,
    cloudFrontOriginAccessIdentitySummary,
    cfoaisId,
    cfoaisS3CanonicalUserId,
    cfoaisComment,

    -- * ContentTypeProfile
    ContentTypeProfile,
    contentTypeProfile,
    ctpProfileId,
    ctpFormat,
    ctpContentType,

    -- * ContentTypeProfileConfig
    ContentTypeProfileConfig,
    contentTypeProfileConfig,
    ctpcContentTypeProfiles,
    ctpcForwardWhenContentTypeIsUnknown,

    -- * ContentTypeProfiles
    ContentTypeProfiles,
    contentTypeProfiles,
    ctpItems,
    ctpQuantity,

    -- * CookieNames
    CookieNames,
    cookieNames,
    cnItems,
    cnQuantity,

    -- * CookiePreference
    CookiePreference,
    cookiePreference,
    cpWhitelistedNames,
    cpForward,

    -- * CustomErrorResponse
    CustomErrorResponse,
    customErrorResponse,
    ceResponsePagePath,
    ceResponseCode,
    ceErrorCachingMinTTL,
    ceErrorCode,

    -- * CustomErrorResponses
    CustomErrorResponses,
    customErrorResponses,
    cerItems,
    cerQuantity,

    -- * CustomHeaders
    CustomHeaders,
    customHeaders,
    chItems,
    chQuantity,

    -- * CustomOriginConfig
    CustomOriginConfig,
    customOriginConfig,
    cocOriginKeepaliveTimeout,
    cocOriginReadTimeout,
    cocOriginSSLProtocols,
    cocHTTPPort,
    cocHTTPSPort,
    cocOriginProtocolPolicy,

    -- * DefaultCacheBehavior
    DefaultCacheBehavior,
    defaultCacheBehavior,
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
    Distribution,
    distribution,
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
    DistributionConfig,
    distributionConfig,
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
    DistributionConfigWithTags,
    distributionConfigWithTags,
    dcwtDistributionConfig,
    dcwtTags,

    -- * DistributionIdList
    DistributionIdList,
    distributionIdList,
    dilItems,
    dilNextMarker,
    dilMarker,
    dilMaxItems,
    dilIsTruncated,
    dilQuantity,

    -- * DistributionList
    DistributionList,
    distributionList,
    dlItems,
    dlNextMarker,
    dlMarker,
    dlMaxItems,
    dlIsTruncated,
    dlQuantity,

    -- * DistributionSummary
    DistributionSummary,
    distributionSummary,
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
    EncryptionEntities,
    encryptionEntities,
    eeItems,
    eeQuantity,

    -- * EncryptionEntity
    EncryptionEntity,
    encryptionEntity,
    eePublicKeyId,
    eeProviderId,
    eeFieldPatterns,

    -- * EndPoint
    EndPoint,
    endPoint,
    epKinesisStreamConfig,
    epStreamType,

    -- * FieldLevelEncryption
    FieldLevelEncryption,
    fieldLevelEncryption,
    fleId,
    fleLastModifiedTime,
    fleFieldLevelEncryptionConfig,

    -- * FieldLevelEncryptionConfig
    FieldLevelEncryptionConfig,
    fieldLevelEncryptionConfig,
    flecQueryArgProfileConfig,
    flecContentTypeProfileConfig,
    flecComment,
    flecCallerReference,

    -- * FieldLevelEncryptionList
    FieldLevelEncryptionList,
    fieldLevelEncryptionList,
    flelItems,
    flelNextMarker,
    flelMaxItems,
    flelQuantity,

    -- * FieldLevelEncryptionProfile
    FieldLevelEncryptionProfile,
    fieldLevelEncryptionProfile,
    flepId,
    flepLastModifiedTime,
    flepFieldLevelEncryptionProfileConfig,

    -- * FieldLevelEncryptionProfileConfig
    FieldLevelEncryptionProfileConfig,
    fieldLevelEncryptionProfileConfig,
    flepcComment,
    flepcName,
    flepcCallerReference,
    flepcEncryptionEntities,

    -- * FieldLevelEncryptionProfileList
    FieldLevelEncryptionProfileList,
    fieldLevelEncryptionProfileList,
    fleplItems,
    fleplNextMarker,
    fleplMaxItems,
    fleplQuantity,

    -- * FieldLevelEncryptionProfileSummary
    FieldLevelEncryptionProfileSummary,
    fieldLevelEncryptionProfileSummary,
    flepsComment,
    flepsId,
    flepsLastModifiedTime,
    flepsName,
    flepsEncryptionEntities,

    -- * FieldLevelEncryptionSummary
    FieldLevelEncryptionSummary,
    fieldLevelEncryptionSummary,
    flesQueryArgProfileConfig,
    flesContentTypeProfileConfig,
    flesComment,
    flesId,
    flesLastModifiedTime,

    -- * FieldPatterns
    FieldPatterns,
    fieldPatterns,
    fpItems,
    fpQuantity,

    -- * ForwardedValues
    ForwardedValues,
    forwardedValues,
    fvQueryStringCacheKeys,
    fvHeaders,
    fvQueryString,
    fvCookies,

    -- * GeoRestriction
    GeoRestriction,
    geoRestriction,
    grItems,
    grRestrictionType,
    grQuantity,

    -- * Headers
    Headers,
    headers,
    hItems,
    hQuantity,

    -- * Invalidation
    Invalidation,
    invalidation,
    iId,
    iStatus,
    iCreateTime,
    iInvalidationBatch,

    -- * InvalidationBatch
    InvalidationBatch,
    invalidationBatch,
    ibPaths,
    ibCallerReference,

    -- * InvalidationList
    InvalidationList,
    invalidationList,
    ilItems,
    ilNextMarker,
    ilMarker,
    ilMaxItems,
    ilIsTruncated,
    ilQuantity,

    -- * InvalidationSummary
    InvalidationSummary,
    invalidationSummary,
    isId,
    isCreateTime,
    isStatus,

    -- * KGKeyPairIds
    KGKeyPairIds,
    kGKeyPairIds,
    kgkpiKeyPairIds,
    kgkpiKeyGroupId,

    -- * KeyGroup
    KeyGroup,
    keyGroup,
    kgId,
    kgLastModifiedTime,
    kgKeyGroupConfig,

    -- * KeyGroupConfig
    KeyGroupConfig,
    keyGroupConfig,
    kgcComment,
    kgcName,
    kgcItems,

    -- * KeyGroupList
    KeyGroupList,
    keyGroupList,
    kglItems,
    kglNextMarker,
    kglMaxItems,
    kglQuantity,

    -- * KeyGroupSummary
    KeyGroupSummary,
    keyGroupSummary,
    kgsKeyGroup,

    -- * KeyPairIds
    KeyPairIds,
    keyPairIds,
    kpiItems,
    kpiQuantity,

    -- * KinesisStreamConfig
    KinesisStreamConfig,
    kinesisStreamConfig,
    kscRoleARN,
    kscStreamARN,

    -- * LambdaFunctionAssociation
    LambdaFunctionAssociation,
    lambdaFunctionAssociation,
    lfaIncludeBody,
    lfaLambdaFunctionARN,
    lfaEventType,

    -- * LambdaFunctionAssociations
    LambdaFunctionAssociations,
    lambdaFunctionAssociations,
    lfaItems,
    lfaQuantity,

    -- * LoggingConfig
    LoggingConfig,
    loggingConfig,
    lcEnabled,
    lcIncludeCookies,
    lcBucket,
    lcPrefix,

    -- * MonitoringSubscription
    MonitoringSubscription,
    monitoringSubscription,
    msRealtimeMetricsSubscriptionConfig,

    -- * Origin
    Origin,
    origin,
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
    OriginCustomHeader,
    originCustomHeader,
    ochHeaderName,
    ochHeaderValue,

    -- * OriginGroup
    OriginGroup,
    originGroup,
    ogId,
    ogFailoverCriteria,
    ogMembers,

    -- * OriginGroupFailoverCriteria
    OriginGroupFailoverCriteria,
    originGroupFailoverCriteria,
    ogfcStatusCodes,

    -- * OriginGroupMember
    OriginGroupMember,
    originGroupMember,
    ogmOriginId,

    -- * OriginGroupMembers
    OriginGroupMembers,
    originGroupMembers,
    ogmQuantity,
    ogmItems,

    -- * OriginGroups
    OriginGroups,
    originGroups,
    ogItems,
    ogQuantity,

    -- * OriginRequestPolicy
    OriginRequestPolicy,
    originRequestPolicy,
    orpId,
    orpLastModifiedTime,
    orpOriginRequestPolicyConfig,

    -- * OriginRequestPolicyConfig
    OriginRequestPolicyConfig,
    originRequestPolicyConfig,
    orpcComment,
    orpcName,
    orpcHeadersConfig,
    orpcCookiesConfig,
    orpcQueryStringsConfig,

    -- * OriginRequestPolicyCookiesConfig
    OriginRequestPolicyCookiesConfig,
    originRequestPolicyCookiesConfig,
    orpccCookies,
    orpccCookieBehavior,

    -- * OriginRequestPolicyHeadersConfig
    OriginRequestPolicyHeadersConfig,
    originRequestPolicyHeadersConfig,
    orphcHeaders,
    orphcHeaderBehavior,

    -- * OriginRequestPolicyList
    OriginRequestPolicyList,
    originRequestPolicyList,
    orplItems,
    orplNextMarker,
    orplMaxItems,
    orplQuantity,

    -- * OriginRequestPolicyQueryStringsConfig
    OriginRequestPolicyQueryStringsConfig,
    originRequestPolicyQueryStringsConfig,
    orpqscQueryStrings,
    orpqscQueryStringBehavior,

    -- * OriginRequestPolicySummary
    OriginRequestPolicySummary,
    originRequestPolicySummary,
    orpsType,
    orpsOriginRequestPolicy,

    -- * OriginSSLProtocols
    OriginSSLProtocols,
    originSSLProtocols,
    ospQuantity,
    ospItems,

    -- * OriginShield
    OriginShield,
    originShield,
    osOriginShieldRegion,
    osEnabled,

    -- * Origins
    Origins,
    origins,
    oQuantity,
    oItems,

    -- * ParametersInCacheKeyAndForwardedToOrigin
    ParametersInCacheKeyAndForwardedToOrigin,
    parametersInCacheKeyAndForwardedToOrigin,
    pickaftoEnableAcceptEncodingBrotli,
    pickaftoEnableAcceptEncodingGzip,
    pickaftoHeadersConfig,
    pickaftoCookiesConfig,
    pickaftoQueryStringsConfig,

    -- * Paths
    Paths,
    paths,
    pItems,
    pQuantity,

    -- * PublicKey
    PublicKey,
    publicKey,
    pkId,
    pkCreatedTime,
    pkPublicKeyConfig,

    -- * PublicKeyConfig
    PublicKeyConfig,
    publicKeyConfig,
    pkcComment,
    pkcCallerReference,
    pkcName,
    pkcEncodedKey,

    -- * PublicKeyList
    PublicKeyList,
    publicKeyList,
    pklItems,
    pklNextMarker,
    pklMaxItems,
    pklQuantity,

    -- * PublicKeySummary
    PublicKeySummary,
    publicKeySummary,
    pksComment,
    pksId,
    pksName,
    pksCreatedTime,
    pksEncodedKey,

    -- * QueryArgProfile
    QueryArgProfile,
    queryArgProfile,
    qapQueryArg,
    qapProfileId,

    -- * QueryArgProfileConfig
    QueryArgProfileConfig,
    queryArgProfileConfig,
    qapcQueryArgProfiles,
    qapcForwardWhenQueryArgProfileIsUnknown,

    -- * QueryArgProfiles
    QueryArgProfiles,
    queryArgProfiles,
    qapItems,
    qapQuantity,

    -- * QueryStringCacheKeys
    QueryStringCacheKeys,
    queryStringCacheKeys,
    qsckItems,
    qsckQuantity,

    -- * QueryStringNames
    QueryStringNames,
    queryStringNames,
    qsnItems,
    qsnQuantity,

    -- * RealtimeLogConfig
    RealtimeLogConfig,
    realtimeLogConfig,
    rlcARN,
    rlcName,
    rlcSamplingRate,
    rlcEndPoints,
    rlcFields,

    -- * RealtimeLogConfigs
    RealtimeLogConfigs,
    realtimeLogConfigs,
    rlcItems,
    rlcNextMarker,
    rlcMaxItems,
    rlcIsTruncated,
    rlcMarker,

    -- * RealtimeMetricsSubscriptionConfig
    RealtimeMetricsSubscriptionConfig,
    realtimeMetricsSubscriptionConfig,
    rmscRealtimeMetricsSubscriptionStatus,

    -- * Restrictions
    Restrictions,
    restrictions,
    rGeoRestriction,

    -- * S3Origin
    S3Origin,
    s3Origin,
    soDomainName,
    soOriginAccessIdentity,

    -- * S3OriginConfig
    S3OriginConfig,
    s3OriginConfig,
    socOriginAccessIdentity,

    -- * Signer
    Signer,
    signer,
    sAWSAccountNumber,
    sKeyPairIds,

    -- * StatusCodes
    StatusCodes,
    statusCodes,
    scQuantity,
    scItems,

    -- * StreamingDistribution
    StreamingDistribution,
    streamingDistribution,
    sdLastModifiedTime,
    sdId,
    sdARN,
    sdStatus,
    sdDomainName,
    sdActiveTrustedSigners,
    sdStreamingDistributionConfig,

    -- * StreamingDistributionConfig
    StreamingDistributionConfig,
    streamingDistributionConfig,
    sdcAliases,
    sdcPriceClass,
    sdcLogging,
    sdcCallerReference,
    sdcS3Origin,
    sdcComment,
    sdcTrustedSigners,
    sdcEnabled,

    -- * StreamingDistributionConfigWithTags
    StreamingDistributionConfigWithTags,
    streamingDistributionConfigWithTags,
    sdcwtStreamingDistributionConfig,
    sdcwtTags,

    -- * StreamingDistributionList
    StreamingDistributionList,
    streamingDistributionList,
    sdlItems,
    sdlNextMarker,
    sdlMarker,
    sdlMaxItems,
    sdlIsTruncated,
    sdlQuantity,

    -- * StreamingDistributionSummary
    StreamingDistributionSummary,
    streamingDistributionSummary,
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
    StreamingLoggingConfig,
    streamingLoggingConfig,
    slcEnabled,
    slcBucket,
    slcPrefix,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TagKeys
    TagKeys,
    tagKeys,
    tkItems,

    -- * Tags
    Tags,
    tags,
    tItems,

    -- * TrustedKeyGroups
    TrustedKeyGroups,
    trustedKeyGroups,
    tkgItems,
    tkgEnabled,
    tkgQuantity,

    -- * TrustedSigners
    TrustedSigners,
    trustedSigners,
    tsItems,
    tsEnabled,
    tsQuantity,

    -- * ViewerCertificate
    ViewerCertificate,
    viewerCertificate,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2020-05-31@ of the Amazon CloudFront SDK configuration.
cloudFront :: Service
cloudFront =
  Service
    { _svcAbbrev = "CloudFront",
      _svcSigner = v4,
      _svcPrefix = "cloudfront",
      _svcVersion = "2020-05-31",
      _svcEndpoint = defaultEndpoint cloudFront,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "CloudFront",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
