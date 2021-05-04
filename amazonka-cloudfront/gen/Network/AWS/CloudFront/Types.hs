{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _RealtimeLogConfigAlreadyExists,
    _FieldLevelEncryptionConfigAlreadyExists,
    _IllegalDelete,
    _InvalidResponseCode,
    _IllegalUpdate,
    _InvalidIfMatchVersion,
    _DistributionAlreadyExists,
    _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior,
    _InvalidErrorCode,
    _InvalidTagging,
    _TooManyOriginCustomHeaders,
    _InvalidOriginReadTimeout,
    _MissingBody,
    _FieldLevelEncryptionProfileInUse,
    _TooManyDistributionsAssociatedToCachePolicy,
    _InvalidLocationCode,
    _KeyGroupAlreadyExists,
    _TrustedKeyGroupDoesNotExist,
    _InvalidRequiredProtocol,
    _TooManyHeadersInForwardedValues,
    _TooManyFieldLevelEncryptionEncryptionEntities,
    _ResourceInUse,
    _InvalidViewerCertificate,
    _NoSuchDistribution,
    _InvalidMinimumProtocolVersion,
    _StreamingDistributionNotDisabled,
    _OriginRequestPolicyAlreadyExists,
    _TooManyCookiesInCachePolicy,
    _CachePolicyInUse,
    _TooManyLambdaFunctionAssociations,
    _CloudFrontOriginAccessIdentityAlreadyExists,
    _InvalidRelativePath,
    _NoSuchOrigin,
    _NoSuchInvalidation,
    _PublicKeyAlreadyExists,
    _TooManyDistributionsAssociatedToOriginRequestPolicy,
    _TooManyFieldLevelEncryptionConfigs,
    _TooManyFieldLevelEncryptionFieldPatterns,
    _TooManyFieldLevelEncryptionContentTypeProfiles,
    _BatchTooLarge,
    _NoSuchRealtimeLogConfig,
    _InvalidOrigin,
    _TooManyCachePolicies,
    _TooManyPublicKeysInKeyGroup,
    _NoSuchFieldLevelEncryptionConfig,
    _TooManyCookieNamesInWhiteList,
    _RealtimeLogConfigInUse,
    _InvalidForwardCookies,
    _FieldLevelEncryptionConfigInUse,
    _TooManyTrustedSigners,
    _InvalidHeadersForS3Origin,
    _InconsistentQuantities,
    _TooManyCookiesInOriginRequestPolicy,
    _InvalidProtocolSettings,
    _TooManyQueryStringParameters,
    _CannotChangeImmutablePublicKeyFields,
    _NoSuchCloudFrontOriginAccessIdentity,
    _TooManyPublicKeys,
    _TrustedSignerDoesNotExist,
    _TooManyInvalidationsInProgress,
    _NoSuchPublicKey,
    _DistributionNotDisabled,
    _TooManyCloudFrontOriginAccessIdentities,
    _InvalidOriginAccessIdentity,
    _PreconditionFailed,
    _TooManyFieldLevelEncryptionProfiles,
    _InvalidQueryStringParameters,
    _TooManyCacheBehaviors,
    _TooManyOriginRequestPolicies,
    _NoSuchFieldLevelEncryptionProfile,
    _TooManyKeyGroups,
    _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig,
    _TooManyQueryStringsInOriginRequestPolicy,
    _TooManyDistributionsWithSingleFunctionARN,
    _InvalidGeoRestrictionParameter,
    _TooManyHeadersInOriginRequestPolicy,
    _TooManyCertificates,
    _NoSuchOriginRequestPolicy,
    _TooManyDistributionsWithLambdaAssociations,
    _InvalidDefaultRootObject,
    _OriginRequestPolicyInUse,
    _TooManyStreamingDistributionCNAMEs,
    _FieldLevelEncryptionProfileSizeExceeded,
    _NoSuchResource,
    _FieldLevelEncryptionProfileAlreadyExists,
    _TooManyDistributions,
    _InvalidTTLOrder,
    _AccessDenied,
    _QueryArgProfileEmpty,
    _TooManyQueryStringsInCachePolicy,
    _TooManyOrigins,
    _TooManyHeadersInCachePolicy,
    _StreamingDistributionAlreadyExists,
    _NoSuchCachePolicy,
    _TooManyKeyGroupsAssociatedToDistribution,
    _TooManyRealtimeLogConfigs,
    _TooManyDistributionsAssociatedToKeyGroup,
    _InvalidLambdaFunctionAssociation,
    _CachePolicyAlreadyExists,
    _TooManyFieldLevelEncryptionQueryArgProfiles,
    _PublicKeyInUse,
    _CNAMEAlreadyExists,
    _InvalidWebACLId,
    _CloudFrontOriginAccessIdentityInUse,
    _TooManyOriginGroupsPerDistribution,
    _TooManyDistributionCNAMEs,
    _NoSuchStreamingDistribution,
    _InvalidOriginKeepaliveTimeout,
    _TooManyStreamingDistributions,
    _InvalidArgument,

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

    -- * HttpVersion
    HttpVersion (..),

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

    -- * SSLSupportMethod
    SSLSupportMethod (..),

    -- * SslProtocol
    SslProtocol (..),

    -- * ViewerProtocolPolicy
    ViewerProtocolPolicy (..),

    -- * ActiveTrustedKeyGroups
    ActiveTrustedKeyGroups (..),
    newActiveTrustedKeyGroups,
    activeTrustedKeyGroups_items,
    activeTrustedKeyGroups_enabled,
    activeTrustedKeyGroups_quantity,

    -- * ActiveTrustedSigners
    ActiveTrustedSigners (..),
    newActiveTrustedSigners,
    activeTrustedSigners_items,
    activeTrustedSigners_enabled,
    activeTrustedSigners_quantity,

    -- * AliasICPRecordal
    AliasICPRecordal (..),
    newAliasICPRecordal,
    aliasICPRecordal_iCPRecordalStatus,
    aliasICPRecordal_cname,

    -- * Aliases
    Aliases (..),
    newAliases,
    aliases_items,
    aliases_quantity,

    -- * AllowedMethods
    AllowedMethods (..),
    newAllowedMethods,
    allowedMethods_cachedMethods,
    allowedMethods_quantity,
    allowedMethods_items,

    -- * CacheBehavior
    CacheBehavior (..),
    newCacheBehavior,
    cacheBehavior_lambdaFunctionAssociations,
    cacheBehavior_allowedMethods,
    cacheBehavior_cachePolicyId,
    cacheBehavior_smoothStreaming,
    cacheBehavior_fieldLevelEncryptionId,
    cacheBehavior_originRequestPolicyId,
    cacheBehavior_maxTTL,
    cacheBehavior_forwardedValues,
    cacheBehavior_defaultTTL,
    cacheBehavior_realtimeLogConfigArn,
    cacheBehavior_minTTL,
    cacheBehavior_compress,
    cacheBehavior_trustedKeyGroups,
    cacheBehavior_trustedSigners,
    cacheBehavior_pathPattern,
    cacheBehavior_targetOriginId,
    cacheBehavior_viewerProtocolPolicy,

    -- * CacheBehaviors
    CacheBehaviors (..),
    newCacheBehaviors,
    cacheBehaviors_items,
    cacheBehaviors_quantity,

    -- * CachePolicy
    CachePolicy (..),
    newCachePolicy,
    cachePolicy_id,
    cachePolicy_lastModifiedTime,
    cachePolicy_cachePolicyConfig,

    -- * CachePolicyConfig
    CachePolicyConfig (..),
    newCachePolicyConfig,
    cachePolicyConfig_comment,
    cachePolicyConfig_maxTTL,
    cachePolicyConfig_parametersInCacheKeyAndForwardedToOrigin,
    cachePolicyConfig_defaultTTL,
    cachePolicyConfig_name,
    cachePolicyConfig_minTTL,

    -- * CachePolicyCookiesConfig
    CachePolicyCookiesConfig (..),
    newCachePolicyCookiesConfig,
    cachePolicyCookiesConfig_cookies,
    cachePolicyCookiesConfig_cookieBehavior,

    -- * CachePolicyHeadersConfig
    CachePolicyHeadersConfig (..),
    newCachePolicyHeadersConfig,
    cachePolicyHeadersConfig_headers,
    cachePolicyHeadersConfig_headerBehavior,

    -- * CachePolicyList
    CachePolicyList (..),
    newCachePolicyList,
    cachePolicyList_items,
    cachePolicyList_nextMarker,
    cachePolicyList_maxItems,
    cachePolicyList_quantity,

    -- * CachePolicyQueryStringsConfig
    CachePolicyQueryStringsConfig (..),
    newCachePolicyQueryStringsConfig,
    cachePolicyQueryStringsConfig_queryStrings,
    cachePolicyQueryStringsConfig_queryStringBehavior,

    -- * CachePolicySummary
    CachePolicySummary (..),
    newCachePolicySummary,
    cachePolicySummary_type,
    cachePolicySummary_cachePolicy,

    -- * CachedMethods
    CachedMethods (..),
    newCachedMethods,
    cachedMethods_quantity,
    cachedMethods_items,

    -- * CloudFrontOriginAccessIdentity
    CloudFrontOriginAccessIdentity (..),
    newCloudFrontOriginAccessIdentity,
    cloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig,
    cloudFrontOriginAccessIdentity_id,
    cloudFrontOriginAccessIdentity_s3CanonicalUserId,

    -- * CloudFrontOriginAccessIdentityConfig
    CloudFrontOriginAccessIdentityConfig (..),
    newCloudFrontOriginAccessIdentityConfig,
    cloudFrontOriginAccessIdentityConfig_callerReference,
    cloudFrontOriginAccessIdentityConfig_comment,

    -- * CloudFrontOriginAccessIdentityList
    CloudFrontOriginAccessIdentityList (..),
    newCloudFrontOriginAccessIdentityList,
    cloudFrontOriginAccessIdentityList_items,
    cloudFrontOriginAccessIdentityList_nextMarker,
    cloudFrontOriginAccessIdentityList_marker,
    cloudFrontOriginAccessIdentityList_maxItems,
    cloudFrontOriginAccessIdentityList_isTruncated,
    cloudFrontOriginAccessIdentityList_quantity,

    -- * CloudFrontOriginAccessIdentitySummary
    CloudFrontOriginAccessIdentitySummary (..),
    newCloudFrontOriginAccessIdentitySummary,
    cloudFrontOriginAccessIdentitySummary_id,
    cloudFrontOriginAccessIdentitySummary_s3CanonicalUserId,
    cloudFrontOriginAccessIdentitySummary_comment,

    -- * ContentTypeProfile
    ContentTypeProfile (..),
    newContentTypeProfile,
    contentTypeProfile_profileId,
    contentTypeProfile_format,
    contentTypeProfile_contentType,

    -- * ContentTypeProfileConfig
    ContentTypeProfileConfig (..),
    newContentTypeProfileConfig,
    contentTypeProfileConfig_contentTypeProfiles,
    contentTypeProfileConfig_forwardWhenContentTypeIsUnknown,

    -- * ContentTypeProfiles
    ContentTypeProfiles (..),
    newContentTypeProfiles,
    contentTypeProfiles_items,
    contentTypeProfiles_quantity,

    -- * CookieNames
    CookieNames (..),
    newCookieNames,
    cookieNames_items,
    cookieNames_quantity,

    -- * CookiePreference
    CookiePreference (..),
    newCookiePreference,
    cookiePreference_whitelistedNames,
    cookiePreference_forward,

    -- * CustomErrorResponse
    CustomErrorResponse (..),
    newCustomErrorResponse,
    customErrorResponse_errorCachingMinTTL,
    customErrorResponse_responseCode,
    customErrorResponse_responsePagePath,
    customErrorResponse_errorCode,

    -- * CustomErrorResponses
    CustomErrorResponses (..),
    newCustomErrorResponses,
    customErrorResponses_items,
    customErrorResponses_quantity,

    -- * CustomHeaders
    CustomHeaders (..),
    newCustomHeaders,
    customHeaders_items,
    customHeaders_quantity,

    -- * CustomOriginConfig
    CustomOriginConfig (..),
    newCustomOriginConfig,
    customOriginConfig_originKeepaliveTimeout,
    customOriginConfig_originSslProtocols,
    customOriginConfig_originReadTimeout,
    customOriginConfig_hTTPPort,
    customOriginConfig_hTTPSPort,
    customOriginConfig_originProtocolPolicy,

    -- * DefaultCacheBehavior
    DefaultCacheBehavior (..),
    newDefaultCacheBehavior,
    defaultCacheBehavior_lambdaFunctionAssociations,
    defaultCacheBehavior_allowedMethods,
    defaultCacheBehavior_cachePolicyId,
    defaultCacheBehavior_smoothStreaming,
    defaultCacheBehavior_fieldLevelEncryptionId,
    defaultCacheBehavior_originRequestPolicyId,
    defaultCacheBehavior_maxTTL,
    defaultCacheBehavior_forwardedValues,
    defaultCacheBehavior_defaultTTL,
    defaultCacheBehavior_realtimeLogConfigArn,
    defaultCacheBehavior_minTTL,
    defaultCacheBehavior_compress,
    defaultCacheBehavior_trustedKeyGroups,
    defaultCacheBehavior_trustedSigners,
    defaultCacheBehavior_targetOriginId,
    defaultCacheBehavior_viewerProtocolPolicy,

    -- * Distribution
    Distribution (..),
    newDistribution,
    distribution_aliasICPRecordals,
    distribution_activeTrustedSigners,
    distribution_activeTrustedKeyGroups,
    distribution_id,
    distribution_arn,
    distribution_status,
    distribution_lastModifiedTime,
    distribution_inProgressInvalidationBatches,
    distribution_domainName,
    distribution_distributionConfig,

    -- * DistributionConfig
    DistributionConfig (..),
    newDistributionConfig,
    distributionConfig_viewerCertificate,
    distributionConfig_customErrorResponses,
    distributionConfig_webACLId,
    distributionConfig_priceClass,
    distributionConfig_logging,
    distributionConfig_originGroups,
    distributionConfig_restrictions,
    distributionConfig_isIPV6Enabled,
    distributionConfig_cacheBehaviors,
    distributionConfig_defaultRootObject,
    distributionConfig_aliases,
    distributionConfig_httpVersion,
    distributionConfig_callerReference,
    distributionConfig_origins,
    distributionConfig_defaultCacheBehavior,
    distributionConfig_comment,
    distributionConfig_enabled,

    -- * DistributionConfigWithTags
    DistributionConfigWithTags (..),
    newDistributionConfigWithTags,
    distributionConfigWithTags_distributionConfig,
    distributionConfigWithTags_tags,

    -- * DistributionIdList
    DistributionIdList (..),
    newDistributionIdList,
    distributionIdList_items,
    distributionIdList_nextMarker,
    distributionIdList_marker,
    distributionIdList_maxItems,
    distributionIdList_isTruncated,
    distributionIdList_quantity,

    -- * DistributionList
    DistributionList (..),
    newDistributionList,
    distributionList_items,
    distributionList_nextMarker,
    distributionList_marker,
    distributionList_maxItems,
    distributionList_isTruncated,
    distributionList_quantity,

    -- * DistributionSummary
    DistributionSummary (..),
    newDistributionSummary,
    distributionSummary_aliasICPRecordals,
    distributionSummary_originGroups,
    distributionSummary_id,
    distributionSummary_arn,
    distributionSummary_status,
    distributionSummary_lastModifiedTime,
    distributionSummary_domainName,
    distributionSummary_aliases,
    distributionSummary_origins,
    distributionSummary_defaultCacheBehavior,
    distributionSummary_cacheBehaviors,
    distributionSummary_customErrorResponses,
    distributionSummary_comment,
    distributionSummary_priceClass,
    distributionSummary_enabled,
    distributionSummary_viewerCertificate,
    distributionSummary_restrictions,
    distributionSummary_webACLId,
    distributionSummary_httpVersion,
    distributionSummary_isIPV6Enabled,

    -- * EncryptionEntities
    EncryptionEntities (..),
    newEncryptionEntities,
    encryptionEntities_items,
    encryptionEntities_quantity,

    -- * EncryptionEntity
    EncryptionEntity (..),
    newEncryptionEntity,
    encryptionEntity_publicKeyId,
    encryptionEntity_providerId,
    encryptionEntity_fieldPatterns,

    -- * EndPoint
    EndPoint (..),
    newEndPoint,
    endPoint_kinesisStreamConfig,
    endPoint_streamType,

    -- * FieldLevelEncryption
    FieldLevelEncryption (..),
    newFieldLevelEncryption,
    fieldLevelEncryption_id,
    fieldLevelEncryption_lastModifiedTime,
    fieldLevelEncryption_fieldLevelEncryptionConfig,

    -- * FieldLevelEncryptionConfig
    FieldLevelEncryptionConfig (..),
    newFieldLevelEncryptionConfig,
    fieldLevelEncryptionConfig_comment,
    fieldLevelEncryptionConfig_contentTypeProfileConfig,
    fieldLevelEncryptionConfig_queryArgProfileConfig,
    fieldLevelEncryptionConfig_callerReference,

    -- * FieldLevelEncryptionList
    FieldLevelEncryptionList (..),
    newFieldLevelEncryptionList,
    fieldLevelEncryptionList_items,
    fieldLevelEncryptionList_nextMarker,
    fieldLevelEncryptionList_maxItems,
    fieldLevelEncryptionList_quantity,

    -- * FieldLevelEncryptionProfile
    FieldLevelEncryptionProfile (..),
    newFieldLevelEncryptionProfile,
    fieldLevelEncryptionProfile_id,
    fieldLevelEncryptionProfile_lastModifiedTime,
    fieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig,

    -- * FieldLevelEncryptionProfileConfig
    FieldLevelEncryptionProfileConfig (..),
    newFieldLevelEncryptionProfileConfig,
    fieldLevelEncryptionProfileConfig_comment,
    fieldLevelEncryptionProfileConfig_name,
    fieldLevelEncryptionProfileConfig_callerReference,
    fieldLevelEncryptionProfileConfig_encryptionEntities,

    -- * FieldLevelEncryptionProfileList
    FieldLevelEncryptionProfileList (..),
    newFieldLevelEncryptionProfileList,
    fieldLevelEncryptionProfileList_items,
    fieldLevelEncryptionProfileList_nextMarker,
    fieldLevelEncryptionProfileList_maxItems,
    fieldLevelEncryptionProfileList_quantity,

    -- * FieldLevelEncryptionProfileSummary
    FieldLevelEncryptionProfileSummary (..),
    newFieldLevelEncryptionProfileSummary,
    fieldLevelEncryptionProfileSummary_comment,
    fieldLevelEncryptionProfileSummary_id,
    fieldLevelEncryptionProfileSummary_lastModifiedTime,
    fieldLevelEncryptionProfileSummary_name,
    fieldLevelEncryptionProfileSummary_encryptionEntities,

    -- * FieldLevelEncryptionSummary
    FieldLevelEncryptionSummary (..),
    newFieldLevelEncryptionSummary,
    fieldLevelEncryptionSummary_comment,
    fieldLevelEncryptionSummary_contentTypeProfileConfig,
    fieldLevelEncryptionSummary_queryArgProfileConfig,
    fieldLevelEncryptionSummary_id,
    fieldLevelEncryptionSummary_lastModifiedTime,

    -- * FieldPatterns
    FieldPatterns (..),
    newFieldPatterns,
    fieldPatterns_items,
    fieldPatterns_quantity,

    -- * ForwardedValues
    ForwardedValues (..),
    newForwardedValues,
    forwardedValues_queryStringCacheKeys,
    forwardedValues_headers,
    forwardedValues_queryString,
    forwardedValues_cookies,

    -- * GeoRestriction
    GeoRestriction (..),
    newGeoRestriction,
    geoRestriction_items,
    geoRestriction_restrictionType,
    geoRestriction_quantity,

    -- * Headers
    Headers (..),
    newHeaders,
    headers_items,
    headers_quantity,

    -- * Invalidation
    Invalidation (..),
    newInvalidation,
    invalidation_id,
    invalidation_status,
    invalidation_createTime,
    invalidation_invalidationBatch,

    -- * InvalidationBatch
    InvalidationBatch (..),
    newInvalidationBatch,
    invalidationBatch_paths,
    invalidationBatch_callerReference,

    -- * InvalidationList
    InvalidationList (..),
    newInvalidationList,
    invalidationList_items,
    invalidationList_nextMarker,
    invalidationList_marker,
    invalidationList_maxItems,
    invalidationList_isTruncated,
    invalidationList_quantity,

    -- * InvalidationSummary
    InvalidationSummary (..),
    newInvalidationSummary,
    invalidationSummary_id,
    invalidationSummary_createTime,
    invalidationSummary_status,

    -- * KGKeyPairIds
    KGKeyPairIds (..),
    newKGKeyPairIds,
    kGKeyPairIds_keyPairIds,
    kGKeyPairIds_keyGroupId,

    -- * KeyGroup
    KeyGroup (..),
    newKeyGroup,
    keyGroup_id,
    keyGroup_lastModifiedTime,
    keyGroup_keyGroupConfig,

    -- * KeyGroupConfig
    KeyGroupConfig (..),
    newKeyGroupConfig,
    keyGroupConfig_comment,
    keyGroupConfig_name,
    keyGroupConfig_items,

    -- * KeyGroupList
    KeyGroupList (..),
    newKeyGroupList,
    keyGroupList_items,
    keyGroupList_nextMarker,
    keyGroupList_maxItems,
    keyGroupList_quantity,

    -- * KeyGroupSummary
    KeyGroupSummary (..),
    newKeyGroupSummary,
    keyGroupSummary_keyGroup,

    -- * KeyPairIds
    KeyPairIds (..),
    newKeyPairIds,
    keyPairIds_items,
    keyPairIds_quantity,

    -- * KinesisStreamConfig
    KinesisStreamConfig (..),
    newKinesisStreamConfig,
    kinesisStreamConfig_roleARN,
    kinesisStreamConfig_streamARN,

    -- * LambdaFunctionAssociation
    LambdaFunctionAssociation (..),
    newLambdaFunctionAssociation,
    lambdaFunctionAssociation_includeBody,
    lambdaFunctionAssociation_lambdaFunctionARN,
    lambdaFunctionAssociation_eventType,

    -- * LambdaFunctionAssociations
    LambdaFunctionAssociations (..),
    newLambdaFunctionAssociations,
    lambdaFunctionAssociations_items,
    lambdaFunctionAssociations_quantity,

    -- * LoggingConfig
    LoggingConfig (..),
    newLoggingConfig,
    loggingConfig_enabled,
    loggingConfig_includeCookies,
    loggingConfig_bucket,
    loggingConfig_prefix,

    -- * MonitoringSubscription
    MonitoringSubscription (..),
    newMonitoringSubscription,
    monitoringSubscription_realtimeMetricsSubscriptionConfig,

    -- * Origin
    Origin (..),
    newOrigin,
    origin_originPath,
    origin_connectionAttempts,
    origin_connectionTimeout,
    origin_customHeaders,
    origin_s3OriginConfig,
    origin_originShield,
    origin_customOriginConfig,
    origin_id,
    origin_domainName,

    -- * OriginCustomHeader
    OriginCustomHeader (..),
    newOriginCustomHeader,
    originCustomHeader_headerName,
    originCustomHeader_headerValue,

    -- * OriginGroup
    OriginGroup (..),
    newOriginGroup,
    originGroup_id,
    originGroup_failoverCriteria,
    originGroup_members,

    -- * OriginGroupFailoverCriteria
    OriginGroupFailoverCriteria (..),
    newOriginGroupFailoverCriteria,
    originGroupFailoverCriteria_statusCodes,

    -- * OriginGroupMember
    OriginGroupMember (..),
    newOriginGroupMember,
    originGroupMember_originId,

    -- * OriginGroupMembers
    OriginGroupMembers (..),
    newOriginGroupMembers,
    originGroupMembers_quantity,
    originGroupMembers_items,

    -- * OriginGroups
    OriginGroups (..),
    newOriginGroups,
    originGroups_items,
    originGroups_quantity,

    -- * OriginRequestPolicy
    OriginRequestPolicy (..),
    newOriginRequestPolicy,
    originRequestPolicy_id,
    originRequestPolicy_lastModifiedTime,
    originRequestPolicy_originRequestPolicyConfig,

    -- * OriginRequestPolicyConfig
    OriginRequestPolicyConfig (..),
    newOriginRequestPolicyConfig,
    originRequestPolicyConfig_comment,
    originRequestPolicyConfig_name,
    originRequestPolicyConfig_headersConfig,
    originRequestPolicyConfig_cookiesConfig,
    originRequestPolicyConfig_queryStringsConfig,

    -- * OriginRequestPolicyCookiesConfig
    OriginRequestPolicyCookiesConfig (..),
    newOriginRequestPolicyCookiesConfig,
    originRequestPolicyCookiesConfig_cookies,
    originRequestPolicyCookiesConfig_cookieBehavior,

    -- * OriginRequestPolicyHeadersConfig
    OriginRequestPolicyHeadersConfig (..),
    newOriginRequestPolicyHeadersConfig,
    originRequestPolicyHeadersConfig_headers,
    originRequestPolicyHeadersConfig_headerBehavior,

    -- * OriginRequestPolicyList
    OriginRequestPolicyList (..),
    newOriginRequestPolicyList,
    originRequestPolicyList_items,
    originRequestPolicyList_nextMarker,
    originRequestPolicyList_maxItems,
    originRequestPolicyList_quantity,

    -- * OriginRequestPolicyQueryStringsConfig
    OriginRequestPolicyQueryStringsConfig (..),
    newOriginRequestPolicyQueryStringsConfig,
    originRequestPolicyQueryStringsConfig_queryStrings,
    originRequestPolicyQueryStringsConfig_queryStringBehavior,

    -- * OriginRequestPolicySummary
    OriginRequestPolicySummary (..),
    newOriginRequestPolicySummary,
    originRequestPolicySummary_type,
    originRequestPolicySummary_originRequestPolicy,

    -- * OriginShield
    OriginShield (..),
    newOriginShield,
    originShield_originShieldRegion,
    originShield_enabled,

    -- * OriginSslProtocols
    OriginSslProtocols (..),
    newOriginSslProtocols,
    originSslProtocols_quantity,
    originSslProtocols_items,

    -- * Origins
    Origins (..),
    newOrigins,
    origins_quantity,
    origins_items,

    -- * ParametersInCacheKeyAndForwardedToOrigin
    ParametersInCacheKeyAndForwardedToOrigin (..),
    newParametersInCacheKeyAndForwardedToOrigin,
    parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingBrotli,
    parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingGzip,
    parametersInCacheKeyAndForwardedToOrigin_headersConfig,
    parametersInCacheKeyAndForwardedToOrigin_cookiesConfig,
    parametersInCacheKeyAndForwardedToOrigin_queryStringsConfig,

    -- * Paths
    Paths (..),
    newPaths,
    paths_items,
    paths_quantity,

    -- * PublicKey
    PublicKey (..),
    newPublicKey,
    publicKey_id,
    publicKey_createdTime,
    publicKey_publicKeyConfig,

    -- * PublicKeyConfig
    PublicKeyConfig (..),
    newPublicKeyConfig,
    publicKeyConfig_comment,
    publicKeyConfig_callerReference,
    publicKeyConfig_name,
    publicKeyConfig_encodedKey,

    -- * PublicKeyList
    PublicKeyList (..),
    newPublicKeyList,
    publicKeyList_items,
    publicKeyList_nextMarker,
    publicKeyList_maxItems,
    publicKeyList_quantity,

    -- * PublicKeySummary
    PublicKeySummary (..),
    newPublicKeySummary,
    publicKeySummary_comment,
    publicKeySummary_id,
    publicKeySummary_name,
    publicKeySummary_createdTime,
    publicKeySummary_encodedKey,

    -- * QueryArgProfile
    QueryArgProfile (..),
    newQueryArgProfile,
    queryArgProfile_queryArg,
    queryArgProfile_profileId,

    -- * QueryArgProfileConfig
    QueryArgProfileConfig (..),
    newQueryArgProfileConfig,
    queryArgProfileConfig_queryArgProfiles,
    queryArgProfileConfig_forwardWhenQueryArgProfileIsUnknown,

    -- * QueryArgProfiles
    QueryArgProfiles (..),
    newQueryArgProfiles,
    queryArgProfiles_items,
    queryArgProfiles_quantity,

    -- * QueryStringCacheKeys
    QueryStringCacheKeys (..),
    newQueryStringCacheKeys,
    queryStringCacheKeys_items,
    queryStringCacheKeys_quantity,

    -- * QueryStringNames
    QueryStringNames (..),
    newQueryStringNames,
    queryStringNames_items,
    queryStringNames_quantity,

    -- * RealtimeLogConfig
    RealtimeLogConfig (..),
    newRealtimeLogConfig,
    realtimeLogConfig_arn,
    realtimeLogConfig_name,
    realtimeLogConfig_samplingRate,
    realtimeLogConfig_endPoints,
    realtimeLogConfig_fields,

    -- * RealtimeLogConfigs
    RealtimeLogConfigs (..),
    newRealtimeLogConfigs,
    realtimeLogConfigs_items,
    realtimeLogConfigs_nextMarker,
    realtimeLogConfigs_maxItems,
    realtimeLogConfigs_isTruncated,
    realtimeLogConfigs_marker,

    -- * RealtimeMetricsSubscriptionConfig
    RealtimeMetricsSubscriptionConfig (..),
    newRealtimeMetricsSubscriptionConfig,
    realtimeMetricsSubscriptionConfig_realtimeMetricsSubscriptionStatus,

    -- * Restrictions
    Restrictions (..),
    newRestrictions,
    restrictions_geoRestriction,

    -- * S3Origin
    S3Origin (..),
    newS3Origin,
    s3Origin_domainName,
    s3Origin_originAccessIdentity,

    -- * S3OriginConfig
    S3OriginConfig (..),
    newS3OriginConfig,
    s3OriginConfig_originAccessIdentity,

    -- * Signer
    Signer (..),
    newSigner,
    signer_awsAccountNumber,
    signer_keyPairIds,

    -- * StatusCodes
    StatusCodes (..),
    newStatusCodes,
    statusCodes_quantity,
    statusCodes_items,

    -- * StreamingDistribution
    StreamingDistribution (..),
    newStreamingDistribution,
    streamingDistribution_lastModifiedTime,
    streamingDistribution_id,
    streamingDistribution_arn,
    streamingDistribution_status,
    streamingDistribution_domainName,
    streamingDistribution_activeTrustedSigners,
    streamingDistribution_streamingDistributionConfig,

    -- * StreamingDistributionConfig
    StreamingDistributionConfig (..),
    newStreamingDistributionConfig,
    streamingDistributionConfig_priceClass,
    streamingDistributionConfig_logging,
    streamingDistributionConfig_aliases,
    streamingDistributionConfig_callerReference,
    streamingDistributionConfig_s3Origin,
    streamingDistributionConfig_comment,
    streamingDistributionConfig_trustedSigners,
    streamingDistributionConfig_enabled,

    -- * StreamingDistributionConfigWithTags
    StreamingDistributionConfigWithTags (..),
    newStreamingDistributionConfigWithTags,
    streamingDistributionConfigWithTags_streamingDistributionConfig,
    streamingDistributionConfigWithTags_tags,

    -- * StreamingDistributionList
    StreamingDistributionList (..),
    newStreamingDistributionList,
    streamingDistributionList_items,
    streamingDistributionList_nextMarker,
    streamingDistributionList_marker,
    streamingDistributionList_maxItems,
    streamingDistributionList_isTruncated,
    streamingDistributionList_quantity,

    -- * StreamingDistributionSummary
    StreamingDistributionSummary (..),
    newStreamingDistributionSummary,
    streamingDistributionSummary_id,
    streamingDistributionSummary_arn,
    streamingDistributionSummary_status,
    streamingDistributionSummary_lastModifiedTime,
    streamingDistributionSummary_domainName,
    streamingDistributionSummary_s3Origin,
    streamingDistributionSummary_aliases,
    streamingDistributionSummary_trustedSigners,
    streamingDistributionSummary_comment,
    streamingDistributionSummary_priceClass,
    streamingDistributionSummary_enabled,

    -- * StreamingLoggingConfig
    StreamingLoggingConfig (..),
    newStreamingLoggingConfig,
    streamingLoggingConfig_enabled,
    streamingLoggingConfig_bucket,
    streamingLoggingConfig_prefix,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TagKeys
    TagKeys (..),
    newTagKeys,
    tagKeys_items,

    -- * Tags
    Tags (..),
    newTags,
    tags_items,

    -- * TrustedKeyGroups
    TrustedKeyGroups (..),
    newTrustedKeyGroups,
    trustedKeyGroups_items,
    trustedKeyGroups_enabled,
    trustedKeyGroups_quantity,

    -- * TrustedSigners
    TrustedSigners (..),
    newTrustedSigners,
    trustedSigners_items,
    trustedSigners_enabled,
    trustedSigners_quantity,

    -- * ViewerCertificate
    ViewerCertificate (..),
    newViewerCertificate,
    viewerCertificate_sSLSupportMethod,
    viewerCertificate_cloudFrontDefaultCertificate,
    viewerCertificate_iAMCertificateId,
    viewerCertificate_aCMCertificateArn,
    viewerCertificate_certificateSource,
    viewerCertificate_certificate,
    viewerCertificate_minimumProtocolVersion,
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
import Network.AWS.CloudFront.Types.Headers
import Network.AWS.CloudFront.Types.HttpVersion
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
import Network.AWS.CloudFront.Types.OriginShield
import Network.AWS.CloudFront.Types.OriginSslProtocols
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
import Network.AWS.CloudFront.Types.SSLSupportMethod
import Network.AWS.CloudFront.Types.Signer
import Network.AWS.CloudFront.Types.SslProtocol
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-05-31@ of the Amazon CloudFront SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "CloudFront",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "cloudfront",
      Prelude._svcSigningName = "cloudfront",
      Prelude._svcVersion = "2020-05-31",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseXMLError "CloudFront",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | A real-time log configuration with this name already exists. You must
-- provide a unique name. To modify an existing real-time log
-- configuration, use @UpdateRealtimeLogConfig@.
_RealtimeLogConfigAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RealtimeLogConfigAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "RealtimeLogConfigAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | The specified configuration for field-level encryption already exists.
_FieldLevelEncryptionConfigAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FieldLevelEncryptionConfigAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "FieldLevelEncryptionConfigAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | You cannot delete a managed policy.
_IllegalDelete :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IllegalDelete =
  Prelude._MatchServiceError
    defaultService
    "IllegalDelete"
    Prelude.. Prelude.hasStatus 400

-- | A response code is not valid.
_InvalidResponseCode :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidResponseCode =
  Prelude._MatchServiceError
    defaultService
    "InvalidResponseCode"
    Prelude.. Prelude.hasStatus 400

-- | The update contains modifications that are not allowed.
_IllegalUpdate :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IllegalUpdate =
  Prelude._MatchServiceError
    defaultService
    "IllegalUpdate"
    Prelude.. Prelude.hasStatus 400

-- | The @If-Match@ version is missing or not valid.
_InvalidIfMatchVersion :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidIfMatchVersion =
  Prelude._MatchServiceError
    defaultService
    "InvalidIfMatchVersion"
    Prelude.. Prelude.hasStatus 400

-- | The caller reference you attempted to create the distribution with is
-- associated with another distribution.
_DistributionAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DistributionAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "DistributionAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | The specified configuration for field-level encryption can\'t be
-- associated with the specified cache behavior.
_IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior =
  Prelude._MatchServiceError
    defaultService
    "IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior"
    Prelude.. Prelude.hasStatus 400

-- | An invalid error code was specified.
_InvalidErrorCode :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidErrorCode =
  Prelude._MatchServiceError
    defaultService
    "InvalidErrorCode"
    Prelude.. Prelude.hasStatus 400

-- | The tagging specified is not valid.
_InvalidTagging :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagging =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagging"
    Prelude.. Prelude.hasStatus 400

-- | Your request contains too many origin custom headers.
_TooManyOriginCustomHeaders :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyOriginCustomHeaders =
  Prelude._MatchServiceError
    defaultService
    "TooManyOriginCustomHeaders"
    Prelude.. Prelude.hasStatus 400

-- | The read timeout specified for the origin is not valid.
_InvalidOriginReadTimeout :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOriginReadTimeout =
  Prelude._MatchServiceError
    defaultService
    "InvalidOriginReadTimeout"
    Prelude.. Prelude.hasStatus 400

-- | This operation requires a body. Ensure that the body is present and the
-- @Content-Type@ header is set.
_MissingBody :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingBody =
  Prelude._MatchServiceError
    defaultService
    "MissingBody"
    Prelude.. Prelude.hasStatus 400

-- | The specified profile for field-level encryption is in use.
_FieldLevelEncryptionProfileInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FieldLevelEncryptionProfileInUse =
  Prelude._MatchServiceError
    defaultService
    "FieldLevelEncryptionProfileInUse"
    Prelude.. Prelude.hasStatus 409

-- | The maximum number of distributions have been associated with the
-- specified cache policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToCachePolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyDistributionsAssociatedToCachePolicy =
  Prelude._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToCachePolicy"
    Prelude.. Prelude.hasStatus 400

-- | The location code specified is not valid.
_InvalidLocationCode :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLocationCode =
  Prelude._MatchServiceError
    defaultService
    "InvalidLocationCode"
    Prelude.. Prelude.hasStatus 400

-- | A key group with this name already exists. You must provide a unique
-- name. To modify an existing key group, use @UpdateKeyGroup@.
_KeyGroupAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_KeyGroupAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "KeyGroupAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | The specified key group does not exist.
_TrustedKeyGroupDoesNotExist :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TrustedKeyGroupDoesNotExist =
  Prelude._MatchServiceError
    defaultService
    "TrustedKeyGroupDoesNotExist"
    Prelude.. Prelude.hasStatus 400

-- | This operation requires the HTTPS protocol. Ensure that you specify the
-- HTTPS protocol in your request, or omit the @RequiredProtocols@ element
-- from your distribution configuration.
_InvalidRequiredProtocol :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRequiredProtocol =
  Prelude._MatchServiceError
    defaultService
    "InvalidRequiredProtocol"
    Prelude.. Prelude.hasStatus 400

-- | Your request contains too many headers in forwarded values.
_TooManyHeadersInForwardedValues :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyHeadersInForwardedValues =
  Prelude._MatchServiceError
    defaultService
    "TooManyHeadersInForwardedValues"
    Prelude.. Prelude.hasStatus 400

-- | The maximum number of encryption entities for field-level encryption
-- have been created.
_TooManyFieldLevelEncryptionEncryptionEntities :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyFieldLevelEncryptionEncryptionEntities =
  Prelude._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionEncryptionEntities"
    Prelude.. Prelude.hasStatus 400

-- | Cannot delete this resource because it is in use.
_ResourceInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceInUse =
  Prelude._MatchServiceError
    defaultService
    "ResourceInUse"
    Prelude.. Prelude.hasStatus 409

-- | A viewer certificate specified is not valid.
_InvalidViewerCertificate :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidViewerCertificate =
  Prelude._MatchServiceError
    defaultService
    "InvalidViewerCertificate"
    Prelude.. Prelude.hasStatus 400

-- | The specified distribution does not exist.
_NoSuchDistribution :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchDistribution =
  Prelude._MatchServiceError
    defaultService
    "NoSuchDistribution"
    Prelude.. Prelude.hasStatus 404

-- | The minimum protocol version specified is not valid.
_InvalidMinimumProtocolVersion :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMinimumProtocolVersion =
  Prelude._MatchServiceError
    defaultService
    "InvalidMinimumProtocolVersion"
    Prelude.. Prelude.hasStatus 400

-- | The specified CloudFront distribution is not disabled. You must disable
-- the distribution before you can delete it.
_StreamingDistributionNotDisabled :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_StreamingDistributionNotDisabled =
  Prelude._MatchServiceError
    defaultService
    "StreamingDistributionNotDisabled"
    Prelude.. Prelude.hasStatus 409

-- | An origin request policy with this name already exists. You must provide
-- a unique name. To modify an existing origin request policy, use
-- @UpdateOriginRequestPolicy@.
_OriginRequestPolicyAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OriginRequestPolicyAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "OriginRequestPolicyAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | The number of cookies in the cache policy exceeds the maximum. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCookiesInCachePolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyCookiesInCachePolicy =
  Prelude._MatchServiceError
    defaultService
    "TooManyCookiesInCachePolicy"
    Prelude.. Prelude.hasStatus 400

-- | Cannot delete the cache policy because it is attached to one or more
-- cache behaviors.
_CachePolicyInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CachePolicyInUse =
  Prelude._MatchServiceError
    defaultService
    "CachePolicyInUse"
    Prelude.. Prelude.hasStatus 409

-- | Your request contains more Lambda function associations than are allowed
-- per distribution.
_TooManyLambdaFunctionAssociations :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyLambdaFunctionAssociations =
  Prelude._MatchServiceError
    defaultService
    "TooManyLambdaFunctionAssociations"
    Prelude.. Prelude.hasStatus 400

-- | If the @CallerReference@ is a value you already sent in a previous
-- request to create an identity but the content of the
-- @CloudFrontOriginAccessIdentityConfig@ is different from the original
-- request, CloudFront returns a
-- @CloudFrontOriginAccessIdentityAlreadyExists@ error.
_CloudFrontOriginAccessIdentityAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudFrontOriginAccessIdentityAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "CloudFrontOriginAccessIdentityAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | The relative path is too big, is not URL-encoded, or does not begin with
-- a slash (\/).
_InvalidRelativePath :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRelativePath =
  Prelude._MatchServiceError
    defaultService
    "InvalidRelativePath"
    Prelude.. Prelude.hasStatus 400

-- | No origin exists with the specified @Origin Id@.
_NoSuchOrigin :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchOrigin =
  Prelude._MatchServiceError
    defaultService
    "NoSuchOrigin"
    Prelude.. Prelude.hasStatus 404

-- | The specified invalidation does not exist.
_NoSuchInvalidation :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchInvalidation =
  Prelude._MatchServiceError
    defaultService
    "NoSuchInvalidation"
    Prelude.. Prelude.hasStatus 404

-- | The specified public key already exists.
_PublicKeyAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PublicKeyAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "PublicKeyAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | The maximum number of distributions have been associated with the
-- specified origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToOriginRequestPolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyDistributionsAssociatedToOriginRequestPolicy =
  Prelude._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToOriginRequestPolicy"
    Prelude.. Prelude.hasStatus 400

-- | The maximum number of configurations for field-level encryption have
-- been created.
_TooManyFieldLevelEncryptionConfigs :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyFieldLevelEncryptionConfigs =
  Prelude._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionConfigs"
    Prelude.. Prelude.hasStatus 400

-- | The maximum number of field patterns for field-level encryption have
-- been created.
_TooManyFieldLevelEncryptionFieldPatterns :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyFieldLevelEncryptionFieldPatterns =
  Prelude._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionFieldPatterns"
    Prelude.. Prelude.hasStatus 400

-- | The maximum number of content type profiles for field-level encryption
-- have been created.
_TooManyFieldLevelEncryptionContentTypeProfiles :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyFieldLevelEncryptionContentTypeProfiles =
  Prelude._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionContentTypeProfiles"
    Prelude.. Prelude.hasStatus 400

-- | Invalidation batch specified is too large.
_BatchTooLarge :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BatchTooLarge =
  Prelude._MatchServiceError
    defaultService
    "BatchTooLarge"
    Prelude.. Prelude.hasStatus 413

-- | The real-time log configuration does not exist.
_NoSuchRealtimeLogConfig :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchRealtimeLogConfig =
  Prelude._MatchServiceError
    defaultService
    "NoSuchRealtimeLogConfig"
    Prelude.. Prelude.hasStatus 404

-- | The Amazon S3 origin server specified does not refer to a valid Amazon
-- S3 bucket.
_InvalidOrigin :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOrigin =
  Prelude._MatchServiceError
    defaultService
    "InvalidOrigin"
    Prelude.. Prelude.hasStatus 400

-- | You have reached the maximum number of cache policies for this AWS
-- account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCachePolicies :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyCachePolicies =
  Prelude._MatchServiceError
    defaultService
    "TooManyCachePolicies"
    Prelude.. Prelude.hasStatus 400

-- | The number of public keys in this key group is more than the maximum
-- allowed. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyPublicKeysInKeyGroup :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyPublicKeysInKeyGroup =
  Prelude._MatchServiceError
    defaultService
    "TooManyPublicKeysInKeyGroup"
    Prelude.. Prelude.hasStatus 400

-- | The specified configuration for field-level encryption doesn\'t exist.
_NoSuchFieldLevelEncryptionConfig :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchFieldLevelEncryptionConfig =
  Prelude._MatchServiceError
    defaultService
    "NoSuchFieldLevelEncryptionConfig"
    Prelude.. Prelude.hasStatus 404

-- | Your request contains more cookie names in the whitelist than are
-- allowed per cache behavior.
_TooManyCookieNamesInWhiteList :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyCookieNamesInWhiteList =
  Prelude._MatchServiceError
    defaultService
    "TooManyCookieNamesInWhiteList"
    Prelude.. Prelude.hasStatus 400

-- | Cannot delete the real-time log configuration because it is attached to
-- one or more cache behaviors.
_RealtimeLogConfigInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RealtimeLogConfigInUse =
  Prelude._MatchServiceError
    defaultService
    "RealtimeLogConfigInUse"
    Prelude.. Prelude.hasStatus 400

-- | Your request contains forward cookies option which doesn\'t match with
-- the expectation for the @whitelisted@ list of cookie names. Either list
-- of cookie names has been specified when not allowed or list of cookie
-- names is missing when expected.
_InvalidForwardCookies :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidForwardCookies =
  Prelude._MatchServiceError
    defaultService
    "InvalidForwardCookies"
    Prelude.. Prelude.hasStatus 400

-- | The specified configuration for field-level encryption is in use.
_FieldLevelEncryptionConfigInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FieldLevelEncryptionConfigInUse =
  Prelude._MatchServiceError
    defaultService
    "FieldLevelEncryptionConfigInUse"
    Prelude.. Prelude.hasStatus 409

-- | Your request contains more trusted signers than are allowed per
-- distribution.
_TooManyTrustedSigners :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTrustedSigners =
  Prelude._MatchServiceError
    defaultService
    "TooManyTrustedSigners"
    Prelude.. Prelude.hasStatus 400

-- | The headers specified are not valid for an Amazon S3 origin.
_InvalidHeadersForS3Origin :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidHeadersForS3Origin =
  Prelude._MatchServiceError
    defaultService
    "InvalidHeadersForS3Origin"
    Prelude.. Prelude.hasStatus 400

-- | The value of @Quantity@ and the size of @Items@ don\'t match.
_InconsistentQuantities :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InconsistentQuantities =
  Prelude._MatchServiceError
    defaultService
    "InconsistentQuantities"
    Prelude.. Prelude.hasStatus 400

-- | The number of cookies in the origin request policy exceeds the maximum.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCookiesInOriginRequestPolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyCookiesInOriginRequestPolicy =
  Prelude._MatchServiceError
    defaultService
    "TooManyCookiesInOriginRequestPolicy"
    Prelude.. Prelude.hasStatus 400

-- | You cannot specify SSLv3 as the minimum protocol version if you only
-- want to support only clients that support Server Name Indication (SNI).
_InvalidProtocolSettings :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidProtocolSettings =
  Prelude._MatchServiceError
    defaultService
    "InvalidProtocolSettings"
    Prelude.. Prelude.hasStatus 400

-- | Your request contains too many query string parameters.
_TooManyQueryStringParameters :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyQueryStringParameters =
  Prelude._MatchServiceError
    defaultService
    "TooManyQueryStringParameters"
    Prelude.. Prelude.hasStatus 400

-- | You can\'t change the value of a public key.
_CannotChangeImmutablePublicKeyFields :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CannotChangeImmutablePublicKeyFields =
  Prelude._MatchServiceError
    defaultService
    "CannotChangeImmutablePublicKeyFields"
    Prelude.. Prelude.hasStatus 400

-- | The specified origin access identity does not exist.
_NoSuchCloudFrontOriginAccessIdentity :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchCloudFrontOriginAccessIdentity =
  Prelude._MatchServiceError
    defaultService
    "NoSuchCloudFrontOriginAccessIdentity"
    Prelude.. Prelude.hasStatus 404

-- | The maximum number of public keys for field-level encryption have been
-- created. To create a new public key, delete one of the existing keys.
_TooManyPublicKeys :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyPublicKeys =
  Prelude._MatchServiceError
    defaultService
    "TooManyPublicKeys"
    Prelude.. Prelude.hasStatus 400

-- | One or more of your trusted signers don\'t exist.
_TrustedSignerDoesNotExist :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TrustedSignerDoesNotExist =
  Prelude._MatchServiceError
    defaultService
    "TrustedSignerDoesNotExist"
    Prelude.. Prelude.hasStatus 400

-- | You have exceeded the maximum number of allowable InProgress
-- invalidation batch requests, or invalidation objects.
_TooManyInvalidationsInProgress :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyInvalidationsInProgress =
  Prelude._MatchServiceError
    defaultService
    "TooManyInvalidationsInProgress"
    Prelude.. Prelude.hasStatus 400

-- | The specified public key doesn\'t exist.
_NoSuchPublicKey :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchPublicKey =
  Prelude._MatchServiceError
    defaultService
    "NoSuchPublicKey"
    Prelude.. Prelude.hasStatus 404

-- | The specified CloudFront distribution is not disabled. You must disable
-- the distribution before you can delete it.
_DistributionNotDisabled :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DistributionNotDisabled =
  Prelude._MatchServiceError
    defaultService
    "DistributionNotDisabled"
    Prelude.. Prelude.hasStatus 409

-- | Processing your request would cause you to exceed the maximum number of
-- origin access identities allowed.
_TooManyCloudFrontOriginAccessIdentities :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyCloudFrontOriginAccessIdentities =
  Prelude._MatchServiceError
    defaultService
    "TooManyCloudFrontOriginAccessIdentities"
    Prelude.. Prelude.hasStatus 400

-- | The origin access identity is not valid or doesn\'t exist.
_InvalidOriginAccessIdentity :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOriginAccessIdentity =
  Prelude._MatchServiceError
    defaultService
    "InvalidOriginAccessIdentity"
    Prelude.. Prelude.hasStatus 400

-- | The precondition given in one or more of the request header fields
-- evaluated to @false@.
_PreconditionFailed :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PreconditionFailed =
  Prelude._MatchServiceError
    defaultService
    "PreconditionFailed"
    Prelude.. Prelude.hasStatus 412

-- | The maximum number of profiles for field-level encryption have been
-- created.
_TooManyFieldLevelEncryptionProfiles :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyFieldLevelEncryptionProfiles =
  Prelude._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionProfiles"
    Prelude.. Prelude.hasStatus 400

-- | The query string parameters specified are not valid.
_InvalidQueryStringParameters :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidQueryStringParameters =
  Prelude._MatchServiceError
    defaultService
    "InvalidQueryStringParameters"
    Prelude.. Prelude.hasStatus 400

-- | You cannot create more cache behaviors for the distribution.
_TooManyCacheBehaviors :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyCacheBehaviors =
  Prelude._MatchServiceError
    defaultService
    "TooManyCacheBehaviors"
    Prelude.. Prelude.hasStatus 400

-- | You have reached the maximum number of origin request policies for this
-- AWS account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyOriginRequestPolicies :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyOriginRequestPolicies =
  Prelude._MatchServiceError
    defaultService
    "TooManyOriginRequestPolicies"
    Prelude.. Prelude.hasStatus 400

-- | The specified profile for field-level encryption doesn\'t exist.
_NoSuchFieldLevelEncryptionProfile :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchFieldLevelEncryptionProfile =
  Prelude._MatchServiceError
    defaultService
    "NoSuchFieldLevelEncryptionProfile"
    Prelude.. Prelude.hasStatus 404

-- | You have reached the maximum number of key groups for this AWS account.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyKeyGroups :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyKeyGroups =
  Prelude._MatchServiceError
    defaultService
    "TooManyKeyGroups"
    Prelude.. Prelude.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified configuration for field-level encryption.
_TooManyDistributionsAssociatedToFieldLevelEncryptionConfig :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyDistributionsAssociatedToFieldLevelEncryptionConfig =
  Prelude._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToFieldLevelEncryptionConfig"
    Prelude.. Prelude.hasStatus 400

-- | The number of query strings in the origin request policy exceeds the
-- maximum. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyQueryStringsInOriginRequestPolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyQueryStringsInOriginRequestPolicy =
  Prelude._MatchServiceError
    defaultService
    "TooManyQueryStringsInOriginRequestPolicy"
    Prelude.. Prelude.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified Lambda function.
_TooManyDistributionsWithSingleFunctionARN :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyDistributionsWithSingleFunctionARN =
  Prelude._MatchServiceError
    defaultService
    "TooManyDistributionsWithSingleFunctionARN"
    Prelude.. Prelude.hasStatus 400

-- | The specified geo restriction parameter is not valid.
_InvalidGeoRestrictionParameter :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidGeoRestrictionParameter =
  Prelude._MatchServiceError
    defaultService
    "InvalidGeoRestrictionParameter"
    Prelude.. Prelude.hasStatus 400

-- | The number of headers in the origin request policy exceeds the maximum.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyHeadersInOriginRequestPolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyHeadersInOriginRequestPolicy =
  Prelude._MatchServiceError
    defaultService
    "TooManyHeadersInOriginRequestPolicy"
    Prelude.. Prelude.hasStatus 400

-- | You cannot create anymore custom SSL\/TLS certificates.
_TooManyCertificates :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyCertificates =
  Prelude._MatchServiceError
    defaultService
    "TooManyCertificates"
    Prelude.. Prelude.hasStatus 400

-- | The origin request policy does not exist.
_NoSuchOriginRequestPolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchOriginRequestPolicy =
  Prelude._MatchServiceError
    defaultService
    "NoSuchOriginRequestPolicy"
    Prelude.. Prelude.hasStatus 404

-- | Processing your request would cause the maximum number of distributions
-- with Lambda function associations per owner to be exceeded.
_TooManyDistributionsWithLambdaAssociations :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyDistributionsWithLambdaAssociations =
  Prelude._MatchServiceError
    defaultService
    "TooManyDistributionsWithLambdaAssociations"
    Prelude.. Prelude.hasStatus 400

-- | The default root object file name is too big or contains an invalid
-- character.
_InvalidDefaultRootObject :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDefaultRootObject =
  Prelude._MatchServiceError
    defaultService
    "InvalidDefaultRootObject"
    Prelude.. Prelude.hasStatus 400

-- | Cannot delete the origin request policy because it is attached to one or
-- more cache behaviors.
_OriginRequestPolicyInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OriginRequestPolicyInUse =
  Prelude._MatchServiceError
    defaultService
    "OriginRequestPolicyInUse"
    Prelude.. Prelude.hasStatus 409

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyStreamingDistributionCNAMEs :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyStreamingDistributionCNAMEs =
  Prelude._MatchServiceError
    defaultService
    "TooManyStreamingDistributionCNAMEs"
    Prelude.. Prelude.hasStatus 400

-- | The maximum size of a profile for field-level encryption was exceeded.
_FieldLevelEncryptionProfileSizeExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FieldLevelEncryptionProfileSizeExceeded =
  Prelude._MatchServiceError
    defaultService
    "FieldLevelEncryptionProfileSizeExceeded"
    Prelude.. Prelude.hasStatus 400

-- | A resource that was specified is not valid.
_NoSuchResource :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchResource =
  Prelude._MatchServiceError
    defaultService
    "NoSuchResource"
    Prelude.. Prelude.hasStatus 404

-- | The specified profile for field-level encryption already exists.
_FieldLevelEncryptionProfileAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FieldLevelEncryptionProfileAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "FieldLevelEncryptionProfileAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | Processing your request would cause you to exceed the maximum number of
-- distributions allowed.
_TooManyDistributions :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyDistributions =
  Prelude._MatchServiceError
    defaultService
    "TooManyDistributions"
    Prelude.. Prelude.hasStatus 400

-- | The TTL order specified is not valid.
_InvalidTTLOrder :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTTLOrder =
  Prelude._MatchServiceError
    defaultService
    "InvalidTTLOrder"
    Prelude.. Prelude.hasStatus 400

-- | Access denied.
_AccessDenied :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessDenied =
  Prelude._MatchServiceError
    defaultService
    "AccessDenied"
    Prelude.. Prelude.hasStatus 403

-- | No profile specified for the field-level encryption query argument.
_QueryArgProfileEmpty :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_QueryArgProfileEmpty =
  Prelude._MatchServiceError
    defaultService
    "QueryArgProfileEmpty"
    Prelude.. Prelude.hasStatus 400

-- | The number of query strings in the cache policy exceeds the maximum. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyQueryStringsInCachePolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyQueryStringsInCachePolicy =
  Prelude._MatchServiceError
    defaultService
    "TooManyQueryStringsInCachePolicy"
    Prelude.. Prelude.hasStatus 400

-- | You cannot create more origins for the distribution.
_TooManyOrigins :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyOrigins =
  Prelude._MatchServiceError
    defaultService
    "TooManyOrigins"
    Prelude.. Prelude.hasStatus 400

-- | The number of headers in the cache policy exceeds the maximum. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyHeadersInCachePolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyHeadersInCachePolicy =
  Prelude._MatchServiceError
    defaultService
    "TooManyHeadersInCachePolicy"
    Prelude.. Prelude.hasStatus 400

-- | The caller reference you attempted to create the streaming distribution
-- with is associated with another distribution
_StreamingDistributionAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_StreamingDistributionAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "StreamingDistributionAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | The cache policy does not exist.
_NoSuchCachePolicy :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchCachePolicy =
  Prelude._MatchServiceError
    defaultService
    "NoSuchCachePolicy"
    Prelude.. Prelude.hasStatus 404

-- | The number of key groups referenced by this distribution is more than
-- the maximum allowed. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyKeyGroupsAssociatedToDistribution :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyKeyGroupsAssociatedToDistribution =
  Prelude._MatchServiceError
    defaultService
    "TooManyKeyGroupsAssociatedToDistribution"
    Prelude.. Prelude.hasStatus 400

-- | You have reached the maximum number of real-time log configurations for
-- this AWS account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyRealtimeLogConfigs :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRealtimeLogConfigs =
  Prelude._MatchServiceError
    defaultService
    "TooManyRealtimeLogConfigs"
    Prelude.. Prelude.hasStatus 400

-- | The number of distributions that reference this key group is more than
-- the maximum allowed. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToKeyGroup :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyDistributionsAssociatedToKeyGroup =
  Prelude._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToKeyGroup"
    Prelude.. Prelude.hasStatus 400

-- | The specified Lambda function association is invalid.
_InvalidLambdaFunctionAssociation :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLambdaFunctionAssociation =
  Prelude._MatchServiceError
    defaultService
    "InvalidLambdaFunctionAssociation"
    Prelude.. Prelude.hasStatus 400

-- | A cache policy with this name already exists. You must provide a unique
-- name. To modify an existing cache policy, use @UpdateCachePolicy@.
_CachePolicyAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CachePolicyAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "CachePolicyAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | The maximum number of query arg profiles for field-level encryption have
-- been created.
_TooManyFieldLevelEncryptionQueryArgProfiles :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyFieldLevelEncryptionQueryArgProfiles =
  Prelude._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionQueryArgProfiles"
    Prelude.. Prelude.hasStatus 400

-- | The specified public key is in use.
_PublicKeyInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PublicKeyInUse =
  Prelude._MatchServiceError
    defaultService
    "PublicKeyInUse"
    Prelude.. Prelude.hasStatus 409

-- | The CNAME specified is already defined for CloudFront.
_CNAMEAlreadyExists :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CNAMEAlreadyExists =
  Prelude._MatchServiceError
    defaultService
    "CNAMEAlreadyExists"
    Prelude.. Prelude.hasStatus 409

-- | A web ACL ID specified is not valid. To specify a web ACL created using
-- the latest version of AWS WAF, use the ACL ARN, for example
-- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
-- To specify a web ACL created using AWS WAF Classic, use the ACL ID, for
-- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
_InvalidWebACLId :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidWebACLId =
  Prelude._MatchServiceError
    defaultService
    "InvalidWebACLId"
    Prelude.. Prelude.hasStatus 400

-- | The Origin Access Identity specified is already in use.
_CloudFrontOriginAccessIdentityInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CloudFrontOriginAccessIdentityInUse =
  Prelude._MatchServiceError
    defaultService
    "CloudFrontOriginAccessIdentityInUse"
    Prelude.. Prelude.hasStatus 409

-- | Processing your request would cause you to exceed the maximum number of
-- origin groups allowed.
_TooManyOriginGroupsPerDistribution :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyOriginGroupsPerDistribution =
  Prelude._MatchServiceError
    defaultService
    "TooManyOriginGroupsPerDistribution"
    Prelude.. Prelude.hasStatus 400

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyDistributionCNAMEs :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyDistributionCNAMEs =
  Prelude._MatchServiceError
    defaultService
    "TooManyDistributionCNAMEs"
    Prelude.. Prelude.hasStatus 400

-- | The specified streaming distribution does not exist.
_NoSuchStreamingDistribution :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchStreamingDistribution =
  Prelude._MatchServiceError
    defaultService
    "NoSuchStreamingDistribution"
    Prelude.. Prelude.hasStatus 404

-- | The keep alive timeout specified for the origin is not valid.
_InvalidOriginKeepaliveTimeout :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOriginKeepaliveTimeout =
  Prelude._MatchServiceError
    defaultService
    "InvalidOriginKeepaliveTimeout"
    Prelude.. Prelude.hasStatus 400

-- | Processing your request would cause you to exceed the maximum number of
-- streaming distributions allowed.
_TooManyStreamingDistributions :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyStreamingDistributions =
  Prelude._MatchServiceError
    defaultService
    "TooManyStreamingDistributions"
    Prelude.. Prelude.hasStatus 400

-- | An argument is invalid.
_InvalidArgument :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidArgument =
  Prelude._MatchServiceError
    defaultService
    "InvalidArgument"
    Prelude.. Prelude.hasStatus 400
