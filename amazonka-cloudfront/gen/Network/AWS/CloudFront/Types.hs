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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-05-31@ of the Amazon CloudFront SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CloudFront",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cloudfront",
      Core._serviceSigningName = "cloudfront",
      Core._serviceVersion = "2020-05-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "CloudFront",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | A real-time log configuration with this name already exists. You must
-- provide a unique name. To modify an existing real-time log
-- configuration, use @UpdateRealtimeLogConfig@.
_RealtimeLogConfigAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RealtimeLogConfigAlreadyExists =
  Core._MatchServiceError
    defaultService
    "RealtimeLogConfigAlreadyExists"
    Core.. Core.hasStatus 409

-- | The specified configuration for field-level encryption already exists.
_FieldLevelEncryptionConfigAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionConfigAlreadyExists =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionConfigAlreadyExists"
    Core.. Core.hasStatus 409

-- | You cannot delete a managed policy.
_IllegalDelete :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalDelete =
  Core._MatchServiceError
    defaultService
    "IllegalDelete"
    Core.. Core.hasStatus 400

-- | A response code is not valid.
_InvalidResponseCode :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResponseCode =
  Core._MatchServiceError
    defaultService
    "InvalidResponseCode"
    Core.. Core.hasStatus 400

-- | The update contains modifications that are not allowed.
_IllegalUpdate :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalUpdate =
  Core._MatchServiceError
    defaultService
    "IllegalUpdate"
    Core.. Core.hasStatus 400

-- | The @If-Match@ version is missing or not valid.
_InvalidIfMatchVersion :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidIfMatchVersion =
  Core._MatchServiceError
    defaultService
    "InvalidIfMatchVersion"
    Core.. Core.hasStatus 400

-- | The caller reference you attempted to create the distribution with is
-- associated with another distribution.
_DistributionAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DistributionAlreadyExists =
  Core._MatchServiceError
    defaultService
    "DistributionAlreadyExists"
    Core.. Core.hasStatus 409

-- | The specified configuration for field-level encryption can\'t be
-- associated with the specified cache behavior.
_IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior =
  Core._MatchServiceError
    defaultService
    "IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior"
    Core.. Core.hasStatus 400

-- | An invalid error code was specified.
_InvalidErrorCode :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidErrorCode =
  Core._MatchServiceError
    defaultService
    "InvalidErrorCode"
    Core.. Core.hasStatus 400

-- | The tagging specified is not valid.
_InvalidTagging :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagging =
  Core._MatchServiceError
    defaultService
    "InvalidTagging"
    Core.. Core.hasStatus 400

-- | Your request contains too many origin custom headers.
_TooManyOriginCustomHeaders :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyOriginCustomHeaders =
  Core._MatchServiceError
    defaultService
    "TooManyOriginCustomHeaders"
    Core.. Core.hasStatus 400

-- | The read timeout specified for the origin is not valid.
_InvalidOriginReadTimeout :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOriginReadTimeout =
  Core._MatchServiceError
    defaultService
    "InvalidOriginReadTimeout"
    Core.. Core.hasStatus 400

-- | This operation requires a body. Ensure that the body is present and the
-- @Content-Type@ header is set.
_MissingBody :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingBody =
  Core._MatchServiceError
    defaultService
    "MissingBody"
    Core.. Core.hasStatus 400

-- | The specified profile for field-level encryption is in use.
_FieldLevelEncryptionProfileInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionProfileInUse =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionProfileInUse"
    Core.. Core.hasStatus 409

-- | The maximum number of distributions have been associated with the
-- specified cache policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsAssociatedToCachePolicy =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToCachePolicy"
    Core.. Core.hasStatus 400

-- | The location code specified is not valid.
_InvalidLocationCode :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLocationCode =
  Core._MatchServiceError
    defaultService
    "InvalidLocationCode"
    Core.. Core.hasStatus 400

-- | A key group with this name already exists. You must provide a unique
-- name. To modify an existing key group, use @UpdateKeyGroup@.
_KeyGroupAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KeyGroupAlreadyExists =
  Core._MatchServiceError
    defaultService
    "KeyGroupAlreadyExists"
    Core.. Core.hasStatus 409

-- | The specified key group does not exist.
_TrustedKeyGroupDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrustedKeyGroupDoesNotExist =
  Core._MatchServiceError
    defaultService
    "TrustedKeyGroupDoesNotExist"
    Core.. Core.hasStatus 400

-- | This operation requires the HTTPS protocol. Ensure that you specify the
-- HTTPS protocol in your request, or omit the @RequiredProtocols@ element
-- from your distribution configuration.
_InvalidRequiredProtocol :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequiredProtocol =
  Core._MatchServiceError
    defaultService
    "InvalidRequiredProtocol"
    Core.. Core.hasStatus 400

-- | Your request contains too many headers in forwarded values.
_TooManyHeadersInForwardedValues :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyHeadersInForwardedValues =
  Core._MatchServiceError
    defaultService
    "TooManyHeadersInForwardedValues"
    Core.. Core.hasStatus 400

-- | The maximum number of encryption entities for field-level encryption
-- have been created.
_TooManyFieldLevelEncryptionEncryptionEntities :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionEncryptionEntities =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionEncryptionEntities"
    Core.. Core.hasStatus 400

-- | Cannot delete this resource because it is in use.
_ResourceInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUse =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"
    Core.. Core.hasStatus 409

-- | A viewer certificate specified is not valid.
_InvalidViewerCertificate :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidViewerCertificate =
  Core._MatchServiceError
    defaultService
    "InvalidViewerCertificate"
    Core.. Core.hasStatus 400

-- | The specified distribution does not exist.
_NoSuchDistribution :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchDistribution =
  Core._MatchServiceError
    defaultService
    "NoSuchDistribution"
    Core.. Core.hasStatus 404

-- | The minimum protocol version specified is not valid.
_InvalidMinimumProtocolVersion :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMinimumProtocolVersion =
  Core._MatchServiceError
    defaultService
    "InvalidMinimumProtocolVersion"
    Core.. Core.hasStatus 400

-- | The specified CloudFront distribution is not disabled. You must disable
-- the distribution before you can delete it.
_StreamingDistributionNotDisabled :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StreamingDistributionNotDisabled =
  Core._MatchServiceError
    defaultService
    "StreamingDistributionNotDisabled"
    Core.. Core.hasStatus 409

-- | An origin request policy with this name already exists. You must provide
-- a unique name. To modify an existing origin request policy, use
-- @UpdateOriginRequestPolicy@.
_OriginRequestPolicyAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OriginRequestPolicyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "OriginRequestPolicyAlreadyExists"
    Core.. Core.hasStatus 409

-- | The number of cookies in the cache policy exceeds the maximum. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCookiesInCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCookiesInCachePolicy =
  Core._MatchServiceError
    defaultService
    "TooManyCookiesInCachePolicy"
    Core.. Core.hasStatus 400

-- | Cannot delete the cache policy because it is attached to one or more
-- cache behaviors.
_CachePolicyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CachePolicyInUse =
  Core._MatchServiceError
    defaultService
    "CachePolicyInUse"
    Core.. Core.hasStatus 409

-- | Your request contains more Lambda function associations than are allowed
-- per distribution.
_TooManyLambdaFunctionAssociations :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyLambdaFunctionAssociations =
  Core._MatchServiceError
    defaultService
    "TooManyLambdaFunctionAssociations"
    Core.. Core.hasStatus 400

-- | If the @CallerReference@ is a value you already sent in a previous
-- request to create an identity but the content of the
-- @CloudFrontOriginAccessIdentityConfig@ is different from the original
-- request, CloudFront returns a
-- @CloudFrontOriginAccessIdentityAlreadyExists@ error.
_CloudFrontOriginAccessIdentityAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudFrontOriginAccessIdentityAlreadyExists =
  Core._MatchServiceError
    defaultService
    "CloudFrontOriginAccessIdentityAlreadyExists"
    Core.. Core.hasStatus 409

-- | The relative path is too big, is not URL-encoded, or does not begin with
-- a slash (\/).
_InvalidRelativePath :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRelativePath =
  Core._MatchServiceError
    defaultService
    "InvalidRelativePath"
    Core.. Core.hasStatus 400

-- | No origin exists with the specified @Origin Id@.
_NoSuchOrigin :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchOrigin =
  Core._MatchServiceError
    defaultService
    "NoSuchOrigin"
    Core.. Core.hasStatus 404

-- | The specified invalidation does not exist.
_NoSuchInvalidation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchInvalidation =
  Core._MatchServiceError
    defaultService
    "NoSuchInvalidation"
    Core.. Core.hasStatus 404

-- | The specified public key already exists.
_PublicKeyAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PublicKeyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "PublicKeyAlreadyExists"
    Core.. Core.hasStatus 409

-- | The maximum number of distributions have been associated with the
-- specified origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsAssociatedToOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToOriginRequestPolicy"
    Core.. Core.hasStatus 400

-- | The maximum number of configurations for field-level encryption have
-- been created.
_TooManyFieldLevelEncryptionConfigs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionConfigs =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionConfigs"
    Core.. Core.hasStatus 400

-- | The maximum number of field patterns for field-level encryption have
-- been created.
_TooManyFieldLevelEncryptionFieldPatterns :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionFieldPatterns =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionFieldPatterns"
    Core.. Core.hasStatus 400

-- | The maximum number of content type profiles for field-level encryption
-- have been created.
_TooManyFieldLevelEncryptionContentTypeProfiles :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionContentTypeProfiles =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionContentTypeProfiles"
    Core.. Core.hasStatus 400

-- | Invalidation batch specified is too large.
_BatchTooLarge :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BatchTooLarge =
  Core._MatchServiceError
    defaultService
    "BatchTooLarge"
    Core.. Core.hasStatus 413

-- | The real-time log configuration does not exist.
_NoSuchRealtimeLogConfig :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchRealtimeLogConfig =
  Core._MatchServiceError
    defaultService
    "NoSuchRealtimeLogConfig"
    Core.. Core.hasStatus 404

-- | The Amazon S3 origin server specified does not refer to a valid Amazon
-- S3 bucket.
_InvalidOrigin :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOrigin =
  Core._MatchServiceError
    defaultService
    "InvalidOrigin"
    Core.. Core.hasStatus 400

-- | You have reached the maximum number of cache policies for this AWS
-- account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCachePolicies :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCachePolicies =
  Core._MatchServiceError
    defaultService
    "TooManyCachePolicies"
    Core.. Core.hasStatus 400

-- | The number of public keys in this key group is more than the maximum
-- allowed. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyPublicKeysInKeyGroup :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyPublicKeysInKeyGroup =
  Core._MatchServiceError
    defaultService
    "TooManyPublicKeysInKeyGroup"
    Core.. Core.hasStatus 400

-- | The specified configuration for field-level encryption doesn\'t exist.
_NoSuchFieldLevelEncryptionConfig :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchFieldLevelEncryptionConfig =
  Core._MatchServiceError
    defaultService
    "NoSuchFieldLevelEncryptionConfig"
    Core.. Core.hasStatus 404

-- | Your request contains more cookie names in the whitelist than are
-- allowed per cache behavior.
_TooManyCookieNamesInWhiteList :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCookieNamesInWhiteList =
  Core._MatchServiceError
    defaultService
    "TooManyCookieNamesInWhiteList"
    Core.. Core.hasStatus 400

-- | Cannot delete the real-time log configuration because it is attached to
-- one or more cache behaviors.
_RealtimeLogConfigInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RealtimeLogConfigInUse =
  Core._MatchServiceError
    defaultService
    "RealtimeLogConfigInUse"
    Core.. Core.hasStatus 400

-- | Your request contains forward cookies option which doesn\'t match with
-- the expectation for the @whitelisted@ list of cookie names. Either list
-- of cookie names has been specified when not allowed or list of cookie
-- names is missing when expected.
_InvalidForwardCookies :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidForwardCookies =
  Core._MatchServiceError
    defaultService
    "InvalidForwardCookies"
    Core.. Core.hasStatus 400

-- | The specified configuration for field-level encryption is in use.
_FieldLevelEncryptionConfigInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionConfigInUse =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionConfigInUse"
    Core.. Core.hasStatus 409

-- | Your request contains more trusted signers than are allowed per
-- distribution.
_TooManyTrustedSigners :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTrustedSigners =
  Core._MatchServiceError
    defaultService
    "TooManyTrustedSigners"
    Core.. Core.hasStatus 400

-- | The headers specified are not valid for an Amazon S3 origin.
_InvalidHeadersForS3Origin :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidHeadersForS3Origin =
  Core._MatchServiceError
    defaultService
    "InvalidHeadersForS3Origin"
    Core.. Core.hasStatus 400

-- | The value of @Quantity@ and the size of @Items@ don\'t match.
_InconsistentQuantities :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InconsistentQuantities =
  Core._MatchServiceError
    defaultService
    "InconsistentQuantities"
    Core.. Core.hasStatus 400

-- | The number of cookies in the origin request policy exceeds the maximum.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCookiesInOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCookiesInOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyCookiesInOriginRequestPolicy"
    Core.. Core.hasStatus 400

-- | You cannot specify SSLv3 as the minimum protocol version if you only
-- want to support only clients that support Server Name Indication (SNI).
_InvalidProtocolSettings :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidProtocolSettings =
  Core._MatchServiceError
    defaultService
    "InvalidProtocolSettings"
    Core.. Core.hasStatus 400

-- | Your request contains too many query string parameters.
_TooManyQueryStringParameters :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyQueryStringParameters =
  Core._MatchServiceError
    defaultService
    "TooManyQueryStringParameters"
    Core.. Core.hasStatus 400

-- | You can\'t change the value of a public key.
_CannotChangeImmutablePublicKeyFields :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CannotChangeImmutablePublicKeyFields =
  Core._MatchServiceError
    defaultService
    "CannotChangeImmutablePublicKeyFields"
    Core.. Core.hasStatus 400

-- | The specified origin access identity does not exist.
_NoSuchCloudFrontOriginAccessIdentity :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchCloudFrontOriginAccessIdentity =
  Core._MatchServiceError
    defaultService
    "NoSuchCloudFrontOriginAccessIdentity"
    Core.. Core.hasStatus 404

-- | The maximum number of public keys for field-level encryption have been
-- created. To create a new public key, delete one of the existing keys.
_TooManyPublicKeys :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyPublicKeys =
  Core._MatchServiceError
    defaultService
    "TooManyPublicKeys"
    Core.. Core.hasStatus 400

-- | One or more of your trusted signers don\'t exist.
_TrustedSignerDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrustedSignerDoesNotExist =
  Core._MatchServiceError
    defaultService
    "TrustedSignerDoesNotExist"
    Core.. Core.hasStatus 400

-- | You have exceeded the maximum number of allowable InProgress
-- invalidation batch requests, or invalidation objects.
_TooManyInvalidationsInProgress :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyInvalidationsInProgress =
  Core._MatchServiceError
    defaultService
    "TooManyInvalidationsInProgress"
    Core.. Core.hasStatus 400

-- | The specified public key doesn\'t exist.
_NoSuchPublicKey :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchPublicKey =
  Core._MatchServiceError
    defaultService
    "NoSuchPublicKey"
    Core.. Core.hasStatus 404

-- | The specified CloudFront distribution is not disabled. You must disable
-- the distribution before you can delete it.
_DistributionNotDisabled :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DistributionNotDisabled =
  Core._MatchServiceError
    defaultService
    "DistributionNotDisabled"
    Core.. Core.hasStatus 409

-- | Processing your request would cause you to exceed the maximum number of
-- origin access identities allowed.
_TooManyCloudFrontOriginAccessIdentities :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCloudFrontOriginAccessIdentities =
  Core._MatchServiceError
    defaultService
    "TooManyCloudFrontOriginAccessIdentities"
    Core.. Core.hasStatus 400

-- | The origin access identity is not valid or doesn\'t exist.
_InvalidOriginAccessIdentity :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOriginAccessIdentity =
  Core._MatchServiceError
    defaultService
    "InvalidOriginAccessIdentity"
    Core.. Core.hasStatus 400

-- | The precondition given in one or more of the request header fields
-- evaluated to @false@.
_PreconditionFailed :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PreconditionFailed =
  Core._MatchServiceError
    defaultService
    "PreconditionFailed"
    Core.. Core.hasStatus 412

-- | The maximum number of profiles for field-level encryption have been
-- created.
_TooManyFieldLevelEncryptionProfiles :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionProfiles =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionProfiles"
    Core.. Core.hasStatus 400

-- | The query string parameters specified are not valid.
_InvalidQueryStringParameters :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidQueryStringParameters =
  Core._MatchServiceError
    defaultService
    "InvalidQueryStringParameters"
    Core.. Core.hasStatus 400

-- | You cannot create more cache behaviors for the distribution.
_TooManyCacheBehaviors :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCacheBehaviors =
  Core._MatchServiceError
    defaultService
    "TooManyCacheBehaviors"
    Core.. Core.hasStatus 400

-- | You have reached the maximum number of origin request policies for this
-- AWS account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyOriginRequestPolicies :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyOriginRequestPolicies =
  Core._MatchServiceError
    defaultService
    "TooManyOriginRequestPolicies"
    Core.. Core.hasStatus 400

-- | The specified profile for field-level encryption doesn\'t exist.
_NoSuchFieldLevelEncryptionProfile :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchFieldLevelEncryptionProfile =
  Core._MatchServiceError
    defaultService
    "NoSuchFieldLevelEncryptionProfile"
    Core.. Core.hasStatus 404

-- | You have reached the maximum number of key groups for this AWS account.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyKeyGroups :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyKeyGroups =
  Core._MatchServiceError
    defaultService
    "TooManyKeyGroups"
    Core.. Core.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified configuration for field-level encryption.
_TooManyDistributionsAssociatedToFieldLevelEncryptionConfig :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsAssociatedToFieldLevelEncryptionConfig =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToFieldLevelEncryptionConfig"
    Core.. Core.hasStatus 400

-- | The number of query strings in the origin request policy exceeds the
-- maximum. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyQueryStringsInOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyQueryStringsInOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyQueryStringsInOriginRequestPolicy"
    Core.. Core.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified Lambda function.
_TooManyDistributionsWithSingleFunctionARN :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsWithSingleFunctionARN =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsWithSingleFunctionARN"
    Core.. Core.hasStatus 400

-- | The specified geo restriction parameter is not valid.
_InvalidGeoRestrictionParameter :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGeoRestrictionParameter =
  Core._MatchServiceError
    defaultService
    "InvalidGeoRestrictionParameter"
    Core.. Core.hasStatus 400

-- | The number of headers in the origin request policy exceeds the maximum.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyHeadersInOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyHeadersInOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyHeadersInOriginRequestPolicy"
    Core.. Core.hasStatus 400

-- | You cannot create anymore custom SSL\/TLS certificates.
_TooManyCertificates :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCertificates =
  Core._MatchServiceError
    defaultService
    "TooManyCertificates"
    Core.. Core.hasStatus 400

-- | The origin request policy does not exist.
_NoSuchOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "NoSuchOriginRequestPolicy"
    Core.. Core.hasStatus 404

-- | Processing your request would cause the maximum number of distributions
-- with Lambda function associations per owner to be exceeded.
_TooManyDistributionsWithLambdaAssociations :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsWithLambdaAssociations =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsWithLambdaAssociations"
    Core.. Core.hasStatus 400

-- | The default root object file name is too big or contains an invalid
-- character.
_InvalidDefaultRootObject :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDefaultRootObject =
  Core._MatchServiceError
    defaultService
    "InvalidDefaultRootObject"
    Core.. Core.hasStatus 400

-- | Cannot delete the origin request policy because it is attached to one or
-- more cache behaviors.
_OriginRequestPolicyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OriginRequestPolicyInUse =
  Core._MatchServiceError
    defaultService
    "OriginRequestPolicyInUse"
    Core.. Core.hasStatus 409

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyStreamingDistributionCNAMEs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyStreamingDistributionCNAMEs =
  Core._MatchServiceError
    defaultService
    "TooManyStreamingDistributionCNAMEs"
    Core.. Core.hasStatus 400

-- | The maximum size of a profile for field-level encryption was exceeded.
_FieldLevelEncryptionProfileSizeExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionProfileSizeExceeded =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionProfileSizeExceeded"
    Core.. Core.hasStatus 400

-- | A resource that was specified is not valid.
_NoSuchResource :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchResource =
  Core._MatchServiceError
    defaultService
    "NoSuchResource"
    Core.. Core.hasStatus 404

-- | The specified profile for field-level encryption already exists.
_FieldLevelEncryptionProfileAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionProfileAlreadyExists =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionProfileAlreadyExists"
    Core.. Core.hasStatus 409

-- | Processing your request would cause you to exceed the maximum number of
-- distributions allowed.
_TooManyDistributions :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributions =
  Core._MatchServiceError
    defaultService
    "TooManyDistributions"
    Core.. Core.hasStatus 400

-- | The TTL order specified is not valid.
_InvalidTTLOrder :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTTLOrder =
  Core._MatchServiceError
    defaultService
    "InvalidTTLOrder"
    Core.. Core.hasStatus 400

-- | Access denied.
_AccessDenied :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDenied =
  Core._MatchServiceError
    defaultService
    "AccessDenied"
    Core.. Core.hasStatus 403

-- | No profile specified for the field-level encryption query argument.
_QueryArgProfileEmpty :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_QueryArgProfileEmpty =
  Core._MatchServiceError
    defaultService
    "QueryArgProfileEmpty"
    Core.. Core.hasStatus 400

-- | The number of query strings in the cache policy exceeds the maximum. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyQueryStringsInCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyQueryStringsInCachePolicy =
  Core._MatchServiceError
    defaultService
    "TooManyQueryStringsInCachePolicy"
    Core.. Core.hasStatus 400

-- | You cannot create more origins for the distribution.
_TooManyOrigins :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyOrigins =
  Core._MatchServiceError
    defaultService
    "TooManyOrigins"
    Core.. Core.hasStatus 400

-- | The number of headers in the cache policy exceeds the maximum. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyHeadersInCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyHeadersInCachePolicy =
  Core._MatchServiceError
    defaultService
    "TooManyHeadersInCachePolicy"
    Core.. Core.hasStatus 400

-- | The caller reference you attempted to create the streaming distribution
-- with is associated with another distribution
_StreamingDistributionAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StreamingDistributionAlreadyExists =
  Core._MatchServiceError
    defaultService
    "StreamingDistributionAlreadyExists"
    Core.. Core.hasStatus 409

-- | The cache policy does not exist.
_NoSuchCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchCachePolicy =
  Core._MatchServiceError
    defaultService
    "NoSuchCachePolicy"
    Core.. Core.hasStatus 404

-- | The number of key groups referenced by this distribution is more than
-- the maximum allowed. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyKeyGroupsAssociatedToDistribution :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyKeyGroupsAssociatedToDistribution =
  Core._MatchServiceError
    defaultService
    "TooManyKeyGroupsAssociatedToDistribution"
    Core.. Core.hasStatus 400

-- | You have reached the maximum number of real-time log configurations for
-- this AWS account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyRealtimeLogConfigs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRealtimeLogConfigs =
  Core._MatchServiceError
    defaultService
    "TooManyRealtimeLogConfigs"
    Core.. Core.hasStatus 400

-- | The number of distributions that reference this key group is more than
-- the maximum allowed. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToKeyGroup :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsAssociatedToKeyGroup =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToKeyGroup"
    Core.. Core.hasStatus 400

-- | The specified Lambda function association is invalid.
_InvalidLambdaFunctionAssociation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLambdaFunctionAssociation =
  Core._MatchServiceError
    defaultService
    "InvalidLambdaFunctionAssociation"
    Core.. Core.hasStatus 400

-- | A cache policy with this name already exists. You must provide a unique
-- name. To modify an existing cache policy, use @UpdateCachePolicy@.
_CachePolicyAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CachePolicyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "CachePolicyAlreadyExists"
    Core.. Core.hasStatus 409

-- | The maximum number of query arg profiles for field-level encryption have
-- been created.
_TooManyFieldLevelEncryptionQueryArgProfiles :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionQueryArgProfiles =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionQueryArgProfiles"
    Core.. Core.hasStatus 400

-- | The specified public key is in use.
_PublicKeyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PublicKeyInUse =
  Core._MatchServiceError
    defaultService
    "PublicKeyInUse"
    Core.. Core.hasStatus 409

-- | The CNAME specified is already defined for CloudFront.
_CNAMEAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CNAMEAlreadyExists =
  Core._MatchServiceError
    defaultService
    "CNAMEAlreadyExists"
    Core.. Core.hasStatus 409

-- | A web ACL ID specified is not valid. To specify a web ACL created using
-- the latest version of AWS WAF, use the ACL ARN, for example
-- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
-- To specify a web ACL created using AWS WAF Classic, use the ACL ID, for
-- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
_InvalidWebACLId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidWebACLId =
  Core._MatchServiceError
    defaultService
    "InvalidWebACLId"
    Core.. Core.hasStatus 400

-- | The Origin Access Identity specified is already in use.
_CloudFrontOriginAccessIdentityInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudFrontOriginAccessIdentityInUse =
  Core._MatchServiceError
    defaultService
    "CloudFrontOriginAccessIdentityInUse"
    Core.. Core.hasStatus 409

-- | Processing your request would cause you to exceed the maximum number of
-- origin groups allowed.
_TooManyOriginGroupsPerDistribution :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyOriginGroupsPerDistribution =
  Core._MatchServiceError
    defaultService
    "TooManyOriginGroupsPerDistribution"
    Core.. Core.hasStatus 400

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyDistributionCNAMEs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionCNAMEs =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionCNAMEs"
    Core.. Core.hasStatus 400

-- | The specified streaming distribution does not exist.
_NoSuchStreamingDistribution :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchStreamingDistribution =
  Core._MatchServiceError
    defaultService
    "NoSuchStreamingDistribution"
    Core.. Core.hasStatus 404

-- | The keep alive timeout specified for the origin is not valid.
_InvalidOriginKeepaliveTimeout :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOriginKeepaliveTimeout =
  Core._MatchServiceError
    defaultService
    "InvalidOriginKeepaliveTimeout"
    Core.. Core.hasStatus 400

-- | Processing your request would cause you to exceed the maximum number of
-- streaming distributions allowed.
_TooManyStreamingDistributions :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyStreamingDistributions =
  Core._MatchServiceError
    defaultService
    "TooManyStreamingDistributions"
    Core.. Core.hasStatus 400

-- | An argument is invalid.
_InvalidArgument :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgument =
  Core._MatchServiceError
    defaultService
    "InvalidArgument"
    Core.. Core.hasStatus 400
