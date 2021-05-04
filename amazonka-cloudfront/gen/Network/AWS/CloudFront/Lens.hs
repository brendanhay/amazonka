{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Lens
  ( -- * Operations

    -- ** UpdatePublicKey
    updatePublicKey_ifMatch,
    updatePublicKey_publicKeyConfig,
    updatePublicKey_id,
    updatePublicKeyResponse_eTag,
    updatePublicKeyResponse_publicKey,
    updatePublicKeyResponse_httpStatus,

    -- ** DeletePublicKey
    deletePublicKey_ifMatch,
    deletePublicKey_id,

    -- ** ListPublicKeys
    listPublicKeys_maxItems,
    listPublicKeys_marker,
    listPublicKeysResponse_publicKeyList,
    listPublicKeysResponse_httpStatus,

    -- ** GetDistribution
    getDistribution_id,
    getDistributionResponse_eTag,
    getDistributionResponse_distribution,
    getDistributionResponse_httpStatus,

    -- ** GetKeyGroupConfig
    getKeyGroupConfig_id,
    getKeyGroupConfigResponse_eTag,
    getKeyGroupConfigResponse_keyGroupConfig,
    getKeyGroupConfigResponse_httpStatus,

    -- ** CreateFieldLevelEncryptionProfile
    createFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig,
    createFieldLevelEncryptionProfileResponse_eTag,
    createFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile,
    createFieldLevelEncryptionProfileResponse_location,
    createFieldLevelEncryptionProfileResponse_httpStatus,

    -- ** GetMonitoringSubscription
    getMonitoringSubscription_distributionId,
    getMonitoringSubscriptionResponse_monitoringSubscription,
    getMonitoringSubscriptionResponse_httpStatus,

    -- ** CreateOriginRequestPolicy
    createOriginRequestPolicy_originRequestPolicyConfig,
    createOriginRequestPolicyResponse_eTag,
    createOriginRequestPolicyResponse_originRequestPolicy,
    createOriginRequestPolicyResponse_location,
    createOriginRequestPolicyResponse_httpStatus,

    -- ** ListDistributionsByCachePolicyId
    listDistributionsByCachePolicyId_maxItems,
    listDistributionsByCachePolicyId_marker,
    listDistributionsByCachePolicyId_cachePolicyId,
    listDistributionsByCachePolicyIdResponse_distributionIdList,
    listDistributionsByCachePolicyIdResponse_httpStatus,

    -- ** ListKeyGroups
    listKeyGroups_maxItems,
    listKeyGroups_marker,
    listKeyGroupsResponse_keyGroupList,
    listKeyGroupsResponse_httpStatus,

    -- ** ListOriginRequestPolicies
    listOriginRequestPolicies_type,
    listOriginRequestPolicies_maxItems,
    listOriginRequestPolicies_marker,
    listOriginRequestPoliciesResponse_originRequestPolicyList,
    listOriginRequestPoliciesResponse_httpStatus,

    -- ** GetKeyGroup
    getKeyGroup_id,
    getKeyGroupResponse_eTag,
    getKeyGroupResponse_keyGroup,
    getKeyGroupResponse_httpStatus,

    -- ** GetDistributionConfig
    getDistributionConfig_id,
    getDistributionConfigResponse_eTag,
    getDistributionConfigResponse_distributionConfig,
    getDistributionConfigResponse_httpStatus,

    -- ** ListDistributions
    listDistributions_maxItems,
    listDistributions_marker,
    listDistributionsResponse_httpStatus,
    listDistributionsResponse_distributionList,

    -- ** UntagResource
    untagResource_resource,
    untagResource_tagKeys,

    -- ** ListDistributionsByWebACLId
    listDistributionsByWebACLId_maxItems,
    listDistributionsByWebACLId_marker,
    listDistributionsByWebACLId_webACLId,
    listDistributionsByWebACLIdResponse_distributionList,
    listDistributionsByWebACLIdResponse_httpStatus,

    -- ** GetCloudFrontOriginAccessIdentity
    getCloudFrontOriginAccessIdentity_id,
    getCloudFrontOriginAccessIdentityResponse_eTag,
    getCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity,
    getCloudFrontOriginAccessIdentityResponse_httpStatus,

    -- ** GetPublicKey
    getPublicKey_id,
    getPublicKeyResponse_eTag,
    getPublicKeyResponse_publicKey,
    getPublicKeyResponse_httpStatus,

    -- ** ListRealtimeLogConfigs
    listRealtimeLogConfigs_maxItems,
    listRealtimeLogConfigs_marker,
    listRealtimeLogConfigsResponse_realtimeLogConfigs,
    listRealtimeLogConfigsResponse_httpStatus,

    -- ** UpdateFieldLevelEncryptionConfig
    updateFieldLevelEncryptionConfig_ifMatch,
    updateFieldLevelEncryptionConfig_fieldLevelEncryptionConfig,
    updateFieldLevelEncryptionConfig_id,
    updateFieldLevelEncryptionConfigResponse_eTag,
    updateFieldLevelEncryptionConfigResponse_fieldLevelEncryption,
    updateFieldLevelEncryptionConfigResponse_httpStatus,

    -- ** CreateCachePolicy
    createCachePolicy_cachePolicyConfig,
    createCachePolicyResponse_eTag,
    createCachePolicyResponse_cachePolicy,
    createCachePolicyResponse_location,
    createCachePolicyResponse_httpStatus,

    -- ** ListDistributionsByKeyGroup
    listDistributionsByKeyGroup_maxItems,
    listDistributionsByKeyGroup_marker,
    listDistributionsByKeyGroup_keyGroupId,
    listDistributionsByKeyGroupResponse_distributionIdList,
    listDistributionsByKeyGroupResponse_httpStatus,

    -- ** TagResource
    tagResource_resource,
    tagResource_tags,

    -- ** ListCachePolicies
    listCachePolicies_type,
    listCachePolicies_maxItems,
    listCachePolicies_marker,
    listCachePoliciesResponse_cachePolicyList,
    listCachePoliciesResponse_httpStatus,

    -- ** ListDistributionsByOriginRequestPolicyId
    listDistributionsByOriginRequestPolicyId_maxItems,
    listDistributionsByOriginRequestPolicyId_marker,
    listDistributionsByOriginRequestPolicyId_originRequestPolicyId,
    listDistributionsByOriginRequestPolicyIdResponse_distributionIdList,
    listDistributionsByOriginRequestPolicyIdResponse_httpStatus,

    -- ** ListFieldLevelEncryptionConfigs
    listFieldLevelEncryptionConfigs_maxItems,
    listFieldLevelEncryptionConfigs_marker,
    listFieldLevelEncryptionConfigsResponse_fieldLevelEncryptionList,
    listFieldLevelEncryptionConfigsResponse_httpStatus,

    -- ** DeleteFieldLevelEncryptionConfig
    deleteFieldLevelEncryptionConfig_ifMatch,
    deleteFieldLevelEncryptionConfig_id,

    -- ** DeleteCachePolicy
    deleteCachePolicy_ifMatch,
    deleteCachePolicy_id,

    -- ** GetFieldLevelEncryption
    getFieldLevelEncryption_id,
    getFieldLevelEncryptionResponse_eTag,
    getFieldLevelEncryptionResponse_fieldLevelEncryption,
    getFieldLevelEncryptionResponse_httpStatus,

    -- ** UpdateCachePolicy
    updateCachePolicy_ifMatch,
    updateCachePolicy_cachePolicyConfig,
    updateCachePolicy_id,
    updateCachePolicyResponse_eTag,
    updateCachePolicyResponse_cachePolicy,
    updateCachePolicyResponse_httpStatus,

    -- ** GetInvalidation
    getInvalidation_distributionId,
    getInvalidation_id,
    getInvalidationResponse_invalidation,
    getInvalidationResponse_httpStatus,

    -- ** GetPublicKeyConfig
    getPublicKeyConfig_id,
    getPublicKeyConfigResponse_eTag,
    getPublicKeyConfigResponse_publicKeyConfig,
    getPublicKeyConfigResponse_httpStatus,

    -- ** GetCloudFrontOriginAccessIdentityConfig
    getCloudFrontOriginAccessIdentityConfig_id,
    getCloudFrontOriginAccessIdentityConfigResponse_eTag,
    getCloudFrontOriginAccessIdentityConfigResponse_cloudFrontOriginAccessIdentityConfig,
    getCloudFrontOriginAccessIdentityConfigResponse_httpStatus,

    -- ** CreateStreamingDistribution
    createStreamingDistribution_streamingDistributionConfig,
    createStreamingDistributionResponse_eTag,
    createStreamingDistributionResponse_streamingDistribution,
    createStreamingDistributionResponse_location,
    createStreamingDistributionResponse_httpStatus,

    -- ** DeleteCloudFrontOriginAccessIdentity
    deleteCloudFrontOriginAccessIdentity_ifMatch,
    deleteCloudFrontOriginAccessIdentity_id,

    -- ** DeleteStreamingDistribution
    deleteStreamingDistribution_ifMatch,
    deleteStreamingDistribution_id,

    -- ** GetFieldLevelEncryptionConfig
    getFieldLevelEncryptionConfig_id,
    getFieldLevelEncryptionConfigResponse_eTag,
    getFieldLevelEncryptionConfigResponse_fieldLevelEncryptionConfig,
    getFieldLevelEncryptionConfigResponse_httpStatus,

    -- ** GetRealtimeLogConfig
    getRealtimeLogConfig_arn,
    getRealtimeLogConfig_name,
    getRealtimeLogConfigResponse_realtimeLogConfig,
    getRealtimeLogConfigResponse_httpStatus,

    -- ** UpdateCloudFrontOriginAccessIdentity
    updateCloudFrontOriginAccessIdentity_ifMatch,
    updateCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig,
    updateCloudFrontOriginAccessIdentity_id,
    updateCloudFrontOriginAccessIdentityResponse_eTag,
    updateCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity,
    updateCloudFrontOriginAccessIdentityResponse_httpStatus,

    -- ** UpdateStreamingDistribution
    updateStreamingDistribution_ifMatch,
    updateStreamingDistribution_streamingDistributionConfig,
    updateStreamingDistribution_id,
    updateStreamingDistributionResponse_eTag,
    updateStreamingDistributionResponse_streamingDistribution,
    updateStreamingDistributionResponse_httpStatus,

    -- ** ListStreamingDistributions
    listStreamingDistributions_maxItems,
    listStreamingDistributions_marker,
    listStreamingDistributionsResponse_httpStatus,
    listStreamingDistributionsResponse_streamingDistributionList,

    -- ** CreateKeyGroup
    createKeyGroup_keyGroupConfig,
    createKeyGroupResponse_eTag,
    createKeyGroupResponse_keyGroup,
    createKeyGroupResponse_location,
    createKeyGroupResponse_httpStatus,

    -- ** UpdateOriginRequestPolicy
    updateOriginRequestPolicy_ifMatch,
    updateOriginRequestPolicy_originRequestPolicyConfig,
    updateOriginRequestPolicy_id,
    updateOriginRequestPolicyResponse_eTag,
    updateOriginRequestPolicyResponse_originRequestPolicy,
    updateOriginRequestPolicyResponse_httpStatus,

    -- ** GetFieldLevelEncryptionProfileConfig
    getFieldLevelEncryptionProfileConfig_id,
    getFieldLevelEncryptionProfileConfigResponse_eTag,
    getFieldLevelEncryptionProfileConfigResponse_fieldLevelEncryptionProfileConfig,
    getFieldLevelEncryptionProfileConfigResponse_httpStatus,

    -- ** DeleteOriginRequestPolicy
    deleteOriginRequestPolicy_ifMatch,
    deleteOriginRequestPolicy_id,

    -- ** ListFieldLevelEncryptionProfiles
    listFieldLevelEncryptionProfiles_maxItems,
    listFieldLevelEncryptionProfiles_marker,
    listFieldLevelEncryptionProfilesResponse_fieldLevelEncryptionProfileList,
    listFieldLevelEncryptionProfilesResponse_httpStatus,

    -- ** DeleteFieldLevelEncryptionProfile
    deleteFieldLevelEncryptionProfile_ifMatch,
    deleteFieldLevelEncryptionProfile_id,

    -- ** GetOriginRequestPolicyConfig
    getOriginRequestPolicyConfig_id,
    getOriginRequestPolicyConfigResponse_eTag,
    getOriginRequestPolicyConfigResponse_originRequestPolicyConfig,
    getOriginRequestPolicyConfigResponse_httpStatus,

    -- ** UpdateKeyGroup
    updateKeyGroup_ifMatch,
    updateKeyGroup_keyGroupConfig,
    updateKeyGroup_id,
    updateKeyGroupResponse_eTag,
    updateKeyGroupResponse_keyGroup,
    updateKeyGroupResponse_httpStatus,

    -- ** DeleteKeyGroup
    deleteKeyGroup_ifMatch,
    deleteKeyGroup_id,

    -- ** CreateStreamingDistributionWithTags
    createStreamingDistributionWithTags_streamingDistributionConfigWithTags,
    createStreamingDistributionWithTagsResponse_eTag,
    createStreamingDistributionWithTagsResponse_streamingDistribution,
    createStreamingDistributionWithTagsResponse_location,
    createStreamingDistributionWithTagsResponse_httpStatus,

    -- ** ListDistributionsByRealtimeLogConfig
    listDistributionsByRealtimeLogConfig_realtimeLogConfigName,
    listDistributionsByRealtimeLogConfig_realtimeLogConfigArn,
    listDistributionsByRealtimeLogConfig_maxItems,
    listDistributionsByRealtimeLogConfig_marker,
    listDistributionsByRealtimeLogConfigResponse_distributionList,
    listDistributionsByRealtimeLogConfigResponse_httpStatus,

    -- ** UpdateFieldLevelEncryptionProfile
    updateFieldLevelEncryptionProfile_ifMatch,
    updateFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig,
    updateFieldLevelEncryptionProfile_id,
    updateFieldLevelEncryptionProfileResponse_eTag,
    updateFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile,
    updateFieldLevelEncryptionProfileResponse_httpStatus,

    -- ** CreateDistribution
    createDistribution_distributionConfig,
    createDistributionResponse_eTag,
    createDistributionResponse_distribution,
    createDistributionResponse_location,
    createDistributionResponse_httpStatus,

    -- ** DeleteMonitoringSubscription
    deleteMonitoringSubscription_distributionId,
    deleteMonitoringSubscriptionResponse_httpStatus,

    -- ** GetFieldLevelEncryptionProfile
    getFieldLevelEncryptionProfile_id,
    getFieldLevelEncryptionProfileResponse_eTag,
    getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile,
    getFieldLevelEncryptionProfileResponse_httpStatus,

    -- ** CreateMonitoringSubscription
    createMonitoringSubscription_monitoringSubscription,
    createMonitoringSubscription_distributionId,
    createMonitoringSubscriptionResponse_monitoringSubscription,
    createMonitoringSubscriptionResponse_httpStatus,

    -- ** GetOriginRequestPolicy
    getOriginRequestPolicy_id,
    getOriginRequestPolicyResponse_eTag,
    getOriginRequestPolicyResponse_originRequestPolicy,
    getOriginRequestPolicyResponse_httpStatus,

    -- ** UpdateDistribution
    updateDistribution_ifMatch,
    updateDistribution_distributionConfig,
    updateDistribution_id,
    updateDistributionResponse_eTag,
    updateDistributionResponse_distribution,
    updateDistributionResponse_httpStatus,

    -- ** DeleteDistribution
    deleteDistribution_ifMatch,
    deleteDistribution_id,

    -- ** DeleteRealtimeLogConfig
    deleteRealtimeLogConfig_arn,
    deleteRealtimeLogConfig_name,

    -- ** GetStreamingDistribution
    getStreamingDistribution_id,
    getStreamingDistributionResponse_eTag,
    getStreamingDistributionResponse_streamingDistribution,
    getStreamingDistributionResponse_httpStatus,

    -- ** CreateInvalidation
    createInvalidation_distributionId,
    createInvalidation_invalidationBatch,
    createInvalidationResponse_invalidation,
    createInvalidationResponse_location,
    createInvalidationResponse_httpStatus,

    -- ** GetCachePolicyConfig
    getCachePolicyConfig_id,
    getCachePolicyConfigResponse_eTag,
    getCachePolicyConfigResponse_cachePolicyConfig,
    getCachePolicyConfigResponse_httpStatus,

    -- ** UpdateRealtimeLogConfig
    updateRealtimeLogConfig_samplingRate,
    updateRealtimeLogConfig_endPoints,
    updateRealtimeLogConfig_arn,
    updateRealtimeLogConfig_name,
    updateRealtimeLogConfig_fields,
    updateRealtimeLogConfigResponse_realtimeLogConfig,
    updateRealtimeLogConfigResponse_httpStatus,

    -- ** CreateRealtimeLogConfig
    createRealtimeLogConfig_endPoints,
    createRealtimeLogConfig_fields,
    createRealtimeLogConfig_name,
    createRealtimeLogConfig_samplingRate,
    createRealtimeLogConfigResponse_realtimeLogConfig,
    createRealtimeLogConfigResponse_httpStatus,

    -- ** CreateDistributionWithTags
    createDistributionWithTags_distributionConfigWithTags,
    createDistributionWithTagsResponse_eTag,
    createDistributionWithTagsResponse_distribution,
    createDistributionWithTagsResponse_location,
    createDistributionWithTagsResponse_httpStatus,

    -- ** CreateFieldLevelEncryptionConfig
    createFieldLevelEncryptionConfig_fieldLevelEncryptionConfig,
    createFieldLevelEncryptionConfigResponse_eTag,
    createFieldLevelEncryptionConfigResponse_fieldLevelEncryption,
    createFieldLevelEncryptionConfigResponse_location,
    createFieldLevelEncryptionConfigResponse_httpStatus,

    -- ** ListInvalidations
    listInvalidations_maxItems,
    listInvalidations_marker,
    listInvalidations_distributionId,
    listInvalidationsResponse_httpStatus,
    listInvalidationsResponse_invalidationList,

    -- ** ListCloudFrontOriginAccessIdentities
    listCloudFrontOriginAccessIdentities_maxItems,
    listCloudFrontOriginAccessIdentities_marker,
    listCloudFrontOriginAccessIdentitiesResponse_httpStatus,
    listCloudFrontOriginAccessIdentitiesResponse_cloudFrontOriginAccessIdentityList,

    -- ** ListTagsForResource
    listTagsForResource_resource,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** GetStreamingDistributionConfig
    getStreamingDistributionConfig_id,
    getStreamingDistributionConfigResponse_eTag,
    getStreamingDistributionConfigResponse_streamingDistributionConfig,
    getStreamingDistributionConfigResponse_httpStatus,

    -- ** GetCachePolicy
    getCachePolicy_id,
    getCachePolicyResponse_eTag,
    getCachePolicyResponse_cachePolicy,
    getCachePolicyResponse_httpStatus,

    -- ** CreateCloudFrontOriginAccessIdentity
    createCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig,
    createCloudFrontOriginAccessIdentityResponse_eTag,
    createCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity,
    createCloudFrontOriginAccessIdentityResponse_location,
    createCloudFrontOriginAccessIdentityResponse_httpStatus,

    -- ** CreatePublicKey
    createPublicKey_publicKeyConfig,
    createPublicKeyResponse_eTag,
    createPublicKeyResponse_publicKey,
    createPublicKeyResponse_location,
    createPublicKeyResponse_httpStatus,

    -- * Types

    -- ** ActiveTrustedKeyGroups
    activeTrustedKeyGroups_items,
    activeTrustedKeyGroups_enabled,
    activeTrustedKeyGroups_quantity,

    -- ** ActiveTrustedSigners
    activeTrustedSigners_items,
    activeTrustedSigners_enabled,
    activeTrustedSigners_quantity,

    -- ** AliasICPRecordal
    aliasICPRecordal_iCPRecordalStatus,
    aliasICPRecordal_cname,

    -- ** Aliases
    aliases_items,
    aliases_quantity,

    -- ** AllowedMethods
    allowedMethods_cachedMethods,
    allowedMethods_quantity,
    allowedMethods_items,

    -- ** CacheBehavior
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

    -- ** CacheBehaviors
    cacheBehaviors_items,
    cacheBehaviors_quantity,

    -- ** CachePolicy
    cachePolicy_id,
    cachePolicy_lastModifiedTime,
    cachePolicy_cachePolicyConfig,

    -- ** CachePolicyConfig
    cachePolicyConfig_comment,
    cachePolicyConfig_maxTTL,
    cachePolicyConfig_parametersInCacheKeyAndForwardedToOrigin,
    cachePolicyConfig_defaultTTL,
    cachePolicyConfig_name,
    cachePolicyConfig_minTTL,

    -- ** CachePolicyCookiesConfig
    cachePolicyCookiesConfig_cookies,
    cachePolicyCookiesConfig_cookieBehavior,

    -- ** CachePolicyHeadersConfig
    cachePolicyHeadersConfig_headers,
    cachePolicyHeadersConfig_headerBehavior,

    -- ** CachePolicyList
    cachePolicyList_items,
    cachePolicyList_nextMarker,
    cachePolicyList_maxItems,
    cachePolicyList_quantity,

    -- ** CachePolicyQueryStringsConfig
    cachePolicyQueryStringsConfig_queryStrings,
    cachePolicyQueryStringsConfig_queryStringBehavior,

    -- ** CachePolicySummary
    cachePolicySummary_type,
    cachePolicySummary_cachePolicy,

    -- ** CachedMethods
    cachedMethods_quantity,
    cachedMethods_items,

    -- ** CloudFrontOriginAccessIdentity
    cloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig,
    cloudFrontOriginAccessIdentity_id,
    cloudFrontOriginAccessIdentity_s3CanonicalUserId,

    -- ** CloudFrontOriginAccessIdentityConfig
    cloudFrontOriginAccessIdentityConfig_callerReference,
    cloudFrontOriginAccessIdentityConfig_comment,

    -- ** CloudFrontOriginAccessIdentityList
    cloudFrontOriginAccessIdentityList_items,
    cloudFrontOriginAccessIdentityList_nextMarker,
    cloudFrontOriginAccessIdentityList_marker,
    cloudFrontOriginAccessIdentityList_maxItems,
    cloudFrontOriginAccessIdentityList_isTruncated,
    cloudFrontOriginAccessIdentityList_quantity,

    -- ** CloudFrontOriginAccessIdentitySummary
    cloudFrontOriginAccessIdentitySummary_id,
    cloudFrontOriginAccessIdentitySummary_s3CanonicalUserId,
    cloudFrontOriginAccessIdentitySummary_comment,

    -- ** ContentTypeProfile
    contentTypeProfile_profileId,
    contentTypeProfile_format,
    contentTypeProfile_contentType,

    -- ** ContentTypeProfileConfig
    contentTypeProfileConfig_contentTypeProfiles,
    contentTypeProfileConfig_forwardWhenContentTypeIsUnknown,

    -- ** ContentTypeProfiles
    contentTypeProfiles_items,
    contentTypeProfiles_quantity,

    -- ** CookieNames
    cookieNames_items,
    cookieNames_quantity,

    -- ** CookiePreference
    cookiePreference_whitelistedNames,
    cookiePreference_forward,

    -- ** CustomErrorResponse
    customErrorResponse_errorCachingMinTTL,
    customErrorResponse_responseCode,
    customErrorResponse_responsePagePath,
    customErrorResponse_errorCode,

    -- ** CustomErrorResponses
    customErrorResponses_items,
    customErrorResponses_quantity,

    -- ** CustomHeaders
    customHeaders_items,
    customHeaders_quantity,

    -- ** CustomOriginConfig
    customOriginConfig_originKeepaliveTimeout,
    customOriginConfig_originSslProtocols,
    customOriginConfig_originReadTimeout,
    customOriginConfig_hTTPPort,
    customOriginConfig_hTTPSPort,
    customOriginConfig_originProtocolPolicy,

    -- ** DefaultCacheBehavior
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

    -- ** Distribution
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

    -- ** DistributionConfig
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

    -- ** DistributionConfigWithTags
    distributionConfigWithTags_distributionConfig,
    distributionConfigWithTags_tags,

    -- ** DistributionIdList
    distributionIdList_items,
    distributionIdList_nextMarker,
    distributionIdList_marker,
    distributionIdList_maxItems,
    distributionIdList_isTruncated,
    distributionIdList_quantity,

    -- ** DistributionList
    distributionList_items,
    distributionList_nextMarker,
    distributionList_marker,
    distributionList_maxItems,
    distributionList_isTruncated,
    distributionList_quantity,

    -- ** DistributionSummary
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

    -- ** EncryptionEntities
    encryptionEntities_items,
    encryptionEntities_quantity,

    -- ** EncryptionEntity
    encryptionEntity_publicKeyId,
    encryptionEntity_providerId,
    encryptionEntity_fieldPatterns,

    -- ** EndPoint
    endPoint_kinesisStreamConfig,
    endPoint_streamType,

    -- ** FieldLevelEncryption
    fieldLevelEncryption_id,
    fieldLevelEncryption_lastModifiedTime,
    fieldLevelEncryption_fieldLevelEncryptionConfig,

    -- ** FieldLevelEncryptionConfig
    fieldLevelEncryptionConfig_comment,
    fieldLevelEncryptionConfig_contentTypeProfileConfig,
    fieldLevelEncryptionConfig_queryArgProfileConfig,
    fieldLevelEncryptionConfig_callerReference,

    -- ** FieldLevelEncryptionList
    fieldLevelEncryptionList_items,
    fieldLevelEncryptionList_nextMarker,
    fieldLevelEncryptionList_maxItems,
    fieldLevelEncryptionList_quantity,

    -- ** FieldLevelEncryptionProfile
    fieldLevelEncryptionProfile_id,
    fieldLevelEncryptionProfile_lastModifiedTime,
    fieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig,

    -- ** FieldLevelEncryptionProfileConfig
    fieldLevelEncryptionProfileConfig_comment,
    fieldLevelEncryptionProfileConfig_name,
    fieldLevelEncryptionProfileConfig_callerReference,
    fieldLevelEncryptionProfileConfig_encryptionEntities,

    -- ** FieldLevelEncryptionProfileList
    fieldLevelEncryptionProfileList_items,
    fieldLevelEncryptionProfileList_nextMarker,
    fieldLevelEncryptionProfileList_maxItems,
    fieldLevelEncryptionProfileList_quantity,

    -- ** FieldLevelEncryptionProfileSummary
    fieldLevelEncryptionProfileSummary_comment,
    fieldLevelEncryptionProfileSummary_id,
    fieldLevelEncryptionProfileSummary_lastModifiedTime,
    fieldLevelEncryptionProfileSummary_name,
    fieldLevelEncryptionProfileSummary_encryptionEntities,

    -- ** FieldLevelEncryptionSummary
    fieldLevelEncryptionSummary_comment,
    fieldLevelEncryptionSummary_contentTypeProfileConfig,
    fieldLevelEncryptionSummary_queryArgProfileConfig,
    fieldLevelEncryptionSummary_id,
    fieldLevelEncryptionSummary_lastModifiedTime,

    -- ** FieldPatterns
    fieldPatterns_items,
    fieldPatterns_quantity,

    -- ** ForwardedValues
    forwardedValues_queryStringCacheKeys,
    forwardedValues_headers,
    forwardedValues_queryString,
    forwardedValues_cookies,

    -- ** GeoRestriction
    geoRestriction_items,
    geoRestriction_restrictionType,
    geoRestriction_quantity,

    -- ** Headers
    headers_items,
    headers_quantity,

    -- ** Invalidation
    invalidation_id,
    invalidation_status,
    invalidation_createTime,
    invalidation_invalidationBatch,

    -- ** InvalidationBatch
    invalidationBatch_paths,
    invalidationBatch_callerReference,

    -- ** InvalidationList
    invalidationList_items,
    invalidationList_nextMarker,
    invalidationList_marker,
    invalidationList_maxItems,
    invalidationList_isTruncated,
    invalidationList_quantity,

    -- ** InvalidationSummary
    invalidationSummary_id,
    invalidationSummary_createTime,
    invalidationSummary_status,

    -- ** KGKeyPairIds
    kGKeyPairIds_keyPairIds,
    kGKeyPairIds_keyGroupId,

    -- ** KeyGroup
    keyGroup_id,
    keyGroup_lastModifiedTime,
    keyGroup_keyGroupConfig,

    -- ** KeyGroupConfig
    keyGroupConfig_comment,
    keyGroupConfig_name,
    keyGroupConfig_items,

    -- ** KeyGroupList
    keyGroupList_items,
    keyGroupList_nextMarker,
    keyGroupList_maxItems,
    keyGroupList_quantity,

    -- ** KeyGroupSummary
    keyGroupSummary_keyGroup,

    -- ** KeyPairIds
    keyPairIds_items,
    keyPairIds_quantity,

    -- ** KinesisStreamConfig
    kinesisStreamConfig_roleARN,
    kinesisStreamConfig_streamARN,

    -- ** LambdaFunctionAssociation
    lambdaFunctionAssociation_includeBody,
    lambdaFunctionAssociation_lambdaFunctionARN,
    lambdaFunctionAssociation_eventType,

    -- ** LambdaFunctionAssociations
    lambdaFunctionAssociations_items,
    lambdaFunctionAssociations_quantity,

    -- ** LoggingConfig
    loggingConfig_enabled,
    loggingConfig_includeCookies,
    loggingConfig_bucket,
    loggingConfig_prefix,

    -- ** MonitoringSubscription
    monitoringSubscription_realtimeMetricsSubscriptionConfig,

    -- ** Origin
    origin_originPath,
    origin_connectionAttempts,
    origin_connectionTimeout,
    origin_customHeaders,
    origin_s3OriginConfig,
    origin_originShield,
    origin_customOriginConfig,
    origin_id,
    origin_domainName,

    -- ** OriginCustomHeader
    originCustomHeader_headerName,
    originCustomHeader_headerValue,

    -- ** OriginGroup
    originGroup_id,
    originGroup_failoverCriteria,
    originGroup_members,

    -- ** OriginGroupFailoverCriteria
    originGroupFailoverCriteria_statusCodes,

    -- ** OriginGroupMember
    originGroupMember_originId,

    -- ** OriginGroupMembers
    originGroupMembers_quantity,
    originGroupMembers_items,

    -- ** OriginGroups
    originGroups_items,
    originGroups_quantity,

    -- ** OriginRequestPolicy
    originRequestPolicy_id,
    originRequestPolicy_lastModifiedTime,
    originRequestPolicy_originRequestPolicyConfig,

    -- ** OriginRequestPolicyConfig
    originRequestPolicyConfig_comment,
    originRequestPolicyConfig_name,
    originRequestPolicyConfig_headersConfig,
    originRequestPolicyConfig_cookiesConfig,
    originRequestPolicyConfig_queryStringsConfig,

    -- ** OriginRequestPolicyCookiesConfig
    originRequestPolicyCookiesConfig_cookies,
    originRequestPolicyCookiesConfig_cookieBehavior,

    -- ** OriginRequestPolicyHeadersConfig
    originRequestPolicyHeadersConfig_headers,
    originRequestPolicyHeadersConfig_headerBehavior,

    -- ** OriginRequestPolicyList
    originRequestPolicyList_items,
    originRequestPolicyList_nextMarker,
    originRequestPolicyList_maxItems,
    originRequestPolicyList_quantity,

    -- ** OriginRequestPolicyQueryStringsConfig
    originRequestPolicyQueryStringsConfig_queryStrings,
    originRequestPolicyQueryStringsConfig_queryStringBehavior,

    -- ** OriginRequestPolicySummary
    originRequestPolicySummary_type,
    originRequestPolicySummary_originRequestPolicy,

    -- ** OriginShield
    originShield_originShieldRegion,
    originShield_enabled,

    -- ** OriginSslProtocols
    originSslProtocols_quantity,
    originSslProtocols_items,

    -- ** Origins
    origins_quantity,
    origins_items,

    -- ** ParametersInCacheKeyAndForwardedToOrigin
    parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingBrotli,
    parametersInCacheKeyAndForwardedToOrigin_enableAcceptEncodingGzip,
    parametersInCacheKeyAndForwardedToOrigin_headersConfig,
    parametersInCacheKeyAndForwardedToOrigin_cookiesConfig,
    parametersInCacheKeyAndForwardedToOrigin_queryStringsConfig,

    -- ** Paths
    paths_items,
    paths_quantity,

    -- ** PublicKey
    publicKey_id,
    publicKey_createdTime,
    publicKey_publicKeyConfig,

    -- ** PublicKeyConfig
    publicKeyConfig_comment,
    publicKeyConfig_callerReference,
    publicKeyConfig_name,
    publicKeyConfig_encodedKey,

    -- ** PublicKeyList
    publicKeyList_items,
    publicKeyList_nextMarker,
    publicKeyList_maxItems,
    publicKeyList_quantity,

    -- ** PublicKeySummary
    publicKeySummary_comment,
    publicKeySummary_id,
    publicKeySummary_name,
    publicKeySummary_createdTime,
    publicKeySummary_encodedKey,

    -- ** QueryArgProfile
    queryArgProfile_queryArg,
    queryArgProfile_profileId,

    -- ** QueryArgProfileConfig
    queryArgProfileConfig_queryArgProfiles,
    queryArgProfileConfig_forwardWhenQueryArgProfileIsUnknown,

    -- ** QueryArgProfiles
    queryArgProfiles_items,
    queryArgProfiles_quantity,

    -- ** QueryStringCacheKeys
    queryStringCacheKeys_items,
    queryStringCacheKeys_quantity,

    -- ** QueryStringNames
    queryStringNames_items,
    queryStringNames_quantity,

    -- ** RealtimeLogConfig
    realtimeLogConfig_arn,
    realtimeLogConfig_name,
    realtimeLogConfig_samplingRate,
    realtimeLogConfig_endPoints,
    realtimeLogConfig_fields,

    -- ** RealtimeLogConfigs
    realtimeLogConfigs_items,
    realtimeLogConfigs_nextMarker,
    realtimeLogConfigs_maxItems,
    realtimeLogConfigs_isTruncated,
    realtimeLogConfigs_marker,

    -- ** RealtimeMetricsSubscriptionConfig
    realtimeMetricsSubscriptionConfig_realtimeMetricsSubscriptionStatus,

    -- ** Restrictions
    restrictions_geoRestriction,

    -- ** S3Origin
    s3Origin_domainName,
    s3Origin_originAccessIdentity,

    -- ** S3OriginConfig
    s3OriginConfig_originAccessIdentity,

    -- ** Signer
    signer_awsAccountNumber,
    signer_keyPairIds,

    -- ** StatusCodes
    statusCodes_quantity,
    statusCodes_items,

    -- ** StreamingDistribution
    streamingDistribution_lastModifiedTime,
    streamingDistribution_id,
    streamingDistribution_arn,
    streamingDistribution_status,
    streamingDistribution_domainName,
    streamingDistribution_activeTrustedSigners,
    streamingDistribution_streamingDistributionConfig,

    -- ** StreamingDistributionConfig
    streamingDistributionConfig_priceClass,
    streamingDistributionConfig_logging,
    streamingDistributionConfig_aliases,
    streamingDistributionConfig_callerReference,
    streamingDistributionConfig_s3Origin,
    streamingDistributionConfig_comment,
    streamingDistributionConfig_trustedSigners,
    streamingDistributionConfig_enabled,

    -- ** StreamingDistributionConfigWithTags
    streamingDistributionConfigWithTags_streamingDistributionConfig,
    streamingDistributionConfigWithTags_tags,

    -- ** StreamingDistributionList
    streamingDistributionList_items,
    streamingDistributionList_nextMarker,
    streamingDistributionList_marker,
    streamingDistributionList_maxItems,
    streamingDistributionList_isTruncated,
    streamingDistributionList_quantity,

    -- ** StreamingDistributionSummary
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

    -- ** StreamingLoggingConfig
    streamingLoggingConfig_enabled,
    streamingLoggingConfig_bucket,
    streamingLoggingConfig_prefix,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TagKeys
    tagKeys_items,

    -- ** Tags
    tags_items,

    -- ** TrustedKeyGroups
    trustedKeyGroups_items,
    trustedKeyGroups_enabled,
    trustedKeyGroups_quantity,

    -- ** TrustedSigners
    trustedSigners_items,
    trustedSigners_enabled,
    trustedSigners_quantity,

    -- ** ViewerCertificate
    viewerCertificate_sSLSupportMethod,
    viewerCertificate_cloudFrontDefaultCertificate,
    viewerCertificate_iAMCertificateId,
    viewerCertificate_aCMCertificateArn,
    viewerCertificate_certificateSource,
    viewerCertificate_certificate,
    viewerCertificate_minimumProtocolVersion,
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
import Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
import Network.AWS.CloudFront.Types.ActiveTrustedSigners
import Network.AWS.CloudFront.Types.AliasICPRecordal
import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.AllowedMethods
import Network.AWS.CloudFront.Types.CacheBehavior
import Network.AWS.CloudFront.Types.CacheBehaviors
import Network.AWS.CloudFront.Types.CachePolicy
import Network.AWS.CloudFront.Types.CachePolicyConfig
import Network.AWS.CloudFront.Types.CachePolicyCookiesConfig
import Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
import Network.AWS.CloudFront.Types.CachePolicyList
import Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
import Network.AWS.CloudFront.Types.CachePolicySummary
import Network.AWS.CloudFront.Types.CachedMethods
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
import Network.AWS.CloudFront.Types.FieldLevelEncryption
import Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
import Network.AWS.CloudFront.Types.FieldLevelEncryptionList
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfile
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary
import Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
import Network.AWS.CloudFront.Types.FieldPatterns
import Network.AWS.CloudFront.Types.ForwardedValues
import Network.AWS.CloudFront.Types.GeoRestriction
import Network.AWS.CloudFront.Types.Headers
import Network.AWS.CloudFront.Types.Invalidation
import Network.AWS.CloudFront.Types.InvalidationBatch
import Network.AWS.CloudFront.Types.InvalidationList
import Network.AWS.CloudFront.Types.InvalidationSummary
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
import Network.AWS.CloudFront.Types.MonitoringSubscription
import Network.AWS.CloudFront.Types.Origin
import Network.AWS.CloudFront.Types.OriginCustomHeader
import Network.AWS.CloudFront.Types.OriginGroup
import Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
import Network.AWS.CloudFront.Types.OriginGroupMember
import Network.AWS.CloudFront.Types.OriginGroupMembers
import Network.AWS.CloudFront.Types.OriginGroups
import Network.AWS.CloudFront.Types.OriginRequestPolicy
import Network.AWS.CloudFront.Types.OriginRequestPolicyConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicyList
import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
import Network.AWS.CloudFront.Types.OriginRequestPolicySummary
import Network.AWS.CloudFront.Types.OriginShield
import Network.AWS.CloudFront.Types.OriginSslProtocols
import Network.AWS.CloudFront.Types.Origins
import Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
import Network.AWS.CloudFront.Types.Paths
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
import Network.AWS.CloudFront.Types.Restrictions
import Network.AWS.CloudFront.Types.S3Origin
import Network.AWS.CloudFront.Types.S3OriginConfig
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
