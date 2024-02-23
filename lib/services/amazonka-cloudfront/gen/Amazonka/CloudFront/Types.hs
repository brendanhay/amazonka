{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFront.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDenied,
    _BatchTooLarge,
    _CNAMEAlreadyExists,
    _CachePolicyAlreadyExists,
    _CachePolicyInUse,
    _CannotChangeImmutablePublicKeyFields,
    _CloudFrontOriginAccessIdentityAlreadyExists,
    _CloudFrontOriginAccessIdentityInUse,
    _ContinuousDeploymentPolicyAlreadyExists,
    _ContinuousDeploymentPolicyInUse,
    _DistributionAlreadyExists,
    _DistributionNotDisabled,
    _FieldLevelEncryptionConfigAlreadyExists,
    _FieldLevelEncryptionConfigInUse,
    _FieldLevelEncryptionProfileAlreadyExists,
    _FieldLevelEncryptionProfileInUse,
    _FieldLevelEncryptionProfileSizeExceeded,
    _FunctionAlreadyExists,
    _FunctionInUse,
    _FunctionSizeLimitExceeded,
    _IllegalDelete,
    _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior,
    _IllegalOriginAccessConfiguration,
    _IllegalUpdate,
    _InconsistentQuantities,
    _InvalidArgument,
    _InvalidDefaultRootObject,
    _InvalidDomainNameForOriginAccessControl,
    _InvalidErrorCode,
    _InvalidForwardCookies,
    _InvalidFunctionAssociation,
    _InvalidGeoRestrictionParameter,
    _InvalidHeadersForS3Origin,
    _InvalidIfMatchVersion,
    _InvalidLambdaFunctionAssociation,
    _InvalidLocationCode,
    _InvalidMinimumProtocolVersion,
    _InvalidOrigin,
    _InvalidOriginAccessControl,
    _InvalidOriginAccessIdentity,
    _InvalidOriginKeepaliveTimeout,
    _InvalidOriginReadTimeout,
    _InvalidProtocolSettings,
    _InvalidQueryStringParameters,
    _InvalidRelativePath,
    _InvalidRequiredProtocol,
    _InvalidResponseCode,
    _InvalidTTLOrder,
    _InvalidTagging,
    _InvalidViewerCertificate,
    _InvalidWebACLId,
    _KeyGroupAlreadyExists,
    _MissingBody,
    _MonitoringSubscriptionAlreadyExists,
    _NoSuchCachePolicy,
    _NoSuchCloudFrontOriginAccessIdentity,
    _NoSuchContinuousDeploymentPolicy,
    _NoSuchDistribution,
    _NoSuchFieldLevelEncryptionConfig,
    _NoSuchFieldLevelEncryptionProfile,
    _NoSuchFunctionExists,
    _NoSuchInvalidation,
    _NoSuchMonitoringSubscription,
    _NoSuchOrigin,
    _NoSuchOriginAccessControl,
    _NoSuchOriginRequestPolicy,
    _NoSuchPublicKey,
    _NoSuchRealtimeLogConfig,
    _NoSuchResource,
    _NoSuchResponseHeadersPolicy,
    _NoSuchStreamingDistribution,
    _OriginAccessControlAlreadyExists,
    _OriginAccessControlInUse,
    _OriginRequestPolicyAlreadyExists,
    _OriginRequestPolicyInUse,
    _PreconditionFailed,
    _PublicKeyAlreadyExists,
    _PublicKeyInUse,
    _QueryArgProfileEmpty,
    _RealtimeLogConfigAlreadyExists,
    _RealtimeLogConfigInUse,
    _RealtimeLogConfigOwnerMismatch,
    _ResourceInUse,
    _ResponseHeadersPolicyAlreadyExists,
    _ResponseHeadersPolicyInUse,
    _StagingDistributionInUse,
    _StreamingDistributionAlreadyExists,
    _StreamingDistributionNotDisabled,
    _TestFunctionFailed,
    _TooLongCSPInResponseHeadersPolicy,
    _TooManyCacheBehaviors,
    _TooManyCachePolicies,
    _TooManyCertificates,
    _TooManyCloudFrontOriginAccessIdentities,
    _TooManyContinuousDeploymentPolicies,
    _TooManyCookieNamesInWhiteList,
    _TooManyCookiesInCachePolicy,
    _TooManyCookiesInOriginRequestPolicy,
    _TooManyCustomHeadersInResponseHeadersPolicy,
    _TooManyDistributionCNAMEs,
    _TooManyDistributions,
    _TooManyDistributionsAssociatedToCachePolicy,
    _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig,
    _TooManyDistributionsAssociatedToKeyGroup,
    _TooManyDistributionsAssociatedToOriginAccessControl,
    _TooManyDistributionsAssociatedToOriginRequestPolicy,
    _TooManyDistributionsAssociatedToResponseHeadersPolicy,
    _TooManyDistributionsWithFunctionAssociations,
    _TooManyDistributionsWithLambdaAssociations,
    _TooManyDistributionsWithSingleFunctionARN,
    _TooManyFieldLevelEncryptionConfigs,
    _TooManyFieldLevelEncryptionContentTypeProfiles,
    _TooManyFieldLevelEncryptionEncryptionEntities,
    _TooManyFieldLevelEncryptionFieldPatterns,
    _TooManyFieldLevelEncryptionProfiles,
    _TooManyFieldLevelEncryptionQueryArgProfiles,
    _TooManyFunctionAssociations,
    _TooManyFunctions,
    _TooManyHeadersInCachePolicy,
    _TooManyHeadersInForwardedValues,
    _TooManyHeadersInOriginRequestPolicy,
    _TooManyInvalidationsInProgress,
    _TooManyKeyGroups,
    _TooManyKeyGroupsAssociatedToDistribution,
    _TooManyLambdaFunctionAssociations,
    _TooManyOriginAccessControls,
    _TooManyOriginCustomHeaders,
    _TooManyOriginGroupsPerDistribution,
    _TooManyOriginRequestPolicies,
    _TooManyOrigins,
    _TooManyPublicKeys,
    _TooManyPublicKeysInKeyGroup,
    _TooManyQueryStringParameters,
    _TooManyQueryStringsInCachePolicy,
    _TooManyQueryStringsInOriginRequestPolicy,
    _TooManyRealtimeLogConfigs,
    _TooManyRemoveHeadersInResponseHeadersPolicy,
    _TooManyResponseHeadersPolicies,
    _TooManyStreamingDistributionCNAMEs,
    _TooManyStreamingDistributions,
    _TooManyTrustedSigners,
    _TrustedKeyGroupDoesNotExist,
    _TrustedSignerDoesNotExist,
    _UnsupportedOperation,

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

    -- * ContinuousDeploymentPolicyType
    ContinuousDeploymentPolicyType (..),

    -- * EventType
    EventType (..),

    -- * Format
    Format (..),

    -- * FrameOptionsList
    FrameOptionsList (..),

    -- * FunctionRuntime
    FunctionRuntime (..),

    -- * FunctionStage
    FunctionStage (..),

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

    -- * OriginAccessControlOriginTypes
    OriginAccessControlOriginTypes (..),

    -- * OriginAccessControlSigningBehaviors
    OriginAccessControlSigningBehaviors (..),

    -- * OriginAccessControlSigningProtocols
    OriginAccessControlSigningProtocols (..),

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

    -- * ReferrerPolicyList
    ReferrerPolicyList (..),

    -- * ResponseHeadersPolicyAccessControlAllowMethodsValues
    ResponseHeadersPolicyAccessControlAllowMethodsValues (..),

    -- * ResponseHeadersPolicyType
    ResponseHeadersPolicyType (..),

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
    aliasICPRecordal_cname,
    aliasICPRecordal_iCPRecordalStatus,

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
    cacheBehavior_allowedMethods,
    cacheBehavior_cachePolicyId,
    cacheBehavior_compress,
    cacheBehavior_defaultTTL,
    cacheBehavior_fieldLevelEncryptionId,
    cacheBehavior_forwardedValues,
    cacheBehavior_functionAssociations,
    cacheBehavior_lambdaFunctionAssociations,
    cacheBehavior_maxTTL,
    cacheBehavior_minTTL,
    cacheBehavior_originRequestPolicyId,
    cacheBehavior_realtimeLogConfigArn,
    cacheBehavior_responseHeadersPolicyId,
    cacheBehavior_smoothStreaming,
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
    cachePolicyConfig_defaultTTL,
    cachePolicyConfig_maxTTL,
    cachePolicyConfig_parametersInCacheKeyAndForwardedToOrigin,
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

    -- * ConflictingAlias
    ConflictingAlias (..),
    newConflictingAlias,
    conflictingAlias_accountId,
    conflictingAlias_alias,
    conflictingAlias_distributionId,

    -- * ConflictingAliasesList
    ConflictingAliasesList (..),
    newConflictingAliasesList,
    conflictingAliasesList_items,
    conflictingAliasesList_maxItems,
    conflictingAliasesList_nextMarker,
    conflictingAliasesList_quantity,

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

    -- * ContinuousDeploymentPolicy
    ContinuousDeploymentPolicy (..),
    newContinuousDeploymentPolicy,
    continuousDeploymentPolicy_id,
    continuousDeploymentPolicy_lastModifiedTime,
    continuousDeploymentPolicy_continuousDeploymentPolicyConfig,

    -- * ContinuousDeploymentPolicyConfig
    ContinuousDeploymentPolicyConfig (..),
    newContinuousDeploymentPolicyConfig,
    continuousDeploymentPolicyConfig_trafficConfig,
    continuousDeploymentPolicyConfig_stagingDistributionDnsNames,
    continuousDeploymentPolicyConfig_enabled,

    -- * ContinuousDeploymentPolicyList
    ContinuousDeploymentPolicyList (..),
    newContinuousDeploymentPolicyList,
    continuousDeploymentPolicyList_items,
    continuousDeploymentPolicyList_nextMarker,
    continuousDeploymentPolicyList_maxItems,
    continuousDeploymentPolicyList_quantity,

    -- * ContinuousDeploymentPolicySummary
    ContinuousDeploymentPolicySummary (..),
    newContinuousDeploymentPolicySummary,
    continuousDeploymentPolicySummary_continuousDeploymentPolicy,

    -- * ContinuousDeploymentSingleHeaderConfig
    ContinuousDeploymentSingleHeaderConfig (..),
    newContinuousDeploymentSingleHeaderConfig,
    continuousDeploymentSingleHeaderConfig_header,
    continuousDeploymentSingleHeaderConfig_value,

    -- * ContinuousDeploymentSingleWeightConfig
    ContinuousDeploymentSingleWeightConfig (..),
    newContinuousDeploymentSingleWeightConfig,
    continuousDeploymentSingleWeightConfig_sessionStickinessConfig,
    continuousDeploymentSingleWeightConfig_weight,

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
    customOriginConfig_originReadTimeout,
    customOriginConfig_originSslProtocols,
    customOriginConfig_hTTPPort,
    customOriginConfig_hTTPSPort,
    customOriginConfig_originProtocolPolicy,

    -- * DefaultCacheBehavior
    DefaultCacheBehavior (..),
    newDefaultCacheBehavior,
    defaultCacheBehavior_allowedMethods,
    defaultCacheBehavior_cachePolicyId,
    defaultCacheBehavior_compress,
    defaultCacheBehavior_defaultTTL,
    defaultCacheBehavior_fieldLevelEncryptionId,
    defaultCacheBehavior_forwardedValues,
    defaultCacheBehavior_functionAssociations,
    defaultCacheBehavior_lambdaFunctionAssociations,
    defaultCacheBehavior_maxTTL,
    defaultCacheBehavior_minTTL,
    defaultCacheBehavior_originRequestPolicyId,
    defaultCacheBehavior_realtimeLogConfigArn,
    defaultCacheBehavior_responseHeadersPolicyId,
    defaultCacheBehavior_smoothStreaming,
    defaultCacheBehavior_trustedKeyGroups,
    defaultCacheBehavior_trustedSigners,
    defaultCacheBehavior_targetOriginId,
    defaultCacheBehavior_viewerProtocolPolicy,

    -- * Distribution
    Distribution (..),
    newDistribution,
    distribution_activeTrustedKeyGroups,
    distribution_activeTrustedSigners,
    distribution_aliasICPRecordals,
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
    distributionConfig_aliases,
    distributionConfig_cacheBehaviors,
    distributionConfig_continuousDeploymentPolicyId,
    distributionConfig_customErrorResponses,
    distributionConfig_defaultRootObject,
    distributionConfig_httpVersion,
    distributionConfig_isIPV6Enabled,
    distributionConfig_logging,
    distributionConfig_originGroups,
    distributionConfig_priceClass,
    distributionConfig_restrictions,
    distributionConfig_staging,
    distributionConfig_viewerCertificate,
    distributionConfig_webACLId,
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
    distributionSummary_staging,

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
    forwardedValues_headers,
    forwardedValues_queryStringCacheKeys,
    forwardedValues_queryString,
    forwardedValues_cookies,

    -- * FunctionAssociation
    FunctionAssociation (..),
    newFunctionAssociation,
    functionAssociation_functionARN,
    functionAssociation_eventType,

    -- * FunctionAssociations
    FunctionAssociations (..),
    newFunctionAssociations,
    functionAssociations_items,
    functionAssociations_quantity,

    -- * FunctionConfig
    FunctionConfig (..),
    newFunctionConfig,
    functionConfig_comment,
    functionConfig_runtime,

    -- * FunctionList
    FunctionList (..),
    newFunctionList,
    functionList_items,
    functionList_nextMarker,
    functionList_maxItems,
    functionList_quantity,

    -- * FunctionMetadata
    FunctionMetadata (..),
    newFunctionMetadata,
    functionMetadata_createdTime,
    functionMetadata_stage,
    functionMetadata_functionARN,
    functionMetadata_lastModifiedTime,

    -- * FunctionSummary
    FunctionSummary (..),
    newFunctionSummary,
    functionSummary_status,
    functionSummary_name,
    functionSummary_functionConfig,
    functionSummary_functionMetadata,

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
    kGKeyPairIds_keyGroupId,
    kGKeyPairIds_keyPairIds,

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
    origin_connectionAttempts,
    origin_connectionTimeout,
    origin_customHeaders,
    origin_customOriginConfig,
    origin_originAccessControlId,
    origin_originPath,
    origin_originShield,
    origin_s3OriginConfig,
    origin_id,
    origin_domainName,

    -- * OriginAccessControl
    OriginAccessControl (..),
    newOriginAccessControl,
    originAccessControl_originAccessControlConfig,
    originAccessControl_id,

    -- * OriginAccessControlConfig
    OriginAccessControlConfig (..),
    newOriginAccessControlConfig,
    originAccessControlConfig_description,
    originAccessControlConfig_name,
    originAccessControlConfig_signingProtocol,
    originAccessControlConfig_signingBehavior,
    originAccessControlConfig_originAccessControlOriginType,

    -- * OriginAccessControlList
    OriginAccessControlList (..),
    newOriginAccessControlList,
    originAccessControlList_items,
    originAccessControlList_nextMarker,
    originAccessControlList_marker,
    originAccessControlList_maxItems,
    originAccessControlList_isTruncated,
    originAccessControlList_quantity,

    -- * OriginAccessControlSummary
    OriginAccessControlSummary (..),
    newOriginAccessControlSummary,
    originAccessControlSummary_id,
    originAccessControlSummary_description,
    originAccessControlSummary_name,
    originAccessControlSummary_signingProtocol,
    originAccessControlSummary_signingBehavior,
    originAccessControlSummary_originAccessControlOriginType,

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

    -- * ResponseHeadersPolicy
    ResponseHeadersPolicy (..),
    newResponseHeadersPolicy,
    responseHeadersPolicy_id,
    responseHeadersPolicy_lastModifiedTime,
    responseHeadersPolicy_responseHeadersPolicyConfig,

    -- * ResponseHeadersPolicyAccessControlAllowHeaders
    ResponseHeadersPolicyAccessControlAllowHeaders (..),
    newResponseHeadersPolicyAccessControlAllowHeaders,
    responseHeadersPolicyAccessControlAllowHeaders_quantity,
    responseHeadersPolicyAccessControlAllowHeaders_items,

    -- * ResponseHeadersPolicyAccessControlAllowMethods
    ResponseHeadersPolicyAccessControlAllowMethods (..),
    newResponseHeadersPolicyAccessControlAllowMethods,
    responseHeadersPolicyAccessControlAllowMethods_quantity,
    responseHeadersPolicyAccessControlAllowMethods_items,

    -- * ResponseHeadersPolicyAccessControlAllowOrigins
    ResponseHeadersPolicyAccessControlAllowOrigins (..),
    newResponseHeadersPolicyAccessControlAllowOrigins,
    responseHeadersPolicyAccessControlAllowOrigins_quantity,
    responseHeadersPolicyAccessControlAllowOrigins_items,

    -- * ResponseHeadersPolicyAccessControlExposeHeaders
    ResponseHeadersPolicyAccessControlExposeHeaders (..),
    newResponseHeadersPolicyAccessControlExposeHeaders,
    responseHeadersPolicyAccessControlExposeHeaders_items,
    responseHeadersPolicyAccessControlExposeHeaders_quantity,

    -- * ResponseHeadersPolicyConfig
    ResponseHeadersPolicyConfig (..),
    newResponseHeadersPolicyConfig,
    responseHeadersPolicyConfig_comment,
    responseHeadersPolicyConfig_corsConfig,
    responseHeadersPolicyConfig_customHeadersConfig,
    responseHeadersPolicyConfig_removeHeadersConfig,
    responseHeadersPolicyConfig_securityHeadersConfig,
    responseHeadersPolicyConfig_serverTimingHeadersConfig,
    responseHeadersPolicyConfig_name,

    -- * ResponseHeadersPolicyContentSecurityPolicy
    ResponseHeadersPolicyContentSecurityPolicy (..),
    newResponseHeadersPolicyContentSecurityPolicy,
    responseHeadersPolicyContentSecurityPolicy_override,
    responseHeadersPolicyContentSecurityPolicy_contentSecurityPolicy,

    -- * ResponseHeadersPolicyContentTypeOptions
    ResponseHeadersPolicyContentTypeOptions (..),
    newResponseHeadersPolicyContentTypeOptions,
    responseHeadersPolicyContentTypeOptions_override,

    -- * ResponseHeadersPolicyCorsConfig
    ResponseHeadersPolicyCorsConfig (..),
    newResponseHeadersPolicyCorsConfig,
    responseHeadersPolicyCorsConfig_accessControlExposeHeaders,
    responseHeadersPolicyCorsConfig_accessControlMaxAgeSec,
    responseHeadersPolicyCorsConfig_accessControlAllowOrigins,
    responseHeadersPolicyCorsConfig_accessControlAllowHeaders,
    responseHeadersPolicyCorsConfig_accessControlAllowMethods,
    responseHeadersPolicyCorsConfig_accessControlAllowCredentials,
    responseHeadersPolicyCorsConfig_originOverride,

    -- * ResponseHeadersPolicyCustomHeader
    ResponseHeadersPolicyCustomHeader (..),
    newResponseHeadersPolicyCustomHeader,
    responseHeadersPolicyCustomHeader_header,
    responseHeadersPolicyCustomHeader_value,
    responseHeadersPolicyCustomHeader_override,

    -- * ResponseHeadersPolicyCustomHeadersConfig
    ResponseHeadersPolicyCustomHeadersConfig (..),
    newResponseHeadersPolicyCustomHeadersConfig,
    responseHeadersPolicyCustomHeadersConfig_items,
    responseHeadersPolicyCustomHeadersConfig_quantity,

    -- * ResponseHeadersPolicyFrameOptions
    ResponseHeadersPolicyFrameOptions (..),
    newResponseHeadersPolicyFrameOptions,
    responseHeadersPolicyFrameOptions_override,
    responseHeadersPolicyFrameOptions_frameOption,

    -- * ResponseHeadersPolicyList
    ResponseHeadersPolicyList (..),
    newResponseHeadersPolicyList,
    responseHeadersPolicyList_items,
    responseHeadersPolicyList_nextMarker,
    responseHeadersPolicyList_maxItems,
    responseHeadersPolicyList_quantity,

    -- * ResponseHeadersPolicyReferrerPolicy
    ResponseHeadersPolicyReferrerPolicy (..),
    newResponseHeadersPolicyReferrerPolicy,
    responseHeadersPolicyReferrerPolicy_override,
    responseHeadersPolicyReferrerPolicy_referrerPolicy,

    -- * ResponseHeadersPolicyRemoveHeader
    ResponseHeadersPolicyRemoveHeader (..),
    newResponseHeadersPolicyRemoveHeader,
    responseHeadersPolicyRemoveHeader_header,

    -- * ResponseHeadersPolicyRemoveHeadersConfig
    ResponseHeadersPolicyRemoveHeadersConfig (..),
    newResponseHeadersPolicyRemoveHeadersConfig,
    responseHeadersPolicyRemoveHeadersConfig_items,
    responseHeadersPolicyRemoveHeadersConfig_quantity,

    -- * ResponseHeadersPolicySecurityHeadersConfig
    ResponseHeadersPolicySecurityHeadersConfig (..),
    newResponseHeadersPolicySecurityHeadersConfig,
    responseHeadersPolicySecurityHeadersConfig_contentSecurityPolicy,
    responseHeadersPolicySecurityHeadersConfig_contentTypeOptions,
    responseHeadersPolicySecurityHeadersConfig_frameOptions,
    responseHeadersPolicySecurityHeadersConfig_referrerPolicy,
    responseHeadersPolicySecurityHeadersConfig_strictTransportSecurity,
    responseHeadersPolicySecurityHeadersConfig_xSSProtection,

    -- * ResponseHeadersPolicyServerTimingHeadersConfig
    ResponseHeadersPolicyServerTimingHeadersConfig (..),
    newResponseHeadersPolicyServerTimingHeadersConfig,
    responseHeadersPolicyServerTimingHeadersConfig_samplingRate,
    responseHeadersPolicyServerTimingHeadersConfig_enabled,

    -- * ResponseHeadersPolicyStrictTransportSecurity
    ResponseHeadersPolicyStrictTransportSecurity (..),
    newResponseHeadersPolicyStrictTransportSecurity,
    responseHeadersPolicyStrictTransportSecurity_includeSubdomains,
    responseHeadersPolicyStrictTransportSecurity_preload,
    responseHeadersPolicyStrictTransportSecurity_override,
    responseHeadersPolicyStrictTransportSecurity_accessControlMaxAgeSec,

    -- * ResponseHeadersPolicySummary
    ResponseHeadersPolicySummary (..),
    newResponseHeadersPolicySummary,
    responseHeadersPolicySummary_type,
    responseHeadersPolicySummary_responseHeadersPolicy,

    -- * ResponseHeadersPolicyXSSProtection
    ResponseHeadersPolicyXSSProtection (..),
    newResponseHeadersPolicyXSSProtection,
    responseHeadersPolicyXSSProtection_modeBlock,
    responseHeadersPolicyXSSProtection_reportUri,
    responseHeadersPolicyXSSProtection_override,
    responseHeadersPolicyXSSProtection_protection,

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

    -- * SessionStickinessConfig
    SessionStickinessConfig (..),
    newSessionStickinessConfig,
    sessionStickinessConfig_idleTTL,
    sessionStickinessConfig_maximumTTL,

    -- * Signer
    Signer (..),
    newSigner,
    signer_awsAccountNumber,
    signer_keyPairIds,

    -- * StagingDistributionDnsNames
    StagingDistributionDnsNames (..),
    newStagingDistributionDnsNames,
    stagingDistributionDnsNames_items,
    stagingDistributionDnsNames_quantity,

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
    streamingDistributionConfig_aliases,
    streamingDistributionConfig_logging,
    streamingDistributionConfig_priceClass,
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

    -- * TestResult
    TestResult (..),
    newTestResult,
    testResult_computeUtilization,
    testResult_functionErrorMessage,
    testResult_functionExecutionLogs,
    testResult_functionOutput,
    testResult_functionSummary,

    -- * TrafficConfig
    TrafficConfig (..),
    newTrafficConfig,
    trafficConfig_singleHeaderConfig,
    trafficConfig_singleWeightConfig,
    trafficConfig_type,

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
    viewerCertificate_aCMCertificateArn,
    viewerCertificate_certificate,
    viewerCertificate_certificateSource,
    viewerCertificate_cloudFrontDefaultCertificate,
    viewerCertificate_iAMCertificateId,
    viewerCertificate_minimumProtocolVersion,
    viewerCertificate_sSLSupportMethod,
  )
where

import Amazonka.CloudFront.Types.ActiveTrustedKeyGroups
import Amazonka.CloudFront.Types.ActiveTrustedSigners
import Amazonka.CloudFront.Types.AliasICPRecordal
import Amazonka.CloudFront.Types.Aliases
import Amazonka.CloudFront.Types.AllowedMethods
import Amazonka.CloudFront.Types.CacheBehavior
import Amazonka.CloudFront.Types.CacheBehaviors
import Amazonka.CloudFront.Types.CachePolicy
import Amazonka.CloudFront.Types.CachePolicyConfig
import Amazonka.CloudFront.Types.CachePolicyCookieBehavior
import Amazonka.CloudFront.Types.CachePolicyCookiesConfig
import Amazonka.CloudFront.Types.CachePolicyHeaderBehavior
import Amazonka.CloudFront.Types.CachePolicyHeadersConfig
import Amazonka.CloudFront.Types.CachePolicyList
import Amazonka.CloudFront.Types.CachePolicyQueryStringBehavior
import Amazonka.CloudFront.Types.CachePolicyQueryStringsConfig
import Amazonka.CloudFront.Types.CachePolicySummary
import Amazonka.CloudFront.Types.CachePolicyType
import Amazonka.CloudFront.Types.CachedMethods
import Amazonka.CloudFront.Types.CertificateSource
import Amazonka.CloudFront.Types.CloudFrontOriginAccessIdentity
import Amazonka.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
import Amazonka.CloudFront.Types.CloudFrontOriginAccessIdentityList
import Amazonka.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
import Amazonka.CloudFront.Types.ConflictingAlias
import Amazonka.CloudFront.Types.ConflictingAliasesList
import Amazonka.CloudFront.Types.ContentTypeProfile
import Amazonka.CloudFront.Types.ContentTypeProfileConfig
import Amazonka.CloudFront.Types.ContentTypeProfiles
import Amazonka.CloudFront.Types.ContinuousDeploymentPolicy
import Amazonka.CloudFront.Types.ContinuousDeploymentPolicyConfig
import Amazonka.CloudFront.Types.ContinuousDeploymentPolicyList
import Amazonka.CloudFront.Types.ContinuousDeploymentPolicySummary
import Amazonka.CloudFront.Types.ContinuousDeploymentPolicyType
import Amazonka.CloudFront.Types.ContinuousDeploymentSingleHeaderConfig
import Amazonka.CloudFront.Types.ContinuousDeploymentSingleWeightConfig
import Amazonka.CloudFront.Types.CookieNames
import Amazonka.CloudFront.Types.CookiePreference
import Amazonka.CloudFront.Types.CustomErrorResponse
import Amazonka.CloudFront.Types.CustomErrorResponses
import Amazonka.CloudFront.Types.CustomHeaders
import Amazonka.CloudFront.Types.CustomOriginConfig
import Amazonka.CloudFront.Types.DefaultCacheBehavior
import Amazonka.CloudFront.Types.Distribution
import Amazonka.CloudFront.Types.DistributionConfig
import Amazonka.CloudFront.Types.DistributionConfigWithTags
import Amazonka.CloudFront.Types.DistributionIdList
import Amazonka.CloudFront.Types.DistributionList
import Amazonka.CloudFront.Types.DistributionSummary
import Amazonka.CloudFront.Types.EncryptionEntities
import Amazonka.CloudFront.Types.EncryptionEntity
import Amazonka.CloudFront.Types.EndPoint
import Amazonka.CloudFront.Types.EventType
import Amazonka.CloudFront.Types.FieldLevelEncryption
import Amazonka.CloudFront.Types.FieldLevelEncryptionConfig
import Amazonka.CloudFront.Types.FieldLevelEncryptionList
import Amazonka.CloudFront.Types.FieldLevelEncryptionProfile
import Amazonka.CloudFront.Types.FieldLevelEncryptionProfileConfig
import Amazonka.CloudFront.Types.FieldLevelEncryptionProfileList
import Amazonka.CloudFront.Types.FieldLevelEncryptionProfileSummary
import Amazonka.CloudFront.Types.FieldLevelEncryptionSummary
import Amazonka.CloudFront.Types.FieldPatterns
import Amazonka.CloudFront.Types.Format
import Amazonka.CloudFront.Types.ForwardedValues
import Amazonka.CloudFront.Types.FrameOptionsList
import Amazonka.CloudFront.Types.FunctionAssociation
import Amazonka.CloudFront.Types.FunctionAssociations
import Amazonka.CloudFront.Types.FunctionConfig
import Amazonka.CloudFront.Types.FunctionList
import Amazonka.CloudFront.Types.FunctionMetadata
import Amazonka.CloudFront.Types.FunctionRuntime
import Amazonka.CloudFront.Types.FunctionStage
import Amazonka.CloudFront.Types.FunctionSummary
import Amazonka.CloudFront.Types.GeoRestriction
import Amazonka.CloudFront.Types.GeoRestrictionType
import Amazonka.CloudFront.Types.Headers
import Amazonka.CloudFront.Types.HttpVersion
import Amazonka.CloudFront.Types.ICPRecordalStatus
import Amazonka.CloudFront.Types.Invalidation
import Amazonka.CloudFront.Types.InvalidationBatch
import Amazonka.CloudFront.Types.InvalidationList
import Amazonka.CloudFront.Types.InvalidationSummary
import Amazonka.CloudFront.Types.ItemSelection
import Amazonka.CloudFront.Types.KGKeyPairIds
import Amazonka.CloudFront.Types.KeyGroup
import Amazonka.CloudFront.Types.KeyGroupConfig
import Amazonka.CloudFront.Types.KeyGroupList
import Amazonka.CloudFront.Types.KeyGroupSummary
import Amazonka.CloudFront.Types.KeyPairIds
import Amazonka.CloudFront.Types.KinesisStreamConfig
import Amazonka.CloudFront.Types.LambdaFunctionAssociation
import Amazonka.CloudFront.Types.LambdaFunctionAssociations
import Amazonka.CloudFront.Types.LoggingConfig
import Amazonka.CloudFront.Types.Method
import Amazonka.CloudFront.Types.MinimumProtocolVersion
import Amazonka.CloudFront.Types.MonitoringSubscription
import Amazonka.CloudFront.Types.Origin
import Amazonka.CloudFront.Types.OriginAccessControl
import Amazonka.CloudFront.Types.OriginAccessControlConfig
import Amazonka.CloudFront.Types.OriginAccessControlList
import Amazonka.CloudFront.Types.OriginAccessControlOriginTypes
import Amazonka.CloudFront.Types.OriginAccessControlSigningBehaviors
import Amazonka.CloudFront.Types.OriginAccessControlSigningProtocols
import Amazonka.CloudFront.Types.OriginAccessControlSummary
import Amazonka.CloudFront.Types.OriginCustomHeader
import Amazonka.CloudFront.Types.OriginGroup
import Amazonka.CloudFront.Types.OriginGroupFailoverCriteria
import Amazonka.CloudFront.Types.OriginGroupMember
import Amazonka.CloudFront.Types.OriginGroupMembers
import Amazonka.CloudFront.Types.OriginGroups
import Amazonka.CloudFront.Types.OriginProtocolPolicy
import Amazonka.CloudFront.Types.OriginRequestPolicy
import Amazonka.CloudFront.Types.OriginRequestPolicyConfig
import Amazonka.CloudFront.Types.OriginRequestPolicyCookieBehavior
import Amazonka.CloudFront.Types.OriginRequestPolicyCookiesConfig
import Amazonka.CloudFront.Types.OriginRequestPolicyHeaderBehavior
import Amazonka.CloudFront.Types.OriginRequestPolicyHeadersConfig
import Amazonka.CloudFront.Types.OriginRequestPolicyList
import Amazonka.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
import Amazonka.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
import Amazonka.CloudFront.Types.OriginRequestPolicySummary
import Amazonka.CloudFront.Types.OriginRequestPolicyType
import Amazonka.CloudFront.Types.OriginShield
import Amazonka.CloudFront.Types.OriginSslProtocols
import Amazonka.CloudFront.Types.Origins
import Amazonka.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
import Amazonka.CloudFront.Types.Paths
import Amazonka.CloudFront.Types.PriceClass
import Amazonka.CloudFront.Types.PublicKey
import Amazonka.CloudFront.Types.PublicKeyConfig
import Amazonka.CloudFront.Types.PublicKeyList
import Amazonka.CloudFront.Types.PublicKeySummary
import Amazonka.CloudFront.Types.QueryArgProfile
import Amazonka.CloudFront.Types.QueryArgProfileConfig
import Amazonka.CloudFront.Types.QueryArgProfiles
import Amazonka.CloudFront.Types.QueryStringCacheKeys
import Amazonka.CloudFront.Types.QueryStringNames
import Amazonka.CloudFront.Types.RealtimeLogConfig
import Amazonka.CloudFront.Types.RealtimeLogConfigs
import Amazonka.CloudFront.Types.RealtimeMetricsSubscriptionConfig
import Amazonka.CloudFront.Types.RealtimeMetricsSubscriptionStatus
import Amazonka.CloudFront.Types.ReferrerPolicyList
import Amazonka.CloudFront.Types.ResponseHeadersPolicy
import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowHeaders
import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowMethods
import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowMethodsValues
import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlAllowOrigins
import Amazonka.CloudFront.Types.ResponseHeadersPolicyAccessControlExposeHeaders
import Amazonka.CloudFront.Types.ResponseHeadersPolicyConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyContentSecurityPolicy
import Amazonka.CloudFront.Types.ResponseHeadersPolicyContentTypeOptions
import Amazonka.CloudFront.Types.ResponseHeadersPolicyCorsConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyCustomHeader
import Amazonka.CloudFront.Types.ResponseHeadersPolicyCustomHeadersConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyFrameOptions
import Amazonka.CloudFront.Types.ResponseHeadersPolicyList
import Amazonka.CloudFront.Types.ResponseHeadersPolicyReferrerPolicy
import Amazonka.CloudFront.Types.ResponseHeadersPolicyRemoveHeader
import Amazonka.CloudFront.Types.ResponseHeadersPolicyRemoveHeadersConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicySecurityHeadersConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyServerTimingHeadersConfig
import Amazonka.CloudFront.Types.ResponseHeadersPolicyStrictTransportSecurity
import Amazonka.CloudFront.Types.ResponseHeadersPolicySummary
import Amazonka.CloudFront.Types.ResponseHeadersPolicyType
import Amazonka.CloudFront.Types.ResponseHeadersPolicyXSSProtection
import Amazonka.CloudFront.Types.Restrictions
import Amazonka.CloudFront.Types.S3Origin
import Amazonka.CloudFront.Types.S3OriginConfig
import Amazonka.CloudFront.Types.SSLSupportMethod
import Amazonka.CloudFront.Types.SessionStickinessConfig
import Amazonka.CloudFront.Types.Signer
import Amazonka.CloudFront.Types.SslProtocol
import Amazonka.CloudFront.Types.StagingDistributionDnsNames
import Amazonka.CloudFront.Types.StatusCodes
import Amazonka.CloudFront.Types.StreamingDistribution
import Amazonka.CloudFront.Types.StreamingDistributionConfig
import Amazonka.CloudFront.Types.StreamingDistributionConfigWithTags
import Amazonka.CloudFront.Types.StreamingDistributionList
import Amazonka.CloudFront.Types.StreamingDistributionSummary
import Amazonka.CloudFront.Types.StreamingLoggingConfig
import Amazonka.CloudFront.Types.Tag
import Amazonka.CloudFront.Types.TagKeys
import Amazonka.CloudFront.Types.Tags
import Amazonka.CloudFront.Types.TestResult
import Amazonka.CloudFront.Types.TrafficConfig
import Amazonka.CloudFront.Types.TrustedKeyGroups
import Amazonka.CloudFront.Types.TrustedSigners
import Amazonka.CloudFront.Types.ViewerCertificate
import Amazonka.CloudFront.Types.ViewerProtocolPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-05-31@ of the Amazon CloudFront SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudFront",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cloudfront",
      Core.signingName = "cloudfront",
      Core.version = "2020-05-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "CloudFront",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Access denied.
_AccessDenied :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDenied =
  Core._MatchServiceError
    defaultService
    "AccessDenied"
    Prelude.. Core.hasStatus 403

-- | Invalidation batch specified is too large.
_BatchTooLarge :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BatchTooLarge =
  Core._MatchServiceError
    defaultService
    "BatchTooLarge"
    Prelude.. Core.hasStatus 413

-- | The CNAME specified is already defined for CloudFront.
_CNAMEAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CNAMEAlreadyExists =
  Core._MatchServiceError
    defaultService
    "CNAMEAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | A cache policy with this name already exists. You must provide a unique
-- name. To modify an existing cache policy, use @UpdateCachePolicy@.
_CachePolicyAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CachePolicyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "CachePolicyAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | Cannot delete the cache policy because it is attached to one or more
-- cache behaviors.
_CachePolicyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CachePolicyInUse =
  Core._MatchServiceError
    defaultService
    "CachePolicyInUse"
    Prelude.. Core.hasStatus 409

-- | You can\'t change the value of a public key.
_CannotChangeImmutablePublicKeyFields :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CannotChangeImmutablePublicKeyFields =
  Core._MatchServiceError
    defaultService
    "CannotChangeImmutablePublicKeyFields"
    Prelude.. Core.hasStatus 400

-- | If the @CallerReference@ is a value you already sent in a previous
-- request to create an identity but the content of the
-- @CloudFrontOriginAccessIdentityConfig@ is different from the original
-- request, CloudFront returns a
-- @CloudFrontOriginAccessIdentityAlreadyExists@ error.
_CloudFrontOriginAccessIdentityAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudFrontOriginAccessIdentityAlreadyExists =
  Core._MatchServiceError
    defaultService
    "CloudFrontOriginAccessIdentityAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The Origin Access Identity specified is already in use.
_CloudFrontOriginAccessIdentityInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CloudFrontOriginAccessIdentityInUse =
  Core._MatchServiceError
    defaultService
    "CloudFrontOriginAccessIdentityInUse"
    Prelude.. Core.hasStatus 409

-- | A continuous deployment policy with this configuration already exists.
_ContinuousDeploymentPolicyAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ContinuousDeploymentPolicyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ContinuousDeploymentPolicyAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | You cannot delete a continuous deployment policy that is associated with
-- a primary distribution.
_ContinuousDeploymentPolicyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ContinuousDeploymentPolicyInUse =
  Core._MatchServiceError
    defaultService
    "ContinuousDeploymentPolicyInUse"
    Prelude.. Core.hasStatus 409

-- | The caller reference you attempted to create the distribution with is
-- associated with another distribution.
_DistributionAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DistributionAlreadyExists =
  Core._MatchServiceError
    defaultService
    "DistributionAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The specified CloudFront distribution is not disabled. You must disable
-- the distribution before you can delete it.
_DistributionNotDisabled :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DistributionNotDisabled =
  Core._MatchServiceError
    defaultService
    "DistributionNotDisabled"
    Prelude.. Core.hasStatus 409

-- | The specified configuration for field-level encryption already exists.
_FieldLevelEncryptionConfigAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FieldLevelEncryptionConfigAlreadyExists =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionConfigAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The specified configuration for field-level encryption is in use.
_FieldLevelEncryptionConfigInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FieldLevelEncryptionConfigInUse =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionConfigInUse"
    Prelude.. Core.hasStatus 409

-- | The specified profile for field-level encryption already exists.
_FieldLevelEncryptionProfileAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FieldLevelEncryptionProfileAlreadyExists =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionProfileAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The specified profile for field-level encryption is in use.
_FieldLevelEncryptionProfileInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FieldLevelEncryptionProfileInUse =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionProfileInUse"
    Prelude.. Core.hasStatus 409

-- | The maximum size of a profile for field-level encryption was exceeded.
_FieldLevelEncryptionProfileSizeExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FieldLevelEncryptionProfileSizeExceeded =
  Core._MatchServiceError
    defaultService
    "FieldLevelEncryptionProfileSizeExceeded"
    Prelude.. Core.hasStatus 400

-- | A function with the same name already exists in this Amazon Web Services
-- account. To create a function, you must provide a unique name. To update
-- an existing function, use @UpdateFunction@.
_FunctionAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FunctionAlreadyExists =
  Core._MatchServiceError
    defaultService
    "FunctionAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | Cannot delete the function because it\'s attached to one or more cache
-- behaviors.
_FunctionInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FunctionInUse =
  Core._MatchServiceError
    defaultService
    "FunctionInUse"
    Prelude.. Core.hasStatus 409

-- | The function is too large. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_FunctionSizeLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_FunctionSizeLimitExceeded =
  Core._MatchServiceError
    defaultService
    "FunctionSizeLimitExceeded"
    Prelude.. Core.hasStatus 413

-- | You cannot delete a managed policy.
_IllegalDelete :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IllegalDelete =
  Core._MatchServiceError
    defaultService
    "IllegalDelete"
    Prelude.. Core.hasStatus 400

-- | The specified configuration for field-level encryption can\'t be
-- associated with the specified cache behavior.
_IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior =
  Core._MatchServiceError
    defaultService
    "IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior"
    Prelude.. Core.hasStatus 400

-- | An origin cannot contain both an origin access control (OAC) and an
-- origin access identity (OAI).
_IllegalOriginAccessConfiguration :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IllegalOriginAccessConfiguration =
  Core._MatchServiceError
    defaultService
    "IllegalOriginAccessConfiguration"
    Prelude.. Core.hasStatus 400

-- | The update contains modifications that are not allowed.
_IllegalUpdate :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_IllegalUpdate =
  Core._MatchServiceError
    defaultService
    "IllegalUpdate"
    Prelude.. Core.hasStatus 400

-- | The value of @Quantity@ and the size of @Items@ don\'t match.
_InconsistentQuantities :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InconsistentQuantities =
  Core._MatchServiceError
    defaultService
    "InconsistentQuantities"
    Prelude.. Core.hasStatus 400

-- | An argument is invalid.
_InvalidArgument :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidArgument =
  Core._MatchServiceError
    defaultService
    "InvalidArgument"
    Prelude.. Core.hasStatus 400

-- | The default root object file name is too big or contains an invalid
-- character.
_InvalidDefaultRootObject :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDefaultRootObject =
  Core._MatchServiceError
    defaultService
    "InvalidDefaultRootObject"
    Prelude.. Core.hasStatus 400

-- | An origin access control is associated with an origin whose domain name
-- is not supported.
_InvalidDomainNameForOriginAccessControl :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDomainNameForOriginAccessControl =
  Core._MatchServiceError
    defaultService
    "InvalidDomainNameForOriginAccessControl"
    Prelude.. Core.hasStatus 400

-- | An invalid error code was specified.
_InvalidErrorCode :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidErrorCode =
  Core._MatchServiceError
    defaultService
    "InvalidErrorCode"
    Prelude.. Core.hasStatus 400

-- | Your request contains forward cookies option which doesn\'t match with
-- the expectation for the @whitelisted@ list of cookie names. Either list
-- of cookie names has been specified when not allowed or list of cookie
-- names is missing when expected.
_InvalidForwardCookies :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidForwardCookies =
  Core._MatchServiceError
    defaultService
    "InvalidForwardCookies"
    Prelude.. Core.hasStatus 400

-- | A CloudFront function association is invalid.
_InvalidFunctionAssociation :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidFunctionAssociation =
  Core._MatchServiceError
    defaultService
    "InvalidFunctionAssociation"
    Prelude.. Core.hasStatus 400

-- | The specified geo restriction parameter is not valid.
_InvalidGeoRestrictionParameter :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidGeoRestrictionParameter =
  Core._MatchServiceError
    defaultService
    "InvalidGeoRestrictionParameter"
    Prelude.. Core.hasStatus 400

-- | The headers specified are not valid for an Amazon S3 origin.
_InvalidHeadersForS3Origin :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidHeadersForS3Origin =
  Core._MatchServiceError
    defaultService
    "InvalidHeadersForS3Origin"
    Prelude.. Core.hasStatus 400

-- | The @If-Match@ version is missing or not valid.
_InvalidIfMatchVersion :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidIfMatchVersion =
  Core._MatchServiceError
    defaultService
    "InvalidIfMatchVersion"
    Prelude.. Core.hasStatus 400

-- | The specified Lambda\@Edge function association is invalid.
_InvalidLambdaFunctionAssociation :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidLambdaFunctionAssociation =
  Core._MatchServiceError
    defaultService
    "InvalidLambdaFunctionAssociation"
    Prelude.. Core.hasStatus 400

-- | The location code specified is not valid.
_InvalidLocationCode :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidLocationCode =
  Core._MatchServiceError
    defaultService
    "InvalidLocationCode"
    Prelude.. Core.hasStatus 400

-- | The minimum protocol version specified is not valid.
_InvalidMinimumProtocolVersion :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidMinimumProtocolVersion =
  Core._MatchServiceError
    defaultService
    "InvalidMinimumProtocolVersion"
    Prelude.. Core.hasStatus 400

-- | The Amazon S3 origin server specified does not refer to a valid Amazon
-- S3 bucket.
_InvalidOrigin :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidOrigin =
  Core._MatchServiceError
    defaultService
    "InvalidOrigin"
    Prelude.. Core.hasStatus 400

-- | The origin access control is not valid.
_InvalidOriginAccessControl :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidOriginAccessControl =
  Core._MatchServiceError
    defaultService
    "InvalidOriginAccessControl"
    Prelude.. Core.hasStatus 400

-- | The origin access identity is not valid or doesn\'t exist.
_InvalidOriginAccessIdentity :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidOriginAccessIdentity =
  Core._MatchServiceError
    defaultService
    "InvalidOriginAccessIdentity"
    Prelude.. Core.hasStatus 400

-- | The keep alive timeout specified for the origin is not valid.
_InvalidOriginKeepaliveTimeout :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidOriginKeepaliveTimeout =
  Core._MatchServiceError
    defaultService
    "InvalidOriginKeepaliveTimeout"
    Prelude.. Core.hasStatus 400

-- | The read timeout specified for the origin is not valid.
_InvalidOriginReadTimeout :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidOriginReadTimeout =
  Core._MatchServiceError
    defaultService
    "InvalidOriginReadTimeout"
    Prelude.. Core.hasStatus 400

-- | You cannot specify SSLv3 as the minimum protocol version if you only
-- want to support only clients that support Server Name Indication (SNI).
_InvalidProtocolSettings :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidProtocolSettings =
  Core._MatchServiceError
    defaultService
    "InvalidProtocolSettings"
    Prelude.. Core.hasStatus 400

-- | The query string parameters specified are not valid.
_InvalidQueryStringParameters :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidQueryStringParameters =
  Core._MatchServiceError
    defaultService
    "InvalidQueryStringParameters"
    Prelude.. Core.hasStatus 400

-- | The relative path is too big, is not URL-encoded, or does not begin with
-- a slash (\/).
_InvalidRelativePath :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRelativePath =
  Core._MatchServiceError
    defaultService
    "InvalidRelativePath"
    Prelude.. Core.hasStatus 400

-- | This operation requires the HTTPS protocol. Ensure that you specify the
-- HTTPS protocol in your request, or omit the @RequiredProtocols@ element
-- from your distribution configuration.
_InvalidRequiredProtocol :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequiredProtocol =
  Core._MatchServiceError
    defaultService
    "InvalidRequiredProtocol"
    Prelude.. Core.hasStatus 400

-- | A response code is not valid.
_InvalidResponseCode :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidResponseCode =
  Core._MatchServiceError
    defaultService
    "InvalidResponseCode"
    Prelude.. Core.hasStatus 400

-- | The TTL order specified is not valid.
_InvalidTTLOrder :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTTLOrder =
  Core._MatchServiceError
    defaultService
    "InvalidTTLOrder"
    Prelude.. Core.hasStatus 400

-- | The tagging specified is not valid.
_InvalidTagging :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTagging =
  Core._MatchServiceError
    defaultService
    "InvalidTagging"
    Prelude.. Core.hasStatus 400

-- | A viewer certificate specified is not valid.
_InvalidViewerCertificate :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidViewerCertificate =
  Core._MatchServiceError
    defaultService
    "InvalidViewerCertificate"
    Prelude.. Core.hasStatus 400

-- | A web ACL ID specified is not valid. To specify a web ACL created using
-- the latest version of WAF, use the ACL ARN, for example
-- @arn:aws:wafv2:us-east-1:123456789012:global\/webacl\/ExampleWebACL\/473e64fd-f30b-4765-81a0-62ad96dd167a@.
-- To specify a web ACL created using WAF Classic, use the ACL ID, for
-- example @473e64fd-f30b-4765-81a0-62ad96dd167a@.
_InvalidWebACLId :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidWebACLId =
  Core._MatchServiceError
    defaultService
    "InvalidWebACLId"
    Prelude.. Core.hasStatus 400

-- | A key group with this name already exists. You must provide a unique
-- name. To modify an existing key group, use @UpdateKeyGroup@.
_KeyGroupAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KeyGroupAlreadyExists =
  Core._MatchServiceError
    defaultService
    "KeyGroupAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | This operation requires a body. Ensure that the body is present and the
-- @Content-Type@ header is set.
_MissingBody :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MissingBody =
  Core._MatchServiceError
    defaultService
    "MissingBody"
    Prelude.. Core.hasStatus 400

-- | A monitoring subscription already exists for the specified distribution.
_MonitoringSubscriptionAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MonitoringSubscriptionAlreadyExists =
  Core._MatchServiceError
    defaultService
    "MonitoringSubscriptionAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The cache policy does not exist.
_NoSuchCachePolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchCachePolicy =
  Core._MatchServiceError
    defaultService
    "NoSuchCachePolicy"
    Prelude.. Core.hasStatus 404

-- | The specified origin access identity does not exist.
_NoSuchCloudFrontOriginAccessIdentity :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchCloudFrontOriginAccessIdentity =
  Core._MatchServiceError
    defaultService
    "NoSuchCloudFrontOriginAccessIdentity"
    Prelude.. Core.hasStatus 404

-- | The continuous deployment policy doesn\'t exist.
_NoSuchContinuousDeploymentPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchContinuousDeploymentPolicy =
  Core._MatchServiceError
    defaultService
    "NoSuchContinuousDeploymentPolicy"
    Prelude.. Core.hasStatus 404

-- | The specified distribution does not exist.
_NoSuchDistribution :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchDistribution =
  Core._MatchServiceError
    defaultService
    "NoSuchDistribution"
    Prelude.. Core.hasStatus 404

-- | The specified configuration for field-level encryption doesn\'t exist.
_NoSuchFieldLevelEncryptionConfig :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchFieldLevelEncryptionConfig =
  Core._MatchServiceError
    defaultService
    "NoSuchFieldLevelEncryptionConfig"
    Prelude.. Core.hasStatus 404

-- | The specified profile for field-level encryption doesn\'t exist.
_NoSuchFieldLevelEncryptionProfile :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchFieldLevelEncryptionProfile =
  Core._MatchServiceError
    defaultService
    "NoSuchFieldLevelEncryptionProfile"
    Prelude.. Core.hasStatus 404

-- | The function does not exist.
_NoSuchFunctionExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchFunctionExists =
  Core._MatchServiceError
    defaultService
    "NoSuchFunctionExists"
    Prelude.. Core.hasStatus 404

-- | The specified invalidation does not exist.
_NoSuchInvalidation :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchInvalidation =
  Core._MatchServiceError
    defaultService
    "NoSuchInvalidation"
    Prelude.. Core.hasStatus 404

-- | A monitoring subscription does not exist for the specified distribution.
_NoSuchMonitoringSubscription :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchMonitoringSubscription =
  Core._MatchServiceError
    defaultService
    "NoSuchMonitoringSubscription"
    Prelude.. Core.hasStatus 404

-- | No origin exists with the specified @Origin Id@.
_NoSuchOrigin :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchOrigin =
  Core._MatchServiceError
    defaultService
    "NoSuchOrigin"
    Prelude.. Core.hasStatus 404

-- | The origin access control does not exist.
_NoSuchOriginAccessControl :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchOriginAccessControl =
  Core._MatchServiceError
    defaultService
    "NoSuchOriginAccessControl"
    Prelude.. Core.hasStatus 404

-- | The origin request policy does not exist.
_NoSuchOriginRequestPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "NoSuchOriginRequestPolicy"
    Prelude.. Core.hasStatus 404

-- | The specified public key doesn\'t exist.
_NoSuchPublicKey :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchPublicKey =
  Core._MatchServiceError
    defaultService
    "NoSuchPublicKey"
    Prelude.. Core.hasStatus 404

-- | The real-time log configuration does not exist.
_NoSuchRealtimeLogConfig :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchRealtimeLogConfig =
  Core._MatchServiceError
    defaultService
    "NoSuchRealtimeLogConfig"
    Prelude.. Core.hasStatus 404

-- | A resource that was specified is not valid.
_NoSuchResource :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchResource =
  Core._MatchServiceError
    defaultService
    "NoSuchResource"
    Prelude.. Core.hasStatus 404

-- | The response headers policy does not exist.
_NoSuchResponseHeadersPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchResponseHeadersPolicy =
  Core._MatchServiceError
    defaultService
    "NoSuchResponseHeadersPolicy"
    Prelude.. Core.hasStatus 404

-- | The specified streaming distribution does not exist.
_NoSuchStreamingDistribution :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoSuchStreamingDistribution =
  Core._MatchServiceError
    defaultService
    "NoSuchStreamingDistribution"
    Prelude.. Core.hasStatus 404

-- | An origin access control with the specified parameters already exists.
_OriginAccessControlAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OriginAccessControlAlreadyExists =
  Core._MatchServiceError
    defaultService
    "OriginAccessControlAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | Cannot delete the origin access control because it\'s in use by one or
-- more distributions.
_OriginAccessControlInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OriginAccessControlInUse =
  Core._MatchServiceError
    defaultService
    "OriginAccessControlInUse"
    Prelude.. Core.hasStatus 409

-- | An origin request policy with this name already exists. You must provide
-- a unique name. To modify an existing origin request policy, use
-- @UpdateOriginRequestPolicy@.
_OriginRequestPolicyAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OriginRequestPolicyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "OriginRequestPolicyAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | Cannot delete the origin request policy because it is attached to one or
-- more cache behaviors.
_OriginRequestPolicyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OriginRequestPolicyInUse =
  Core._MatchServiceError
    defaultService
    "OriginRequestPolicyInUse"
    Prelude.. Core.hasStatus 409

-- | The precondition in one or more of the request fields evaluated to
-- @false@.
_PreconditionFailed :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PreconditionFailed =
  Core._MatchServiceError
    defaultService
    "PreconditionFailed"
    Prelude.. Core.hasStatus 412

-- | The specified public key already exists.
_PublicKeyAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PublicKeyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "PublicKeyAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The specified public key is in use.
_PublicKeyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PublicKeyInUse =
  Core._MatchServiceError
    defaultService
    "PublicKeyInUse"
    Prelude.. Core.hasStatus 409

-- | No profile specified for the field-level encryption query argument.
_QueryArgProfileEmpty :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_QueryArgProfileEmpty =
  Core._MatchServiceError
    defaultService
    "QueryArgProfileEmpty"
    Prelude.. Core.hasStatus 400

-- | A real-time log configuration with this name already exists. You must
-- provide a unique name. To modify an existing real-time log
-- configuration, use @UpdateRealtimeLogConfig@.
_RealtimeLogConfigAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RealtimeLogConfigAlreadyExists =
  Core._MatchServiceError
    defaultService
    "RealtimeLogConfigAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | Cannot delete the real-time log configuration because it is attached to
-- one or more cache behaviors.
_RealtimeLogConfigInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RealtimeLogConfigInUse =
  Core._MatchServiceError
    defaultService
    "RealtimeLogConfigInUse"
    Prelude.. Core.hasStatus 400

-- | The specified real-time log configuration belongs to a different Amazon
-- Web Services account.
_RealtimeLogConfigOwnerMismatch :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RealtimeLogConfigOwnerMismatch =
  Core._MatchServiceError
    defaultService
    "RealtimeLogConfigOwnerMismatch"
    Prelude.. Core.hasStatus 401

-- | Cannot delete this resource because it is in use.
_ResourceInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUse =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"
    Prelude.. Core.hasStatus 409

-- | A response headers policy with this name already exists. You must
-- provide a unique name. To modify an existing response headers policy,
-- use @UpdateResponseHeadersPolicy@.
_ResponseHeadersPolicyAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResponseHeadersPolicyAlreadyExists =
  Core._MatchServiceError
    defaultService
    "ResponseHeadersPolicyAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | Cannot delete the response headers policy because it is attached to one
-- or more cache behaviors in a CloudFront distribution.
_ResponseHeadersPolicyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResponseHeadersPolicyInUse =
  Core._MatchServiceError
    defaultService
    "ResponseHeadersPolicyInUse"
    Prelude.. Core.hasStatus 409

-- | A continuous deployment policy for this staging distribution already
-- exists.
_StagingDistributionInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StagingDistributionInUse =
  Core._MatchServiceError
    defaultService
    "StagingDistributionInUse"
    Prelude.. Core.hasStatus 409

-- | The caller reference you attempted to create the streaming distribution
-- with is associated with another distribution
_StreamingDistributionAlreadyExists :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StreamingDistributionAlreadyExists =
  Core._MatchServiceError
    defaultService
    "StreamingDistributionAlreadyExists"
    Prelude.. Core.hasStatus 409

-- | The specified CloudFront distribution is not disabled. You must disable
-- the distribution before you can delete it.
_StreamingDistributionNotDisabled :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StreamingDistributionNotDisabled =
  Core._MatchServiceError
    defaultService
    "StreamingDistributionNotDisabled"
    Prelude.. Core.hasStatus 409

-- | The CloudFront function failed.
_TestFunctionFailed :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TestFunctionFailed =
  Core._MatchServiceError
    defaultService
    "TestFunctionFailed"
    Prelude.. Core.hasStatus 500

-- | The length of the @Content-Security-Policy@ header value in the response
-- headers policy exceeds the maximum.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooLongCSPInResponseHeadersPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooLongCSPInResponseHeadersPolicy =
  Core._MatchServiceError
    defaultService
    "TooLongCSPInResponseHeadersPolicy"
    Prelude.. Core.hasStatus 400

-- | You cannot create more cache behaviors for the distribution.
_TooManyCacheBehaviors :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyCacheBehaviors =
  Core._MatchServiceError
    defaultService
    "TooManyCacheBehaviors"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of cache policies for this Amazon
-- Web Services account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCachePolicies :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyCachePolicies =
  Core._MatchServiceError
    defaultService
    "TooManyCachePolicies"
    Prelude.. Core.hasStatus 400

-- | You cannot create anymore custom SSL\/TLS certificates.
_TooManyCertificates :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyCertificates =
  Core._MatchServiceError
    defaultService
    "TooManyCertificates"
    Prelude.. Core.hasStatus 400

-- | Processing your request would cause you to exceed the maximum number of
-- origin access identities allowed.
_TooManyCloudFrontOriginAccessIdentities :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyCloudFrontOriginAccessIdentities =
  Core._MatchServiceError
    defaultService
    "TooManyCloudFrontOriginAccessIdentities"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of continuous deployment policies
-- for this Amazon Web Services account.
_TooManyContinuousDeploymentPolicies :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyContinuousDeploymentPolicies =
  Core._MatchServiceError
    defaultService
    "TooManyContinuousDeploymentPolicies"
    Prelude.. Core.hasStatus 400

-- | Your request contains more cookie names in the whitelist than are
-- allowed per cache behavior.
_TooManyCookieNamesInWhiteList :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyCookieNamesInWhiteList =
  Core._MatchServiceError
    defaultService
    "TooManyCookieNamesInWhiteList"
    Prelude.. Core.hasStatus 400

-- | The number of cookies in the cache policy exceeds the maximum. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCookiesInCachePolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyCookiesInCachePolicy =
  Core._MatchServiceError
    defaultService
    "TooManyCookiesInCachePolicy"
    Prelude.. Core.hasStatus 400

-- | The number of cookies in the origin request policy exceeds the maximum.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCookiesInOriginRequestPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyCookiesInOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyCookiesInOriginRequestPolicy"
    Prelude.. Core.hasStatus 400

-- | The number of custom headers in the response headers policy exceeds the
-- maximum.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyCustomHeadersInResponseHeadersPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyCustomHeadersInResponseHeadersPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyCustomHeadersInResponseHeadersPolicy"
    Prelude.. Core.hasStatus 400

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyDistributionCNAMEs :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionCNAMEs =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionCNAMEs"
    Prelude.. Core.hasStatus 400

-- | Processing your request would cause you to exceed the maximum number of
-- distributions allowed.
_TooManyDistributions :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributions =
  Core._MatchServiceError
    defaultService
    "TooManyDistributions"
    Prelude.. Core.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified cache policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToCachePolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionsAssociatedToCachePolicy =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToCachePolicy"
    Prelude.. Core.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified configuration for field-level encryption.
_TooManyDistributionsAssociatedToFieldLevelEncryptionConfig :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionsAssociatedToFieldLevelEncryptionConfig =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToFieldLevelEncryptionConfig"
    Prelude.. Core.hasStatus 400

-- | The number of distributions that reference this key group is more than
-- the maximum allowed. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToKeyGroup :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionsAssociatedToKeyGroup =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToKeyGroup"
    Prelude.. Core.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified origin access control.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToOriginAccessControl :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionsAssociatedToOriginAccessControl =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToOriginAccessControl"
    Prelude.. Core.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToOriginRequestPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionsAssociatedToOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToOriginRequestPolicy"
    Prelude.. Core.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified response headers policy.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsAssociatedToResponseHeadersPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionsAssociatedToResponseHeadersPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsAssociatedToResponseHeadersPolicy"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of distributions that are associated
-- with a CloudFront function. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyDistributionsWithFunctionAssociations :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionsWithFunctionAssociations =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsWithFunctionAssociations"
    Prelude.. Core.hasStatus 400

-- | Processing your request would cause the maximum number of distributions
-- with Lambda\@Edge function associations per owner to be exceeded.
_TooManyDistributionsWithLambdaAssociations :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionsWithLambdaAssociations =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsWithLambdaAssociations"
    Prelude.. Core.hasStatus 400

-- | The maximum number of distributions have been associated with the
-- specified Lambda\@Edge function.
_TooManyDistributionsWithSingleFunctionARN :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyDistributionsWithSingleFunctionARN =
  Core._MatchServiceError
    defaultService
    "TooManyDistributionsWithSingleFunctionARN"
    Prelude.. Core.hasStatus 400

-- | The maximum number of configurations for field-level encryption have
-- been created.
_TooManyFieldLevelEncryptionConfigs :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyFieldLevelEncryptionConfigs =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionConfigs"
    Prelude.. Core.hasStatus 400

-- | The maximum number of content type profiles for field-level encryption
-- have been created.
_TooManyFieldLevelEncryptionContentTypeProfiles :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyFieldLevelEncryptionContentTypeProfiles =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionContentTypeProfiles"
    Prelude.. Core.hasStatus 400

-- | The maximum number of encryption entities for field-level encryption
-- have been created.
_TooManyFieldLevelEncryptionEncryptionEntities :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyFieldLevelEncryptionEncryptionEntities =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionEncryptionEntities"
    Prelude.. Core.hasStatus 400

-- | The maximum number of field patterns for field-level encryption have
-- been created.
_TooManyFieldLevelEncryptionFieldPatterns :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyFieldLevelEncryptionFieldPatterns =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionFieldPatterns"
    Prelude.. Core.hasStatus 400

-- | The maximum number of profiles for field-level encryption have been
-- created.
_TooManyFieldLevelEncryptionProfiles :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyFieldLevelEncryptionProfiles =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionProfiles"
    Prelude.. Core.hasStatus 400

-- | The maximum number of query arg profiles for field-level encryption have
-- been created.
_TooManyFieldLevelEncryptionQueryArgProfiles :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyFieldLevelEncryptionQueryArgProfiles =
  Core._MatchServiceError
    defaultService
    "TooManyFieldLevelEncryptionQueryArgProfiles"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of CloudFront function associations
-- for this distribution. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyFunctionAssociations :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyFunctionAssociations =
  Core._MatchServiceError
    defaultService
    "TooManyFunctionAssociations"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of CloudFront functions for this
-- Amazon Web Services account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyFunctions :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyFunctions =
  Core._MatchServiceError
    defaultService
    "TooManyFunctions"
    Prelude.. Core.hasStatus 400

-- | The number of headers in the cache policy exceeds the maximum. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyHeadersInCachePolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyHeadersInCachePolicy =
  Core._MatchServiceError
    defaultService
    "TooManyHeadersInCachePolicy"
    Prelude.. Core.hasStatus 400

-- | Your request contains too many headers in forwarded values.
_TooManyHeadersInForwardedValues :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyHeadersInForwardedValues =
  Core._MatchServiceError
    defaultService
    "TooManyHeadersInForwardedValues"
    Prelude.. Core.hasStatus 400

-- | The number of headers in the origin request policy exceeds the maximum.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyHeadersInOriginRequestPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyHeadersInOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyHeadersInOriginRequestPolicy"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the maximum number of allowable InProgress
-- invalidation batch requests, or invalidation objects.
_TooManyInvalidationsInProgress :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyInvalidationsInProgress =
  Core._MatchServiceError
    defaultService
    "TooManyInvalidationsInProgress"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of key groups for this Amazon Web
-- Services account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyKeyGroups :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyKeyGroups =
  Core._MatchServiceError
    defaultService
    "TooManyKeyGroups"
    Prelude.. Core.hasStatus 400

-- | The number of key groups referenced by this distribution is more than
-- the maximum allowed. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyKeyGroupsAssociatedToDistribution :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyKeyGroupsAssociatedToDistribution =
  Core._MatchServiceError
    defaultService
    "TooManyKeyGroupsAssociatedToDistribution"
    Prelude.. Core.hasStatus 400

-- | Your request contains more Lambda\@Edge function associations than are
-- allowed per distribution.
_TooManyLambdaFunctionAssociations :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyLambdaFunctionAssociations =
  Core._MatchServiceError
    defaultService
    "TooManyLambdaFunctionAssociations"
    Prelude.. Core.hasStatus 400

-- | The number of origin access controls in your Amazon Web Services account
-- exceeds the maximum allowed.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyOriginAccessControls :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyOriginAccessControls =
  Core._MatchServiceError
    defaultService
    "TooManyOriginAccessControls"
    Prelude.. Core.hasStatus 400

-- | Your request contains too many origin custom headers.
_TooManyOriginCustomHeaders :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyOriginCustomHeaders =
  Core._MatchServiceError
    defaultService
    "TooManyOriginCustomHeaders"
    Prelude.. Core.hasStatus 400

-- | Processing your request would cause you to exceed the maximum number of
-- origin groups allowed.
_TooManyOriginGroupsPerDistribution :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyOriginGroupsPerDistribution =
  Core._MatchServiceError
    defaultService
    "TooManyOriginGroupsPerDistribution"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of origin request policies for this
-- Amazon Web Services account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyOriginRequestPolicies :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyOriginRequestPolicies =
  Core._MatchServiceError
    defaultService
    "TooManyOriginRequestPolicies"
    Prelude.. Core.hasStatus 400

-- | You cannot create more origins for the distribution.
_TooManyOrigins :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyOrigins =
  Core._MatchServiceError
    defaultService
    "TooManyOrigins"
    Prelude.. Core.hasStatus 400

-- | The maximum number of public keys for field-level encryption have been
-- created. To create a new public key, delete one of the existing keys.
_TooManyPublicKeys :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyPublicKeys =
  Core._MatchServiceError
    defaultService
    "TooManyPublicKeys"
    Prelude.. Core.hasStatus 400

-- | The number of public keys in this key group is more than the maximum
-- allowed. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyPublicKeysInKeyGroup :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyPublicKeysInKeyGroup =
  Core._MatchServiceError
    defaultService
    "TooManyPublicKeysInKeyGroup"
    Prelude.. Core.hasStatus 400

-- | Your request contains too many query string parameters.
_TooManyQueryStringParameters :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyQueryStringParameters =
  Core._MatchServiceError
    defaultService
    "TooManyQueryStringParameters"
    Prelude.. Core.hasStatus 400

-- | The number of query strings in the cache policy exceeds the maximum. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyQueryStringsInCachePolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyQueryStringsInCachePolicy =
  Core._MatchServiceError
    defaultService
    "TooManyQueryStringsInCachePolicy"
    Prelude.. Core.hasStatus 400

-- | The number of query strings in the origin request policy exceeds the
-- maximum. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyQueryStringsInOriginRequestPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyQueryStringsInOriginRequestPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyQueryStringsInOriginRequestPolicy"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of real-time log configurations for
-- this Amazon Web Services account. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyRealtimeLogConfigs :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRealtimeLogConfigs =
  Core._MatchServiceError
    defaultService
    "TooManyRealtimeLogConfigs"
    Prelude.. Core.hasStatus 400

-- | The number of headers in @RemoveHeadersConfig@ in the response headers
-- policy exceeds the maximum.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyRemoveHeadersInResponseHeadersPolicy :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRemoveHeadersInResponseHeadersPolicy =
  Core._MatchServiceError
    defaultService
    "TooManyRemoveHeadersInResponseHeadersPolicy"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of response headers policies for
-- this Amazon Web Services account.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas>
-- (formerly known as limits) in the /Amazon CloudFront Developer Guide/.
_TooManyResponseHeadersPolicies :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyResponseHeadersPolicies =
  Core._MatchServiceError
    defaultService
    "TooManyResponseHeadersPolicies"
    Prelude.. Core.hasStatus 400

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyStreamingDistributionCNAMEs :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyStreamingDistributionCNAMEs =
  Core._MatchServiceError
    defaultService
    "TooManyStreamingDistributionCNAMEs"
    Prelude.. Core.hasStatus 400

-- | Processing your request would cause you to exceed the maximum number of
-- streaming distributions allowed.
_TooManyStreamingDistributions :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyStreamingDistributions =
  Core._MatchServiceError
    defaultService
    "TooManyStreamingDistributions"
    Prelude.. Core.hasStatus 400

-- | Your request contains more trusted signers than are allowed per
-- distribution.
_TooManyTrustedSigners :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTrustedSigners =
  Core._MatchServiceError
    defaultService
    "TooManyTrustedSigners"
    Prelude.. Core.hasStatus 400

-- | The specified key group does not exist.
_TrustedKeyGroupDoesNotExist :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TrustedKeyGroupDoesNotExist =
  Core._MatchServiceError
    defaultService
    "TrustedKeyGroupDoesNotExist"
    Prelude.. Core.hasStatus 400

-- | One or more of your trusted signers don\'t exist.
_TrustedSignerDoesNotExist :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TrustedSignerDoesNotExist =
  Core._MatchServiceError
    defaultService
    "TrustedSignerDoesNotExist"
    Prelude.. Core.hasStatus 400

-- | This operation is not supported in this region.
_UnsupportedOperation :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedOperation =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperation"
    Prelude.. Core.hasStatus 400
