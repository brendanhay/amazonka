{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudFront
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-05-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon CloudFront
--
-- This is the /Amazon CloudFront API Reference/. This guide is for
-- developers who need detailed information about CloudFront API actions,
-- data types, and errors. For detailed information about CloudFront
-- features, see the /Amazon CloudFront Developer Guide/.
module Amazonka.CloudFront
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** TooManyKeyGroupsAssociatedToDistribution
    _TooManyKeyGroupsAssociatedToDistribution,

    -- ** FunctionInUse
    _FunctionInUse,

    -- ** InvalidHeadersForS3Origin
    _InvalidHeadersForS3Origin,

    -- ** NoSuchFieldLevelEncryptionConfig
    _NoSuchFieldLevelEncryptionConfig,

    -- ** TooManyFunctionAssociations
    _TooManyFunctionAssociations,

    -- ** InvalidResponseCode
    _InvalidResponseCode,

    -- ** TooManyCookieNamesInWhiteList
    _TooManyCookieNamesInWhiteList,

    -- ** InvalidViewerCertificate
    _InvalidViewerCertificate,

    -- ** IllegalOriginAccessConfiguration
    _IllegalOriginAccessConfiguration,

    -- ** BatchTooLarge
    _BatchTooLarge,

    -- ** InvalidOriginAccessIdentity
    _InvalidOriginAccessIdentity,

    -- ** DistributionAlreadyExists
    _DistributionAlreadyExists,

    -- ** NoSuchFieldLevelEncryptionProfile
    _NoSuchFieldLevelEncryptionProfile,

    -- ** NoSuchStreamingDistribution
    _NoSuchStreamingDistribution,

    -- ** TooManyFieldLevelEncryptionQueryArgProfiles
    _TooManyFieldLevelEncryptionQueryArgProfiles,

    -- ** TooManyInvalidationsInProgress
    _TooManyInvalidationsInProgress,

    -- ** TooManyKeyGroups
    _TooManyKeyGroups,

    -- ** TooManyStreamingDistributionCNAMEs
    _TooManyStreamingDistributionCNAMEs,

    -- ** NoSuchOrigin
    _NoSuchOrigin,

    -- ** InvalidOriginReadTimeout
    _InvalidOriginReadTimeout,

    -- ** InvalidOriginKeepaliveTimeout
    _InvalidOriginKeepaliveTimeout,

    -- ** TooManyStreamingDistributions
    _TooManyStreamingDistributions,

    -- ** TooManyRealtimeLogConfigs
    _TooManyRealtimeLogConfigs,

    -- ** TooManyDistributions
    _TooManyDistributions,

    -- ** NoSuchPublicKey
    _NoSuchPublicKey,

    -- ** TooManyCacheBehaviors
    _TooManyCacheBehaviors,

    -- ** AccessDenied
    _AccessDenied,

    -- ** InvalidFunctionAssociation
    _InvalidFunctionAssociation,

    -- ** CNAMEAlreadyExists
    _CNAMEAlreadyExists,

    -- ** TooManyFieldLevelEncryptionProfiles
    _TooManyFieldLevelEncryptionProfiles,

    -- ** InvalidLambdaFunctionAssociation
    _InvalidLambdaFunctionAssociation,

    -- ** TestFunctionFailed
    _TestFunctionFailed,

    -- ** FunctionSizeLimitExceeded
    _FunctionSizeLimitExceeded,

    -- ** FieldLevelEncryptionProfileAlreadyExists
    _FieldLevelEncryptionProfileAlreadyExists,

    -- ** NoSuchCloudFrontOriginAccessIdentity
    _NoSuchCloudFrontOriginAccessIdentity,

    -- ** TooManyDistributionsWithSingleFunctionARN
    _TooManyDistributionsWithSingleFunctionARN,

    -- ** InvalidForwardCookies
    _InvalidForwardCookies,

    -- ** TooManyTrustedSigners
    _TooManyTrustedSigners,

    -- ** OriginRequestPolicyAlreadyExists
    _OriginRequestPolicyAlreadyExists,

    -- ** TooManyHeadersInForwardedValues
    _TooManyHeadersInForwardedValues,

    -- ** OriginAccessControlInUse
    _OriginAccessControlInUse,

    -- ** FieldLevelEncryptionProfileSizeExceeded
    _FieldLevelEncryptionProfileSizeExceeded,

    -- ** NoSuchOriginAccessControl
    _NoSuchOriginAccessControl,

    -- ** FieldLevelEncryptionProfileInUse
    _FieldLevelEncryptionProfileInUse,

    -- ** IllegalUpdate
    _IllegalUpdate,

    -- ** InvalidErrorCode
    _InvalidErrorCode,

    -- ** InvalidDefaultRootObject
    _InvalidDefaultRootObject,

    -- ** OriginAccessControlAlreadyExists
    _OriginAccessControlAlreadyExists,

    -- ** TooManyQueryStringsInOriginRequestPolicy
    _TooManyQueryStringsInOriginRequestPolicy,

    -- ** TooManyCookiesInCachePolicy
    _TooManyCookiesInCachePolicy,

    -- ** DistributionNotDisabled
    _DistributionNotDisabled,

    -- ** NoSuchInvalidation
    _NoSuchInvalidation,

    -- ** NoSuchFunctionExists
    _NoSuchFunctionExists,

    -- ** TooManyCloudFrontOriginAccessIdentities
    _TooManyCloudFrontOriginAccessIdentities,

    -- ** InvalidDomainNameForOriginAccessControl
    _InvalidDomainNameForOriginAccessControl,

    -- ** FunctionAlreadyExists
    _FunctionAlreadyExists,

    -- ** InvalidArgument
    _InvalidArgument,

    -- ** NoSuchMonitoringSubscription
    _NoSuchMonitoringSubscription,

    -- ** InvalidWebACLId
    _InvalidWebACLId,

    -- ** InvalidQueryStringParameters
    _InvalidQueryStringParameters,

    -- ** MonitoringSubscriptionAlreadyExists
    _MonitoringSubscriptionAlreadyExists,

    -- ** TooManyCertificates
    _TooManyCertificates,

    -- ** CachePolicyInUse
    _CachePolicyInUse,

    -- ** NoSuchResource
    _NoSuchResource,

    -- ** PreconditionFailed
    _PreconditionFailed,

    -- ** KeyGroupAlreadyExists
    _KeyGroupAlreadyExists,

    -- ** TooManyDistributionsAssociatedToKeyGroup
    _TooManyDistributionsAssociatedToKeyGroup,

    -- ** TrustedSignerDoesNotExist
    _TrustedSignerDoesNotExist,

    -- ** TooManyFunctions
    _TooManyFunctions,

    -- ** TooManyQueryStringParameters
    _TooManyQueryStringParameters,

    -- ** InvalidOrigin
    _InvalidOrigin,

    -- ** TooManyOriginCustomHeaders
    _TooManyOriginCustomHeaders,

    -- ** InvalidTTLOrder
    _InvalidTTLOrder,

    -- ** InvalidRequiredProtocol
    _InvalidRequiredProtocol,

    -- ** TooManyDistributionsAssociatedToOriginAccessControl
    _TooManyDistributionsAssociatedToOriginAccessControl,

    -- ** TooManyCookiesInOriginRequestPolicy
    _TooManyCookiesInOriginRequestPolicy,

    -- ** RealtimeLogConfigInUse
    _RealtimeLogConfigInUse,

    -- ** ResponseHeadersPolicyAlreadyExists
    _ResponseHeadersPolicyAlreadyExists,

    -- ** QueryArgProfileEmpty
    _QueryArgProfileEmpty,

    -- ** InvalidIfMatchVersion
    _InvalidIfMatchVersion,

    -- ** TooManyPublicKeysInKeyGroup
    _TooManyPublicKeysInKeyGroup,

    -- ** ResponseHeadersPolicyInUse
    _ResponseHeadersPolicyInUse,

    -- ** InvalidRelativePath
    _InvalidRelativePath,

    -- ** TooManyCachePolicies
    _TooManyCachePolicies,

    -- ** RealtimeLogConfigAlreadyExists
    _RealtimeLogConfigAlreadyExists,

    -- ** TooManyHeadersInCachePolicy
    _TooManyHeadersInCachePolicy,

    -- ** TooLongCSPInResponseHeadersPolicy
    _TooLongCSPInResponseHeadersPolicy,

    -- ** TooManyDistributionsAssociatedToOriginRequestPolicy
    _TooManyDistributionsAssociatedToOriginRequestPolicy,

    -- ** InvalidTagging
    _InvalidTagging,

    -- ** FieldLevelEncryptionConfigAlreadyExists
    _FieldLevelEncryptionConfigAlreadyExists,

    -- ** StreamingDistributionAlreadyExists
    _StreamingDistributionAlreadyExists,

    -- ** RealtimeLogConfigOwnerMismatch
    _RealtimeLogConfigOwnerMismatch,

    -- ** TooManyLambdaFunctionAssociations
    _TooManyLambdaFunctionAssociations,

    -- ** InvalidGeoRestrictionParameter
    _InvalidGeoRestrictionParameter,

    -- ** IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior
    _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior,

    -- ** TrustedKeyGroupDoesNotExist
    _TrustedKeyGroupDoesNotExist,

    -- ** TooManyQueryStringsInCachePolicy
    _TooManyQueryStringsInCachePolicy,

    -- ** TooManyResponseHeadersPolicies
    _TooManyResponseHeadersPolicies,

    -- ** TooManyOriginGroupsPerDistribution
    _TooManyOriginGroupsPerDistribution,

    -- ** TooManyDistributionsWithFunctionAssociations
    _TooManyDistributionsWithFunctionAssociations,

    -- ** InvalidLocationCode
    _InvalidLocationCode,

    -- ** FieldLevelEncryptionConfigInUse
    _FieldLevelEncryptionConfigInUse,

    -- ** ResourceInUse
    _ResourceInUse,

    -- ** TooManyDistributionCNAMEs
    _TooManyDistributionCNAMEs,

    -- ** TooManyFieldLevelEncryptionEncryptionEntities
    _TooManyFieldLevelEncryptionEncryptionEntities,

    -- ** MissingBody
    _MissingBody,

    -- ** TooManyPublicKeys
    _TooManyPublicKeys,

    -- ** TooManyFieldLevelEncryptionFieldPatterns
    _TooManyFieldLevelEncryptionFieldPatterns,

    -- ** TooManyOrigins
    _TooManyOrigins,

    -- ** PublicKeyAlreadyExists
    _PublicKeyAlreadyExists,

    -- ** CannotChangeImmutablePublicKeyFields
    _CannotChangeImmutablePublicKeyFields,

    -- ** NoSuchOriginRequestPolicy
    _NoSuchOriginRequestPolicy,

    -- ** TooManyDistributionsAssociatedToFieldLevelEncryptionConfig
    _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig,

    -- ** TooManyFieldLevelEncryptionContentTypeProfiles
    _TooManyFieldLevelEncryptionContentTypeProfiles,

    -- ** StreamingDistributionNotDisabled
    _StreamingDistributionNotDisabled,

    -- ** TooManyDistributionsWithLambdaAssociations
    _TooManyDistributionsWithLambdaAssociations,

    -- ** CloudFrontOriginAccessIdentityInUse
    _CloudFrontOriginAccessIdentityInUse,

    -- ** PublicKeyInUse
    _PublicKeyInUse,

    -- ** UnsupportedOperation
    _UnsupportedOperation,

    -- ** IllegalDelete
    _IllegalDelete,

    -- ** NoSuchDistribution
    _NoSuchDistribution,

    -- ** InconsistentQuantities
    _InconsistentQuantities,

    -- ** InvalidOriginAccessControl
    _InvalidOriginAccessControl,

    -- ** TooManyDistributionsAssociatedToResponseHeadersPolicy
    _TooManyDistributionsAssociatedToResponseHeadersPolicy,

    -- ** CloudFrontOriginAccessIdentityAlreadyExists
    _CloudFrontOriginAccessIdentityAlreadyExists,

    -- ** TooManyCustomHeadersInResponseHeadersPolicy
    _TooManyCustomHeadersInResponseHeadersPolicy,

    -- ** OriginRequestPolicyInUse
    _OriginRequestPolicyInUse,

    -- ** CachePolicyAlreadyExists
    _CachePolicyAlreadyExists,

    -- ** NoSuchResponseHeadersPolicy
    _NoSuchResponseHeadersPolicy,

    -- ** TooManyOriginAccessControls
    _TooManyOriginAccessControls,

    -- ** InvalidProtocolSettings
    _InvalidProtocolSettings,

    -- ** StagingDistributionInUse
    _StagingDistributionInUse,

    -- ** NoSuchCachePolicy
    _NoSuchCachePolicy,

    -- ** TooManyFieldLevelEncryptionConfigs
    _TooManyFieldLevelEncryptionConfigs,

    -- ** TooManyDistributionsAssociatedToCachePolicy
    _TooManyDistributionsAssociatedToCachePolicy,

    -- ** NoSuchRealtimeLogConfig
    _NoSuchRealtimeLogConfig,

    -- ** TooManyHeadersInOriginRequestPolicy
    _TooManyHeadersInOriginRequestPolicy,

    -- ** TooManyOriginRequestPolicies
    _TooManyOriginRequestPolicies,

    -- ** InvalidMinimumProtocolVersion
    _InvalidMinimumProtocolVersion,

    -- * Waiters
    -- $waiters

    -- ** DistributionDeployed
    newDistributionDeployed,

    -- ** InvalidationCompleted
    newInvalidationCompleted,

    -- ** StreamingDistributionDeployed
    newStreamingDistributionDeployed,

    -- * Operations
    -- $operations

    -- ** AssociateAlias
    AssociateAlias (AssociateAlias'),
    newAssociateAlias,
    AssociateAliasResponse (AssociateAliasResponse'),
    newAssociateAliasResponse,

    -- ** CopyDistribution
    CopyDistribution (CopyDistribution'),
    newCopyDistribution,
    CopyDistributionResponse (CopyDistributionResponse'),
    newCopyDistributionResponse,

    -- ** CreateCachePolicy
    CreateCachePolicy (CreateCachePolicy'),
    newCreateCachePolicy,
    CreateCachePolicyResponse (CreateCachePolicyResponse'),
    newCreateCachePolicyResponse,

    -- ** CreateCloudFrontOriginAccessIdentity
    CreateCloudFrontOriginAccessIdentity (CreateCloudFrontOriginAccessIdentity'),
    newCreateCloudFrontOriginAccessIdentity,
    CreateCloudFrontOriginAccessIdentityResponse (CreateCloudFrontOriginAccessIdentityResponse'),
    newCreateCloudFrontOriginAccessIdentityResponse,

    -- ** CreateContinuousDeploymentPolicy
    CreateContinuousDeploymentPolicy (CreateContinuousDeploymentPolicy'),
    newCreateContinuousDeploymentPolicy,
    CreateContinuousDeploymentPolicyResponse (CreateContinuousDeploymentPolicyResponse'),
    newCreateContinuousDeploymentPolicyResponse,

    -- ** CreateDistribution
    CreateDistribution (CreateDistribution'),
    newCreateDistribution,
    CreateDistributionResponse (CreateDistributionResponse'),
    newCreateDistributionResponse,

    -- ** CreateDistributionWithTags
    CreateDistributionWithTags (CreateDistributionWithTags'),
    newCreateDistributionWithTags,
    CreateDistributionWithTagsResponse (CreateDistributionWithTagsResponse'),
    newCreateDistributionWithTagsResponse,

    -- ** CreateFieldLevelEncryptionConfig
    CreateFieldLevelEncryptionConfig (CreateFieldLevelEncryptionConfig'),
    newCreateFieldLevelEncryptionConfig,
    CreateFieldLevelEncryptionConfigResponse (CreateFieldLevelEncryptionConfigResponse'),
    newCreateFieldLevelEncryptionConfigResponse,

    -- ** CreateFieldLevelEncryptionProfile
    CreateFieldLevelEncryptionProfile (CreateFieldLevelEncryptionProfile'),
    newCreateFieldLevelEncryptionProfile,
    CreateFieldLevelEncryptionProfileResponse (CreateFieldLevelEncryptionProfileResponse'),
    newCreateFieldLevelEncryptionProfileResponse,

    -- ** CreateFunction
    CreateFunction (CreateFunction'),
    newCreateFunction,
    CreateFunctionResponse (CreateFunctionResponse'),
    newCreateFunctionResponse,

    -- ** CreateInvalidation
    CreateInvalidation (CreateInvalidation'),
    newCreateInvalidation,
    CreateInvalidationResponse (CreateInvalidationResponse'),
    newCreateInvalidationResponse,

    -- ** CreateKeyGroup
    CreateKeyGroup (CreateKeyGroup'),
    newCreateKeyGroup,
    CreateKeyGroupResponse (CreateKeyGroupResponse'),
    newCreateKeyGroupResponse,

    -- ** CreateMonitoringSubscription
    CreateMonitoringSubscription (CreateMonitoringSubscription'),
    newCreateMonitoringSubscription,
    CreateMonitoringSubscriptionResponse (CreateMonitoringSubscriptionResponse'),
    newCreateMonitoringSubscriptionResponse,

    -- ** CreateOriginAccessControl
    CreateOriginAccessControl (CreateOriginAccessControl'),
    newCreateOriginAccessControl,
    CreateOriginAccessControlResponse (CreateOriginAccessControlResponse'),
    newCreateOriginAccessControlResponse,

    -- ** CreateOriginRequestPolicy
    CreateOriginRequestPolicy (CreateOriginRequestPolicy'),
    newCreateOriginRequestPolicy,
    CreateOriginRequestPolicyResponse (CreateOriginRequestPolicyResponse'),
    newCreateOriginRequestPolicyResponse,

    -- ** CreatePublicKey
    CreatePublicKey (CreatePublicKey'),
    newCreatePublicKey,
    CreatePublicKeyResponse (CreatePublicKeyResponse'),
    newCreatePublicKeyResponse,

    -- ** CreateRealtimeLogConfig
    CreateRealtimeLogConfig (CreateRealtimeLogConfig'),
    newCreateRealtimeLogConfig,
    CreateRealtimeLogConfigResponse (CreateRealtimeLogConfigResponse'),
    newCreateRealtimeLogConfigResponse,

    -- ** CreateResponseHeadersPolicy
    CreateResponseHeadersPolicy (CreateResponseHeadersPolicy'),
    newCreateResponseHeadersPolicy,
    CreateResponseHeadersPolicyResponse (CreateResponseHeadersPolicyResponse'),
    newCreateResponseHeadersPolicyResponse,

    -- ** CreateStreamingDistribution
    CreateStreamingDistribution (CreateStreamingDistribution'),
    newCreateStreamingDistribution,
    CreateStreamingDistributionResponse (CreateStreamingDistributionResponse'),
    newCreateStreamingDistributionResponse,

    -- ** CreateStreamingDistributionWithTags
    CreateStreamingDistributionWithTags (CreateStreamingDistributionWithTags'),
    newCreateStreamingDistributionWithTags,
    CreateStreamingDistributionWithTagsResponse (CreateStreamingDistributionWithTagsResponse'),
    newCreateStreamingDistributionWithTagsResponse,

    -- ** DeleteCachePolicy
    DeleteCachePolicy (DeleteCachePolicy'),
    newDeleteCachePolicy,
    DeleteCachePolicyResponse (DeleteCachePolicyResponse'),
    newDeleteCachePolicyResponse,

    -- ** DeleteCloudFrontOriginAccessIdentity
    DeleteCloudFrontOriginAccessIdentity (DeleteCloudFrontOriginAccessIdentity'),
    newDeleteCloudFrontOriginAccessIdentity,
    DeleteCloudFrontOriginAccessIdentityResponse (DeleteCloudFrontOriginAccessIdentityResponse'),
    newDeleteCloudFrontOriginAccessIdentityResponse,

    -- ** DeleteContinuousDeploymentPolicy
    DeleteContinuousDeploymentPolicy (DeleteContinuousDeploymentPolicy'),
    newDeleteContinuousDeploymentPolicy,
    DeleteContinuousDeploymentPolicyResponse (DeleteContinuousDeploymentPolicyResponse'),
    newDeleteContinuousDeploymentPolicyResponse,

    -- ** DeleteDistribution
    DeleteDistribution (DeleteDistribution'),
    newDeleteDistribution,
    DeleteDistributionResponse (DeleteDistributionResponse'),
    newDeleteDistributionResponse,

    -- ** DeleteFieldLevelEncryptionConfig
    DeleteFieldLevelEncryptionConfig (DeleteFieldLevelEncryptionConfig'),
    newDeleteFieldLevelEncryptionConfig,
    DeleteFieldLevelEncryptionConfigResponse (DeleteFieldLevelEncryptionConfigResponse'),
    newDeleteFieldLevelEncryptionConfigResponse,

    -- ** DeleteFieldLevelEncryptionProfile
    DeleteFieldLevelEncryptionProfile (DeleteFieldLevelEncryptionProfile'),
    newDeleteFieldLevelEncryptionProfile,
    DeleteFieldLevelEncryptionProfileResponse (DeleteFieldLevelEncryptionProfileResponse'),
    newDeleteFieldLevelEncryptionProfileResponse,

    -- ** DeleteFunction
    DeleteFunction (DeleteFunction'),
    newDeleteFunction,
    DeleteFunctionResponse (DeleteFunctionResponse'),
    newDeleteFunctionResponse,

    -- ** DeleteKeyGroup
    DeleteKeyGroup (DeleteKeyGroup'),
    newDeleteKeyGroup,
    DeleteKeyGroupResponse (DeleteKeyGroupResponse'),
    newDeleteKeyGroupResponse,

    -- ** DeleteMonitoringSubscription
    DeleteMonitoringSubscription (DeleteMonitoringSubscription'),
    newDeleteMonitoringSubscription,
    DeleteMonitoringSubscriptionResponse (DeleteMonitoringSubscriptionResponse'),
    newDeleteMonitoringSubscriptionResponse,

    -- ** DeleteOriginAccessControl
    DeleteOriginAccessControl (DeleteOriginAccessControl'),
    newDeleteOriginAccessControl,
    DeleteOriginAccessControlResponse (DeleteOriginAccessControlResponse'),
    newDeleteOriginAccessControlResponse,

    -- ** DeleteOriginRequestPolicy
    DeleteOriginRequestPolicy (DeleteOriginRequestPolicy'),
    newDeleteOriginRequestPolicy,
    DeleteOriginRequestPolicyResponse (DeleteOriginRequestPolicyResponse'),
    newDeleteOriginRequestPolicyResponse,

    -- ** DeletePublicKey
    DeletePublicKey (DeletePublicKey'),
    newDeletePublicKey,
    DeletePublicKeyResponse (DeletePublicKeyResponse'),
    newDeletePublicKeyResponse,

    -- ** DeleteRealtimeLogConfig
    DeleteRealtimeLogConfig (DeleteRealtimeLogConfig'),
    newDeleteRealtimeLogConfig,
    DeleteRealtimeLogConfigResponse (DeleteRealtimeLogConfigResponse'),
    newDeleteRealtimeLogConfigResponse,

    -- ** DeleteResponseHeadersPolicy
    DeleteResponseHeadersPolicy (DeleteResponseHeadersPolicy'),
    newDeleteResponseHeadersPolicy,
    DeleteResponseHeadersPolicyResponse (DeleteResponseHeadersPolicyResponse'),
    newDeleteResponseHeadersPolicyResponse,

    -- ** DeleteStreamingDistribution
    DeleteStreamingDistribution (DeleteStreamingDistribution'),
    newDeleteStreamingDistribution,
    DeleteStreamingDistributionResponse (DeleteStreamingDistributionResponse'),
    newDeleteStreamingDistributionResponse,

    -- ** DescribeFunction
    DescribeFunction (DescribeFunction'),
    newDescribeFunction,
    DescribeFunctionResponse (DescribeFunctionResponse'),
    newDescribeFunctionResponse,

    -- ** GetCachePolicy
    GetCachePolicy (GetCachePolicy'),
    newGetCachePolicy,
    GetCachePolicyResponse (GetCachePolicyResponse'),
    newGetCachePolicyResponse,

    -- ** GetCachePolicyConfig
    GetCachePolicyConfig (GetCachePolicyConfig'),
    newGetCachePolicyConfig,
    GetCachePolicyConfigResponse (GetCachePolicyConfigResponse'),
    newGetCachePolicyConfigResponse,

    -- ** GetCloudFrontOriginAccessIdentity
    GetCloudFrontOriginAccessIdentity (GetCloudFrontOriginAccessIdentity'),
    newGetCloudFrontOriginAccessIdentity,
    GetCloudFrontOriginAccessIdentityResponse (GetCloudFrontOriginAccessIdentityResponse'),
    newGetCloudFrontOriginAccessIdentityResponse,

    -- ** GetCloudFrontOriginAccessIdentityConfig
    GetCloudFrontOriginAccessIdentityConfig (GetCloudFrontOriginAccessIdentityConfig'),
    newGetCloudFrontOriginAccessIdentityConfig,
    GetCloudFrontOriginAccessIdentityConfigResponse (GetCloudFrontOriginAccessIdentityConfigResponse'),
    newGetCloudFrontOriginAccessIdentityConfigResponse,

    -- ** GetContinuousDeploymentPolicy
    GetContinuousDeploymentPolicy (GetContinuousDeploymentPolicy'),
    newGetContinuousDeploymentPolicy,
    GetContinuousDeploymentPolicyResponse (GetContinuousDeploymentPolicyResponse'),
    newGetContinuousDeploymentPolicyResponse,

    -- ** GetContinuousDeploymentPolicyConfig
    GetContinuousDeploymentPolicyConfig (GetContinuousDeploymentPolicyConfig'),
    newGetContinuousDeploymentPolicyConfig,
    GetContinuousDeploymentPolicyConfigResponse (GetContinuousDeploymentPolicyConfigResponse'),
    newGetContinuousDeploymentPolicyConfigResponse,

    -- ** GetDistribution
    GetDistribution (GetDistribution'),
    newGetDistribution,
    GetDistributionResponse (GetDistributionResponse'),
    newGetDistributionResponse,

    -- ** GetDistributionConfig
    GetDistributionConfig (GetDistributionConfig'),
    newGetDistributionConfig,
    GetDistributionConfigResponse (GetDistributionConfigResponse'),
    newGetDistributionConfigResponse,

    -- ** GetFieldLevelEncryption
    GetFieldLevelEncryption (GetFieldLevelEncryption'),
    newGetFieldLevelEncryption,
    GetFieldLevelEncryptionResponse (GetFieldLevelEncryptionResponse'),
    newGetFieldLevelEncryptionResponse,

    -- ** GetFieldLevelEncryptionConfig
    GetFieldLevelEncryptionConfig (GetFieldLevelEncryptionConfig'),
    newGetFieldLevelEncryptionConfig,
    GetFieldLevelEncryptionConfigResponse (GetFieldLevelEncryptionConfigResponse'),
    newGetFieldLevelEncryptionConfigResponse,

    -- ** GetFieldLevelEncryptionProfile
    GetFieldLevelEncryptionProfile (GetFieldLevelEncryptionProfile'),
    newGetFieldLevelEncryptionProfile,
    GetFieldLevelEncryptionProfileResponse (GetFieldLevelEncryptionProfileResponse'),
    newGetFieldLevelEncryptionProfileResponse,

    -- ** GetFieldLevelEncryptionProfileConfig
    GetFieldLevelEncryptionProfileConfig (GetFieldLevelEncryptionProfileConfig'),
    newGetFieldLevelEncryptionProfileConfig,
    GetFieldLevelEncryptionProfileConfigResponse (GetFieldLevelEncryptionProfileConfigResponse'),
    newGetFieldLevelEncryptionProfileConfigResponse,

    -- ** GetFunction
    GetFunction (GetFunction'),
    newGetFunction,
    GetFunctionResponse (GetFunctionResponse'),
    newGetFunctionResponse,

    -- ** GetInvalidation
    GetInvalidation (GetInvalidation'),
    newGetInvalidation,
    GetInvalidationResponse (GetInvalidationResponse'),
    newGetInvalidationResponse,

    -- ** GetKeyGroup
    GetKeyGroup (GetKeyGroup'),
    newGetKeyGroup,
    GetKeyGroupResponse (GetKeyGroupResponse'),
    newGetKeyGroupResponse,

    -- ** GetKeyGroupConfig
    GetKeyGroupConfig (GetKeyGroupConfig'),
    newGetKeyGroupConfig,
    GetKeyGroupConfigResponse (GetKeyGroupConfigResponse'),
    newGetKeyGroupConfigResponse,

    -- ** GetMonitoringSubscription
    GetMonitoringSubscription (GetMonitoringSubscription'),
    newGetMonitoringSubscription,
    GetMonitoringSubscriptionResponse (GetMonitoringSubscriptionResponse'),
    newGetMonitoringSubscriptionResponse,

    -- ** GetOriginAccessControl
    GetOriginAccessControl (GetOriginAccessControl'),
    newGetOriginAccessControl,
    GetOriginAccessControlResponse (GetOriginAccessControlResponse'),
    newGetOriginAccessControlResponse,

    -- ** GetOriginAccessControlConfig
    GetOriginAccessControlConfig (GetOriginAccessControlConfig'),
    newGetOriginAccessControlConfig,
    GetOriginAccessControlConfigResponse (GetOriginAccessControlConfigResponse'),
    newGetOriginAccessControlConfigResponse,

    -- ** GetOriginRequestPolicy
    GetOriginRequestPolicy (GetOriginRequestPolicy'),
    newGetOriginRequestPolicy,
    GetOriginRequestPolicyResponse (GetOriginRequestPolicyResponse'),
    newGetOriginRequestPolicyResponse,

    -- ** GetOriginRequestPolicyConfig
    GetOriginRequestPolicyConfig (GetOriginRequestPolicyConfig'),
    newGetOriginRequestPolicyConfig,
    GetOriginRequestPolicyConfigResponse (GetOriginRequestPolicyConfigResponse'),
    newGetOriginRequestPolicyConfigResponse,

    -- ** GetPublicKey
    GetPublicKey (GetPublicKey'),
    newGetPublicKey,
    GetPublicKeyResponse (GetPublicKeyResponse'),
    newGetPublicKeyResponse,

    -- ** GetPublicKeyConfig
    GetPublicKeyConfig (GetPublicKeyConfig'),
    newGetPublicKeyConfig,
    GetPublicKeyConfigResponse (GetPublicKeyConfigResponse'),
    newGetPublicKeyConfigResponse,

    -- ** GetRealtimeLogConfig
    GetRealtimeLogConfig (GetRealtimeLogConfig'),
    newGetRealtimeLogConfig,
    GetRealtimeLogConfigResponse (GetRealtimeLogConfigResponse'),
    newGetRealtimeLogConfigResponse,

    -- ** GetResponseHeadersPolicy
    GetResponseHeadersPolicy (GetResponseHeadersPolicy'),
    newGetResponseHeadersPolicy,
    GetResponseHeadersPolicyResponse (GetResponseHeadersPolicyResponse'),
    newGetResponseHeadersPolicyResponse,

    -- ** GetResponseHeadersPolicyConfig
    GetResponseHeadersPolicyConfig (GetResponseHeadersPolicyConfig'),
    newGetResponseHeadersPolicyConfig,
    GetResponseHeadersPolicyConfigResponse (GetResponseHeadersPolicyConfigResponse'),
    newGetResponseHeadersPolicyConfigResponse,

    -- ** GetStreamingDistribution
    GetStreamingDistribution (GetStreamingDistribution'),
    newGetStreamingDistribution,
    GetStreamingDistributionResponse (GetStreamingDistributionResponse'),
    newGetStreamingDistributionResponse,

    -- ** GetStreamingDistributionConfig
    GetStreamingDistributionConfig (GetStreamingDistributionConfig'),
    newGetStreamingDistributionConfig,
    GetStreamingDistributionConfigResponse (GetStreamingDistributionConfigResponse'),
    newGetStreamingDistributionConfigResponse,

    -- ** ListCachePolicies
    ListCachePolicies (ListCachePolicies'),
    newListCachePolicies,
    ListCachePoliciesResponse (ListCachePoliciesResponse'),
    newListCachePoliciesResponse,

    -- ** ListCloudFrontOriginAccessIdentities (Paginated)
    ListCloudFrontOriginAccessIdentities (ListCloudFrontOriginAccessIdentities'),
    newListCloudFrontOriginAccessIdentities,
    ListCloudFrontOriginAccessIdentitiesResponse (ListCloudFrontOriginAccessIdentitiesResponse'),
    newListCloudFrontOriginAccessIdentitiesResponse,

    -- ** ListConflictingAliases
    ListConflictingAliases (ListConflictingAliases'),
    newListConflictingAliases,
    ListConflictingAliasesResponse (ListConflictingAliasesResponse'),
    newListConflictingAliasesResponse,

    -- ** ListContinuousDeploymentPolicies
    ListContinuousDeploymentPolicies (ListContinuousDeploymentPolicies'),
    newListContinuousDeploymentPolicies,
    ListContinuousDeploymentPoliciesResponse (ListContinuousDeploymentPoliciesResponse'),
    newListContinuousDeploymentPoliciesResponse,

    -- ** ListDistributions (Paginated)
    ListDistributions (ListDistributions'),
    newListDistributions,
    ListDistributionsResponse (ListDistributionsResponse'),
    newListDistributionsResponse,

    -- ** ListDistributionsByCachePolicyId
    ListDistributionsByCachePolicyId (ListDistributionsByCachePolicyId'),
    newListDistributionsByCachePolicyId,
    ListDistributionsByCachePolicyIdResponse (ListDistributionsByCachePolicyIdResponse'),
    newListDistributionsByCachePolicyIdResponse,

    -- ** ListDistributionsByKeyGroup
    ListDistributionsByKeyGroup (ListDistributionsByKeyGroup'),
    newListDistributionsByKeyGroup,
    ListDistributionsByKeyGroupResponse (ListDistributionsByKeyGroupResponse'),
    newListDistributionsByKeyGroupResponse,

    -- ** ListDistributionsByOriginRequestPolicyId
    ListDistributionsByOriginRequestPolicyId (ListDistributionsByOriginRequestPolicyId'),
    newListDistributionsByOriginRequestPolicyId,
    ListDistributionsByOriginRequestPolicyIdResponse (ListDistributionsByOriginRequestPolicyIdResponse'),
    newListDistributionsByOriginRequestPolicyIdResponse,

    -- ** ListDistributionsByRealtimeLogConfig
    ListDistributionsByRealtimeLogConfig (ListDistributionsByRealtimeLogConfig'),
    newListDistributionsByRealtimeLogConfig,
    ListDistributionsByRealtimeLogConfigResponse (ListDistributionsByRealtimeLogConfigResponse'),
    newListDistributionsByRealtimeLogConfigResponse,

    -- ** ListDistributionsByResponseHeadersPolicyId
    ListDistributionsByResponseHeadersPolicyId (ListDistributionsByResponseHeadersPolicyId'),
    newListDistributionsByResponseHeadersPolicyId,
    ListDistributionsByResponseHeadersPolicyIdResponse (ListDistributionsByResponseHeadersPolicyIdResponse'),
    newListDistributionsByResponseHeadersPolicyIdResponse,

    -- ** ListDistributionsByWebACLId
    ListDistributionsByWebACLId (ListDistributionsByWebACLId'),
    newListDistributionsByWebACLId,
    ListDistributionsByWebACLIdResponse (ListDistributionsByWebACLIdResponse'),
    newListDistributionsByWebACLIdResponse,

    -- ** ListFieldLevelEncryptionConfigs
    ListFieldLevelEncryptionConfigs (ListFieldLevelEncryptionConfigs'),
    newListFieldLevelEncryptionConfigs,
    ListFieldLevelEncryptionConfigsResponse (ListFieldLevelEncryptionConfigsResponse'),
    newListFieldLevelEncryptionConfigsResponse,

    -- ** ListFieldLevelEncryptionProfiles
    ListFieldLevelEncryptionProfiles (ListFieldLevelEncryptionProfiles'),
    newListFieldLevelEncryptionProfiles,
    ListFieldLevelEncryptionProfilesResponse (ListFieldLevelEncryptionProfilesResponse'),
    newListFieldLevelEncryptionProfilesResponse,

    -- ** ListFunctions
    ListFunctions (ListFunctions'),
    newListFunctions,
    ListFunctionsResponse (ListFunctionsResponse'),
    newListFunctionsResponse,

    -- ** ListInvalidations (Paginated)
    ListInvalidations (ListInvalidations'),
    newListInvalidations,
    ListInvalidationsResponse (ListInvalidationsResponse'),
    newListInvalidationsResponse,

    -- ** ListKeyGroups
    ListKeyGroups (ListKeyGroups'),
    newListKeyGroups,
    ListKeyGroupsResponse (ListKeyGroupsResponse'),
    newListKeyGroupsResponse,

    -- ** ListOriginAccessControls
    ListOriginAccessControls (ListOriginAccessControls'),
    newListOriginAccessControls,
    ListOriginAccessControlsResponse (ListOriginAccessControlsResponse'),
    newListOriginAccessControlsResponse,

    -- ** ListOriginRequestPolicies
    ListOriginRequestPolicies (ListOriginRequestPolicies'),
    newListOriginRequestPolicies,
    ListOriginRequestPoliciesResponse (ListOriginRequestPoliciesResponse'),
    newListOriginRequestPoliciesResponse,

    -- ** ListPublicKeys
    ListPublicKeys (ListPublicKeys'),
    newListPublicKeys,
    ListPublicKeysResponse (ListPublicKeysResponse'),
    newListPublicKeysResponse,

    -- ** ListRealtimeLogConfigs
    ListRealtimeLogConfigs (ListRealtimeLogConfigs'),
    newListRealtimeLogConfigs,
    ListRealtimeLogConfigsResponse (ListRealtimeLogConfigsResponse'),
    newListRealtimeLogConfigsResponse,

    -- ** ListResponseHeadersPolicies
    ListResponseHeadersPolicies (ListResponseHeadersPolicies'),
    newListResponseHeadersPolicies,
    ListResponseHeadersPoliciesResponse (ListResponseHeadersPoliciesResponse'),
    newListResponseHeadersPoliciesResponse,

    -- ** ListStreamingDistributions (Paginated)
    ListStreamingDistributions (ListStreamingDistributions'),
    newListStreamingDistributions,
    ListStreamingDistributionsResponse (ListStreamingDistributionsResponse'),
    newListStreamingDistributionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PublishFunction
    PublishFunction (PublishFunction'),
    newPublishFunction,
    PublishFunctionResponse (PublishFunctionResponse'),
    newPublishFunctionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestFunction
    TestFunction (TestFunction'),
    newTestFunction,
    TestFunctionResponse (TestFunctionResponse'),
    newTestFunctionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateCachePolicy
    UpdateCachePolicy (UpdateCachePolicy'),
    newUpdateCachePolicy,
    UpdateCachePolicyResponse (UpdateCachePolicyResponse'),
    newUpdateCachePolicyResponse,

    -- ** UpdateCloudFrontOriginAccessIdentity
    UpdateCloudFrontOriginAccessIdentity (UpdateCloudFrontOriginAccessIdentity'),
    newUpdateCloudFrontOriginAccessIdentity,
    UpdateCloudFrontOriginAccessIdentityResponse (UpdateCloudFrontOriginAccessIdentityResponse'),
    newUpdateCloudFrontOriginAccessIdentityResponse,

    -- ** UpdateContinuousDeploymentPolicy
    UpdateContinuousDeploymentPolicy (UpdateContinuousDeploymentPolicy'),
    newUpdateContinuousDeploymentPolicy,
    UpdateContinuousDeploymentPolicyResponse (UpdateContinuousDeploymentPolicyResponse'),
    newUpdateContinuousDeploymentPolicyResponse,

    -- ** UpdateDistribution
    UpdateDistribution (UpdateDistribution'),
    newUpdateDistribution,
    UpdateDistributionResponse (UpdateDistributionResponse'),
    newUpdateDistributionResponse,

    -- ** UpdateFieldLevelEncryptionConfig
    UpdateFieldLevelEncryptionConfig (UpdateFieldLevelEncryptionConfig'),
    newUpdateFieldLevelEncryptionConfig,
    UpdateFieldLevelEncryptionConfigResponse (UpdateFieldLevelEncryptionConfigResponse'),
    newUpdateFieldLevelEncryptionConfigResponse,

    -- ** UpdateFieldLevelEncryptionProfile
    UpdateFieldLevelEncryptionProfile (UpdateFieldLevelEncryptionProfile'),
    newUpdateFieldLevelEncryptionProfile,
    UpdateFieldLevelEncryptionProfileResponse (UpdateFieldLevelEncryptionProfileResponse'),
    newUpdateFieldLevelEncryptionProfileResponse,

    -- ** UpdateFunction
    UpdateFunction (UpdateFunction'),
    newUpdateFunction,
    UpdateFunctionResponse (UpdateFunctionResponse'),
    newUpdateFunctionResponse,

    -- ** UpdateKeyGroup
    UpdateKeyGroup (UpdateKeyGroup'),
    newUpdateKeyGroup,
    UpdateKeyGroupResponse (UpdateKeyGroupResponse'),
    newUpdateKeyGroupResponse,

    -- ** UpdateOriginAccessControl
    UpdateOriginAccessControl (UpdateOriginAccessControl'),
    newUpdateOriginAccessControl,
    UpdateOriginAccessControlResponse (UpdateOriginAccessControlResponse'),
    newUpdateOriginAccessControlResponse,

    -- ** UpdateOriginRequestPolicy
    UpdateOriginRequestPolicy (UpdateOriginRequestPolicy'),
    newUpdateOriginRequestPolicy,
    UpdateOriginRequestPolicyResponse (UpdateOriginRequestPolicyResponse'),
    newUpdateOriginRequestPolicyResponse,

    -- ** UpdatePublicKey
    UpdatePublicKey (UpdatePublicKey'),
    newUpdatePublicKey,
    UpdatePublicKeyResponse (UpdatePublicKeyResponse'),
    newUpdatePublicKeyResponse,

    -- ** UpdateRealtimeLogConfig
    UpdateRealtimeLogConfig (UpdateRealtimeLogConfig'),
    newUpdateRealtimeLogConfig,
    UpdateRealtimeLogConfigResponse (UpdateRealtimeLogConfigResponse'),
    newUpdateRealtimeLogConfigResponse,

    -- ** UpdateResponseHeadersPolicy
    UpdateResponseHeadersPolicy (UpdateResponseHeadersPolicy'),
    newUpdateResponseHeadersPolicy,
    UpdateResponseHeadersPolicyResponse (UpdateResponseHeadersPolicyResponse'),
    newUpdateResponseHeadersPolicyResponse,

    -- ** UpdateStreamingDistribution
    UpdateStreamingDistribution (UpdateStreamingDistribution'),
    newUpdateStreamingDistribution,
    UpdateStreamingDistributionResponse (UpdateStreamingDistributionResponse'),
    newUpdateStreamingDistributionResponse,

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

    -- ** ContinuousDeploymentPolicyType
    ContinuousDeploymentPolicyType (..),

    -- ** EventType
    EventType (..),

    -- ** Format
    Format (..),

    -- ** FrameOptionsList
    FrameOptionsList (..),

    -- ** FunctionRuntime
    FunctionRuntime (..),

    -- ** FunctionStage
    FunctionStage (..),

    -- ** GeoRestrictionType
    GeoRestrictionType (..),

    -- ** HttpVersion
    HttpVersion (..),

    -- ** ICPRecordalStatus
    ICPRecordalStatus (..),

    -- ** ItemSelection
    ItemSelection (..),

    -- ** Method
    Method (..),

    -- ** MinimumProtocolVersion
    MinimumProtocolVersion (..),

    -- ** OriginAccessControlOriginTypes
    OriginAccessControlOriginTypes (..),

    -- ** OriginAccessControlSigningBehaviors
    OriginAccessControlSigningBehaviors (..),

    -- ** OriginAccessControlSigningProtocols
    OriginAccessControlSigningProtocols (..),

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

    -- ** ReferrerPolicyList
    ReferrerPolicyList (..),

    -- ** ResponseHeadersPolicyAccessControlAllowMethodsValues
    ResponseHeadersPolicyAccessControlAllowMethodsValues (..),

    -- ** ResponseHeadersPolicyType
    ResponseHeadersPolicyType (..),

    -- ** SSLSupportMethod
    SSLSupportMethod (..),

    -- ** SslProtocol
    SslProtocol (..),

    -- ** ViewerProtocolPolicy
    ViewerProtocolPolicy (..),

    -- ** ActiveTrustedKeyGroups
    ActiveTrustedKeyGroups (ActiveTrustedKeyGroups'),
    newActiveTrustedKeyGroups,

    -- ** ActiveTrustedSigners
    ActiveTrustedSigners (ActiveTrustedSigners'),
    newActiveTrustedSigners,

    -- ** AliasICPRecordal
    AliasICPRecordal (AliasICPRecordal'),
    newAliasICPRecordal,

    -- ** Aliases
    Aliases (Aliases'),
    newAliases,

    -- ** AllowedMethods
    AllowedMethods (AllowedMethods'),
    newAllowedMethods,

    -- ** CacheBehavior
    CacheBehavior (CacheBehavior'),
    newCacheBehavior,

    -- ** CacheBehaviors
    CacheBehaviors (CacheBehaviors'),
    newCacheBehaviors,

    -- ** CachePolicy
    CachePolicy (CachePolicy'),
    newCachePolicy,

    -- ** CachePolicyConfig
    CachePolicyConfig (CachePolicyConfig'),
    newCachePolicyConfig,

    -- ** CachePolicyCookiesConfig
    CachePolicyCookiesConfig (CachePolicyCookiesConfig'),
    newCachePolicyCookiesConfig,

    -- ** CachePolicyHeadersConfig
    CachePolicyHeadersConfig (CachePolicyHeadersConfig'),
    newCachePolicyHeadersConfig,

    -- ** CachePolicyList
    CachePolicyList (CachePolicyList'),
    newCachePolicyList,

    -- ** CachePolicyQueryStringsConfig
    CachePolicyQueryStringsConfig (CachePolicyQueryStringsConfig'),
    newCachePolicyQueryStringsConfig,

    -- ** CachePolicySummary
    CachePolicySummary (CachePolicySummary'),
    newCachePolicySummary,

    -- ** CachedMethods
    CachedMethods (CachedMethods'),
    newCachedMethods,

    -- ** CloudFrontOriginAccessIdentity
    CloudFrontOriginAccessIdentity (CloudFrontOriginAccessIdentity'),
    newCloudFrontOriginAccessIdentity,

    -- ** CloudFrontOriginAccessIdentityConfig
    CloudFrontOriginAccessIdentityConfig (CloudFrontOriginAccessIdentityConfig'),
    newCloudFrontOriginAccessIdentityConfig,

    -- ** CloudFrontOriginAccessIdentityList
    CloudFrontOriginAccessIdentityList (CloudFrontOriginAccessIdentityList'),
    newCloudFrontOriginAccessIdentityList,

    -- ** CloudFrontOriginAccessIdentitySummary
    CloudFrontOriginAccessIdentitySummary (CloudFrontOriginAccessIdentitySummary'),
    newCloudFrontOriginAccessIdentitySummary,

    -- ** ConflictingAlias
    ConflictingAlias (ConflictingAlias'),
    newConflictingAlias,

    -- ** ConflictingAliasesList
    ConflictingAliasesList (ConflictingAliasesList'),
    newConflictingAliasesList,

    -- ** ContentTypeProfile
    ContentTypeProfile (ContentTypeProfile'),
    newContentTypeProfile,

    -- ** ContentTypeProfileConfig
    ContentTypeProfileConfig (ContentTypeProfileConfig'),
    newContentTypeProfileConfig,

    -- ** ContentTypeProfiles
    ContentTypeProfiles (ContentTypeProfiles'),
    newContentTypeProfiles,

    -- ** ContinuousDeploymentPolicy
    ContinuousDeploymentPolicy (ContinuousDeploymentPolicy'),
    newContinuousDeploymentPolicy,

    -- ** ContinuousDeploymentPolicyConfig
    ContinuousDeploymentPolicyConfig (ContinuousDeploymentPolicyConfig'),
    newContinuousDeploymentPolicyConfig,

    -- ** ContinuousDeploymentPolicyList
    ContinuousDeploymentPolicyList (ContinuousDeploymentPolicyList'),
    newContinuousDeploymentPolicyList,

    -- ** ContinuousDeploymentPolicySummary
    ContinuousDeploymentPolicySummary (ContinuousDeploymentPolicySummary'),
    newContinuousDeploymentPolicySummary,

    -- ** ContinuousDeploymentSingleHeaderConfig
    ContinuousDeploymentSingleHeaderConfig (ContinuousDeploymentSingleHeaderConfig'),
    newContinuousDeploymentSingleHeaderConfig,

    -- ** ContinuousDeploymentSingleWeightConfig
    ContinuousDeploymentSingleWeightConfig (ContinuousDeploymentSingleWeightConfig'),
    newContinuousDeploymentSingleWeightConfig,

    -- ** CookieNames
    CookieNames (CookieNames'),
    newCookieNames,

    -- ** CookiePreference
    CookiePreference (CookiePreference'),
    newCookiePreference,

    -- ** CustomErrorResponse
    CustomErrorResponse (CustomErrorResponse'),
    newCustomErrorResponse,

    -- ** CustomErrorResponses
    CustomErrorResponses (CustomErrorResponses'),
    newCustomErrorResponses,

    -- ** CustomHeaders
    CustomHeaders (CustomHeaders'),
    newCustomHeaders,

    -- ** CustomOriginConfig
    CustomOriginConfig (CustomOriginConfig'),
    newCustomOriginConfig,

    -- ** DefaultCacheBehavior
    DefaultCacheBehavior (DefaultCacheBehavior'),
    newDefaultCacheBehavior,

    -- ** Distribution
    Distribution (Distribution'),
    newDistribution,

    -- ** DistributionConfig
    DistributionConfig (DistributionConfig'),
    newDistributionConfig,

    -- ** DistributionConfigWithTags
    DistributionConfigWithTags (DistributionConfigWithTags'),
    newDistributionConfigWithTags,

    -- ** DistributionIdList
    DistributionIdList (DistributionIdList'),
    newDistributionIdList,

    -- ** DistributionList
    DistributionList (DistributionList'),
    newDistributionList,

    -- ** DistributionSummary
    DistributionSummary (DistributionSummary'),
    newDistributionSummary,

    -- ** EncryptionEntities
    EncryptionEntities (EncryptionEntities'),
    newEncryptionEntities,

    -- ** EncryptionEntity
    EncryptionEntity (EncryptionEntity'),
    newEncryptionEntity,

    -- ** EndPoint
    EndPoint (EndPoint'),
    newEndPoint,

    -- ** FieldLevelEncryption
    FieldLevelEncryption (FieldLevelEncryption'),
    newFieldLevelEncryption,

    -- ** FieldLevelEncryptionConfig
    FieldLevelEncryptionConfig (FieldLevelEncryptionConfig'),
    newFieldLevelEncryptionConfig,

    -- ** FieldLevelEncryptionList
    FieldLevelEncryptionList (FieldLevelEncryptionList'),
    newFieldLevelEncryptionList,

    -- ** FieldLevelEncryptionProfile
    FieldLevelEncryptionProfile (FieldLevelEncryptionProfile'),
    newFieldLevelEncryptionProfile,

    -- ** FieldLevelEncryptionProfileConfig
    FieldLevelEncryptionProfileConfig (FieldLevelEncryptionProfileConfig'),
    newFieldLevelEncryptionProfileConfig,

    -- ** FieldLevelEncryptionProfileList
    FieldLevelEncryptionProfileList (FieldLevelEncryptionProfileList'),
    newFieldLevelEncryptionProfileList,

    -- ** FieldLevelEncryptionProfileSummary
    FieldLevelEncryptionProfileSummary (FieldLevelEncryptionProfileSummary'),
    newFieldLevelEncryptionProfileSummary,

    -- ** FieldLevelEncryptionSummary
    FieldLevelEncryptionSummary (FieldLevelEncryptionSummary'),
    newFieldLevelEncryptionSummary,

    -- ** FieldPatterns
    FieldPatterns (FieldPatterns'),
    newFieldPatterns,

    -- ** ForwardedValues
    ForwardedValues (ForwardedValues'),
    newForwardedValues,

    -- ** FunctionAssociation
    FunctionAssociation (FunctionAssociation'),
    newFunctionAssociation,

    -- ** FunctionAssociations
    FunctionAssociations (FunctionAssociations'),
    newFunctionAssociations,

    -- ** FunctionConfig
    FunctionConfig (FunctionConfig'),
    newFunctionConfig,

    -- ** FunctionList
    FunctionList (FunctionList'),
    newFunctionList,

    -- ** FunctionMetadata
    FunctionMetadata (FunctionMetadata'),
    newFunctionMetadata,

    -- ** FunctionSummary
    FunctionSummary (FunctionSummary'),
    newFunctionSummary,

    -- ** GeoRestriction
    GeoRestriction (GeoRestriction'),
    newGeoRestriction,

    -- ** Headers
    Headers (Headers'),
    newHeaders,

    -- ** Invalidation
    Invalidation (Invalidation'),
    newInvalidation,

    -- ** InvalidationBatch
    InvalidationBatch (InvalidationBatch'),
    newInvalidationBatch,

    -- ** InvalidationList
    InvalidationList (InvalidationList'),
    newInvalidationList,

    -- ** InvalidationSummary
    InvalidationSummary (InvalidationSummary'),
    newInvalidationSummary,

    -- ** KGKeyPairIds
    KGKeyPairIds (KGKeyPairIds'),
    newKGKeyPairIds,

    -- ** KeyGroup
    KeyGroup (KeyGroup'),
    newKeyGroup,

    -- ** KeyGroupConfig
    KeyGroupConfig (KeyGroupConfig'),
    newKeyGroupConfig,

    -- ** KeyGroupList
    KeyGroupList (KeyGroupList'),
    newKeyGroupList,

    -- ** KeyGroupSummary
    KeyGroupSummary (KeyGroupSummary'),
    newKeyGroupSummary,

    -- ** KeyPairIds
    KeyPairIds (KeyPairIds'),
    newKeyPairIds,

    -- ** KinesisStreamConfig
    KinesisStreamConfig (KinesisStreamConfig'),
    newKinesisStreamConfig,

    -- ** LambdaFunctionAssociation
    LambdaFunctionAssociation (LambdaFunctionAssociation'),
    newLambdaFunctionAssociation,

    -- ** LambdaFunctionAssociations
    LambdaFunctionAssociations (LambdaFunctionAssociations'),
    newLambdaFunctionAssociations,

    -- ** LoggingConfig
    LoggingConfig (LoggingConfig'),
    newLoggingConfig,

    -- ** MonitoringSubscription
    MonitoringSubscription (MonitoringSubscription'),
    newMonitoringSubscription,

    -- ** Origin
    Origin (Origin'),
    newOrigin,

    -- ** OriginAccessControl
    OriginAccessControl (OriginAccessControl'),
    newOriginAccessControl,

    -- ** OriginAccessControlConfig
    OriginAccessControlConfig (OriginAccessControlConfig'),
    newOriginAccessControlConfig,

    -- ** OriginAccessControlList
    OriginAccessControlList (OriginAccessControlList'),
    newOriginAccessControlList,

    -- ** OriginAccessControlSummary
    OriginAccessControlSummary (OriginAccessControlSummary'),
    newOriginAccessControlSummary,

    -- ** OriginCustomHeader
    OriginCustomHeader (OriginCustomHeader'),
    newOriginCustomHeader,

    -- ** OriginGroup
    OriginGroup (OriginGroup'),
    newOriginGroup,

    -- ** OriginGroupFailoverCriteria
    OriginGroupFailoverCriteria (OriginGroupFailoverCriteria'),
    newOriginGroupFailoverCriteria,

    -- ** OriginGroupMember
    OriginGroupMember (OriginGroupMember'),
    newOriginGroupMember,

    -- ** OriginGroupMembers
    OriginGroupMembers (OriginGroupMembers'),
    newOriginGroupMembers,

    -- ** OriginGroups
    OriginGroups (OriginGroups'),
    newOriginGroups,

    -- ** OriginRequestPolicy
    OriginRequestPolicy (OriginRequestPolicy'),
    newOriginRequestPolicy,

    -- ** OriginRequestPolicyConfig
    OriginRequestPolicyConfig (OriginRequestPolicyConfig'),
    newOriginRequestPolicyConfig,

    -- ** OriginRequestPolicyCookiesConfig
    OriginRequestPolicyCookiesConfig (OriginRequestPolicyCookiesConfig'),
    newOriginRequestPolicyCookiesConfig,

    -- ** OriginRequestPolicyHeadersConfig
    OriginRequestPolicyHeadersConfig (OriginRequestPolicyHeadersConfig'),
    newOriginRequestPolicyHeadersConfig,

    -- ** OriginRequestPolicyList
    OriginRequestPolicyList (OriginRequestPolicyList'),
    newOriginRequestPolicyList,

    -- ** OriginRequestPolicyQueryStringsConfig
    OriginRequestPolicyQueryStringsConfig (OriginRequestPolicyQueryStringsConfig'),
    newOriginRequestPolicyQueryStringsConfig,

    -- ** OriginRequestPolicySummary
    OriginRequestPolicySummary (OriginRequestPolicySummary'),
    newOriginRequestPolicySummary,

    -- ** OriginShield
    OriginShield (OriginShield'),
    newOriginShield,

    -- ** OriginSslProtocols
    OriginSslProtocols (OriginSslProtocols'),
    newOriginSslProtocols,

    -- ** Origins
    Origins (Origins'),
    newOrigins,

    -- ** ParametersInCacheKeyAndForwardedToOrigin
    ParametersInCacheKeyAndForwardedToOrigin (ParametersInCacheKeyAndForwardedToOrigin'),
    newParametersInCacheKeyAndForwardedToOrigin,

    -- ** Paths
    Paths (Paths'),
    newPaths,

    -- ** PublicKey
    PublicKey (PublicKey'),
    newPublicKey,

    -- ** PublicKeyConfig
    PublicKeyConfig (PublicKeyConfig'),
    newPublicKeyConfig,

    -- ** PublicKeyList
    PublicKeyList (PublicKeyList'),
    newPublicKeyList,

    -- ** PublicKeySummary
    PublicKeySummary (PublicKeySummary'),
    newPublicKeySummary,

    -- ** QueryArgProfile
    QueryArgProfile (QueryArgProfile'),
    newQueryArgProfile,

    -- ** QueryArgProfileConfig
    QueryArgProfileConfig (QueryArgProfileConfig'),
    newQueryArgProfileConfig,

    -- ** QueryArgProfiles
    QueryArgProfiles (QueryArgProfiles'),
    newQueryArgProfiles,

    -- ** QueryStringCacheKeys
    QueryStringCacheKeys (QueryStringCacheKeys'),
    newQueryStringCacheKeys,

    -- ** QueryStringNames
    QueryStringNames (QueryStringNames'),
    newQueryStringNames,

    -- ** RealtimeLogConfig
    RealtimeLogConfig (RealtimeLogConfig'),
    newRealtimeLogConfig,

    -- ** RealtimeLogConfigs
    RealtimeLogConfigs (RealtimeLogConfigs'),
    newRealtimeLogConfigs,

    -- ** RealtimeMetricsSubscriptionConfig
    RealtimeMetricsSubscriptionConfig (RealtimeMetricsSubscriptionConfig'),
    newRealtimeMetricsSubscriptionConfig,

    -- ** ResponseHeadersPolicy
    ResponseHeadersPolicy (ResponseHeadersPolicy'),
    newResponseHeadersPolicy,

    -- ** ResponseHeadersPolicyAccessControlAllowHeaders
    ResponseHeadersPolicyAccessControlAllowHeaders (ResponseHeadersPolicyAccessControlAllowHeaders'),
    newResponseHeadersPolicyAccessControlAllowHeaders,

    -- ** ResponseHeadersPolicyAccessControlAllowMethods
    ResponseHeadersPolicyAccessControlAllowMethods (ResponseHeadersPolicyAccessControlAllowMethods'),
    newResponseHeadersPolicyAccessControlAllowMethods,

    -- ** ResponseHeadersPolicyAccessControlAllowOrigins
    ResponseHeadersPolicyAccessControlAllowOrigins (ResponseHeadersPolicyAccessControlAllowOrigins'),
    newResponseHeadersPolicyAccessControlAllowOrigins,

    -- ** ResponseHeadersPolicyAccessControlExposeHeaders
    ResponseHeadersPolicyAccessControlExposeHeaders (ResponseHeadersPolicyAccessControlExposeHeaders'),
    newResponseHeadersPolicyAccessControlExposeHeaders,

    -- ** ResponseHeadersPolicyConfig
    ResponseHeadersPolicyConfig (ResponseHeadersPolicyConfig'),
    newResponseHeadersPolicyConfig,

    -- ** ResponseHeadersPolicyContentSecurityPolicy
    ResponseHeadersPolicyContentSecurityPolicy (ResponseHeadersPolicyContentSecurityPolicy'),
    newResponseHeadersPolicyContentSecurityPolicy,

    -- ** ResponseHeadersPolicyContentTypeOptions
    ResponseHeadersPolicyContentTypeOptions (ResponseHeadersPolicyContentTypeOptions'),
    newResponseHeadersPolicyContentTypeOptions,

    -- ** ResponseHeadersPolicyCorsConfig
    ResponseHeadersPolicyCorsConfig (ResponseHeadersPolicyCorsConfig'),
    newResponseHeadersPolicyCorsConfig,

    -- ** ResponseHeadersPolicyCustomHeader
    ResponseHeadersPolicyCustomHeader (ResponseHeadersPolicyCustomHeader'),
    newResponseHeadersPolicyCustomHeader,

    -- ** ResponseHeadersPolicyCustomHeadersConfig
    ResponseHeadersPolicyCustomHeadersConfig (ResponseHeadersPolicyCustomHeadersConfig'),
    newResponseHeadersPolicyCustomHeadersConfig,

    -- ** ResponseHeadersPolicyFrameOptions
    ResponseHeadersPolicyFrameOptions (ResponseHeadersPolicyFrameOptions'),
    newResponseHeadersPolicyFrameOptions,

    -- ** ResponseHeadersPolicyList
    ResponseHeadersPolicyList (ResponseHeadersPolicyList'),
    newResponseHeadersPolicyList,

    -- ** ResponseHeadersPolicyReferrerPolicy
    ResponseHeadersPolicyReferrerPolicy (ResponseHeadersPolicyReferrerPolicy'),
    newResponseHeadersPolicyReferrerPolicy,

    -- ** ResponseHeadersPolicySecurityHeadersConfig
    ResponseHeadersPolicySecurityHeadersConfig (ResponseHeadersPolicySecurityHeadersConfig'),
    newResponseHeadersPolicySecurityHeadersConfig,

    -- ** ResponseHeadersPolicyServerTimingHeadersConfig
    ResponseHeadersPolicyServerTimingHeadersConfig (ResponseHeadersPolicyServerTimingHeadersConfig'),
    newResponseHeadersPolicyServerTimingHeadersConfig,

    -- ** ResponseHeadersPolicyStrictTransportSecurity
    ResponseHeadersPolicyStrictTransportSecurity (ResponseHeadersPolicyStrictTransportSecurity'),
    newResponseHeadersPolicyStrictTransportSecurity,

    -- ** ResponseHeadersPolicySummary
    ResponseHeadersPolicySummary (ResponseHeadersPolicySummary'),
    newResponseHeadersPolicySummary,

    -- ** ResponseHeadersPolicyXSSProtection
    ResponseHeadersPolicyXSSProtection (ResponseHeadersPolicyXSSProtection'),
    newResponseHeadersPolicyXSSProtection,

    -- ** Restrictions
    Restrictions (Restrictions'),
    newRestrictions,

    -- ** S3Origin
    S3Origin (S3Origin'),
    newS3Origin,

    -- ** S3OriginConfig
    S3OriginConfig (S3OriginConfig'),
    newS3OriginConfig,

    -- ** SessionStickinessConfig
    SessionStickinessConfig (SessionStickinessConfig'),
    newSessionStickinessConfig,

    -- ** Signer
    Signer (Signer'),
    newSigner,

    -- ** StagingDistributionDnsNames
    StagingDistributionDnsNames (StagingDistributionDnsNames'),
    newStagingDistributionDnsNames,

    -- ** StatusCodes
    StatusCodes (StatusCodes'),
    newStatusCodes,

    -- ** StreamingDistribution
    StreamingDistribution (StreamingDistribution'),
    newStreamingDistribution,

    -- ** StreamingDistributionConfig
    StreamingDistributionConfig (StreamingDistributionConfig'),
    newStreamingDistributionConfig,

    -- ** StreamingDistributionConfigWithTags
    StreamingDistributionConfigWithTags (StreamingDistributionConfigWithTags'),
    newStreamingDistributionConfigWithTags,

    -- ** StreamingDistributionList
    StreamingDistributionList (StreamingDistributionList'),
    newStreamingDistributionList,

    -- ** StreamingDistributionSummary
    StreamingDistributionSummary (StreamingDistributionSummary'),
    newStreamingDistributionSummary,

    -- ** StreamingLoggingConfig
    StreamingLoggingConfig (StreamingLoggingConfig'),
    newStreamingLoggingConfig,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagKeys
    TagKeys (TagKeys'),
    newTagKeys,

    -- ** Tags
    Tags (Tags'),
    newTags,

    -- ** TestResult
    TestResult (TestResult'),
    newTestResult,

    -- ** TrafficConfig
    TrafficConfig (TrafficConfig'),
    newTrafficConfig,

    -- ** TrustedKeyGroups
    TrustedKeyGroups (TrustedKeyGroups'),
    newTrustedKeyGroups,

    -- ** TrustedSigners
    TrustedSigners (TrustedSigners'),
    newTrustedSigners,

    -- ** ViewerCertificate
    ViewerCertificate (ViewerCertificate'),
    newViewerCertificate,
  )
where

import Amazonka.CloudFront.AssociateAlias
import Amazonka.CloudFront.CopyDistribution
import Amazonka.CloudFront.CreateCachePolicy
import Amazonka.CloudFront.CreateCloudFrontOriginAccessIdentity
import Amazonka.CloudFront.CreateContinuousDeploymentPolicy
import Amazonka.CloudFront.CreateDistribution
import Amazonka.CloudFront.CreateDistributionWithTags
import Amazonka.CloudFront.CreateFieldLevelEncryptionConfig
import Amazonka.CloudFront.CreateFieldLevelEncryptionProfile
import Amazonka.CloudFront.CreateFunction
import Amazonka.CloudFront.CreateInvalidation
import Amazonka.CloudFront.CreateKeyGroup
import Amazonka.CloudFront.CreateMonitoringSubscription
import Amazonka.CloudFront.CreateOriginAccessControl
import Amazonka.CloudFront.CreateOriginRequestPolicy
import Amazonka.CloudFront.CreatePublicKey
import Amazonka.CloudFront.CreateRealtimeLogConfig
import Amazonka.CloudFront.CreateResponseHeadersPolicy
import Amazonka.CloudFront.CreateStreamingDistribution
import Amazonka.CloudFront.CreateStreamingDistributionWithTags
import Amazonka.CloudFront.DeleteCachePolicy
import Amazonka.CloudFront.DeleteCloudFrontOriginAccessIdentity
import Amazonka.CloudFront.DeleteContinuousDeploymentPolicy
import Amazonka.CloudFront.DeleteDistribution
import Amazonka.CloudFront.DeleteFieldLevelEncryptionConfig
import Amazonka.CloudFront.DeleteFieldLevelEncryptionProfile
import Amazonka.CloudFront.DeleteFunction
import Amazonka.CloudFront.DeleteKeyGroup
import Amazonka.CloudFront.DeleteMonitoringSubscription
import Amazonka.CloudFront.DeleteOriginAccessControl
import Amazonka.CloudFront.DeleteOriginRequestPolicy
import Amazonka.CloudFront.DeletePublicKey
import Amazonka.CloudFront.DeleteRealtimeLogConfig
import Amazonka.CloudFront.DeleteResponseHeadersPolicy
import Amazonka.CloudFront.DeleteStreamingDistribution
import Amazonka.CloudFront.DescribeFunction
import Amazonka.CloudFront.GetCachePolicy
import Amazonka.CloudFront.GetCachePolicyConfig
import Amazonka.CloudFront.GetCloudFrontOriginAccessIdentity
import Amazonka.CloudFront.GetCloudFrontOriginAccessIdentityConfig
import Amazonka.CloudFront.GetContinuousDeploymentPolicy
import Amazonka.CloudFront.GetContinuousDeploymentPolicyConfig
import Amazonka.CloudFront.GetDistribution
import Amazonka.CloudFront.GetDistributionConfig
import Amazonka.CloudFront.GetFieldLevelEncryption
import Amazonka.CloudFront.GetFieldLevelEncryptionConfig
import Amazonka.CloudFront.GetFieldLevelEncryptionProfile
import Amazonka.CloudFront.GetFieldLevelEncryptionProfileConfig
import Amazonka.CloudFront.GetFunction
import Amazonka.CloudFront.GetInvalidation
import Amazonka.CloudFront.GetKeyGroup
import Amazonka.CloudFront.GetKeyGroupConfig
import Amazonka.CloudFront.GetMonitoringSubscription
import Amazonka.CloudFront.GetOriginAccessControl
import Amazonka.CloudFront.GetOriginAccessControlConfig
import Amazonka.CloudFront.GetOriginRequestPolicy
import Amazonka.CloudFront.GetOriginRequestPolicyConfig
import Amazonka.CloudFront.GetPublicKey
import Amazonka.CloudFront.GetPublicKeyConfig
import Amazonka.CloudFront.GetRealtimeLogConfig
import Amazonka.CloudFront.GetResponseHeadersPolicy
import Amazonka.CloudFront.GetResponseHeadersPolicyConfig
import Amazonka.CloudFront.GetStreamingDistribution
import Amazonka.CloudFront.GetStreamingDistributionConfig
import Amazonka.CloudFront.Lens
import Amazonka.CloudFront.ListCachePolicies
import Amazonka.CloudFront.ListCloudFrontOriginAccessIdentities
import Amazonka.CloudFront.ListConflictingAliases
import Amazonka.CloudFront.ListContinuousDeploymentPolicies
import Amazonka.CloudFront.ListDistributions
import Amazonka.CloudFront.ListDistributionsByCachePolicyId
import Amazonka.CloudFront.ListDistributionsByKeyGroup
import Amazonka.CloudFront.ListDistributionsByOriginRequestPolicyId
import Amazonka.CloudFront.ListDistributionsByRealtimeLogConfig
import Amazonka.CloudFront.ListDistributionsByResponseHeadersPolicyId
import Amazonka.CloudFront.ListDistributionsByWebACLId
import Amazonka.CloudFront.ListFieldLevelEncryptionConfigs
import Amazonka.CloudFront.ListFieldLevelEncryptionProfiles
import Amazonka.CloudFront.ListFunctions
import Amazonka.CloudFront.ListInvalidations
import Amazonka.CloudFront.ListKeyGroups
import Amazonka.CloudFront.ListOriginAccessControls
import Amazonka.CloudFront.ListOriginRequestPolicies
import Amazonka.CloudFront.ListPublicKeys
import Amazonka.CloudFront.ListRealtimeLogConfigs
import Amazonka.CloudFront.ListResponseHeadersPolicies
import Amazonka.CloudFront.ListStreamingDistributions
import Amazonka.CloudFront.ListTagsForResource
import Amazonka.CloudFront.PublishFunction
import Amazonka.CloudFront.TagResource
import Amazonka.CloudFront.TestFunction
import Amazonka.CloudFront.Types
import Amazonka.CloudFront.UntagResource
import Amazonka.CloudFront.UpdateCachePolicy
import Amazonka.CloudFront.UpdateCloudFrontOriginAccessIdentity
import Amazonka.CloudFront.UpdateContinuousDeploymentPolicy
import Amazonka.CloudFront.UpdateDistribution
import Amazonka.CloudFront.UpdateFieldLevelEncryptionConfig
import Amazonka.CloudFront.UpdateFieldLevelEncryptionProfile
import Amazonka.CloudFront.UpdateFunction
import Amazonka.CloudFront.UpdateKeyGroup
import Amazonka.CloudFront.UpdateOriginAccessControl
import Amazonka.CloudFront.UpdateOriginRequestPolicy
import Amazonka.CloudFront.UpdatePublicKey
import Amazonka.CloudFront.UpdateRealtimeLogConfig
import Amazonka.CloudFront.UpdateResponseHeadersPolicy
import Amazonka.CloudFront.UpdateStreamingDistribution
import Amazonka.CloudFront.Waiters

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
