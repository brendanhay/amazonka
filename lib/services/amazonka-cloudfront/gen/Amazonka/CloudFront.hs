{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudFront
-- Copyright   : (c) 2013-2021 Brendan Hay
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

    -- ** RealtimeLogConfigOwnerMismatch
    _RealtimeLogConfigOwnerMismatch,

    -- ** TooManyOriginCustomHeaders
    _TooManyOriginCustomHeaders,

    -- ** InvalidTagging
    _InvalidTagging,

    -- ** InvalidErrorCode
    _InvalidErrorCode,

    -- ** NoSuchFieldLevelEncryptionProfile
    _NoSuchFieldLevelEncryptionProfile,

    -- ** FieldLevelEncryptionProfileInUse
    _FieldLevelEncryptionProfileInUse,

    -- ** TooManyDistributionsWithFunctionAssociations
    _TooManyDistributionsWithFunctionAssociations,

    -- ** InvalidOriginReadTimeout
    _InvalidOriginReadTimeout,

    -- ** TooManyFieldLevelEncryptionProfiles
    _TooManyFieldLevelEncryptionProfiles,

    -- ** TooManyCacheBehaviors
    _TooManyCacheBehaviors,

    -- ** TooManyCloudFrontOriginAccessIdentities
    _TooManyCloudFrontOriginAccessIdentities,

    -- ** RealtimeLogConfigAlreadyExists
    _RealtimeLogConfigAlreadyExists,

    -- ** TestFunctionFailed
    _TestFunctionFailed,

    -- ** InvalidOriginAccessIdentity
    _InvalidOriginAccessIdentity,

    -- ** TooManyFunctionAssociations
    _TooManyFunctionAssociations,

    -- ** DistributionNotDisabled
    _DistributionNotDisabled,

    -- ** NoSuchStreamingDistribution
    _NoSuchStreamingDistribution,

    -- ** InconsistentQuantities
    _InconsistentQuantities,

    -- ** InvalidArgument
    _InvalidArgument,

    -- ** InvalidOriginKeepaliveTimeout
    _InvalidOriginKeepaliveTimeout,

    -- ** TooManyInvalidationsInProgress
    _TooManyInvalidationsInProgress,

    -- ** InvalidWebACLId
    _InvalidWebACLId,

    -- ** TooManyQueryStringParameters
    _TooManyQueryStringParameters,

    -- ** TooManyFieldLevelEncryptionQueryArgProfiles
    _TooManyFieldLevelEncryptionQueryArgProfiles,

    -- ** TooManyDistributionCNAMEs
    _TooManyDistributionCNAMEs,

    -- ** NoSuchCloudFrontOriginAccessIdentity
    _NoSuchCloudFrontOriginAccessIdentity,

    -- ** CloudFrontOriginAccessIdentityInUse
    _CloudFrontOriginAccessIdentityInUse,

    -- ** InvalidFunctionAssociation
    _InvalidFunctionAssociation,

    -- ** TooManyStreamingDistributions
    _TooManyStreamingDistributions,

    -- ** CannotChangeImmutablePublicKeyFields
    _CannotChangeImmutablePublicKeyFields,

    -- ** BatchTooLarge
    _BatchTooLarge,

    -- ** TooManyCookieNamesInWhiteList
    _TooManyCookieNamesInWhiteList,

    -- ** TooManyPublicKeysInKeyGroup
    _TooManyPublicKeysInKeyGroup,

    -- ** InvalidLambdaFunctionAssociation
    _InvalidLambdaFunctionAssociation,

    -- ** TooManyKeyGroupsAssociatedToDistribution
    _TooManyKeyGroupsAssociatedToDistribution,

    -- ** NoSuchRealtimeLogConfig
    _NoSuchRealtimeLogConfig,

    -- ** InvalidForwardCookies
    _InvalidForwardCookies,

    -- ** FieldLevelEncryptionConfigInUse
    _FieldLevelEncryptionConfigInUse,

    -- ** TooManyTrustedSigners
    _TooManyTrustedSigners,

    -- ** TooManyDistributionsAssociatedToKeyGroup
    _TooManyDistributionsAssociatedToKeyGroup,

    -- ** InvalidOrigin
    _InvalidOrigin,

    -- ** CachePolicyInUse
    _CachePolicyInUse,

    -- ** NoSuchInvalidation
    _NoSuchInvalidation,

    -- ** PublicKeyAlreadyExists
    _PublicKeyAlreadyExists,

    -- ** UnsupportedOperation
    _UnsupportedOperation,

    -- ** NoSuchOrigin
    _NoSuchOrigin,

    -- ** TooManyHeadersInCachePolicy
    _TooManyHeadersInCachePolicy,

    -- ** NoSuchCachePolicy
    _NoSuchCachePolicy,

    -- ** InvalidTTLOrder
    _InvalidTTLOrder,

    -- ** StreamingDistributionNotDisabled
    _StreamingDistributionNotDisabled,

    -- ** OriginRequestPolicyAlreadyExists
    _OriginRequestPolicyAlreadyExists,

    -- ** TooManyHeadersInForwardedValues
    _TooManyHeadersInForwardedValues,

    -- ** NoSuchResource
    _NoSuchResource,

    -- ** TooManyFieldLevelEncryptionEncryptionEntities
    _TooManyFieldLevelEncryptionEncryptionEntities,

    -- ** TooManyFunctions
    _TooManyFunctions,

    -- ** TooManyStreamingDistributionCNAMEs
    _TooManyStreamingDistributionCNAMEs,

    -- ** FieldLevelEncryptionProfileAlreadyExists
    _FieldLevelEncryptionProfileAlreadyExists,

    -- ** KeyGroupAlreadyExists
    _KeyGroupAlreadyExists,

    -- ** TrustedKeyGroupDoesNotExist
    _TrustedKeyGroupDoesNotExist,

    -- ** ResourceInUse
    _ResourceInUse,

    -- ** InvalidRequiredProtocol
    _InvalidRequiredProtocol,

    -- ** TooManyDistributions
    _TooManyDistributions,

    -- ** TooManyDistributionsWithSingleFunctionARN
    _TooManyDistributionsWithSingleFunctionARN,

    -- ** TooManyHeadersInOriginRequestPolicy
    _TooManyHeadersInOriginRequestPolicy,

    -- ** TooManyCertificates
    _TooManyCertificates,

    -- ** FunctionSizeLimitExceeded
    _FunctionSizeLimitExceeded,

    -- ** NoSuchOriginRequestPolicy
    _NoSuchOriginRequestPolicy,

    -- ** DistributionAlreadyExists
    _DistributionAlreadyExists,

    -- ** TooManyDistributionsAssociatedToFieldLevelEncryptionConfig
    _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig,

    -- ** TooManyKeyGroups
    _TooManyKeyGroups,

    -- ** InvalidQueryStringParameters
    _InvalidQueryStringParameters,

    -- ** MissingBody
    _MissingBody,

    -- ** FunctionAlreadyExists
    _FunctionAlreadyExists,

    -- ** IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior
    _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior,

    -- ** TooManyOriginRequestPolicies
    _TooManyOriginRequestPolicies,

    -- ** IllegalDelete
    _IllegalDelete,

    -- ** IllegalUpdate
    _IllegalUpdate,

    -- ** InvalidIfMatchVersion
    _InvalidIfMatchVersion,

    -- ** FieldLevelEncryptionConfigAlreadyExists
    _FieldLevelEncryptionConfigAlreadyExists,

    -- ** PreconditionFailed
    _PreconditionFailed,

    -- ** InvalidResponseCode
    _InvalidResponseCode,

    -- ** InvalidHeadersForS3Origin
    _InvalidHeadersForS3Origin,

    -- ** CNAMEAlreadyExists
    _CNAMEAlreadyExists,

    -- ** NoSuchPublicKey
    _NoSuchPublicKey,

    -- ** PublicKeyInUse
    _PublicKeyInUse,

    -- ** TrustedSignerDoesNotExist
    _TrustedSignerDoesNotExist,

    -- ** InvalidProtocolSettings
    _InvalidProtocolSettings,

    -- ** CachePolicyAlreadyExists
    _CachePolicyAlreadyExists,

    -- ** TooManyCookiesInOriginRequestPolicy
    _TooManyCookiesInOriginRequestPolicy,

    -- ** TooManyOriginGroupsPerDistribution
    _TooManyOriginGroupsPerDistribution,

    -- ** TooManyPublicKeys
    _TooManyPublicKeys,

    -- ** NoSuchFieldLevelEncryptionConfig
    _NoSuchFieldLevelEncryptionConfig,

    -- ** TooManyRealtimeLogConfigs
    _TooManyRealtimeLogConfigs,

    -- ** RealtimeLogConfigInUse
    _RealtimeLogConfigInUse,

    -- ** TooManyCachePolicies
    _TooManyCachePolicies,

    -- ** TooManyFieldLevelEncryptionContentTypeProfiles
    _TooManyFieldLevelEncryptionContentTypeProfiles,

    -- ** TooManyFieldLevelEncryptionFieldPatterns
    _TooManyFieldLevelEncryptionFieldPatterns,

    -- ** TooManyFieldLevelEncryptionConfigs
    _TooManyFieldLevelEncryptionConfigs,

    -- ** TooManyLambdaFunctionAssociations
    _TooManyLambdaFunctionAssociations,

    -- ** CloudFrontOriginAccessIdentityAlreadyExists
    _CloudFrontOriginAccessIdentityAlreadyExists,

    -- ** TooManyQueryStringsInCachePolicy
    _TooManyQueryStringsInCachePolicy,

    -- ** TooManyOrigins
    _TooManyOrigins,

    -- ** InvalidRelativePath
    _InvalidRelativePath,

    -- ** StreamingDistributionAlreadyExists
    _StreamingDistributionAlreadyExists,

    -- ** TooManyDistributionsAssociatedToOriginRequestPolicy
    _TooManyDistributionsAssociatedToOriginRequestPolicy,

    -- ** QueryArgProfileEmpty
    _QueryArgProfileEmpty,

    -- ** TooManyCookiesInCachePolicy
    _TooManyCookiesInCachePolicy,

    -- ** InvalidMinimumProtocolVersion
    _InvalidMinimumProtocolVersion,

    -- ** AccessDenied
    _AccessDenied,

    -- ** InvalidViewerCertificate
    _InvalidViewerCertificate,

    -- ** NoSuchDistribution
    _NoSuchDistribution,

    -- ** NoSuchFunctionExists
    _NoSuchFunctionExists,

    -- ** FunctionInUse
    _FunctionInUse,

    -- ** FieldLevelEncryptionProfileSizeExceeded
    _FieldLevelEncryptionProfileSizeExceeded,

    -- ** TooManyQueryStringsInOriginRequestPolicy
    _TooManyQueryStringsInOriginRequestPolicy,

    -- ** InvalidDefaultRootObject
    _InvalidDefaultRootObject,

    -- ** TooManyDistributionsWithLambdaAssociations
    _TooManyDistributionsWithLambdaAssociations,

    -- ** TooManyDistributionsAssociatedToCachePolicy
    _TooManyDistributionsAssociatedToCachePolicy,

    -- ** InvalidGeoRestrictionParameter
    _InvalidGeoRestrictionParameter,

    -- ** OriginRequestPolicyInUse
    _OriginRequestPolicyInUse,

    -- ** InvalidLocationCode
    _InvalidLocationCode,

    -- * Waiters
    -- $waiters

    -- ** StreamingDistributionDeployed
    newStreamingDistributionDeployed,

    -- ** DistributionDeployed
    newDistributionDeployed,

    -- ** InvalidationCompleted
    newInvalidationCompleted,

    -- * Operations
    -- $operations

    -- ** DeleteOriginRequestPolicy
    DeleteOriginRequestPolicy (DeleteOriginRequestPolicy'),
    newDeleteOriginRequestPolicy,
    DeleteOriginRequestPolicyResponse (DeleteOriginRequestPolicyResponse'),
    newDeleteOriginRequestPolicyResponse,

    -- ** UpdateOriginRequestPolicy
    UpdateOriginRequestPolicy (UpdateOriginRequestPolicy'),
    newUpdateOriginRequestPolicy,
    UpdateOriginRequestPolicyResponse (UpdateOriginRequestPolicyResponse'),
    newUpdateOriginRequestPolicyResponse,

    -- ** ListConflictingAliases
    ListConflictingAliases (ListConflictingAliases'),
    newListConflictingAliases,
    ListConflictingAliasesResponse (ListConflictingAliasesResponse'),
    newListConflictingAliasesResponse,

    -- ** DeleteStreamingDistribution
    DeleteStreamingDistribution (DeleteStreamingDistribution'),
    newDeleteStreamingDistribution,
    DeleteStreamingDistributionResponse (DeleteStreamingDistributionResponse'),
    newDeleteStreamingDistributionResponse,

    -- ** UpdateStreamingDistribution
    UpdateStreamingDistribution (UpdateStreamingDistribution'),
    newUpdateStreamingDistribution,
    UpdateStreamingDistributionResponse (UpdateStreamingDistributionResponse'),
    newUpdateStreamingDistributionResponse,

    -- ** ListPublicKeys
    ListPublicKeys (ListPublicKeys'),
    newListPublicKeys,
    ListPublicKeysResponse (ListPublicKeysResponse'),
    newListPublicKeysResponse,

    -- ** GetFieldLevelEncryptionConfig
    GetFieldLevelEncryptionConfig (GetFieldLevelEncryptionConfig'),
    newGetFieldLevelEncryptionConfig,
    GetFieldLevelEncryptionConfigResponse (GetFieldLevelEncryptionConfigResponse'),
    newGetFieldLevelEncryptionConfigResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreatePublicKey
    CreatePublicKey (CreatePublicKey'),
    newCreatePublicKey,
    CreatePublicKeyResponse (CreatePublicKeyResponse'),
    newCreatePublicKeyResponse,

    -- ** GetPublicKeyConfig
    GetPublicKeyConfig (GetPublicKeyConfig'),
    newGetPublicKeyConfig,
    GetPublicKeyConfigResponse (GetPublicKeyConfigResponse'),
    newGetPublicKeyConfigResponse,

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

    -- ** DeleteCachePolicy
    DeleteCachePolicy (DeleteCachePolicy'),
    newDeleteCachePolicy,
    DeleteCachePolicyResponse (DeleteCachePolicyResponse'),
    newDeleteCachePolicyResponse,

    -- ** UpdateCachePolicy
    UpdateCachePolicy (UpdateCachePolicy'),
    newUpdateCachePolicy,
    UpdateCachePolicyResponse (UpdateCachePolicyResponse'),
    newUpdateCachePolicyResponse,

    -- ** GetFieldLevelEncryption
    GetFieldLevelEncryption (GetFieldLevelEncryption'),
    newGetFieldLevelEncryption,
    GetFieldLevelEncryptionResponse (GetFieldLevelEncryptionResponse'),
    newGetFieldLevelEncryptionResponse,

    -- ** ListRealtimeLogConfigs
    ListRealtimeLogConfigs (ListRealtimeLogConfigs'),
    newListRealtimeLogConfigs,
    ListRealtimeLogConfigsResponse (ListRealtimeLogConfigsResponse'),
    newListRealtimeLogConfigsResponse,

    -- ** GetPublicKey
    GetPublicKey (GetPublicKey'),
    newGetPublicKey,
    GetPublicKeyResponse (GetPublicKeyResponse'),
    newGetPublicKeyResponse,

    -- ** DeleteRealtimeLogConfig
    DeleteRealtimeLogConfig (DeleteRealtimeLogConfig'),
    newDeleteRealtimeLogConfig,
    DeleteRealtimeLogConfigResponse (DeleteRealtimeLogConfigResponse'),
    newDeleteRealtimeLogConfigResponse,

    -- ** UpdateRealtimeLogConfig
    UpdateRealtimeLogConfig (UpdateRealtimeLogConfig'),
    newUpdateRealtimeLogConfig,
    UpdateRealtimeLogConfigResponse (UpdateRealtimeLogConfigResponse'),
    newUpdateRealtimeLogConfigResponse,

    -- ** ListDistributionsByOriginRequestPolicyId
    ListDistributionsByOriginRequestPolicyId (ListDistributionsByOriginRequestPolicyId'),
    newListDistributionsByOriginRequestPolicyId,
    ListDistributionsByOriginRequestPolicyIdResponse (ListDistributionsByOriginRequestPolicyIdResponse'),
    newListDistributionsByOriginRequestPolicyIdResponse,

    -- ** DeleteFieldLevelEncryptionConfig
    DeleteFieldLevelEncryptionConfig (DeleteFieldLevelEncryptionConfig'),
    newDeleteFieldLevelEncryptionConfig,
    DeleteFieldLevelEncryptionConfigResponse (DeleteFieldLevelEncryptionConfigResponse'),
    newDeleteFieldLevelEncryptionConfigResponse,

    -- ** UpdateFieldLevelEncryptionConfig
    UpdateFieldLevelEncryptionConfig (UpdateFieldLevelEncryptionConfig'),
    newUpdateFieldLevelEncryptionConfig,
    UpdateFieldLevelEncryptionConfigResponse (UpdateFieldLevelEncryptionConfigResponse'),
    newUpdateFieldLevelEncryptionConfigResponse,

    -- ** GetKeyGroup
    GetKeyGroup (GetKeyGroup'),
    newGetKeyGroup,
    GetKeyGroupResponse (GetKeyGroupResponse'),
    newGetKeyGroupResponse,

    -- ** CreateDistribution
    CreateDistribution (CreateDistribution'),
    newCreateDistribution,
    CreateDistributionResponse (CreateDistributionResponse'),
    newCreateDistributionResponse,

    -- ** GetFieldLevelEncryptionProfile
    GetFieldLevelEncryptionProfile (GetFieldLevelEncryptionProfile'),
    newGetFieldLevelEncryptionProfile,
    GetFieldLevelEncryptionProfileResponse (GetFieldLevelEncryptionProfileResponse'),
    newGetFieldLevelEncryptionProfileResponse,

    -- ** DeleteMonitoringSubscription
    DeleteMonitoringSubscription (DeleteMonitoringSubscription'),
    newDeleteMonitoringSubscription,
    DeleteMonitoringSubscriptionResponse (DeleteMonitoringSubscriptionResponse'),
    newDeleteMonitoringSubscriptionResponse,

    -- ** CreateFunction
    CreateFunction (CreateFunction'),
    newCreateFunction,
    CreateFunctionResponse (CreateFunctionResponse'),
    newCreateFunctionResponse,

    -- ** GetDistributionConfig
    GetDistributionConfig (GetDistributionConfig'),
    newGetDistributionConfig,
    GetDistributionConfigResponse (GetDistributionConfigResponse'),
    newGetDistributionConfigResponse,

    -- ** CreateStreamingDistributionWithTags
    CreateStreamingDistributionWithTags (CreateStreamingDistributionWithTags'),
    newCreateStreamingDistributionWithTags,
    CreateStreamingDistributionWithTagsResponse (CreateStreamingDistributionWithTagsResponse'),
    newCreateStreamingDistributionWithTagsResponse,

    -- ** DeleteFieldLevelEncryptionProfile
    DeleteFieldLevelEncryptionProfile (DeleteFieldLevelEncryptionProfile'),
    newDeleteFieldLevelEncryptionProfile,
    DeleteFieldLevelEncryptionProfileResponse (DeleteFieldLevelEncryptionProfileResponse'),
    newDeleteFieldLevelEncryptionProfileResponse,

    -- ** UpdateFieldLevelEncryptionProfile
    UpdateFieldLevelEncryptionProfile (UpdateFieldLevelEncryptionProfile'),
    newUpdateFieldLevelEncryptionProfile,
    UpdateFieldLevelEncryptionProfileResponse (UpdateFieldLevelEncryptionProfileResponse'),
    newUpdateFieldLevelEncryptionProfileResponse,

    -- ** ListDistributionsByCachePolicyId
    ListDistributionsByCachePolicyId (ListDistributionsByCachePolicyId'),
    newListDistributionsByCachePolicyId,
    ListDistributionsByCachePolicyIdResponse (ListDistributionsByCachePolicyIdResponse'),
    newListDistributionsByCachePolicyIdResponse,

    -- ** TestFunction
    TestFunction (TestFunction'),
    newTestFunction,
    TestFunctionResponse (TestFunctionResponse'),
    newTestFunctionResponse,

    -- ** CreateFieldLevelEncryptionProfile
    CreateFieldLevelEncryptionProfile (CreateFieldLevelEncryptionProfile'),
    newCreateFieldLevelEncryptionProfile,
    CreateFieldLevelEncryptionProfileResponse (CreateFieldLevelEncryptionProfileResponse'),
    newCreateFieldLevelEncryptionProfileResponse,

    -- ** GetKeyGroupConfig
    GetKeyGroupConfig (GetKeyGroupConfig'),
    newGetKeyGroupConfig,
    GetKeyGroupConfigResponse (GetKeyGroupConfigResponse'),
    newGetKeyGroupConfigResponse,

    -- ** GetDistribution
    GetDistribution (GetDistribution'),
    newGetDistribution,
    GetDistributionResponse (GetDistributionResponse'),
    newGetDistributionResponse,

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

    -- ** CreateKeyGroup
    CreateKeyGroup (CreateKeyGroup'),
    newCreateKeyGroup,
    CreateKeyGroupResponse (CreateKeyGroupResponse'),
    newCreateKeyGroupResponse,

    -- ** UpdateCloudFrontOriginAccessIdentity
    UpdateCloudFrontOriginAccessIdentity (UpdateCloudFrontOriginAccessIdentity'),
    newUpdateCloudFrontOriginAccessIdentity,
    UpdateCloudFrontOriginAccessIdentityResponse (UpdateCloudFrontOriginAccessIdentityResponse'),
    newUpdateCloudFrontOriginAccessIdentityResponse,

    -- ** DeleteCloudFrontOriginAccessIdentity
    DeleteCloudFrontOriginAccessIdentity (DeleteCloudFrontOriginAccessIdentity'),
    newDeleteCloudFrontOriginAccessIdentity,
    DeleteCloudFrontOriginAccessIdentityResponse (DeleteCloudFrontOriginAccessIdentityResponse'),
    newDeleteCloudFrontOriginAccessIdentityResponse,

    -- ** ListStreamingDistributions (Paginated)
    ListStreamingDistributions (ListStreamingDistributions'),
    newListStreamingDistributions,
    ListStreamingDistributionsResponse (ListStreamingDistributionsResponse'),
    newListStreamingDistributionsResponse,

    -- ** DeletePublicKey
    DeletePublicKey (DeletePublicKey'),
    newDeletePublicKey,
    DeletePublicKeyResponse (DeletePublicKeyResponse'),
    newDeletePublicKeyResponse,

    -- ** UpdatePublicKey
    UpdatePublicKey (UpdatePublicKey'),
    newUpdatePublicKey,
    UpdatePublicKeyResponse (UpdatePublicKeyResponse'),
    newUpdatePublicKeyResponse,

    -- ** GetRealtimeLogConfig
    GetRealtimeLogConfig (GetRealtimeLogConfig'),
    newGetRealtimeLogConfig,
    GetRealtimeLogConfigResponse (GetRealtimeLogConfigResponse'),
    newGetRealtimeLogConfigResponse,

    -- ** DescribeFunction
    DescribeFunction (DescribeFunction'),
    newDescribeFunction,
    DescribeFunctionResponse (DescribeFunctionResponse'),
    newDescribeFunctionResponse,

    -- ** GetStreamingDistributionConfig
    GetStreamingDistributionConfig (GetStreamingDistributionConfig'),
    newGetStreamingDistributionConfig,
    GetStreamingDistributionConfigResponse (GetStreamingDistributionConfigResponse'),
    newGetStreamingDistributionConfigResponse,

    -- ** GetCloudFrontOriginAccessIdentityConfig
    GetCloudFrontOriginAccessIdentityConfig (GetCloudFrontOriginAccessIdentityConfig'),
    newGetCloudFrontOriginAccessIdentityConfig,
    GetCloudFrontOriginAccessIdentityConfigResponse (GetCloudFrontOriginAccessIdentityConfigResponse'),
    newGetCloudFrontOriginAccessIdentityConfigResponse,

    -- ** CreateStreamingDistribution
    CreateStreamingDistribution (CreateStreamingDistribution'),
    newCreateStreamingDistribution,
    CreateStreamingDistributionResponse (CreateStreamingDistributionResponse'),
    newCreateStreamingDistributionResponse,

    -- ** CreateCloudFrontOriginAccessIdentity
    CreateCloudFrontOriginAccessIdentity (CreateCloudFrontOriginAccessIdentity'),
    newCreateCloudFrontOriginAccessIdentity,
    CreateCloudFrontOriginAccessIdentityResponse (CreateCloudFrontOriginAccessIdentityResponse'),
    newCreateCloudFrontOriginAccessIdentityResponse,

    -- ** ListCloudFrontOriginAccessIdentities (Paginated)
    ListCloudFrontOriginAccessIdentities (ListCloudFrontOriginAccessIdentities'),
    newListCloudFrontOriginAccessIdentities,
    ListCloudFrontOriginAccessIdentitiesResponse (ListCloudFrontOriginAccessIdentitiesResponse'),
    newListCloudFrontOriginAccessIdentitiesResponse,

    -- ** GetInvalidation
    GetInvalidation (GetInvalidation'),
    newGetInvalidation,
    GetInvalidationResponse (GetInvalidationResponse'),
    newGetInvalidationResponse,

    -- ** GetCachePolicy
    GetCachePolicy (GetCachePolicy'),
    newGetCachePolicy,
    GetCachePolicyResponse (GetCachePolicyResponse'),
    newGetCachePolicyResponse,

    -- ** AssociateAlias
    AssociateAlias (AssociateAlias'),
    newAssociateAlias,
    AssociateAliasResponse (AssociateAliasResponse'),
    newAssociateAliasResponse,

    -- ** CreateRealtimeLogConfig
    CreateRealtimeLogConfig (CreateRealtimeLogConfig'),
    newCreateRealtimeLogConfig,
    CreateRealtimeLogConfigResponse (CreateRealtimeLogConfigResponse'),
    newCreateRealtimeLogConfigResponse,

    -- ** ListInvalidations (Paginated)
    ListInvalidations (ListInvalidations'),
    newListInvalidations,
    ListInvalidationsResponse (ListInvalidationsResponse'),
    newListInvalidationsResponse,

    -- ** CreateInvalidation
    CreateInvalidation (CreateInvalidation'),
    newCreateInvalidation,
    CreateInvalidationResponse (CreateInvalidationResponse'),
    newCreateInvalidationResponse,

    -- ** GetCloudFrontOriginAccessIdentity
    GetCloudFrontOriginAccessIdentity (GetCloudFrontOriginAccessIdentity'),
    newGetCloudFrontOriginAccessIdentity,
    GetCloudFrontOriginAccessIdentityResponse (GetCloudFrontOriginAccessIdentityResponse'),
    newGetCloudFrontOriginAccessIdentityResponse,

    -- ** ListCachePolicies
    ListCachePolicies (ListCachePolicies'),
    newListCachePolicies,
    ListCachePoliciesResponse (ListCachePoliciesResponse'),
    newListCachePoliciesResponse,

    -- ** CreateCachePolicy
    CreateCachePolicy (CreateCachePolicy'),
    newCreateCachePolicy,
    CreateCachePolicyResponse (CreateCachePolicyResponse'),
    newCreateCachePolicyResponse,

    -- ** GetCachePolicyConfig
    GetCachePolicyConfig (GetCachePolicyConfig'),
    newGetCachePolicyConfig,
    GetCachePolicyConfigResponse (GetCachePolicyConfigResponse'),
    newGetCachePolicyConfigResponse,

    -- ** ListFieldLevelEncryptionConfigs
    ListFieldLevelEncryptionConfigs (ListFieldLevelEncryptionConfigs'),
    newListFieldLevelEncryptionConfigs,
    ListFieldLevelEncryptionConfigsResponse (ListFieldLevelEncryptionConfigsResponse'),
    newListFieldLevelEncryptionConfigsResponse,

    -- ** ListDistributionsByKeyGroup
    ListDistributionsByKeyGroup (ListDistributionsByKeyGroup'),
    newListDistributionsByKeyGroup,
    ListDistributionsByKeyGroupResponse (ListDistributionsByKeyGroupResponse'),
    newListDistributionsByKeyGroupResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetStreamingDistribution
    GetStreamingDistribution (GetStreamingDistribution'),
    newGetStreamingDistribution,
    GetStreamingDistributionResponse (GetStreamingDistributionResponse'),
    newGetStreamingDistributionResponse,

    -- ** UpdateDistribution
    UpdateDistribution (UpdateDistribution'),
    newUpdateDistribution,
    UpdateDistributionResponse (UpdateDistributionResponse'),
    newUpdateDistributionResponse,

    -- ** UpdateFunction
    UpdateFunction (UpdateFunction'),
    newUpdateFunction,
    UpdateFunctionResponse (UpdateFunctionResponse'),
    newUpdateFunctionResponse,

    -- ** DeleteDistribution
    DeleteDistribution (DeleteDistribution'),
    newDeleteDistribution,
    DeleteDistributionResponse (DeleteDistributionResponse'),
    newDeleteDistributionResponse,

    -- ** DeleteFunction
    DeleteFunction (DeleteFunction'),
    newDeleteFunction,
    DeleteFunctionResponse (DeleteFunctionResponse'),
    newDeleteFunctionResponse,

    -- ** GetOriginRequestPolicy
    GetOriginRequestPolicy (GetOriginRequestPolicy'),
    newGetOriginRequestPolicy,
    GetOriginRequestPolicyResponse (GetOriginRequestPolicyResponse'),
    newGetOriginRequestPolicyResponse,

    -- ** PublishFunction
    PublishFunction (PublishFunction'),
    newPublishFunction,
    PublishFunctionResponse (PublishFunctionResponse'),
    newPublishFunctionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateMonitoringSubscription
    CreateMonitoringSubscription (CreateMonitoringSubscription'),
    newCreateMonitoringSubscription,
    CreateMonitoringSubscriptionResponse (CreateMonitoringSubscriptionResponse'),
    newCreateMonitoringSubscriptionResponse,

    -- ** ListDistributionsByWebACLId
    ListDistributionsByWebACLId (ListDistributionsByWebACLId'),
    newListDistributionsByWebACLId,
    ListDistributionsByWebACLIdResponse (ListDistributionsByWebACLIdResponse'),
    newListDistributionsByWebACLIdResponse,

    -- ** ListDistributions (Paginated)
    ListDistributions (ListDistributions'),
    newListDistributions,
    ListDistributionsResponse (ListDistributionsResponse'),
    newListDistributionsResponse,

    -- ** ListFunctions
    ListFunctions (ListFunctions'),
    newListFunctions,
    ListFunctionsResponse (ListFunctionsResponse'),
    newListFunctionsResponse,

    -- ** ListDistributionsByRealtimeLogConfig
    ListDistributionsByRealtimeLogConfig (ListDistributionsByRealtimeLogConfig'),
    newListDistributionsByRealtimeLogConfig,
    ListDistributionsByRealtimeLogConfigResponse (ListDistributionsByRealtimeLogConfigResponse'),
    newListDistributionsByRealtimeLogConfigResponse,

    -- ** CreateOriginRequestPolicy
    CreateOriginRequestPolicy (CreateOriginRequestPolicy'),
    newCreateOriginRequestPolicy,
    CreateOriginRequestPolicyResponse (CreateOriginRequestPolicyResponse'),
    newCreateOriginRequestPolicyResponse,

    -- ** ListKeyGroups
    ListKeyGroups (ListKeyGroups'),
    newListKeyGroups,
    ListKeyGroupsResponse (ListKeyGroupsResponse'),
    newListKeyGroupsResponse,

    -- ** ListFieldLevelEncryptionProfiles
    ListFieldLevelEncryptionProfiles (ListFieldLevelEncryptionProfiles'),
    newListFieldLevelEncryptionProfiles,
    ListFieldLevelEncryptionProfilesResponse (ListFieldLevelEncryptionProfilesResponse'),
    newListFieldLevelEncryptionProfilesResponse,

    -- ** GetMonitoringSubscription
    GetMonitoringSubscription (GetMonitoringSubscription'),
    newGetMonitoringSubscription,
    GetMonitoringSubscriptionResponse (GetMonitoringSubscriptionResponse'),
    newGetMonitoringSubscriptionResponse,

    -- ** UpdateKeyGroup
    UpdateKeyGroup (UpdateKeyGroup'),
    newUpdateKeyGroup,
    UpdateKeyGroupResponse (UpdateKeyGroupResponse'),
    newUpdateKeyGroupResponse,

    -- ** DeleteKeyGroup
    DeleteKeyGroup (DeleteKeyGroup'),
    newDeleteKeyGroup,
    DeleteKeyGroupResponse (DeleteKeyGroupResponse'),
    newDeleteKeyGroupResponse,

    -- ** ListOriginRequestPolicies
    ListOriginRequestPolicies (ListOriginRequestPolicies'),
    newListOriginRequestPolicies,
    ListOriginRequestPoliciesResponse (ListOriginRequestPoliciesResponse'),
    newListOriginRequestPoliciesResponse,

    -- ** GetOriginRequestPolicyConfig
    GetOriginRequestPolicyConfig (GetOriginRequestPolicyConfig'),
    newGetOriginRequestPolicyConfig,
    GetOriginRequestPolicyConfigResponse (GetOriginRequestPolicyConfigResponse'),
    newGetOriginRequestPolicyConfigResponse,

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

    -- ** Restrictions
    Restrictions (Restrictions'),
    newRestrictions,

    -- ** S3Origin
    S3Origin (S3Origin'),
    newS3Origin,

    -- ** S3OriginConfig
    S3OriginConfig (S3OriginConfig'),
    newS3OriginConfig,

    -- ** Signer
    Signer (Signer'),
    newSigner,

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
import Amazonka.CloudFront.CreateCachePolicy
import Amazonka.CloudFront.CreateCloudFrontOriginAccessIdentity
import Amazonka.CloudFront.CreateDistribution
import Amazonka.CloudFront.CreateDistributionWithTags
import Amazonka.CloudFront.CreateFieldLevelEncryptionConfig
import Amazonka.CloudFront.CreateFieldLevelEncryptionProfile
import Amazonka.CloudFront.CreateFunction
import Amazonka.CloudFront.CreateInvalidation
import Amazonka.CloudFront.CreateKeyGroup
import Amazonka.CloudFront.CreateMonitoringSubscription
import Amazonka.CloudFront.CreateOriginRequestPolicy
import Amazonka.CloudFront.CreatePublicKey
import Amazonka.CloudFront.CreateRealtimeLogConfig
import Amazonka.CloudFront.CreateStreamingDistribution
import Amazonka.CloudFront.CreateStreamingDistributionWithTags
import Amazonka.CloudFront.DeleteCachePolicy
import Amazonka.CloudFront.DeleteCloudFrontOriginAccessIdentity
import Amazonka.CloudFront.DeleteDistribution
import Amazonka.CloudFront.DeleteFieldLevelEncryptionConfig
import Amazonka.CloudFront.DeleteFieldLevelEncryptionProfile
import Amazonka.CloudFront.DeleteFunction
import Amazonka.CloudFront.DeleteKeyGroup
import Amazonka.CloudFront.DeleteMonitoringSubscription
import Amazonka.CloudFront.DeleteOriginRequestPolicy
import Amazonka.CloudFront.DeletePublicKey
import Amazonka.CloudFront.DeleteRealtimeLogConfig
import Amazonka.CloudFront.DeleteStreamingDistribution
import Amazonka.CloudFront.DescribeFunction
import Amazonka.CloudFront.GetCachePolicy
import Amazonka.CloudFront.GetCachePolicyConfig
import Amazonka.CloudFront.GetCloudFrontOriginAccessIdentity
import Amazonka.CloudFront.GetCloudFrontOriginAccessIdentityConfig
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
import Amazonka.CloudFront.GetOriginRequestPolicy
import Amazonka.CloudFront.GetOriginRequestPolicyConfig
import Amazonka.CloudFront.GetPublicKey
import Amazonka.CloudFront.GetPublicKeyConfig
import Amazonka.CloudFront.GetRealtimeLogConfig
import Amazonka.CloudFront.GetStreamingDistribution
import Amazonka.CloudFront.GetStreamingDistributionConfig
import Amazonka.CloudFront.Lens
import Amazonka.CloudFront.ListCachePolicies
import Amazonka.CloudFront.ListCloudFrontOriginAccessIdentities
import Amazonka.CloudFront.ListConflictingAliases
import Amazonka.CloudFront.ListDistributions
import Amazonka.CloudFront.ListDistributionsByCachePolicyId
import Amazonka.CloudFront.ListDistributionsByKeyGroup
import Amazonka.CloudFront.ListDistributionsByOriginRequestPolicyId
import Amazonka.CloudFront.ListDistributionsByRealtimeLogConfig
import Amazonka.CloudFront.ListDistributionsByWebACLId
import Amazonka.CloudFront.ListFieldLevelEncryptionConfigs
import Amazonka.CloudFront.ListFieldLevelEncryptionProfiles
import Amazonka.CloudFront.ListFunctions
import Amazonka.CloudFront.ListInvalidations
import Amazonka.CloudFront.ListKeyGroups
import Amazonka.CloudFront.ListOriginRequestPolicies
import Amazonka.CloudFront.ListPublicKeys
import Amazonka.CloudFront.ListRealtimeLogConfigs
import Amazonka.CloudFront.ListStreamingDistributions
import Amazonka.CloudFront.ListTagsForResource
import Amazonka.CloudFront.PublishFunction
import Amazonka.CloudFront.TagResource
import Amazonka.CloudFront.TestFunction
import Amazonka.CloudFront.Types
import Amazonka.CloudFront.UntagResource
import Amazonka.CloudFront.UpdateCachePolicy
import Amazonka.CloudFront.UpdateCloudFrontOriginAccessIdentity
import Amazonka.CloudFront.UpdateDistribution
import Amazonka.CloudFront.UpdateFieldLevelEncryptionConfig
import Amazonka.CloudFront.UpdateFieldLevelEncryptionProfile
import Amazonka.CloudFront.UpdateFunction
import Amazonka.CloudFront.UpdateKeyGroup
import Amazonka.CloudFront.UpdateOriginRequestPolicy
import Amazonka.CloudFront.UpdatePublicKey
import Amazonka.CloudFront.UpdateRealtimeLogConfig
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
