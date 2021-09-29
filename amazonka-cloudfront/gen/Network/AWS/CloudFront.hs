{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.CloudFront
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-05-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon CloudFront is a web service that speeds up distribution of your
-- static and dynamic web content, for example, .html, .css, .php, image,
-- and media files, to end users. CloudFront delivers your content through
-- a worldwide network of edge locations. When an end user requests content
-- that you\'re serving with CloudFront, the user is routed to the edge
-- location that provides the lowest latency, so content is delivered with
-- the best possible performance. If the content is already in that edge
-- location, CloudFront delivers it immediately. If the content is not
-- currently in that edge location, CloudFront retrieves it from an Amazon
-- S3 bucket or an HTTP server (for example, a web server) that you have
-- identified as the source for the definitive version of your content.
module Network.AWS.CloudFront
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidResponseCode
    _InvalidResponseCode,

    -- ** TestFunctionFailed
    _TestFunctionFailed,

    -- ** RealtimeLogConfigAlreadyExists
    _RealtimeLogConfigAlreadyExists,

    -- ** FieldLevelEncryptionConfigAlreadyExists
    _FieldLevelEncryptionConfigAlreadyExists,

    -- ** InvalidIfMatchVersion
    _InvalidIfMatchVersion,

    -- ** IllegalUpdate
    _IllegalUpdate,

    -- ** IllegalDelete
    _IllegalDelete,

    -- ** InvalidOriginReadTimeout
    _InvalidOriginReadTimeout,

    -- ** FunctionAlreadyExists
    _FunctionAlreadyExists,

    -- ** TooManyDistributionsWithFunctionAssociations
    _TooManyDistributionsWithFunctionAssociations,

    -- ** MissingBody
    _MissingBody,

    -- ** TooManyOriginCustomHeaders
    _TooManyOriginCustomHeaders,

    -- ** DistributionAlreadyExists
    _DistributionAlreadyExists,

    -- ** IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior
    _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior,

    -- ** FieldLevelEncryptionProfileInUse
    _FieldLevelEncryptionProfileInUse,

    -- ** InvalidTagging
    _InvalidTagging,

    -- ** RealtimeLogConfigOwnerMismatch
    _RealtimeLogConfigOwnerMismatch,

    -- ** InvalidErrorCode
    _InvalidErrorCode,

    -- ** InvalidLocationCode
    _InvalidLocationCode,

    -- ** TooManyDistributionsAssociatedToCachePolicy
    _TooManyDistributionsAssociatedToCachePolicy,

    -- ** TooManyHeadersInForwardedValues
    _TooManyHeadersInForwardedValues,

    -- ** InvalidViewerCertificate
    _InvalidViewerCertificate,

    -- ** InvalidRequiredProtocol
    _InvalidRequiredProtocol,

    -- ** TooManyFieldLevelEncryptionEncryptionEntities
    _TooManyFieldLevelEncryptionEncryptionEntities,

    -- ** NoSuchDistribution
    _NoSuchDistribution,

    -- ** NoSuchFunctionExists
    _NoSuchFunctionExists,

    -- ** ResourceInUse
    _ResourceInUse,

    -- ** TrustedKeyGroupDoesNotExist
    _TrustedKeyGroupDoesNotExist,

    -- ** KeyGroupAlreadyExists
    _KeyGroupAlreadyExists,

    -- ** OriginRequestPolicyAlreadyExists
    _OriginRequestPolicyAlreadyExists,

    -- ** StreamingDistributionNotDisabled
    _StreamingDistributionNotDisabled,

    -- ** TooManyCookiesInCachePolicy
    _TooManyCookiesInCachePolicy,

    -- ** InvalidMinimumProtocolVersion
    _InvalidMinimumProtocolVersion,

    -- ** InvalidRelativePath
    _InvalidRelativePath,

    -- ** TooManyLambdaFunctionAssociations
    _TooManyLambdaFunctionAssociations,

    -- ** CloudFrontOriginAccessIdentityAlreadyExists
    _CloudFrontOriginAccessIdentityAlreadyExists,

    -- ** CachePolicyInUse
    _CachePolicyInUse,

    -- ** NoSuchInvalidation
    _NoSuchInvalidation,

    -- ** NoSuchOrigin
    _NoSuchOrigin,

    -- ** UnsupportedOperation
    _UnsupportedOperation,

    -- ** PublicKeyAlreadyExists
    _PublicKeyAlreadyExists,

    -- ** TooManyDistributionsAssociatedToOriginRequestPolicy
    _TooManyDistributionsAssociatedToOriginRequestPolicy,

    -- ** TooManyCookieNamesInWhiteList
    _TooManyCookieNamesInWhiteList,

    -- ** NoSuchFieldLevelEncryptionConfig
    _NoSuchFieldLevelEncryptionConfig,

    -- ** TooManyTrustedSigners
    _TooManyTrustedSigners,

    -- ** TooManyFieldLevelEncryptionConfigs
    _TooManyFieldLevelEncryptionConfigs,

    -- ** TooManyFieldLevelEncryptionContentTypeProfiles
    _TooManyFieldLevelEncryptionContentTypeProfiles,

    -- ** TooManyCachePolicies
    _TooManyCachePolicies,

    -- ** TooManyFieldLevelEncryptionFieldPatterns
    _TooManyFieldLevelEncryptionFieldPatterns,

    -- ** InvalidOrigin
    _InvalidOrigin,

    -- ** BatchTooLarge
    _BatchTooLarge,

    -- ** RealtimeLogConfigInUse
    _RealtimeLogConfigInUse,

    -- ** InvalidForwardCookies
    _InvalidForwardCookies,

    -- ** FieldLevelEncryptionConfigInUse
    _FieldLevelEncryptionConfigInUse,

    -- ** NoSuchRealtimeLogConfig
    _NoSuchRealtimeLogConfig,

    -- ** TooManyPublicKeysInKeyGroup
    _TooManyPublicKeysInKeyGroup,

    -- ** InvalidHeadersForS3Origin
    _InvalidHeadersForS3Origin,

    -- ** TooManyCookiesInOriginRequestPolicy
    _TooManyCookiesInOriginRequestPolicy,

    -- ** TooManyPublicKeys
    _TooManyPublicKeys,

    -- ** TooManyInvalidationsInProgress
    _TooManyInvalidationsInProgress,

    -- ** TooManyQueryStringParameters
    _TooManyQueryStringParameters,

    -- ** InvalidFunctionAssociation
    _InvalidFunctionAssociation,

    -- ** TrustedSignerDoesNotExist
    _TrustedSignerDoesNotExist,

    -- ** NoSuchCloudFrontOriginAccessIdentity
    _NoSuchCloudFrontOriginAccessIdentity,

    -- ** CannotChangeImmutablePublicKeyFields
    _CannotChangeImmutablePublicKeyFields,

    -- ** InvalidProtocolSettings
    _InvalidProtocolSettings,

    -- ** InconsistentQuantities
    _InconsistentQuantities,

    -- ** NoSuchPublicKey
    _NoSuchPublicKey,

    -- ** PreconditionFailed
    _PreconditionFailed,

    -- ** DistributionNotDisabled
    _DistributionNotDisabled,

    -- ** TooManyCloudFrontOriginAccessIdentities
    _TooManyCloudFrontOriginAccessIdentities,

    -- ** InvalidOriginAccessIdentity
    _InvalidOriginAccessIdentity,

    -- ** TooManyFunctionAssociations
    _TooManyFunctionAssociations,

    -- ** NoSuchFieldLevelEncryptionProfile
    _NoSuchFieldLevelEncryptionProfile,

    -- ** TooManyDistributionsAssociatedToFieldLevelEncryptionConfig
    _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig,

    -- ** TooManyCacheBehaviors
    _TooManyCacheBehaviors,

    -- ** TooManyOriginRequestPolicies
    _TooManyOriginRequestPolicies,

    -- ** TooManyFieldLevelEncryptionProfiles
    _TooManyFieldLevelEncryptionProfiles,

    -- ** InvalidQueryStringParameters
    _InvalidQueryStringParameters,

    -- ** TooManyKeyGroups
    _TooManyKeyGroups,

    -- ** TooManyHeadersInOriginRequestPolicy
    _TooManyHeadersInOriginRequestPolicy,

    -- ** TooManyQueryStringsInOriginRequestPolicy
    _TooManyQueryStringsInOriginRequestPolicy,

    -- ** TooManyCertificates
    _TooManyCertificates,

    -- ** InvalidDefaultRootObject
    _InvalidDefaultRootObject,

    -- ** TooManyDistributionsWithSingleFunctionARN
    _TooManyDistributionsWithSingleFunctionARN,

    -- ** OriginRequestPolicyInUse
    _OriginRequestPolicyInUse,

    -- ** InvalidGeoRestrictionParameter
    _InvalidGeoRestrictionParameter,

    -- ** NoSuchOriginRequestPolicy
    _NoSuchOriginRequestPolicy,

    -- ** TooManyDistributionsWithLambdaAssociations
    _TooManyDistributionsWithLambdaAssociations,

    -- ** FunctionSizeLimitExceeded
    _FunctionSizeLimitExceeded,

    -- ** FieldLevelEncryptionProfileAlreadyExists
    _FieldLevelEncryptionProfileAlreadyExists,

    -- ** TooManyFunctions
    _TooManyFunctions,

    -- ** TooManyStreamingDistributionCNAMEs
    _TooManyStreamingDistributionCNAMEs,

    -- ** TooManyDistributions
    _TooManyDistributions,

    -- ** FunctionInUse
    _FunctionInUse,

    -- ** FieldLevelEncryptionProfileSizeExceeded
    _FieldLevelEncryptionProfileSizeExceeded,

    -- ** NoSuchResource
    _NoSuchResource,

    -- ** InvalidTTLOrder
    _InvalidTTLOrder,

    -- ** AccessDenied
    _AccessDenied,

    -- ** QueryArgProfileEmpty
    _QueryArgProfileEmpty,

    -- ** TooManyOrigins
    _TooManyOrigins,

    -- ** TooManyQueryStringsInCachePolicy
    _TooManyQueryStringsInCachePolicy,

    -- ** TooManyHeadersInCachePolicy
    _TooManyHeadersInCachePolicy,

    -- ** NoSuchCachePolicy
    _NoSuchCachePolicy,

    -- ** StreamingDistributionAlreadyExists
    _StreamingDistributionAlreadyExists,

    -- ** TooManyKeyGroupsAssociatedToDistribution
    _TooManyKeyGroupsAssociatedToDistribution,

    -- ** InvalidLambdaFunctionAssociation
    _InvalidLambdaFunctionAssociation,

    -- ** TooManyDistributionsAssociatedToKeyGroup
    _TooManyDistributionsAssociatedToKeyGroup,

    -- ** TooManyRealtimeLogConfigs
    _TooManyRealtimeLogConfigs,

    -- ** TooManyOriginGroupsPerDistribution
    _TooManyOriginGroupsPerDistribution,

    -- ** CloudFrontOriginAccessIdentityInUse
    _CloudFrontOriginAccessIdentityInUse,

    -- ** CachePolicyAlreadyExists
    _CachePolicyAlreadyExists,

    -- ** TooManyFieldLevelEncryptionQueryArgProfiles
    _TooManyFieldLevelEncryptionQueryArgProfiles,

    -- ** TooManyStreamingDistributions
    _TooManyStreamingDistributions,

    -- ** TooManyDistributionCNAMEs
    _TooManyDistributionCNAMEs,

    -- ** NoSuchStreamingDistribution
    _NoSuchStreamingDistribution,

    -- ** CNAMEAlreadyExists
    _CNAMEAlreadyExists,

    -- ** InvalidWebACLId
    _InvalidWebACLId,

    -- ** InvalidOriginKeepaliveTimeout
    _InvalidOriginKeepaliveTimeout,

    -- ** PublicKeyInUse
    _PublicKeyInUse,

    -- ** InvalidArgument
    _InvalidArgument,

    -- * Waiters
    -- $waiters

    -- ** InvalidationCompleted
    newInvalidationCompleted,

    -- ** DistributionDeployed
    newDistributionDeployed,

    -- ** StreamingDistributionDeployed
    newStreamingDistributionDeployed,

    -- * Operations
    -- $operations

    -- ** DeletePublicKey
    DeletePublicKey (DeletePublicKey'),
    newDeletePublicKey,
    DeletePublicKeyResponse (DeletePublicKeyResponse'),
    newDeletePublicKeyResponse,

    -- ** ListPublicKeys
    ListPublicKeys (ListPublicKeys'),
    newListPublicKeys,
    ListPublicKeysResponse (ListPublicKeysResponse'),
    newListPublicKeysResponse,

    -- ** UpdatePublicKey
    UpdatePublicKey (UpdatePublicKey'),
    newUpdatePublicKey,
    UpdatePublicKeyResponse (UpdatePublicKeyResponse'),
    newUpdatePublicKeyResponse,

    -- ** GetDistribution
    GetDistribution (GetDistribution'),
    newGetDistribution,
    GetDistributionResponse (GetDistributionResponse'),
    newGetDistributionResponse,

    -- ** GetFunction
    GetFunction (GetFunction'),
    newGetFunction,
    GetFunctionResponse (GetFunctionResponse'),
    newGetFunctionResponse,

    -- ** ListConflictingAliases
    ListConflictingAliases (ListConflictingAliases'),
    newListConflictingAliases,
    ListConflictingAliasesResponse (ListConflictingAliasesResponse'),
    newListConflictingAliasesResponse,

    -- ** GetKeyGroupConfig
    GetKeyGroupConfig (GetKeyGroupConfig'),
    newGetKeyGroupConfig,
    GetKeyGroupConfigResponse (GetKeyGroupConfigResponse'),
    newGetKeyGroupConfigResponse,

    -- ** CreateFieldLevelEncryptionProfile
    CreateFieldLevelEncryptionProfile (CreateFieldLevelEncryptionProfile'),
    newCreateFieldLevelEncryptionProfile,
    CreateFieldLevelEncryptionProfileResponse (CreateFieldLevelEncryptionProfileResponse'),
    newCreateFieldLevelEncryptionProfileResponse,

    -- ** ListKeyGroups
    ListKeyGroups (ListKeyGroups'),
    newListKeyGroups,
    ListKeyGroupsResponse (ListKeyGroupsResponse'),
    newListKeyGroupsResponse,

    -- ** GetMonitoringSubscription
    GetMonitoringSubscription (GetMonitoringSubscription'),
    newGetMonitoringSubscription,
    GetMonitoringSubscriptionResponse (GetMonitoringSubscriptionResponse'),
    newGetMonitoringSubscriptionResponse,

    -- ** ListDistributionsByCachePolicyId
    ListDistributionsByCachePolicyId (ListDistributionsByCachePolicyId'),
    newListDistributionsByCachePolicyId,
    ListDistributionsByCachePolicyIdResponse (ListDistributionsByCachePolicyIdResponse'),
    newListDistributionsByCachePolicyIdResponse,

    -- ** ListOriginRequestPolicies
    ListOriginRequestPolicies (ListOriginRequestPolicies'),
    newListOriginRequestPolicies,
    ListOriginRequestPoliciesResponse (ListOriginRequestPoliciesResponse'),
    newListOriginRequestPoliciesResponse,

    -- ** CreateOriginRequestPolicy
    CreateOriginRequestPolicy (CreateOriginRequestPolicy'),
    newCreateOriginRequestPolicy,
    CreateOriginRequestPolicyResponse (CreateOriginRequestPolicyResponse'),
    newCreateOriginRequestPolicyResponse,

    -- ** GetKeyGroup
    GetKeyGroup (GetKeyGroup'),
    newGetKeyGroup,
    GetKeyGroupResponse (GetKeyGroupResponse'),
    newGetKeyGroupResponse,

    -- ** GetDistributionConfig
    GetDistributionConfig (GetDistributionConfig'),
    newGetDistributionConfig,
    GetDistributionConfigResponse (GetDistributionConfigResponse'),
    newGetDistributionConfigResponse,

    -- ** ListFunctions
    ListFunctions (ListFunctions'),
    newListFunctions,
    ListFunctionsResponse (ListFunctionsResponse'),
    newListFunctionsResponse,

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

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UpdateFieldLevelEncryptionConfig
    UpdateFieldLevelEncryptionConfig (UpdateFieldLevelEncryptionConfig'),
    newUpdateFieldLevelEncryptionConfig,
    UpdateFieldLevelEncryptionConfigResponse (UpdateFieldLevelEncryptionConfigResponse'),
    newUpdateFieldLevelEncryptionConfigResponse,

    -- ** GetCloudFrontOriginAccessIdentity
    GetCloudFrontOriginAccessIdentity (GetCloudFrontOriginAccessIdentity'),
    newGetCloudFrontOriginAccessIdentity,
    GetCloudFrontOriginAccessIdentityResponse (GetCloudFrontOriginAccessIdentityResponse'),
    newGetCloudFrontOriginAccessIdentityResponse,

    -- ** DeleteFieldLevelEncryptionConfig
    DeleteFieldLevelEncryptionConfig (DeleteFieldLevelEncryptionConfig'),
    newDeleteFieldLevelEncryptionConfig,
    DeleteFieldLevelEncryptionConfigResponse (DeleteFieldLevelEncryptionConfigResponse'),
    newDeleteFieldLevelEncryptionConfigResponse,

    -- ** ListRealtimeLogConfigs
    ListRealtimeLogConfigs (ListRealtimeLogConfigs'),
    newListRealtimeLogConfigs,
    ListRealtimeLogConfigsResponse (ListRealtimeLogConfigsResponse'),
    newListRealtimeLogConfigsResponse,

    -- ** ListCachePolicies
    ListCachePolicies (ListCachePolicies'),
    newListCachePolicies,
    ListCachePoliciesResponse (ListCachePoliciesResponse'),
    newListCachePoliciesResponse,

    -- ** GetPublicKey
    GetPublicKey (GetPublicKey'),
    newGetPublicKey,
    GetPublicKeyResponse (GetPublicKeyResponse'),
    newGetPublicKeyResponse,

    -- ** CreateCachePolicy
    CreateCachePolicy (CreateCachePolicy'),
    newCreateCachePolicy,
    CreateCachePolicyResponse (CreateCachePolicyResponse'),
    newCreateCachePolicyResponse,

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

    -- ** ListFieldLevelEncryptionConfigs
    ListFieldLevelEncryptionConfigs (ListFieldLevelEncryptionConfigs'),
    newListFieldLevelEncryptionConfigs,
    ListFieldLevelEncryptionConfigsResponse (ListFieldLevelEncryptionConfigsResponse'),
    newListFieldLevelEncryptionConfigsResponse,

    -- ** GetFieldLevelEncryption
    GetFieldLevelEncryption (GetFieldLevelEncryption'),
    newGetFieldLevelEncryption,
    GetFieldLevelEncryptionResponse (GetFieldLevelEncryptionResponse'),
    newGetFieldLevelEncryptionResponse,

    -- ** UpdateCachePolicy
    UpdateCachePolicy (UpdateCachePolicy'),
    newUpdateCachePolicy,
    UpdateCachePolicyResponse (UpdateCachePolicyResponse'),
    newUpdateCachePolicyResponse,

    -- ** DeleteCachePolicy
    DeleteCachePolicy (DeleteCachePolicy'),
    newDeleteCachePolicy,
    DeleteCachePolicyResponse (DeleteCachePolicyResponse'),
    newDeleteCachePolicyResponse,

    -- ** GetInvalidation
    GetInvalidation (GetInvalidation'),
    newGetInvalidation,
    GetInvalidationResponse (GetInvalidationResponse'),
    newGetInvalidationResponse,

    -- ** CreateStreamingDistribution
    CreateStreamingDistribution (CreateStreamingDistribution'),
    newCreateStreamingDistribution,
    CreateStreamingDistributionResponse (CreateStreamingDistributionResponse'),
    newCreateStreamingDistributionResponse,

    -- ** GetPublicKeyConfig
    GetPublicKeyConfig (GetPublicKeyConfig'),
    newGetPublicKeyConfig,
    GetPublicKeyConfigResponse (GetPublicKeyConfigResponse'),
    newGetPublicKeyConfigResponse,

    -- ** GetCloudFrontOriginAccessIdentityConfig
    GetCloudFrontOriginAccessIdentityConfig (GetCloudFrontOriginAccessIdentityConfig'),
    newGetCloudFrontOriginAccessIdentityConfig,
    GetCloudFrontOriginAccessIdentityConfigResponse (GetCloudFrontOriginAccessIdentityConfigResponse'),
    newGetCloudFrontOriginAccessIdentityConfigResponse,

    -- ** UpdateCloudFrontOriginAccessIdentity
    UpdateCloudFrontOriginAccessIdentity (UpdateCloudFrontOriginAccessIdentity'),
    newUpdateCloudFrontOriginAccessIdentity,
    UpdateCloudFrontOriginAccessIdentityResponse (UpdateCloudFrontOriginAccessIdentityResponse'),
    newUpdateCloudFrontOriginAccessIdentityResponse,

    -- ** GetRealtimeLogConfig
    GetRealtimeLogConfig (GetRealtimeLogConfig'),
    newGetRealtimeLogConfig,
    GetRealtimeLogConfigResponse (GetRealtimeLogConfigResponse'),
    newGetRealtimeLogConfigResponse,

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

    -- ** DescribeFunction
    DescribeFunction (DescribeFunction'),
    newDescribeFunction,
    DescribeFunctionResponse (DescribeFunctionResponse'),
    newDescribeFunctionResponse,

    -- ** DeleteCloudFrontOriginAccessIdentity
    DeleteCloudFrontOriginAccessIdentity (DeleteCloudFrontOriginAccessIdentity'),
    newDeleteCloudFrontOriginAccessIdentity,
    DeleteCloudFrontOriginAccessIdentityResponse (DeleteCloudFrontOriginAccessIdentityResponse'),
    newDeleteCloudFrontOriginAccessIdentityResponse,

    -- ** GetFieldLevelEncryptionConfig
    GetFieldLevelEncryptionConfig (GetFieldLevelEncryptionConfig'),
    newGetFieldLevelEncryptionConfig,
    GetFieldLevelEncryptionConfigResponse (GetFieldLevelEncryptionConfigResponse'),
    newGetFieldLevelEncryptionConfigResponse,

    -- ** ListStreamingDistributions (Paginated)
    ListStreamingDistributions (ListStreamingDistributions'),
    newListStreamingDistributions,
    ListStreamingDistributionsResponse (ListStreamingDistributionsResponse'),
    newListStreamingDistributionsResponse,

    -- ** GetFieldLevelEncryptionProfileConfig
    GetFieldLevelEncryptionProfileConfig (GetFieldLevelEncryptionProfileConfig'),
    newGetFieldLevelEncryptionProfileConfig,
    GetFieldLevelEncryptionProfileConfigResponse (GetFieldLevelEncryptionProfileConfigResponse'),
    newGetFieldLevelEncryptionProfileConfigResponse,

    -- ** DeleteOriginRequestPolicy
    DeleteOriginRequestPolicy (DeleteOriginRequestPolicy'),
    newDeleteOriginRequestPolicy,
    DeleteOriginRequestPolicyResponse (DeleteOriginRequestPolicyResponse'),
    newDeleteOriginRequestPolicyResponse,

    -- ** TestFunction
    TestFunction (TestFunction'),
    newTestFunction,
    TestFunctionResponse (TestFunctionResponse'),
    newTestFunctionResponse,

    -- ** CreateKeyGroup
    CreateKeyGroup (CreateKeyGroup'),
    newCreateKeyGroup,
    CreateKeyGroupResponse (CreateKeyGroupResponse'),
    newCreateKeyGroupResponse,

    -- ** UpdateOriginRequestPolicy
    UpdateOriginRequestPolicy (UpdateOriginRequestPolicy'),
    newUpdateOriginRequestPolicy,
    UpdateOriginRequestPolicyResponse (UpdateOriginRequestPolicyResponse'),
    newUpdateOriginRequestPolicyResponse,

    -- ** UpdateFieldLevelEncryptionProfile
    UpdateFieldLevelEncryptionProfile (UpdateFieldLevelEncryptionProfile'),
    newUpdateFieldLevelEncryptionProfile,
    UpdateFieldLevelEncryptionProfileResponse (UpdateFieldLevelEncryptionProfileResponse'),
    newUpdateFieldLevelEncryptionProfileResponse,

    -- ** DeleteKeyGroup
    DeleteKeyGroup (DeleteKeyGroup'),
    newDeleteKeyGroup,
    DeleteKeyGroupResponse (DeleteKeyGroupResponse'),
    newDeleteKeyGroupResponse,

    -- ** GetOriginRequestPolicyConfig
    GetOriginRequestPolicyConfig (GetOriginRequestPolicyConfig'),
    newGetOriginRequestPolicyConfig,
    GetOriginRequestPolicyConfigResponse (GetOriginRequestPolicyConfigResponse'),
    newGetOriginRequestPolicyConfigResponse,

    -- ** ListFieldLevelEncryptionProfiles
    ListFieldLevelEncryptionProfiles (ListFieldLevelEncryptionProfiles'),
    newListFieldLevelEncryptionProfiles,
    ListFieldLevelEncryptionProfilesResponse (ListFieldLevelEncryptionProfilesResponse'),
    newListFieldLevelEncryptionProfilesResponse,

    -- ** DeleteFieldLevelEncryptionProfile
    DeleteFieldLevelEncryptionProfile (DeleteFieldLevelEncryptionProfile'),
    newDeleteFieldLevelEncryptionProfile,
    DeleteFieldLevelEncryptionProfileResponse (DeleteFieldLevelEncryptionProfileResponse'),
    newDeleteFieldLevelEncryptionProfileResponse,

    -- ** UpdateKeyGroup
    UpdateKeyGroup (UpdateKeyGroup'),
    newUpdateKeyGroup,
    UpdateKeyGroupResponse (UpdateKeyGroupResponse'),
    newUpdateKeyGroupResponse,

    -- ** CreateStreamingDistributionWithTags
    CreateStreamingDistributionWithTags (CreateStreamingDistributionWithTags'),
    newCreateStreamingDistributionWithTags,
    CreateStreamingDistributionWithTagsResponse (CreateStreamingDistributionWithTagsResponse'),
    newCreateStreamingDistributionWithTagsResponse,

    -- ** ListDistributionsByRealtimeLogConfig
    ListDistributionsByRealtimeLogConfig (ListDistributionsByRealtimeLogConfig'),
    newListDistributionsByRealtimeLogConfig,
    ListDistributionsByRealtimeLogConfigResponse (ListDistributionsByRealtimeLogConfigResponse'),
    newListDistributionsByRealtimeLogConfigResponse,

    -- ** CreateFunction
    CreateFunction (CreateFunction'),
    newCreateFunction,
    CreateFunctionResponse (CreateFunctionResponse'),
    newCreateFunctionResponse,

    -- ** DeleteMonitoringSubscription
    DeleteMonitoringSubscription (DeleteMonitoringSubscription'),
    newDeleteMonitoringSubscription,
    DeleteMonitoringSubscriptionResponse (DeleteMonitoringSubscriptionResponse'),
    newDeleteMonitoringSubscriptionResponse,

    -- ** GetFieldLevelEncryptionProfile
    GetFieldLevelEncryptionProfile (GetFieldLevelEncryptionProfile'),
    newGetFieldLevelEncryptionProfile,
    GetFieldLevelEncryptionProfileResponse (GetFieldLevelEncryptionProfileResponse'),
    newGetFieldLevelEncryptionProfileResponse,

    -- ** CreateDistribution
    CreateDistribution (CreateDistribution'),
    newCreateDistribution,
    CreateDistributionResponse (CreateDistributionResponse'),
    newCreateDistributionResponse,

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

    -- ** UpdateDistribution
    UpdateDistribution (UpdateDistribution'),
    newUpdateDistribution,
    UpdateDistributionResponse (UpdateDistributionResponse'),
    newUpdateDistributionResponse,

    -- ** CreateMonitoringSubscription
    CreateMonitoringSubscription (CreateMonitoringSubscription'),
    newCreateMonitoringSubscription,
    CreateMonitoringSubscriptionResponse (CreateMonitoringSubscriptionResponse'),
    newCreateMonitoringSubscriptionResponse,

    -- ** GetOriginRequestPolicy
    GetOriginRequestPolicy (GetOriginRequestPolicy'),
    newGetOriginRequestPolicy,
    GetOriginRequestPolicyResponse (GetOriginRequestPolicyResponse'),
    newGetOriginRequestPolicyResponse,

    -- ** UpdateFunction
    UpdateFunction (UpdateFunction'),
    newUpdateFunction,
    UpdateFunctionResponse (UpdateFunctionResponse'),
    newUpdateFunctionResponse,

    -- ** GetCachePolicyConfig
    GetCachePolicyConfig (GetCachePolicyConfig'),
    newGetCachePolicyConfig,
    GetCachePolicyConfigResponse (GetCachePolicyConfigResponse'),
    newGetCachePolicyConfigResponse,

    -- ** GetStreamingDistribution
    GetStreamingDistribution (GetStreamingDistribution'),
    newGetStreamingDistribution,
    GetStreamingDistributionResponse (GetStreamingDistributionResponse'),
    newGetStreamingDistributionResponse,

    -- ** CreateInvalidation
    CreateInvalidation (CreateInvalidation'),
    newCreateInvalidation,
    CreateInvalidationResponse (CreateInvalidationResponse'),
    newCreateInvalidationResponse,

    -- ** UpdateRealtimeLogConfig
    UpdateRealtimeLogConfig (UpdateRealtimeLogConfig'),
    newUpdateRealtimeLogConfig,
    UpdateRealtimeLogConfigResponse (UpdateRealtimeLogConfigResponse'),
    newUpdateRealtimeLogConfigResponse,

    -- ** DeleteRealtimeLogConfig
    DeleteRealtimeLogConfig (DeleteRealtimeLogConfig'),
    newDeleteRealtimeLogConfig,
    DeleteRealtimeLogConfigResponse (DeleteRealtimeLogConfigResponse'),
    newDeleteRealtimeLogConfigResponse,

    -- ** ListInvalidations (Paginated)
    ListInvalidations (ListInvalidations'),
    newListInvalidations,
    ListInvalidationsResponse (ListInvalidationsResponse'),
    newListInvalidationsResponse,

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

    -- ** CreateRealtimeLogConfig
    CreateRealtimeLogConfig (CreateRealtimeLogConfig'),
    newCreateRealtimeLogConfig,
    CreateRealtimeLogConfigResponse (CreateRealtimeLogConfigResponse'),
    newCreateRealtimeLogConfigResponse,

    -- ** GetStreamingDistributionConfig
    GetStreamingDistributionConfig (GetStreamingDistributionConfig'),
    newGetStreamingDistributionConfig,
    GetStreamingDistributionConfigResponse (GetStreamingDistributionConfigResponse'),
    newGetStreamingDistributionConfigResponse,

    -- ** GetCachePolicy
    GetCachePolicy (GetCachePolicy'),
    newGetCachePolicy,
    GetCachePolicyResponse (GetCachePolicyResponse'),
    newGetCachePolicyResponse,

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

    -- ** AssociateAlias
    AssociateAlias (AssociateAlias'),
    newAssociateAlias,
    AssociateAliasResponse (AssociateAliasResponse'),
    newAssociateAliasResponse,

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

import Network.AWS.CloudFront.AssociateAlias
import Network.AWS.CloudFront.CreateCachePolicy
import Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.CreateDistribution
import Network.AWS.CloudFront.CreateDistributionWithTags
import Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
import Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
import Network.AWS.CloudFront.CreateFunction
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
import Network.AWS.CloudFront.DeleteFunction
import Network.AWS.CloudFront.DeleteKeyGroup
import Network.AWS.CloudFront.DeleteMonitoringSubscription
import Network.AWS.CloudFront.DeleteOriginRequestPolicy
import Network.AWS.CloudFront.DeletePublicKey
import Network.AWS.CloudFront.DeleteRealtimeLogConfig
import Network.AWS.CloudFront.DeleteStreamingDistribution
import Network.AWS.CloudFront.DescribeFunction
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
import Network.AWS.CloudFront.GetFunction
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
import Network.AWS.CloudFront.Lens
import Network.AWS.CloudFront.ListCachePolicies
import Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
import Network.AWS.CloudFront.ListConflictingAliases
import Network.AWS.CloudFront.ListDistributions
import Network.AWS.CloudFront.ListDistributionsByCachePolicyId
import Network.AWS.CloudFront.ListDistributionsByKeyGroup
import Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
import Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
import Network.AWS.CloudFront.ListDistributionsByWebACLId
import Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
import Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
import Network.AWS.CloudFront.ListFunctions
import Network.AWS.CloudFront.ListInvalidations
import Network.AWS.CloudFront.ListKeyGroups
import Network.AWS.CloudFront.ListOriginRequestPolicies
import Network.AWS.CloudFront.ListPublicKeys
import Network.AWS.CloudFront.ListRealtimeLogConfigs
import Network.AWS.CloudFront.ListStreamingDistributions
import Network.AWS.CloudFront.ListTagsForResource
import Network.AWS.CloudFront.PublishFunction
import Network.AWS.CloudFront.TagResource
import Network.AWS.CloudFront.TestFunction
import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.UntagResource
import Network.AWS.CloudFront.UpdateCachePolicy
import Network.AWS.CloudFront.UpdateCloudFrontOriginAccessIdentity
import Network.AWS.CloudFront.UpdateDistribution
import Network.AWS.CloudFront.UpdateFieldLevelEncryptionConfig
import Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile
import Network.AWS.CloudFront.UpdateFunction
import Network.AWS.CloudFront.UpdateKeyGroup
import Network.AWS.CloudFront.UpdateOriginRequestPolicy
import Network.AWS.CloudFront.UpdatePublicKey
import Network.AWS.CloudFront.UpdateRealtimeLogConfig
import Network.AWS.CloudFront.UpdateStreamingDistribution
import Network.AWS.CloudFront.Waiters

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
