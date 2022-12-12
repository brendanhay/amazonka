{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.OpenSearchServerless
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-11-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Use the Amazon OpenSearch Serverless API to create, configure, and
-- manage OpenSearch Serverless collections and security policies.
--
-- OpenSearch Serverless is an on-demand, pre-provisioned serverless
-- configuration for Amazon OpenSearch Service. OpenSearch Serverless
-- removes the operational complexities of provisioning, configuring, and
-- tuning your OpenSearch clusters. It enables you to easily search and
-- analyze petabytes of data without having to worry about the underlying
-- infrastructure and data management.
--
-- To learn more about OpenSearch Serverless, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-overview.html What is Amazon OpenSearch Serverless?>
module Amazonka.OpenSearchServerless
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchGetCollection
    BatchGetCollection (BatchGetCollection'),
    newBatchGetCollection,
    BatchGetCollectionResponse (BatchGetCollectionResponse'),
    newBatchGetCollectionResponse,

    -- ** BatchGetVpcEndpoint
    BatchGetVpcEndpoint (BatchGetVpcEndpoint'),
    newBatchGetVpcEndpoint,
    BatchGetVpcEndpointResponse (BatchGetVpcEndpointResponse'),
    newBatchGetVpcEndpointResponse,

    -- ** CreateAccessPolicy
    CreateAccessPolicy (CreateAccessPolicy'),
    newCreateAccessPolicy,
    CreateAccessPolicyResponse (CreateAccessPolicyResponse'),
    newCreateAccessPolicyResponse,

    -- ** CreateCollection
    CreateCollection (CreateCollection'),
    newCreateCollection,
    CreateCollectionResponse (CreateCollectionResponse'),
    newCreateCollectionResponse,

    -- ** CreateSecurityConfig
    CreateSecurityConfig (CreateSecurityConfig'),
    newCreateSecurityConfig,
    CreateSecurityConfigResponse (CreateSecurityConfigResponse'),
    newCreateSecurityConfigResponse,

    -- ** CreateSecurityPolicy
    CreateSecurityPolicy (CreateSecurityPolicy'),
    newCreateSecurityPolicy,
    CreateSecurityPolicyResponse (CreateSecurityPolicyResponse'),
    newCreateSecurityPolicyResponse,

    -- ** CreateVpcEndpoint
    CreateVpcEndpoint (CreateVpcEndpoint'),
    newCreateVpcEndpoint,
    CreateVpcEndpointResponse (CreateVpcEndpointResponse'),
    newCreateVpcEndpointResponse,

    -- ** DeleteAccessPolicy
    DeleteAccessPolicy (DeleteAccessPolicy'),
    newDeleteAccessPolicy,
    DeleteAccessPolicyResponse (DeleteAccessPolicyResponse'),
    newDeleteAccessPolicyResponse,

    -- ** DeleteCollection
    DeleteCollection (DeleteCollection'),
    newDeleteCollection,
    DeleteCollectionResponse (DeleteCollectionResponse'),
    newDeleteCollectionResponse,

    -- ** DeleteSecurityConfig
    DeleteSecurityConfig (DeleteSecurityConfig'),
    newDeleteSecurityConfig,
    DeleteSecurityConfigResponse (DeleteSecurityConfigResponse'),
    newDeleteSecurityConfigResponse,

    -- ** DeleteSecurityPolicy
    DeleteSecurityPolicy (DeleteSecurityPolicy'),
    newDeleteSecurityPolicy,
    DeleteSecurityPolicyResponse (DeleteSecurityPolicyResponse'),
    newDeleteSecurityPolicyResponse,

    -- ** DeleteVpcEndpoint
    DeleteVpcEndpoint (DeleteVpcEndpoint'),
    newDeleteVpcEndpoint,
    DeleteVpcEndpointResponse (DeleteVpcEndpointResponse'),
    newDeleteVpcEndpointResponse,

    -- ** GetAccessPolicy
    GetAccessPolicy (GetAccessPolicy'),
    newGetAccessPolicy,
    GetAccessPolicyResponse (GetAccessPolicyResponse'),
    newGetAccessPolicyResponse,

    -- ** GetAccountSettings
    GetAccountSettings (GetAccountSettings'),
    newGetAccountSettings,
    GetAccountSettingsResponse (GetAccountSettingsResponse'),
    newGetAccountSettingsResponse,

    -- ** GetPoliciesStats
    GetPoliciesStats (GetPoliciesStats'),
    newGetPoliciesStats,
    GetPoliciesStatsResponse (GetPoliciesStatsResponse'),
    newGetPoliciesStatsResponse,

    -- ** GetSecurityConfig
    GetSecurityConfig (GetSecurityConfig'),
    newGetSecurityConfig,
    GetSecurityConfigResponse (GetSecurityConfigResponse'),
    newGetSecurityConfigResponse,

    -- ** GetSecurityPolicy
    GetSecurityPolicy (GetSecurityPolicy'),
    newGetSecurityPolicy,
    GetSecurityPolicyResponse (GetSecurityPolicyResponse'),
    newGetSecurityPolicyResponse,

    -- ** ListAccessPolicies
    ListAccessPolicies (ListAccessPolicies'),
    newListAccessPolicies,
    ListAccessPoliciesResponse (ListAccessPoliciesResponse'),
    newListAccessPoliciesResponse,

    -- ** ListCollections
    ListCollections (ListCollections'),
    newListCollections,
    ListCollectionsResponse (ListCollectionsResponse'),
    newListCollectionsResponse,

    -- ** ListSecurityConfigs
    ListSecurityConfigs (ListSecurityConfigs'),
    newListSecurityConfigs,
    ListSecurityConfigsResponse (ListSecurityConfigsResponse'),
    newListSecurityConfigsResponse,

    -- ** ListSecurityPolicies
    ListSecurityPolicies (ListSecurityPolicies'),
    newListSecurityPolicies,
    ListSecurityPoliciesResponse (ListSecurityPoliciesResponse'),
    newListSecurityPoliciesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListVpcEndpoints
    ListVpcEndpoints (ListVpcEndpoints'),
    newListVpcEndpoints,
    ListVpcEndpointsResponse (ListVpcEndpointsResponse'),
    newListVpcEndpointsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAccessPolicy
    UpdateAccessPolicy (UpdateAccessPolicy'),
    newUpdateAccessPolicy,
    UpdateAccessPolicyResponse (UpdateAccessPolicyResponse'),
    newUpdateAccessPolicyResponse,

    -- ** UpdateAccountSettings
    UpdateAccountSettings (UpdateAccountSettings'),
    newUpdateAccountSettings,
    UpdateAccountSettingsResponse (UpdateAccountSettingsResponse'),
    newUpdateAccountSettingsResponse,

    -- ** UpdateCollection
    UpdateCollection (UpdateCollection'),
    newUpdateCollection,
    UpdateCollectionResponse (UpdateCollectionResponse'),
    newUpdateCollectionResponse,

    -- ** UpdateSecurityConfig
    UpdateSecurityConfig (UpdateSecurityConfig'),
    newUpdateSecurityConfig,
    UpdateSecurityConfigResponse (UpdateSecurityConfigResponse'),
    newUpdateSecurityConfigResponse,

    -- ** UpdateSecurityPolicy
    UpdateSecurityPolicy (UpdateSecurityPolicy'),
    newUpdateSecurityPolicy,
    UpdateSecurityPolicyResponse (UpdateSecurityPolicyResponse'),
    newUpdateSecurityPolicyResponse,

    -- ** UpdateVpcEndpoint
    UpdateVpcEndpoint (UpdateVpcEndpoint'),
    newUpdateVpcEndpoint,
    UpdateVpcEndpointResponse (UpdateVpcEndpointResponse'),
    newUpdateVpcEndpointResponse,

    -- * Types

    -- ** AccessPolicyType
    AccessPolicyType (..),

    -- ** CollectionStatus
    CollectionStatus (..),

    -- ** CollectionType
    CollectionType (..),

    -- ** SecurityConfigType
    SecurityConfigType (..),

    -- ** SecurityPolicyType
    SecurityPolicyType (..),

    -- ** VpcEndpointStatus
    VpcEndpointStatus (..),

    -- ** AccessPolicyDetail
    AccessPolicyDetail (AccessPolicyDetail'),
    newAccessPolicyDetail,

    -- ** AccessPolicyStats
    AccessPolicyStats (AccessPolicyStats'),
    newAccessPolicyStats,

    -- ** AccessPolicySummary
    AccessPolicySummary (AccessPolicySummary'),
    newAccessPolicySummary,

    -- ** AccountSettingsDetail
    AccountSettingsDetail (AccountSettingsDetail'),
    newAccountSettingsDetail,

    -- ** CapacityLimits
    CapacityLimits (CapacityLimits'),
    newCapacityLimits,

    -- ** CollectionDetail
    CollectionDetail (CollectionDetail'),
    newCollectionDetail,

    -- ** CollectionErrorDetail
    CollectionErrorDetail (CollectionErrorDetail'),
    newCollectionErrorDetail,

    -- ** CollectionFilters
    CollectionFilters (CollectionFilters'),
    newCollectionFilters,

    -- ** CollectionSummary
    CollectionSummary (CollectionSummary'),
    newCollectionSummary,

    -- ** CreateCollectionDetail
    CreateCollectionDetail (CreateCollectionDetail'),
    newCreateCollectionDetail,

    -- ** CreateVpcEndpointDetail
    CreateVpcEndpointDetail (CreateVpcEndpointDetail'),
    newCreateVpcEndpointDetail,

    -- ** DeleteCollectionDetail
    DeleteCollectionDetail (DeleteCollectionDetail'),
    newDeleteCollectionDetail,

    -- ** DeleteVpcEndpointDetail
    DeleteVpcEndpointDetail (DeleteVpcEndpointDetail'),
    newDeleteVpcEndpointDetail,

    -- ** Document
    Document (Document'),
    newDocument,

    -- ** SamlConfigOptions
    SamlConfigOptions (SamlConfigOptions'),
    newSamlConfigOptions,

    -- ** SecurityConfigDetail
    SecurityConfigDetail (SecurityConfigDetail'),
    newSecurityConfigDetail,

    -- ** SecurityConfigStats
    SecurityConfigStats (SecurityConfigStats'),
    newSecurityConfigStats,

    -- ** SecurityConfigSummary
    SecurityConfigSummary (SecurityConfigSummary'),
    newSecurityConfigSummary,

    -- ** SecurityPolicyDetail
    SecurityPolicyDetail (SecurityPolicyDetail'),
    newSecurityPolicyDetail,

    -- ** SecurityPolicyStats
    SecurityPolicyStats (SecurityPolicyStats'),
    newSecurityPolicyStats,

    -- ** SecurityPolicySummary
    SecurityPolicySummary (SecurityPolicySummary'),
    newSecurityPolicySummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UpdateCollectionDetail
    UpdateCollectionDetail (UpdateCollectionDetail'),
    newUpdateCollectionDetail,

    -- ** UpdateVpcEndpointDetail
    UpdateVpcEndpointDetail (UpdateVpcEndpointDetail'),
    newUpdateVpcEndpointDetail,

    -- ** VpcEndpointDetail
    VpcEndpointDetail (VpcEndpointDetail'),
    newVpcEndpointDetail,

    -- ** VpcEndpointErrorDetail
    VpcEndpointErrorDetail (VpcEndpointErrorDetail'),
    newVpcEndpointErrorDetail,

    -- ** VpcEndpointFilters
    VpcEndpointFilters (VpcEndpointFilters'),
    newVpcEndpointFilters,

    -- ** VpcEndpointSummary
    VpcEndpointSummary (VpcEndpointSummary'),
    newVpcEndpointSummary,
  )
where

import Amazonka.OpenSearchServerless.BatchGetCollection
import Amazonka.OpenSearchServerless.BatchGetVpcEndpoint
import Amazonka.OpenSearchServerless.CreateAccessPolicy
import Amazonka.OpenSearchServerless.CreateCollection
import Amazonka.OpenSearchServerless.CreateSecurityConfig
import Amazonka.OpenSearchServerless.CreateSecurityPolicy
import Amazonka.OpenSearchServerless.CreateVpcEndpoint
import Amazonka.OpenSearchServerless.DeleteAccessPolicy
import Amazonka.OpenSearchServerless.DeleteCollection
import Amazonka.OpenSearchServerless.DeleteSecurityConfig
import Amazonka.OpenSearchServerless.DeleteSecurityPolicy
import Amazonka.OpenSearchServerless.DeleteVpcEndpoint
import Amazonka.OpenSearchServerless.GetAccessPolicy
import Amazonka.OpenSearchServerless.GetAccountSettings
import Amazonka.OpenSearchServerless.GetPoliciesStats
import Amazonka.OpenSearchServerless.GetSecurityConfig
import Amazonka.OpenSearchServerless.GetSecurityPolicy
import Amazonka.OpenSearchServerless.Lens
import Amazonka.OpenSearchServerless.ListAccessPolicies
import Amazonka.OpenSearchServerless.ListCollections
import Amazonka.OpenSearchServerless.ListSecurityConfigs
import Amazonka.OpenSearchServerless.ListSecurityPolicies
import Amazonka.OpenSearchServerless.ListTagsForResource
import Amazonka.OpenSearchServerless.ListVpcEndpoints
import Amazonka.OpenSearchServerless.TagResource
import Amazonka.OpenSearchServerless.Types
import Amazonka.OpenSearchServerless.UntagResource
import Amazonka.OpenSearchServerless.UpdateAccessPolicy
import Amazonka.OpenSearchServerless.UpdateAccountSettings
import Amazonka.OpenSearchServerless.UpdateCollection
import Amazonka.OpenSearchServerless.UpdateSecurityConfig
import Amazonka.OpenSearchServerless.UpdateSecurityPolicy
import Amazonka.OpenSearchServerless.UpdateVpcEndpoint
import Amazonka.OpenSearchServerless.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'OpenSearchServerless'.

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
