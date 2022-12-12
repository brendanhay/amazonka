{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearchServerless.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Lens
  ( -- * Operations

    -- ** BatchGetCollection
    batchGetCollection_ids,
    batchGetCollection_names,
    batchGetCollectionResponse_collectionDetails,
    batchGetCollectionResponse_collectionErrorDetails,
    batchGetCollectionResponse_httpStatus,

    -- ** BatchGetVpcEndpoint
    batchGetVpcEndpoint_ids,
    batchGetVpcEndpointResponse_vpcEndpointDetails,
    batchGetVpcEndpointResponse_vpcEndpointErrorDetails,
    batchGetVpcEndpointResponse_httpStatus,

    -- ** CreateAccessPolicy
    createAccessPolicy_clientToken,
    createAccessPolicy_description,
    createAccessPolicy_name,
    createAccessPolicy_policy,
    createAccessPolicy_type,
    createAccessPolicyResponse_accessPolicyDetail,
    createAccessPolicyResponse_httpStatus,

    -- ** CreateCollection
    createCollection_clientToken,
    createCollection_description,
    createCollection_tags,
    createCollection_type,
    createCollection_name,
    createCollectionResponse_createCollectionDetail,
    createCollectionResponse_httpStatus,

    -- ** CreateSecurityConfig
    createSecurityConfig_clientToken,
    createSecurityConfig_description,
    createSecurityConfig_samlOptions,
    createSecurityConfig_name,
    createSecurityConfig_type,
    createSecurityConfigResponse_securityConfigDetail,
    createSecurityConfigResponse_httpStatus,

    -- ** CreateSecurityPolicy
    createSecurityPolicy_clientToken,
    createSecurityPolicy_description,
    createSecurityPolicy_name,
    createSecurityPolicy_policy,
    createSecurityPolicy_type,
    createSecurityPolicyResponse_securityPolicyDetail,
    createSecurityPolicyResponse_httpStatus,

    -- ** CreateVpcEndpoint
    createVpcEndpoint_clientToken,
    createVpcEndpoint_securityGroupIds,
    createVpcEndpoint_name,
    createVpcEndpoint_subnetIds,
    createVpcEndpoint_vpcId,
    createVpcEndpointResponse_createVpcEndpointDetail,
    createVpcEndpointResponse_httpStatus,

    -- ** DeleteAccessPolicy
    deleteAccessPolicy_clientToken,
    deleteAccessPolicy_name,
    deleteAccessPolicy_type,
    deleteAccessPolicyResponse_httpStatus,

    -- ** DeleteCollection
    deleteCollection_clientToken,
    deleteCollection_id,
    deleteCollectionResponse_deleteCollectionDetail,
    deleteCollectionResponse_httpStatus,

    -- ** DeleteSecurityConfig
    deleteSecurityConfig_clientToken,
    deleteSecurityConfig_id,
    deleteSecurityConfigResponse_httpStatus,

    -- ** DeleteSecurityPolicy
    deleteSecurityPolicy_clientToken,
    deleteSecurityPolicy_name,
    deleteSecurityPolicy_type,
    deleteSecurityPolicyResponse_httpStatus,

    -- ** DeleteVpcEndpoint
    deleteVpcEndpoint_clientToken,
    deleteVpcEndpoint_id,
    deleteVpcEndpointResponse_deleteVpcEndpointDetail,
    deleteVpcEndpointResponse_httpStatus,

    -- ** GetAccessPolicy
    getAccessPolicy_name,
    getAccessPolicy_type,
    getAccessPolicyResponse_accessPolicyDetail,
    getAccessPolicyResponse_httpStatus,

    -- ** GetAccountSettings
    getAccountSettingsResponse_accountSettingsDetail,
    getAccountSettingsResponse_httpStatus,

    -- ** GetPoliciesStats
    getPoliciesStatsResponse_accessPolicyStats,
    getPoliciesStatsResponse_securityConfigStats,
    getPoliciesStatsResponse_securityPolicyStats,
    getPoliciesStatsResponse_totalPolicyCount,
    getPoliciesStatsResponse_httpStatus,

    -- ** GetSecurityConfig
    getSecurityConfig_id,
    getSecurityConfigResponse_securityConfigDetail,
    getSecurityConfigResponse_httpStatus,

    -- ** GetSecurityPolicy
    getSecurityPolicy_name,
    getSecurityPolicy_type,
    getSecurityPolicyResponse_securityPolicyDetail,
    getSecurityPolicyResponse_httpStatus,

    -- ** ListAccessPolicies
    listAccessPolicies_maxResults,
    listAccessPolicies_nextToken,
    listAccessPolicies_resource,
    listAccessPolicies_type,
    listAccessPoliciesResponse_accessPolicySummaries,
    listAccessPoliciesResponse_nextToken,
    listAccessPoliciesResponse_httpStatus,

    -- ** ListCollections
    listCollections_collectionFilters,
    listCollections_maxResults,
    listCollections_nextToken,
    listCollectionsResponse_collectionSummaries,
    listCollectionsResponse_nextToken,
    listCollectionsResponse_httpStatus,

    -- ** ListSecurityConfigs
    listSecurityConfigs_maxResults,
    listSecurityConfigs_nextToken,
    listSecurityConfigs_type,
    listSecurityConfigsResponse_nextToken,
    listSecurityConfigsResponse_securityConfigSummaries,
    listSecurityConfigsResponse_httpStatus,

    -- ** ListSecurityPolicies
    listSecurityPolicies_maxResults,
    listSecurityPolicies_nextToken,
    listSecurityPolicies_resource,
    listSecurityPolicies_type,
    listSecurityPoliciesResponse_nextToken,
    listSecurityPoliciesResponse_securityPolicySummaries,
    listSecurityPoliciesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVpcEndpoints
    listVpcEndpoints_maxResults,
    listVpcEndpoints_nextToken,
    listVpcEndpoints_vpcEndpointFilters,
    listVpcEndpointsResponse_nextToken,
    listVpcEndpointsResponse_vpcEndpointSummaries,
    listVpcEndpointsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAccessPolicy
    updateAccessPolicy_clientToken,
    updateAccessPolicy_description,
    updateAccessPolicy_policy,
    updateAccessPolicy_name,
    updateAccessPolicy_policyVersion,
    updateAccessPolicy_type,
    updateAccessPolicyResponse_accessPolicyDetail,
    updateAccessPolicyResponse_httpStatus,

    -- ** UpdateAccountSettings
    updateAccountSettings_capacityLimits,
    updateAccountSettingsResponse_accountSettingsDetail,
    updateAccountSettingsResponse_httpStatus,

    -- ** UpdateCollection
    updateCollection_clientToken,
    updateCollection_description,
    updateCollection_id,
    updateCollectionResponse_updateCollectionDetail,
    updateCollectionResponse_httpStatus,

    -- ** UpdateSecurityConfig
    updateSecurityConfig_clientToken,
    updateSecurityConfig_description,
    updateSecurityConfig_samlOptions,
    updateSecurityConfig_configVersion,
    updateSecurityConfig_id,
    updateSecurityConfigResponse_securityConfigDetail,
    updateSecurityConfigResponse_httpStatus,

    -- ** UpdateSecurityPolicy
    updateSecurityPolicy_clientToken,
    updateSecurityPolicy_description,
    updateSecurityPolicy_policy,
    updateSecurityPolicy_name,
    updateSecurityPolicy_policyVersion,
    updateSecurityPolicy_type,
    updateSecurityPolicyResponse_securityPolicyDetail,
    updateSecurityPolicyResponse_httpStatus,

    -- ** UpdateVpcEndpoint
    updateVpcEndpoint_addSecurityGroupIds,
    updateVpcEndpoint_addSubnetIds,
    updateVpcEndpoint_clientToken,
    updateVpcEndpoint_removeSecurityGroupIds,
    updateVpcEndpoint_removeSubnetIds,
    updateVpcEndpoint_id,
    updateVpcEndpointResponse_updateVpcEndpointDetail,
    updateVpcEndpointResponse_httpStatus,

    -- * Types

    -- ** AccessPolicyDetail
    accessPolicyDetail_createdDate,
    accessPolicyDetail_description,
    accessPolicyDetail_lastModifiedDate,
    accessPolicyDetail_name,
    accessPolicyDetail_policy,
    accessPolicyDetail_policyVersion,
    accessPolicyDetail_type,

    -- ** AccessPolicyStats
    accessPolicyStats_dataPolicyCount,

    -- ** AccessPolicySummary
    accessPolicySummary_createdDate,
    accessPolicySummary_description,
    accessPolicySummary_lastModifiedDate,
    accessPolicySummary_name,
    accessPolicySummary_policyVersion,
    accessPolicySummary_type,

    -- ** AccountSettingsDetail
    accountSettingsDetail_capacityLimits,

    -- ** CapacityLimits
    capacityLimits_maxIndexingCapacityInOCU,
    capacityLimits_maxSearchCapacityInOCU,

    -- ** CollectionDetail
    collectionDetail_arn,
    collectionDetail_collectionEndpoint,
    collectionDetail_createdDate,
    collectionDetail_dashboardEndpoint,
    collectionDetail_description,
    collectionDetail_id,
    collectionDetail_kmsKeyArn,
    collectionDetail_lastModifiedDate,
    collectionDetail_name,
    collectionDetail_status,
    collectionDetail_type,

    -- ** CollectionErrorDetail
    collectionErrorDetail_errorCode,
    collectionErrorDetail_errorMessage,
    collectionErrorDetail_id,
    collectionErrorDetail_name,

    -- ** CollectionFilters
    collectionFilters_name,
    collectionFilters_status,

    -- ** CollectionSummary
    collectionSummary_arn,
    collectionSummary_id,
    collectionSummary_name,
    collectionSummary_status,

    -- ** CreateCollectionDetail
    createCollectionDetail_arn,
    createCollectionDetail_createdDate,
    createCollectionDetail_description,
    createCollectionDetail_id,
    createCollectionDetail_kmsKeyArn,
    createCollectionDetail_lastModifiedDate,
    createCollectionDetail_name,
    createCollectionDetail_status,
    createCollectionDetail_type,

    -- ** CreateVpcEndpointDetail
    createVpcEndpointDetail_id,
    createVpcEndpointDetail_name,
    createVpcEndpointDetail_status,

    -- ** DeleteCollectionDetail
    deleteCollectionDetail_id,
    deleteCollectionDetail_name,
    deleteCollectionDetail_status,

    -- ** DeleteVpcEndpointDetail
    deleteVpcEndpointDetail_id,
    deleteVpcEndpointDetail_name,
    deleteVpcEndpointDetail_status,

    -- ** Document

    -- ** SamlConfigOptions
    samlConfigOptions_groupAttribute,
    samlConfigOptions_sessionTimeout,
    samlConfigOptions_userAttribute,
    samlConfigOptions_metadata,

    -- ** SecurityConfigDetail
    securityConfigDetail_configVersion,
    securityConfigDetail_createdDate,
    securityConfigDetail_description,
    securityConfigDetail_id,
    securityConfigDetail_lastModifiedDate,
    securityConfigDetail_samlOptions,
    securityConfigDetail_type,

    -- ** SecurityConfigStats
    securityConfigStats_samlConfigCount,

    -- ** SecurityConfigSummary
    securityConfigSummary_configVersion,
    securityConfigSummary_createdDate,
    securityConfigSummary_description,
    securityConfigSummary_id,
    securityConfigSummary_lastModifiedDate,
    securityConfigSummary_type,

    -- ** SecurityPolicyDetail
    securityPolicyDetail_createdDate,
    securityPolicyDetail_description,
    securityPolicyDetail_lastModifiedDate,
    securityPolicyDetail_name,
    securityPolicyDetail_policy,
    securityPolicyDetail_policyVersion,
    securityPolicyDetail_type,

    -- ** SecurityPolicyStats
    securityPolicyStats_encryptionPolicyCount,
    securityPolicyStats_networkPolicyCount,

    -- ** SecurityPolicySummary
    securityPolicySummary_createdDate,
    securityPolicySummary_description,
    securityPolicySummary_lastModifiedDate,
    securityPolicySummary_name,
    securityPolicySummary_policyVersion,
    securityPolicySummary_type,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UpdateCollectionDetail
    updateCollectionDetail_arn,
    updateCollectionDetail_createdDate,
    updateCollectionDetail_description,
    updateCollectionDetail_id,
    updateCollectionDetail_lastModifiedDate,
    updateCollectionDetail_name,
    updateCollectionDetail_status,
    updateCollectionDetail_type,

    -- ** UpdateVpcEndpointDetail
    updateVpcEndpointDetail_id,
    updateVpcEndpointDetail_lastModifiedDate,
    updateVpcEndpointDetail_name,
    updateVpcEndpointDetail_securityGroupIds,
    updateVpcEndpointDetail_status,
    updateVpcEndpointDetail_subnetIds,

    -- ** VpcEndpointDetail
    vpcEndpointDetail_createdDate,
    vpcEndpointDetail_id,
    vpcEndpointDetail_name,
    vpcEndpointDetail_securityGroupIds,
    vpcEndpointDetail_status,
    vpcEndpointDetail_subnetIds,
    vpcEndpointDetail_vpcId,

    -- ** VpcEndpointErrorDetail
    vpcEndpointErrorDetail_errorCode,
    vpcEndpointErrorDetail_errorMessage,
    vpcEndpointErrorDetail_id,

    -- ** VpcEndpointFilters
    vpcEndpointFilters_status,

    -- ** VpcEndpointSummary
    vpcEndpointSummary_id,
    vpcEndpointSummary_name,
    vpcEndpointSummary_status,
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
import Amazonka.OpenSearchServerless.ListAccessPolicies
import Amazonka.OpenSearchServerless.ListCollections
import Amazonka.OpenSearchServerless.ListSecurityConfigs
import Amazonka.OpenSearchServerless.ListSecurityPolicies
import Amazonka.OpenSearchServerless.ListTagsForResource
import Amazonka.OpenSearchServerless.ListVpcEndpoints
import Amazonka.OpenSearchServerless.TagResource
import Amazonka.OpenSearchServerless.Types.AccessPolicyDetail
import Amazonka.OpenSearchServerless.Types.AccessPolicyStats
import Amazonka.OpenSearchServerless.Types.AccessPolicySummary
import Amazonka.OpenSearchServerless.Types.AccountSettingsDetail
import Amazonka.OpenSearchServerless.Types.CapacityLimits
import Amazonka.OpenSearchServerless.Types.CollectionDetail
import Amazonka.OpenSearchServerless.Types.CollectionErrorDetail
import Amazonka.OpenSearchServerless.Types.CollectionFilters
import Amazonka.OpenSearchServerless.Types.CollectionSummary
import Amazonka.OpenSearchServerless.Types.CreateCollectionDetail
import Amazonka.OpenSearchServerless.Types.CreateVpcEndpointDetail
import Amazonka.OpenSearchServerless.Types.DeleteCollectionDetail
import Amazonka.OpenSearchServerless.Types.DeleteVpcEndpointDetail
import Amazonka.OpenSearchServerless.Types.Document
import Amazonka.OpenSearchServerless.Types.SamlConfigOptions
import Amazonka.OpenSearchServerless.Types.SecurityConfigDetail
import Amazonka.OpenSearchServerless.Types.SecurityConfigStats
import Amazonka.OpenSearchServerless.Types.SecurityConfigSummary
import Amazonka.OpenSearchServerless.Types.SecurityPolicyDetail
import Amazonka.OpenSearchServerless.Types.SecurityPolicyStats
import Amazonka.OpenSearchServerless.Types.SecurityPolicySummary
import Amazonka.OpenSearchServerless.Types.Tag
import Amazonka.OpenSearchServerless.Types.UpdateCollectionDetail
import Amazonka.OpenSearchServerless.Types.UpdateVpcEndpointDetail
import Amazonka.OpenSearchServerless.Types.VpcEndpointDetail
import Amazonka.OpenSearchServerless.Types.VpcEndpointErrorDetail
import Amazonka.OpenSearchServerless.Types.VpcEndpointFilters
import Amazonka.OpenSearchServerless.Types.VpcEndpointSummary
import Amazonka.OpenSearchServerless.UntagResource
import Amazonka.OpenSearchServerless.UpdateAccessPolicy
import Amazonka.OpenSearchServerless.UpdateAccountSettings
import Amazonka.OpenSearchServerless.UpdateCollection
import Amazonka.OpenSearchServerless.UpdateSecurityConfig
import Amazonka.OpenSearchServerless.UpdateSecurityPolicy
import Amazonka.OpenSearchServerless.UpdateVpcEndpoint
