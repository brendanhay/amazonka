{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.TNB.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Lens
  ( -- * Operations

    -- ** CancelSolNetworkOperation
    cancelSolNetworkOperation_nsLcmOpOccId,

    -- ** CreateSolFunctionPackage
    createSolFunctionPackage_tags,
    createSolFunctionPackageResponse_tags,
    createSolFunctionPackageResponse_httpStatus,
    createSolFunctionPackageResponse_arn,
    createSolFunctionPackageResponse_id,
    createSolFunctionPackageResponse_onboardingState,
    createSolFunctionPackageResponse_operationalState,
    createSolFunctionPackageResponse_usageState,

    -- ** CreateSolNetworkInstance
    createSolNetworkInstance_nsDescription,
    createSolNetworkInstance_tags,
    createSolNetworkInstance_nsName,
    createSolNetworkInstance_nsdInfoId,
    createSolNetworkInstanceResponse_tags,
    createSolNetworkInstanceResponse_httpStatus,
    createSolNetworkInstanceResponse_arn,
    createSolNetworkInstanceResponse_id,
    createSolNetworkInstanceResponse_nsInstanceName,
    createSolNetworkInstanceResponse_nsdInfoId,

    -- ** CreateSolNetworkPackage
    createSolNetworkPackage_tags,
    createSolNetworkPackageResponse_tags,
    createSolNetworkPackageResponse_httpStatus,
    createSolNetworkPackageResponse_arn,
    createSolNetworkPackageResponse_id,
    createSolNetworkPackageResponse_nsdOnboardingState,
    createSolNetworkPackageResponse_nsdOperationalState,
    createSolNetworkPackageResponse_nsdUsageState,

    -- ** DeleteSolFunctionPackage
    deleteSolFunctionPackage_vnfPkgId,

    -- ** DeleteSolNetworkInstance
    deleteSolNetworkInstance_nsInstanceId,

    -- ** DeleteSolNetworkPackage
    deleteSolNetworkPackage_nsdInfoId,

    -- ** GetSolFunctionInstance
    getSolFunctionInstance_vnfInstanceId,
    getSolFunctionInstanceResponse_instantiatedVnfInfo,
    getSolFunctionInstanceResponse_tags,
    getSolFunctionInstanceResponse_vnfProductName,
    getSolFunctionInstanceResponse_vnfProvider,
    getSolFunctionInstanceResponse_vnfdVersion,
    getSolFunctionInstanceResponse_httpStatus,
    getSolFunctionInstanceResponse_arn,
    getSolFunctionInstanceResponse_id,
    getSolFunctionInstanceResponse_instantiationState,
    getSolFunctionInstanceResponse_metadata,
    getSolFunctionInstanceResponse_nsInstanceId,
    getSolFunctionInstanceResponse_vnfPkgId,
    getSolFunctionInstanceResponse_vnfdId,

    -- ** GetSolFunctionPackage
    getSolFunctionPackage_vnfPkgId,
    getSolFunctionPackageResponse_metadata,
    getSolFunctionPackageResponse_tags,
    getSolFunctionPackageResponse_vnfProductName,
    getSolFunctionPackageResponse_vnfProvider,
    getSolFunctionPackageResponse_vnfdId,
    getSolFunctionPackageResponse_vnfdVersion,
    getSolFunctionPackageResponse_httpStatus,
    getSolFunctionPackageResponse_arn,
    getSolFunctionPackageResponse_id,
    getSolFunctionPackageResponse_onboardingState,
    getSolFunctionPackageResponse_operationalState,
    getSolFunctionPackageResponse_usageState,

    -- ** GetSolFunctionPackageContent
    getSolFunctionPackageContent_accept,
    getSolFunctionPackageContent_vnfPkgId,
    getSolFunctionPackageContentResponse_contentType,
    getSolFunctionPackageContentResponse_packageContent,
    getSolFunctionPackageContentResponse_httpStatus,

    -- ** GetSolFunctionPackageDescriptor
    getSolFunctionPackageDescriptor_accept,
    getSolFunctionPackageDescriptor_vnfPkgId,
    getSolFunctionPackageDescriptorResponse_contentType,
    getSolFunctionPackageDescriptorResponse_vnfd,
    getSolFunctionPackageDescriptorResponse_httpStatus,

    -- ** GetSolNetworkInstance
    getSolNetworkInstance_nsInstanceId,
    getSolNetworkInstanceResponse_lcmOpInfo,
    getSolNetworkInstanceResponse_nsState,
    getSolNetworkInstanceResponse_tags,
    getSolNetworkInstanceResponse_httpStatus,
    getSolNetworkInstanceResponse_arn,
    getSolNetworkInstanceResponse_id,
    getSolNetworkInstanceResponse_metadata,
    getSolNetworkInstanceResponse_nsInstanceDescription,
    getSolNetworkInstanceResponse_nsInstanceName,
    getSolNetworkInstanceResponse_nsdId,
    getSolNetworkInstanceResponse_nsdInfoId,

    -- ** GetSolNetworkOperation
    getSolNetworkOperation_nsLcmOpOccId,
    getSolNetworkOperationResponse_error,
    getSolNetworkOperationResponse_id,
    getSolNetworkOperationResponse_lcmOperationType,
    getSolNetworkOperationResponse_metadata,
    getSolNetworkOperationResponse_nsInstanceId,
    getSolNetworkOperationResponse_operationState,
    getSolNetworkOperationResponse_tags,
    getSolNetworkOperationResponse_tasks,
    getSolNetworkOperationResponse_httpStatus,
    getSolNetworkOperationResponse_arn,

    -- ** GetSolNetworkPackage
    getSolNetworkPackage_nsdInfoId,
    getSolNetworkPackageResponse_tags,
    getSolNetworkPackageResponse_httpStatus,
    getSolNetworkPackageResponse_arn,
    getSolNetworkPackageResponse_id,
    getSolNetworkPackageResponse_metadata,
    getSolNetworkPackageResponse_nsdId,
    getSolNetworkPackageResponse_nsdName,
    getSolNetworkPackageResponse_nsdOnboardingState,
    getSolNetworkPackageResponse_nsdOperationalState,
    getSolNetworkPackageResponse_nsdUsageState,
    getSolNetworkPackageResponse_nsdVersion,
    getSolNetworkPackageResponse_vnfPkgIds,

    -- ** GetSolNetworkPackageContent
    getSolNetworkPackageContent_accept,
    getSolNetworkPackageContent_nsdInfoId,
    getSolNetworkPackageContentResponse_contentType,
    getSolNetworkPackageContentResponse_nsdContent,
    getSolNetworkPackageContentResponse_httpStatus,

    -- ** GetSolNetworkPackageDescriptor
    getSolNetworkPackageDescriptor_nsdInfoId,
    getSolNetworkPackageDescriptorResponse_contentType,
    getSolNetworkPackageDescriptorResponse_nsd,
    getSolNetworkPackageDescriptorResponse_httpStatus,

    -- ** InstantiateSolNetworkInstance
    instantiateSolNetworkInstance_additionalParamsForNs,
    instantiateSolNetworkInstance_dryRun,
    instantiateSolNetworkInstance_tags,
    instantiateSolNetworkInstance_nsInstanceId,
    instantiateSolNetworkInstanceResponse_tags,
    instantiateSolNetworkInstanceResponse_httpStatus,
    instantiateSolNetworkInstanceResponse_nsLcmOpOccId,

    -- ** ListSolFunctionInstances
    listSolFunctionInstances_maxResults,
    listSolFunctionInstances_nextToken,
    listSolFunctionInstancesResponse_functionInstances,
    listSolFunctionInstancesResponse_nextToken,
    listSolFunctionInstancesResponse_httpStatus,

    -- ** ListSolFunctionPackages
    listSolFunctionPackages_maxResults,
    listSolFunctionPackages_nextToken,
    listSolFunctionPackagesResponse_nextToken,
    listSolFunctionPackagesResponse_httpStatus,
    listSolFunctionPackagesResponse_functionPackages,

    -- ** ListSolNetworkInstances
    listSolNetworkInstances_maxResults,
    listSolNetworkInstances_nextToken,
    listSolNetworkInstancesResponse_networkInstances,
    listSolNetworkInstancesResponse_nextToken,
    listSolNetworkInstancesResponse_httpStatus,

    -- ** ListSolNetworkOperations
    listSolNetworkOperations_maxResults,
    listSolNetworkOperations_nextToken,
    listSolNetworkOperationsResponse_networkOperations,
    listSolNetworkOperationsResponse_nextToken,
    listSolNetworkOperationsResponse_httpStatus,

    -- ** ListSolNetworkPackages
    listSolNetworkPackages_maxResults,
    listSolNetworkPackages_nextToken,
    listSolNetworkPackagesResponse_nextToken,
    listSolNetworkPackagesResponse_httpStatus,
    listSolNetworkPackagesResponse_networkPackages,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** PutSolFunctionPackageContent
    putSolFunctionPackageContent_contentType,
    putSolFunctionPackageContent_file,
    putSolFunctionPackageContent_vnfPkgId,
    putSolFunctionPackageContentResponse_httpStatus,
    putSolFunctionPackageContentResponse_id,
    putSolFunctionPackageContentResponse_metadata,
    putSolFunctionPackageContentResponse_vnfProductName,
    putSolFunctionPackageContentResponse_vnfProvider,
    putSolFunctionPackageContentResponse_vnfdId,
    putSolFunctionPackageContentResponse_vnfdVersion,

    -- ** PutSolNetworkPackageContent
    putSolNetworkPackageContent_contentType,
    putSolNetworkPackageContent_file,
    putSolNetworkPackageContent_nsdInfoId,
    putSolNetworkPackageContentResponse_httpStatus,
    putSolNetworkPackageContentResponse_arn,
    putSolNetworkPackageContentResponse_id,
    putSolNetworkPackageContentResponse_metadata,
    putSolNetworkPackageContentResponse_nsdId,
    putSolNetworkPackageContentResponse_nsdName,
    putSolNetworkPackageContentResponse_nsdVersion,
    putSolNetworkPackageContentResponse_vnfPkgIds,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TerminateSolNetworkInstance
    terminateSolNetworkInstance_tags,
    terminateSolNetworkInstance_nsInstanceId,
    terminateSolNetworkInstanceResponse_nsLcmOpOccId,
    terminateSolNetworkInstanceResponse_tags,
    terminateSolNetworkInstanceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateSolFunctionPackage
    updateSolFunctionPackage_operationalState,
    updateSolFunctionPackage_vnfPkgId,
    updateSolFunctionPackageResponse_httpStatus,
    updateSolFunctionPackageResponse_operationalState,

    -- ** UpdateSolNetworkInstance
    updateSolNetworkInstance_modifyVnfInfoData,
    updateSolNetworkInstance_tags,
    updateSolNetworkInstance_nsInstanceId,
    updateSolNetworkInstance_updateType,
    updateSolNetworkInstanceResponse_nsLcmOpOccId,
    updateSolNetworkInstanceResponse_tags,
    updateSolNetworkInstanceResponse_httpStatus,

    -- ** UpdateSolNetworkPackage
    updateSolNetworkPackage_nsdInfoId,
    updateSolNetworkPackage_nsdOperationalState,
    updateSolNetworkPackageResponse_httpStatus,
    updateSolNetworkPackageResponse_nsdOperationalState,

    -- ** ValidateSolFunctionPackageContent
    validateSolFunctionPackageContent_contentType,
    validateSolFunctionPackageContent_file,
    validateSolFunctionPackageContent_vnfPkgId,
    validateSolFunctionPackageContentResponse_httpStatus,
    validateSolFunctionPackageContentResponse_id,
    validateSolFunctionPackageContentResponse_metadata,
    validateSolFunctionPackageContentResponse_vnfProductName,
    validateSolFunctionPackageContentResponse_vnfProvider,
    validateSolFunctionPackageContentResponse_vnfdId,
    validateSolFunctionPackageContentResponse_vnfdVersion,

    -- ** ValidateSolNetworkPackageContent
    validateSolNetworkPackageContent_contentType,
    validateSolNetworkPackageContent_file,
    validateSolNetworkPackageContent_nsdInfoId,
    validateSolNetworkPackageContentResponse_httpStatus,
    validateSolNetworkPackageContentResponse_arn,
    validateSolNetworkPackageContentResponse_id,
    validateSolNetworkPackageContentResponse_metadata,
    validateSolNetworkPackageContentResponse_nsdId,
    validateSolNetworkPackageContentResponse_nsdName,
    validateSolNetworkPackageContentResponse_nsdVersion,
    validateSolNetworkPackageContentResponse_vnfPkgIds,

    -- * Types

    -- ** Document

    -- ** ErrorInfo
    errorInfo_cause,
    errorInfo_details,

    -- ** FunctionArtifactMeta
    functionArtifactMeta_overrides,

    -- ** GetSolFunctionInstanceMetadata
    getSolFunctionInstanceMetadata_createdAt,
    getSolFunctionInstanceMetadata_lastModified,

    -- ** GetSolFunctionPackageMetadata
    getSolFunctionPackageMetadata_vnfd,
    getSolFunctionPackageMetadata_createdAt,
    getSolFunctionPackageMetadata_lastModified,

    -- ** GetSolInstantiatedVnfInfo
    getSolInstantiatedVnfInfo_vnfState,

    -- ** GetSolNetworkInstanceMetadata
    getSolNetworkInstanceMetadata_createdAt,
    getSolNetworkInstanceMetadata_lastModified,

    -- ** GetSolNetworkOperationMetadata
    getSolNetworkOperationMetadata_createdAt,
    getSolNetworkOperationMetadata_lastModified,

    -- ** GetSolNetworkOperationTaskDetails
    getSolNetworkOperationTaskDetails_taskContext,
    getSolNetworkOperationTaskDetails_taskEndTime,
    getSolNetworkOperationTaskDetails_taskErrorDetails,
    getSolNetworkOperationTaskDetails_taskName,
    getSolNetworkOperationTaskDetails_taskStartTime,
    getSolNetworkOperationTaskDetails_taskStatus,

    -- ** GetSolNetworkPackageMetadata
    getSolNetworkPackageMetadata_nsd,
    getSolNetworkPackageMetadata_createdAt,
    getSolNetworkPackageMetadata_lastModified,

    -- ** GetSolVnfInfo
    getSolVnfInfo_vnfState,
    getSolVnfInfo_vnfcResourceInfo,

    -- ** GetSolVnfcResourceInfo
    getSolVnfcResourceInfo_metadata,

    -- ** GetSolVnfcResourceInfoMetadata
    getSolVnfcResourceInfoMetadata_cluster,
    getSolVnfcResourceInfoMetadata_helmChart,
    getSolVnfcResourceInfoMetadata_nodeGroup,

    -- ** LcmOperationInfo
    lcmOperationInfo_nsLcmOpOccId,

    -- ** ListSolFunctionInstanceInfo
    listSolFunctionInstanceInfo_instantiatedVnfInfo,
    listSolFunctionInstanceInfo_vnfPkgName,
    listSolFunctionInstanceInfo_arn,
    listSolFunctionInstanceInfo_id,
    listSolFunctionInstanceInfo_instantiationState,
    listSolFunctionInstanceInfo_metadata,
    listSolFunctionInstanceInfo_nsInstanceId,
    listSolFunctionInstanceInfo_vnfPkgId,

    -- ** ListSolFunctionInstanceMetadata
    listSolFunctionInstanceMetadata_createdAt,
    listSolFunctionInstanceMetadata_lastModified,

    -- ** ListSolFunctionPackageInfo
    listSolFunctionPackageInfo_metadata,
    listSolFunctionPackageInfo_vnfProductName,
    listSolFunctionPackageInfo_vnfProvider,
    listSolFunctionPackageInfo_vnfdId,
    listSolFunctionPackageInfo_vnfdVersion,
    listSolFunctionPackageInfo_arn,
    listSolFunctionPackageInfo_id,
    listSolFunctionPackageInfo_onboardingState,
    listSolFunctionPackageInfo_operationalState,
    listSolFunctionPackageInfo_usageState,

    -- ** ListSolFunctionPackageMetadata
    listSolFunctionPackageMetadata_createdAt,
    listSolFunctionPackageMetadata_lastModified,

    -- ** ListSolNetworkInstanceInfo
    listSolNetworkInstanceInfo_arn,
    listSolNetworkInstanceInfo_id,
    listSolNetworkInstanceInfo_metadata,
    listSolNetworkInstanceInfo_nsInstanceDescription,
    listSolNetworkInstanceInfo_nsInstanceName,
    listSolNetworkInstanceInfo_nsState,
    listSolNetworkInstanceInfo_nsdId,
    listSolNetworkInstanceInfo_nsdInfoId,

    -- ** ListSolNetworkInstanceMetadata
    listSolNetworkInstanceMetadata_createdAt,
    listSolNetworkInstanceMetadata_lastModified,

    -- ** ListSolNetworkOperationsInfo
    listSolNetworkOperationsInfo_error,
    listSolNetworkOperationsInfo_metadata,
    listSolNetworkOperationsInfo_arn,
    listSolNetworkOperationsInfo_id,
    listSolNetworkOperationsInfo_lcmOperationType,
    listSolNetworkOperationsInfo_nsInstanceId,
    listSolNetworkOperationsInfo_operationState,

    -- ** ListSolNetworkOperationsMetadata
    listSolNetworkOperationsMetadata_createdAt,
    listSolNetworkOperationsMetadata_lastModified,

    -- ** ListSolNetworkPackageInfo
    listSolNetworkPackageInfo_nsdDesigner,
    listSolNetworkPackageInfo_nsdId,
    listSolNetworkPackageInfo_nsdInvariantId,
    listSolNetworkPackageInfo_nsdName,
    listSolNetworkPackageInfo_nsdVersion,
    listSolNetworkPackageInfo_vnfPkgIds,
    listSolNetworkPackageInfo_arn,
    listSolNetworkPackageInfo_id,
    listSolNetworkPackageInfo_metadata,
    listSolNetworkPackageInfo_nsdOnboardingState,
    listSolNetworkPackageInfo_nsdOperationalState,
    listSolNetworkPackageInfo_nsdUsageState,

    -- ** ListSolNetworkPackageMetadata
    listSolNetworkPackageMetadata_createdAt,
    listSolNetworkPackageMetadata_lastModified,

    -- ** NetworkArtifactMeta
    networkArtifactMeta_overrides,

    -- ** ProblemDetails
    problemDetails_title,
    problemDetails_detail,

    -- ** PutSolFunctionPackageContentMetadata
    putSolFunctionPackageContentMetadata_vnfd,

    -- ** PutSolNetworkPackageContentMetadata
    putSolNetworkPackageContentMetadata_nsd,

    -- ** ToscaOverride
    toscaOverride_defaultValue,
    toscaOverride_name,

    -- ** UpdateSolNetworkModify
    updateSolNetworkModify_vnfConfigurableProperties,
    updateSolNetworkModify_vnfInstanceId,

    -- ** ValidateSolFunctionPackageContentMetadata
    validateSolFunctionPackageContentMetadata_vnfd,

    -- ** ValidateSolNetworkPackageContentMetadata
    validateSolNetworkPackageContentMetadata_nsd,
  )
where

import Amazonka.TNB.CancelSolNetworkOperation
import Amazonka.TNB.CreateSolFunctionPackage
import Amazonka.TNB.CreateSolNetworkInstance
import Amazonka.TNB.CreateSolNetworkPackage
import Amazonka.TNB.DeleteSolFunctionPackage
import Amazonka.TNB.DeleteSolNetworkInstance
import Amazonka.TNB.DeleteSolNetworkPackage
import Amazonka.TNB.GetSolFunctionInstance
import Amazonka.TNB.GetSolFunctionPackage
import Amazonka.TNB.GetSolFunctionPackageContent
import Amazonka.TNB.GetSolFunctionPackageDescriptor
import Amazonka.TNB.GetSolNetworkInstance
import Amazonka.TNB.GetSolNetworkOperation
import Amazonka.TNB.GetSolNetworkPackage
import Amazonka.TNB.GetSolNetworkPackageContent
import Amazonka.TNB.GetSolNetworkPackageDescriptor
import Amazonka.TNB.InstantiateSolNetworkInstance
import Amazonka.TNB.ListSolFunctionInstances
import Amazonka.TNB.ListSolFunctionPackages
import Amazonka.TNB.ListSolNetworkInstances
import Amazonka.TNB.ListSolNetworkOperations
import Amazonka.TNB.ListSolNetworkPackages
import Amazonka.TNB.ListTagsForResource
import Amazonka.TNB.PutSolFunctionPackageContent
import Amazonka.TNB.PutSolNetworkPackageContent
import Amazonka.TNB.TagResource
import Amazonka.TNB.TerminateSolNetworkInstance
import Amazonka.TNB.Types.Document
import Amazonka.TNB.Types.ErrorInfo
import Amazonka.TNB.Types.FunctionArtifactMeta
import Amazonka.TNB.Types.GetSolFunctionInstanceMetadata
import Amazonka.TNB.Types.GetSolFunctionPackageMetadata
import Amazonka.TNB.Types.GetSolInstantiatedVnfInfo
import Amazonka.TNB.Types.GetSolNetworkInstanceMetadata
import Amazonka.TNB.Types.GetSolNetworkOperationMetadata
import Amazonka.TNB.Types.GetSolNetworkOperationTaskDetails
import Amazonka.TNB.Types.GetSolNetworkPackageMetadata
import Amazonka.TNB.Types.GetSolVnfInfo
import Amazonka.TNB.Types.GetSolVnfcResourceInfo
import Amazonka.TNB.Types.GetSolVnfcResourceInfoMetadata
import Amazonka.TNB.Types.LcmOperationInfo
import Amazonka.TNB.Types.ListSolFunctionInstanceInfo
import Amazonka.TNB.Types.ListSolFunctionInstanceMetadata
import Amazonka.TNB.Types.ListSolFunctionPackageInfo
import Amazonka.TNB.Types.ListSolFunctionPackageMetadata
import Amazonka.TNB.Types.ListSolNetworkInstanceInfo
import Amazonka.TNB.Types.ListSolNetworkInstanceMetadata
import Amazonka.TNB.Types.ListSolNetworkOperationsInfo
import Amazonka.TNB.Types.ListSolNetworkOperationsMetadata
import Amazonka.TNB.Types.ListSolNetworkPackageInfo
import Amazonka.TNB.Types.ListSolNetworkPackageMetadata
import Amazonka.TNB.Types.NetworkArtifactMeta
import Amazonka.TNB.Types.ProblemDetails
import Amazonka.TNB.Types.PutSolFunctionPackageContentMetadata
import Amazonka.TNB.Types.PutSolNetworkPackageContentMetadata
import Amazonka.TNB.Types.ToscaOverride
import Amazonka.TNB.Types.UpdateSolNetworkModify
import Amazonka.TNB.Types.ValidateSolFunctionPackageContentMetadata
import Amazonka.TNB.Types.ValidateSolNetworkPackageContentMetadata
import Amazonka.TNB.UntagResource
import Amazonka.TNB.UpdateSolFunctionPackage
import Amazonka.TNB.UpdateSolNetworkInstance
import Amazonka.TNB.UpdateSolNetworkPackage
import Amazonka.TNB.ValidateSolFunctionPackageContent
import Amazonka.TNB.ValidateSolNetworkPackageContent
