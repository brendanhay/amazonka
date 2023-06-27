{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.TNB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2008-10-21@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Telco Network Builder (TNB) is a network automation
-- service that helps you deploy and manage telecom networks. AWS TNB helps
-- you with the lifecycle management of your telecommunication network
-- functions throughout planning, deployment, and post-deployment
-- activities.
module Amazonka.TNB
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelSolNetworkOperation
    CancelSolNetworkOperation (CancelSolNetworkOperation'),
    newCancelSolNetworkOperation,
    CancelSolNetworkOperationResponse (CancelSolNetworkOperationResponse'),
    newCancelSolNetworkOperationResponse,

    -- ** CreateSolFunctionPackage
    CreateSolFunctionPackage (CreateSolFunctionPackage'),
    newCreateSolFunctionPackage,
    CreateSolFunctionPackageResponse (CreateSolFunctionPackageResponse'),
    newCreateSolFunctionPackageResponse,

    -- ** CreateSolNetworkInstance
    CreateSolNetworkInstance (CreateSolNetworkInstance'),
    newCreateSolNetworkInstance,
    CreateSolNetworkInstanceResponse (CreateSolNetworkInstanceResponse'),
    newCreateSolNetworkInstanceResponse,

    -- ** CreateSolNetworkPackage
    CreateSolNetworkPackage (CreateSolNetworkPackage'),
    newCreateSolNetworkPackage,
    CreateSolNetworkPackageResponse (CreateSolNetworkPackageResponse'),
    newCreateSolNetworkPackageResponse,

    -- ** DeleteSolFunctionPackage
    DeleteSolFunctionPackage (DeleteSolFunctionPackage'),
    newDeleteSolFunctionPackage,
    DeleteSolFunctionPackageResponse (DeleteSolFunctionPackageResponse'),
    newDeleteSolFunctionPackageResponse,

    -- ** DeleteSolNetworkInstance
    DeleteSolNetworkInstance (DeleteSolNetworkInstance'),
    newDeleteSolNetworkInstance,
    DeleteSolNetworkInstanceResponse (DeleteSolNetworkInstanceResponse'),
    newDeleteSolNetworkInstanceResponse,

    -- ** DeleteSolNetworkPackage
    DeleteSolNetworkPackage (DeleteSolNetworkPackage'),
    newDeleteSolNetworkPackage,
    DeleteSolNetworkPackageResponse (DeleteSolNetworkPackageResponse'),
    newDeleteSolNetworkPackageResponse,

    -- ** GetSolFunctionInstance
    GetSolFunctionInstance (GetSolFunctionInstance'),
    newGetSolFunctionInstance,
    GetSolFunctionInstanceResponse (GetSolFunctionInstanceResponse'),
    newGetSolFunctionInstanceResponse,

    -- ** GetSolFunctionPackage
    GetSolFunctionPackage (GetSolFunctionPackage'),
    newGetSolFunctionPackage,
    GetSolFunctionPackageResponse (GetSolFunctionPackageResponse'),
    newGetSolFunctionPackageResponse,

    -- ** GetSolFunctionPackageContent
    GetSolFunctionPackageContent (GetSolFunctionPackageContent'),
    newGetSolFunctionPackageContent,
    GetSolFunctionPackageContentResponse (GetSolFunctionPackageContentResponse'),
    newGetSolFunctionPackageContentResponse,

    -- ** GetSolFunctionPackageDescriptor
    GetSolFunctionPackageDescriptor (GetSolFunctionPackageDescriptor'),
    newGetSolFunctionPackageDescriptor,
    GetSolFunctionPackageDescriptorResponse (GetSolFunctionPackageDescriptorResponse'),
    newGetSolFunctionPackageDescriptorResponse,

    -- ** GetSolNetworkInstance
    GetSolNetworkInstance (GetSolNetworkInstance'),
    newGetSolNetworkInstance,
    GetSolNetworkInstanceResponse (GetSolNetworkInstanceResponse'),
    newGetSolNetworkInstanceResponse,

    -- ** GetSolNetworkOperation
    GetSolNetworkOperation (GetSolNetworkOperation'),
    newGetSolNetworkOperation,
    GetSolNetworkOperationResponse (GetSolNetworkOperationResponse'),
    newGetSolNetworkOperationResponse,

    -- ** GetSolNetworkPackage
    GetSolNetworkPackage (GetSolNetworkPackage'),
    newGetSolNetworkPackage,
    GetSolNetworkPackageResponse (GetSolNetworkPackageResponse'),
    newGetSolNetworkPackageResponse,

    -- ** GetSolNetworkPackageContent
    GetSolNetworkPackageContent (GetSolNetworkPackageContent'),
    newGetSolNetworkPackageContent,
    GetSolNetworkPackageContentResponse (GetSolNetworkPackageContentResponse'),
    newGetSolNetworkPackageContentResponse,

    -- ** GetSolNetworkPackageDescriptor
    GetSolNetworkPackageDescriptor (GetSolNetworkPackageDescriptor'),
    newGetSolNetworkPackageDescriptor,
    GetSolNetworkPackageDescriptorResponse (GetSolNetworkPackageDescriptorResponse'),
    newGetSolNetworkPackageDescriptorResponse,

    -- ** InstantiateSolNetworkInstance
    InstantiateSolNetworkInstance (InstantiateSolNetworkInstance'),
    newInstantiateSolNetworkInstance,
    InstantiateSolNetworkInstanceResponse (InstantiateSolNetworkInstanceResponse'),
    newInstantiateSolNetworkInstanceResponse,

    -- ** ListSolFunctionInstances (Paginated)
    ListSolFunctionInstances (ListSolFunctionInstances'),
    newListSolFunctionInstances,
    ListSolFunctionInstancesResponse (ListSolFunctionInstancesResponse'),
    newListSolFunctionInstancesResponse,

    -- ** ListSolFunctionPackages (Paginated)
    ListSolFunctionPackages (ListSolFunctionPackages'),
    newListSolFunctionPackages,
    ListSolFunctionPackagesResponse (ListSolFunctionPackagesResponse'),
    newListSolFunctionPackagesResponse,

    -- ** ListSolNetworkInstances (Paginated)
    ListSolNetworkInstances (ListSolNetworkInstances'),
    newListSolNetworkInstances,
    ListSolNetworkInstancesResponse (ListSolNetworkInstancesResponse'),
    newListSolNetworkInstancesResponse,

    -- ** ListSolNetworkOperations (Paginated)
    ListSolNetworkOperations (ListSolNetworkOperations'),
    newListSolNetworkOperations,
    ListSolNetworkOperationsResponse (ListSolNetworkOperationsResponse'),
    newListSolNetworkOperationsResponse,

    -- ** ListSolNetworkPackages (Paginated)
    ListSolNetworkPackages (ListSolNetworkPackages'),
    newListSolNetworkPackages,
    ListSolNetworkPackagesResponse (ListSolNetworkPackagesResponse'),
    newListSolNetworkPackagesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutSolFunctionPackageContent
    PutSolFunctionPackageContent (PutSolFunctionPackageContent'),
    newPutSolFunctionPackageContent,
    PutSolFunctionPackageContentResponse (PutSolFunctionPackageContentResponse'),
    newPutSolFunctionPackageContentResponse,

    -- ** PutSolNetworkPackageContent
    PutSolNetworkPackageContent (PutSolNetworkPackageContent'),
    newPutSolNetworkPackageContent,
    PutSolNetworkPackageContentResponse (PutSolNetworkPackageContentResponse'),
    newPutSolNetworkPackageContentResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TerminateSolNetworkInstance
    TerminateSolNetworkInstance (TerminateSolNetworkInstance'),
    newTerminateSolNetworkInstance,
    TerminateSolNetworkInstanceResponse (TerminateSolNetworkInstanceResponse'),
    newTerminateSolNetworkInstanceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateSolFunctionPackage
    UpdateSolFunctionPackage (UpdateSolFunctionPackage'),
    newUpdateSolFunctionPackage,
    UpdateSolFunctionPackageResponse (UpdateSolFunctionPackageResponse'),
    newUpdateSolFunctionPackageResponse,

    -- ** UpdateSolNetworkInstance
    UpdateSolNetworkInstance (UpdateSolNetworkInstance'),
    newUpdateSolNetworkInstance,
    UpdateSolNetworkInstanceResponse (UpdateSolNetworkInstanceResponse'),
    newUpdateSolNetworkInstanceResponse,

    -- ** UpdateSolNetworkPackage
    UpdateSolNetworkPackage (UpdateSolNetworkPackage'),
    newUpdateSolNetworkPackage,
    UpdateSolNetworkPackageResponse (UpdateSolNetworkPackageResponse'),
    newUpdateSolNetworkPackageResponse,

    -- ** ValidateSolFunctionPackageContent
    ValidateSolFunctionPackageContent (ValidateSolFunctionPackageContent'),
    newValidateSolFunctionPackageContent,
    ValidateSolFunctionPackageContentResponse (ValidateSolFunctionPackageContentResponse'),
    newValidateSolFunctionPackageContentResponse,

    -- ** ValidateSolNetworkPackageContent
    ValidateSolNetworkPackageContent (ValidateSolNetworkPackageContent'),
    newValidateSolNetworkPackageContent,
    ValidateSolNetworkPackageContentResponse (ValidateSolNetworkPackageContentResponse'),
    newValidateSolNetworkPackageContentResponse,

    -- * Types

    -- ** DescriptorContentType
    DescriptorContentType (..),

    -- ** LcmOperationType
    LcmOperationType (..),

    -- ** NsLcmOperationState
    NsLcmOperationState (..),

    -- ** NsState
    NsState (..),

    -- ** NsdOnboardingState
    NsdOnboardingState (..),

    -- ** NsdOperationalState
    NsdOperationalState (..),

    -- ** NsdUsageState
    NsdUsageState (..),

    -- ** OnboardingState
    OnboardingState (..),

    -- ** OperationalState
    OperationalState (..),

    -- ** PackageContentType
    PackageContentType (..),

    -- ** TaskStatus
    TaskStatus (..),

    -- ** UpdateSolNetworkType
    UpdateSolNetworkType (..),

    -- ** UsageState
    UsageState (..),

    -- ** VnfInstantiationState
    VnfInstantiationState (..),

    -- ** VnfOperationalState
    VnfOperationalState (..),

    -- ** Document
    Document (Document'),
    newDocument,

    -- ** ErrorInfo
    ErrorInfo (ErrorInfo'),
    newErrorInfo,

    -- ** FunctionArtifactMeta
    FunctionArtifactMeta (FunctionArtifactMeta'),
    newFunctionArtifactMeta,

    -- ** GetSolFunctionInstanceMetadata
    GetSolFunctionInstanceMetadata (GetSolFunctionInstanceMetadata'),
    newGetSolFunctionInstanceMetadata,

    -- ** GetSolFunctionPackageMetadata
    GetSolFunctionPackageMetadata (GetSolFunctionPackageMetadata'),
    newGetSolFunctionPackageMetadata,

    -- ** GetSolInstantiatedVnfInfo
    GetSolInstantiatedVnfInfo (GetSolInstantiatedVnfInfo'),
    newGetSolInstantiatedVnfInfo,

    -- ** GetSolNetworkInstanceMetadata
    GetSolNetworkInstanceMetadata (GetSolNetworkInstanceMetadata'),
    newGetSolNetworkInstanceMetadata,

    -- ** GetSolNetworkOperationMetadata
    GetSolNetworkOperationMetadata (GetSolNetworkOperationMetadata'),
    newGetSolNetworkOperationMetadata,

    -- ** GetSolNetworkOperationTaskDetails
    GetSolNetworkOperationTaskDetails (GetSolNetworkOperationTaskDetails'),
    newGetSolNetworkOperationTaskDetails,

    -- ** GetSolNetworkPackageMetadata
    GetSolNetworkPackageMetadata (GetSolNetworkPackageMetadata'),
    newGetSolNetworkPackageMetadata,

    -- ** GetSolVnfInfo
    GetSolVnfInfo (GetSolVnfInfo'),
    newGetSolVnfInfo,

    -- ** GetSolVnfcResourceInfo
    GetSolVnfcResourceInfo (GetSolVnfcResourceInfo'),
    newGetSolVnfcResourceInfo,

    -- ** GetSolVnfcResourceInfoMetadata
    GetSolVnfcResourceInfoMetadata (GetSolVnfcResourceInfoMetadata'),
    newGetSolVnfcResourceInfoMetadata,

    -- ** LcmOperationInfo
    LcmOperationInfo (LcmOperationInfo'),
    newLcmOperationInfo,

    -- ** ListSolFunctionInstanceInfo
    ListSolFunctionInstanceInfo (ListSolFunctionInstanceInfo'),
    newListSolFunctionInstanceInfo,

    -- ** ListSolFunctionInstanceMetadata
    ListSolFunctionInstanceMetadata (ListSolFunctionInstanceMetadata'),
    newListSolFunctionInstanceMetadata,

    -- ** ListSolFunctionPackageInfo
    ListSolFunctionPackageInfo (ListSolFunctionPackageInfo'),
    newListSolFunctionPackageInfo,

    -- ** ListSolFunctionPackageMetadata
    ListSolFunctionPackageMetadata (ListSolFunctionPackageMetadata'),
    newListSolFunctionPackageMetadata,

    -- ** ListSolNetworkInstanceInfo
    ListSolNetworkInstanceInfo (ListSolNetworkInstanceInfo'),
    newListSolNetworkInstanceInfo,

    -- ** ListSolNetworkInstanceMetadata
    ListSolNetworkInstanceMetadata (ListSolNetworkInstanceMetadata'),
    newListSolNetworkInstanceMetadata,

    -- ** ListSolNetworkOperationsInfo
    ListSolNetworkOperationsInfo (ListSolNetworkOperationsInfo'),
    newListSolNetworkOperationsInfo,

    -- ** ListSolNetworkOperationsMetadata
    ListSolNetworkOperationsMetadata (ListSolNetworkOperationsMetadata'),
    newListSolNetworkOperationsMetadata,

    -- ** ListSolNetworkPackageInfo
    ListSolNetworkPackageInfo (ListSolNetworkPackageInfo'),
    newListSolNetworkPackageInfo,

    -- ** ListSolNetworkPackageMetadata
    ListSolNetworkPackageMetadata (ListSolNetworkPackageMetadata'),
    newListSolNetworkPackageMetadata,

    -- ** NetworkArtifactMeta
    NetworkArtifactMeta (NetworkArtifactMeta'),
    newNetworkArtifactMeta,

    -- ** ProblemDetails
    ProblemDetails (ProblemDetails'),
    newProblemDetails,

    -- ** PutSolFunctionPackageContentMetadata
    PutSolFunctionPackageContentMetadata (PutSolFunctionPackageContentMetadata'),
    newPutSolFunctionPackageContentMetadata,

    -- ** PutSolNetworkPackageContentMetadata
    PutSolNetworkPackageContentMetadata (PutSolNetworkPackageContentMetadata'),
    newPutSolNetworkPackageContentMetadata,

    -- ** ToscaOverride
    ToscaOverride (ToscaOverride'),
    newToscaOverride,

    -- ** UpdateSolNetworkModify
    UpdateSolNetworkModify (UpdateSolNetworkModify'),
    newUpdateSolNetworkModify,

    -- ** ValidateSolFunctionPackageContentMetadata
    ValidateSolFunctionPackageContentMetadata (ValidateSolFunctionPackageContentMetadata'),
    newValidateSolFunctionPackageContentMetadata,

    -- ** ValidateSolNetworkPackageContentMetadata
    ValidateSolNetworkPackageContentMetadata (ValidateSolNetworkPackageContentMetadata'),
    newValidateSolNetworkPackageContentMetadata,
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
import Amazonka.TNB.Lens
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
import Amazonka.TNB.Types
import Amazonka.TNB.UntagResource
import Amazonka.TNB.UpdateSolFunctionPackage
import Amazonka.TNB.UpdateSolNetworkInstance
import Amazonka.TNB.UpdateSolNetworkPackage
import Amazonka.TNB.ValidateSolFunctionPackageContent
import Amazonka.TNB.ValidateSolNetworkPackageContent
import Amazonka.TNB.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'TNB'.

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
