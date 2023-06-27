{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.TNB.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * DescriptorContentType
    DescriptorContentType (..),

    -- * LcmOperationType
    LcmOperationType (..),

    -- * NsLcmOperationState
    NsLcmOperationState (..),

    -- * NsState
    NsState (..),

    -- * NsdOnboardingState
    NsdOnboardingState (..),

    -- * NsdOperationalState
    NsdOperationalState (..),

    -- * NsdUsageState
    NsdUsageState (..),

    -- * OnboardingState
    OnboardingState (..),

    -- * OperationalState
    OperationalState (..),

    -- * PackageContentType
    PackageContentType (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * UpdateSolNetworkType
    UpdateSolNetworkType (..),

    -- * UsageState
    UsageState (..),

    -- * VnfInstantiationState
    VnfInstantiationState (..),

    -- * VnfOperationalState
    VnfOperationalState (..),

    -- * Document
    Document (..),
    newDocument,

    -- * ErrorInfo
    ErrorInfo (..),
    newErrorInfo,
    errorInfo_cause,
    errorInfo_details,

    -- * FunctionArtifactMeta
    FunctionArtifactMeta (..),
    newFunctionArtifactMeta,
    functionArtifactMeta_overrides,

    -- * GetSolFunctionInstanceMetadata
    GetSolFunctionInstanceMetadata (..),
    newGetSolFunctionInstanceMetadata,
    getSolFunctionInstanceMetadata_createdAt,
    getSolFunctionInstanceMetadata_lastModified,

    -- * GetSolFunctionPackageMetadata
    GetSolFunctionPackageMetadata (..),
    newGetSolFunctionPackageMetadata,
    getSolFunctionPackageMetadata_vnfd,
    getSolFunctionPackageMetadata_createdAt,
    getSolFunctionPackageMetadata_lastModified,

    -- * GetSolInstantiatedVnfInfo
    GetSolInstantiatedVnfInfo (..),
    newGetSolInstantiatedVnfInfo,
    getSolInstantiatedVnfInfo_vnfState,

    -- * GetSolNetworkInstanceMetadata
    GetSolNetworkInstanceMetadata (..),
    newGetSolNetworkInstanceMetadata,
    getSolNetworkInstanceMetadata_createdAt,
    getSolNetworkInstanceMetadata_lastModified,

    -- * GetSolNetworkOperationMetadata
    GetSolNetworkOperationMetadata (..),
    newGetSolNetworkOperationMetadata,
    getSolNetworkOperationMetadata_createdAt,
    getSolNetworkOperationMetadata_lastModified,

    -- * GetSolNetworkOperationTaskDetails
    GetSolNetworkOperationTaskDetails (..),
    newGetSolNetworkOperationTaskDetails,
    getSolNetworkOperationTaskDetails_taskContext,
    getSolNetworkOperationTaskDetails_taskEndTime,
    getSolNetworkOperationTaskDetails_taskErrorDetails,
    getSolNetworkOperationTaskDetails_taskName,
    getSolNetworkOperationTaskDetails_taskStartTime,
    getSolNetworkOperationTaskDetails_taskStatus,

    -- * GetSolNetworkPackageMetadata
    GetSolNetworkPackageMetadata (..),
    newGetSolNetworkPackageMetadata,
    getSolNetworkPackageMetadata_nsd,
    getSolNetworkPackageMetadata_createdAt,
    getSolNetworkPackageMetadata_lastModified,

    -- * GetSolVnfInfo
    GetSolVnfInfo (..),
    newGetSolVnfInfo,
    getSolVnfInfo_vnfState,
    getSolVnfInfo_vnfcResourceInfo,

    -- * GetSolVnfcResourceInfo
    GetSolVnfcResourceInfo (..),
    newGetSolVnfcResourceInfo,
    getSolVnfcResourceInfo_metadata,

    -- * GetSolVnfcResourceInfoMetadata
    GetSolVnfcResourceInfoMetadata (..),
    newGetSolVnfcResourceInfoMetadata,
    getSolVnfcResourceInfoMetadata_cluster,
    getSolVnfcResourceInfoMetadata_helmChart,
    getSolVnfcResourceInfoMetadata_nodeGroup,

    -- * LcmOperationInfo
    LcmOperationInfo (..),
    newLcmOperationInfo,
    lcmOperationInfo_nsLcmOpOccId,

    -- * ListSolFunctionInstanceInfo
    ListSolFunctionInstanceInfo (..),
    newListSolFunctionInstanceInfo,
    listSolFunctionInstanceInfo_instantiatedVnfInfo,
    listSolFunctionInstanceInfo_vnfPkgName,
    listSolFunctionInstanceInfo_arn,
    listSolFunctionInstanceInfo_id,
    listSolFunctionInstanceInfo_instantiationState,
    listSolFunctionInstanceInfo_metadata,
    listSolFunctionInstanceInfo_nsInstanceId,
    listSolFunctionInstanceInfo_vnfPkgId,

    -- * ListSolFunctionInstanceMetadata
    ListSolFunctionInstanceMetadata (..),
    newListSolFunctionInstanceMetadata,
    listSolFunctionInstanceMetadata_createdAt,
    listSolFunctionInstanceMetadata_lastModified,

    -- * ListSolFunctionPackageInfo
    ListSolFunctionPackageInfo (..),
    newListSolFunctionPackageInfo,
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

    -- * ListSolFunctionPackageMetadata
    ListSolFunctionPackageMetadata (..),
    newListSolFunctionPackageMetadata,
    listSolFunctionPackageMetadata_createdAt,
    listSolFunctionPackageMetadata_lastModified,

    -- * ListSolNetworkInstanceInfo
    ListSolNetworkInstanceInfo (..),
    newListSolNetworkInstanceInfo,
    listSolNetworkInstanceInfo_arn,
    listSolNetworkInstanceInfo_id,
    listSolNetworkInstanceInfo_metadata,
    listSolNetworkInstanceInfo_nsInstanceDescription,
    listSolNetworkInstanceInfo_nsInstanceName,
    listSolNetworkInstanceInfo_nsState,
    listSolNetworkInstanceInfo_nsdId,
    listSolNetworkInstanceInfo_nsdInfoId,

    -- * ListSolNetworkInstanceMetadata
    ListSolNetworkInstanceMetadata (..),
    newListSolNetworkInstanceMetadata,
    listSolNetworkInstanceMetadata_createdAt,
    listSolNetworkInstanceMetadata_lastModified,

    -- * ListSolNetworkOperationsInfo
    ListSolNetworkOperationsInfo (..),
    newListSolNetworkOperationsInfo,
    listSolNetworkOperationsInfo_error,
    listSolNetworkOperationsInfo_metadata,
    listSolNetworkOperationsInfo_arn,
    listSolNetworkOperationsInfo_id,
    listSolNetworkOperationsInfo_lcmOperationType,
    listSolNetworkOperationsInfo_nsInstanceId,
    listSolNetworkOperationsInfo_operationState,

    -- * ListSolNetworkOperationsMetadata
    ListSolNetworkOperationsMetadata (..),
    newListSolNetworkOperationsMetadata,
    listSolNetworkOperationsMetadata_createdAt,
    listSolNetworkOperationsMetadata_lastModified,

    -- * ListSolNetworkPackageInfo
    ListSolNetworkPackageInfo (..),
    newListSolNetworkPackageInfo,
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

    -- * ListSolNetworkPackageMetadata
    ListSolNetworkPackageMetadata (..),
    newListSolNetworkPackageMetadata,
    listSolNetworkPackageMetadata_createdAt,
    listSolNetworkPackageMetadata_lastModified,

    -- * NetworkArtifactMeta
    NetworkArtifactMeta (..),
    newNetworkArtifactMeta,
    networkArtifactMeta_overrides,

    -- * ProblemDetails
    ProblemDetails (..),
    newProblemDetails,
    problemDetails_title,
    problemDetails_detail,

    -- * PutSolFunctionPackageContentMetadata
    PutSolFunctionPackageContentMetadata (..),
    newPutSolFunctionPackageContentMetadata,
    putSolFunctionPackageContentMetadata_vnfd,

    -- * PutSolNetworkPackageContentMetadata
    PutSolNetworkPackageContentMetadata (..),
    newPutSolNetworkPackageContentMetadata,
    putSolNetworkPackageContentMetadata_nsd,

    -- * ToscaOverride
    ToscaOverride (..),
    newToscaOverride,
    toscaOverride_defaultValue,
    toscaOverride_name,

    -- * UpdateSolNetworkModify
    UpdateSolNetworkModify (..),
    newUpdateSolNetworkModify,
    updateSolNetworkModify_vnfConfigurableProperties,
    updateSolNetworkModify_vnfInstanceId,

    -- * ValidateSolFunctionPackageContentMetadata
    ValidateSolFunctionPackageContentMetadata (..),
    newValidateSolFunctionPackageContentMetadata,
    validateSolFunctionPackageContentMetadata_vnfd,

    -- * ValidateSolNetworkPackageContentMetadata
    ValidateSolNetworkPackageContentMetadata (..),
    newValidateSolNetworkPackageContentMetadata,
    validateSolNetworkPackageContentMetadata_nsd,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.TNB.Types.DescriptorContentType
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
import Amazonka.TNB.Types.LcmOperationType
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
import Amazonka.TNB.Types.NsLcmOperationState
import Amazonka.TNB.Types.NsState
import Amazonka.TNB.Types.NsdOnboardingState
import Amazonka.TNB.Types.NsdOperationalState
import Amazonka.TNB.Types.NsdUsageState
import Amazonka.TNB.Types.OnboardingState
import Amazonka.TNB.Types.OperationalState
import Amazonka.TNB.Types.PackageContentType
import Amazonka.TNB.Types.ProblemDetails
import Amazonka.TNB.Types.PutSolFunctionPackageContentMetadata
import Amazonka.TNB.Types.PutSolNetworkPackageContentMetadata
import Amazonka.TNB.Types.TaskStatus
import Amazonka.TNB.Types.ToscaOverride
import Amazonka.TNB.Types.UpdateSolNetworkModify
import Amazonka.TNB.Types.UpdateSolNetworkType
import Amazonka.TNB.Types.UsageState
import Amazonka.TNB.Types.ValidateSolFunctionPackageContentMetadata
import Amazonka.TNB.Types.ValidateSolNetworkPackageContentMetadata
import Amazonka.TNB.Types.VnfInstantiationState
import Amazonka.TNB.Types.VnfOperationalState

-- | API version @2008-10-21@ of the Amazon Telco Network Builder SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "TNB",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "tnb",
      Core.signingName = "tnb",
      Core.version = "2008-10-21",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "TNB",
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

-- | Insufficient permissions to make request.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Unexpected error occurred. Problem on the server.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Request references a resource that doesn\'t exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Service quotas have been exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Exception caused by throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Unable to process the request because the client provided input failed
-- to satisfy request constraints.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
