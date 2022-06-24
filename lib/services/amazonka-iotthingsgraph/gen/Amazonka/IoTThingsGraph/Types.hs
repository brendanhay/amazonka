{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTThingsGraph.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _ResourceNotFoundException,
    _ResourceInUseException,
    _LimitExceededException,
    _ThrottlingException,
    _InvalidRequestException,
    _InternalFailureException,

    -- * DefinitionLanguage
    DefinitionLanguage (..),

    -- * DeploymentTarget
    DeploymentTarget (..),

    -- * EntityFilterName
    EntityFilterName (..),

    -- * EntityType
    EntityType (..),

    -- * FlowExecutionEventType
    FlowExecutionEventType (..),

    -- * FlowExecutionStatus
    FlowExecutionStatus (..),

    -- * FlowTemplateFilterName
    FlowTemplateFilterName (..),

    -- * NamespaceDeletionStatus
    NamespaceDeletionStatus (..),

    -- * NamespaceDeletionStatusErrorCodes
    NamespaceDeletionStatusErrorCodes (..),

    -- * SystemInstanceDeploymentStatus
    SystemInstanceDeploymentStatus (..),

    -- * SystemInstanceFilterName
    SystemInstanceFilterName (..),

    -- * SystemTemplateFilterName
    SystemTemplateFilterName (..),

    -- * UploadStatus
    UploadStatus (..),

    -- * DefinitionDocument
    DefinitionDocument (..),
    newDefinitionDocument,
    definitionDocument_language,
    definitionDocument_text,

    -- * DependencyRevision
    DependencyRevision (..),
    newDependencyRevision,
    dependencyRevision_revisionNumber,
    dependencyRevision_id,

    -- * EntityDescription
    EntityDescription (..),
    newEntityDescription,
    entityDescription_type,
    entityDescription_arn,
    entityDescription_id,
    entityDescription_createdAt,
    entityDescription_definition,

    -- * EntityFilter
    EntityFilter (..),
    newEntityFilter,
    entityFilter_name,
    entityFilter_value,

    -- * FlowExecutionMessage
    FlowExecutionMessage (..),
    newFlowExecutionMessage,
    flowExecutionMessage_eventType,
    flowExecutionMessage_timestamp,
    flowExecutionMessage_messageId,
    flowExecutionMessage_payload,

    -- * FlowExecutionSummary
    FlowExecutionSummary (..),
    newFlowExecutionSummary,
    flowExecutionSummary_flowTemplateId,
    flowExecutionSummary_systemInstanceId,
    flowExecutionSummary_status,
    flowExecutionSummary_flowExecutionId,
    flowExecutionSummary_createdAt,
    flowExecutionSummary_updatedAt,

    -- * FlowTemplateDescription
    FlowTemplateDescription (..),
    newFlowTemplateDescription,
    flowTemplateDescription_validatedNamespaceVersion,
    flowTemplateDescription_summary,
    flowTemplateDescription_definition,

    -- * FlowTemplateFilter
    FlowTemplateFilter (..),
    newFlowTemplateFilter,
    flowTemplateFilter_name,
    flowTemplateFilter_value,

    -- * FlowTemplateSummary
    FlowTemplateSummary (..),
    newFlowTemplateSummary,
    flowTemplateSummary_revisionNumber,
    flowTemplateSummary_arn,
    flowTemplateSummary_id,
    flowTemplateSummary_createdAt,

    -- * MetricsConfiguration
    MetricsConfiguration (..),
    newMetricsConfiguration,
    metricsConfiguration_cloudMetricEnabled,
    metricsConfiguration_metricRuleRoleArn,

    -- * SystemInstanceDescription
    SystemInstanceDescription (..),
    newSystemInstanceDescription,
    systemInstanceDescription_validatedNamespaceVersion,
    systemInstanceDescription_s3BucketName,
    systemInstanceDescription_summary,
    systemInstanceDescription_validatedDependencyRevisions,
    systemInstanceDescription_metricsConfiguration,
    systemInstanceDescription_flowActionsRoleArn,
    systemInstanceDescription_definition,

    -- * SystemInstanceFilter
    SystemInstanceFilter (..),
    newSystemInstanceFilter,
    systemInstanceFilter_name,
    systemInstanceFilter_value,

    -- * SystemInstanceSummary
    SystemInstanceSummary (..),
    newSystemInstanceSummary,
    systemInstanceSummary_greengrassGroupVersionId,
    systemInstanceSummary_arn,
    systemInstanceSummary_status,
    systemInstanceSummary_target,
    systemInstanceSummary_id,
    systemInstanceSummary_greengrassGroupId,
    systemInstanceSummary_greengrassGroupName,
    systemInstanceSummary_createdAt,
    systemInstanceSummary_updatedAt,

    -- * SystemTemplateDescription
    SystemTemplateDescription (..),
    newSystemTemplateDescription,
    systemTemplateDescription_validatedNamespaceVersion,
    systemTemplateDescription_summary,
    systemTemplateDescription_definition,

    -- * SystemTemplateFilter
    SystemTemplateFilter (..),
    newSystemTemplateFilter,
    systemTemplateFilter_name,
    systemTemplateFilter_value,

    -- * SystemTemplateSummary
    SystemTemplateSummary (..),
    newSystemTemplateSummary,
    systemTemplateSummary_revisionNumber,
    systemTemplateSummary_arn,
    systemTemplateSummary_id,
    systemTemplateSummary_createdAt,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Thing
    Thing (..),
    newThing,
    thing_thingName,
    thing_thingArn,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.DefinitionDocument
import Amazonka.IoTThingsGraph.Types.DefinitionLanguage
import Amazonka.IoTThingsGraph.Types.DependencyRevision
import Amazonka.IoTThingsGraph.Types.DeploymentTarget
import Amazonka.IoTThingsGraph.Types.EntityDescription
import Amazonka.IoTThingsGraph.Types.EntityFilter
import Amazonka.IoTThingsGraph.Types.EntityFilterName
import Amazonka.IoTThingsGraph.Types.EntityType
import Amazonka.IoTThingsGraph.Types.FlowExecutionEventType
import Amazonka.IoTThingsGraph.Types.FlowExecutionMessage
import Amazonka.IoTThingsGraph.Types.FlowExecutionStatus
import Amazonka.IoTThingsGraph.Types.FlowExecutionSummary
import Amazonka.IoTThingsGraph.Types.FlowTemplateDescription
import Amazonka.IoTThingsGraph.Types.FlowTemplateFilter
import Amazonka.IoTThingsGraph.Types.FlowTemplateFilterName
import Amazonka.IoTThingsGraph.Types.FlowTemplateSummary
import Amazonka.IoTThingsGraph.Types.MetricsConfiguration
import Amazonka.IoTThingsGraph.Types.NamespaceDeletionStatus
import Amazonka.IoTThingsGraph.Types.NamespaceDeletionStatusErrorCodes
import Amazonka.IoTThingsGraph.Types.SystemInstanceDeploymentStatus
import Amazonka.IoTThingsGraph.Types.SystemInstanceDescription
import Amazonka.IoTThingsGraph.Types.SystemInstanceFilter
import Amazonka.IoTThingsGraph.Types.SystemInstanceFilterName
import Amazonka.IoTThingsGraph.Types.SystemInstanceSummary
import Amazonka.IoTThingsGraph.Types.SystemTemplateDescription
import Amazonka.IoTThingsGraph.Types.SystemTemplateFilter
import Amazonka.IoTThingsGraph.Types.SystemTemplateFilterName
import Amazonka.IoTThingsGraph.Types.SystemTemplateSummary
import Amazonka.IoTThingsGraph.Types.Tag
import Amazonka.IoTThingsGraph.Types.Thing
import Amazonka.IoTThingsGraph.Types.UploadStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-09-06@ of the Amazon IoT Things Graph SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "IoTThingsGraph",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "iotthingsgraph",
      Core._serviceSigningName = "iotthingsgraph",
      Core._serviceVersion = "2018-09-06",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "IoTThingsGraph",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- |
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- |
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- |
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- |
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- |
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- |
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- |
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
