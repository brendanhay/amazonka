{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTThingsGraph.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTThingsGraph.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _ResourceAlreadyExistsException,
    _ThrottlingException,
    _InternalFailureException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

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
    entityDescription_arn,
    entityDescription_createdAt,
    entityDescription_definition,
    entityDescription_id,
    entityDescription_type,

    -- * EntityFilter
    EntityFilter (..),
    newEntityFilter,
    entityFilter_value,
    entityFilter_name,

    -- * FlowExecutionMessage
    FlowExecutionMessage (..),
    newFlowExecutionMessage,
    flowExecutionMessage_payload,
    flowExecutionMessage_eventType,
    flowExecutionMessage_timestamp,
    flowExecutionMessage_messageId,

    -- * FlowExecutionSummary
    FlowExecutionSummary (..),
    newFlowExecutionSummary,
    flowExecutionSummary_status,
    flowExecutionSummary_flowTemplateId,
    flowExecutionSummary_createdAt,
    flowExecutionSummary_flowExecutionId,
    flowExecutionSummary_systemInstanceId,
    flowExecutionSummary_updatedAt,

    -- * FlowTemplateDescription
    FlowTemplateDescription (..),
    newFlowTemplateDescription,
    flowTemplateDescription_summary,
    flowTemplateDescription_definition,
    flowTemplateDescription_validatedNamespaceVersion,

    -- * FlowTemplateFilter
    FlowTemplateFilter (..),
    newFlowTemplateFilter,
    flowTemplateFilter_name,
    flowTemplateFilter_value,

    -- * FlowTemplateSummary
    FlowTemplateSummary (..),
    newFlowTemplateSummary,
    flowTemplateSummary_arn,
    flowTemplateSummary_createdAt,
    flowTemplateSummary_revisionNumber,
    flowTemplateSummary_id,

    -- * MetricsConfiguration
    MetricsConfiguration (..),
    newMetricsConfiguration,
    metricsConfiguration_cloudMetricEnabled,
    metricsConfiguration_metricRuleRoleArn,

    -- * SystemInstanceDescription
    SystemInstanceDescription (..),
    newSystemInstanceDescription,
    systemInstanceDescription_summary,
    systemInstanceDescription_metricsConfiguration,
    systemInstanceDescription_validatedDependencyRevisions,
    systemInstanceDescription_definition,
    systemInstanceDescription_validatedNamespaceVersion,
    systemInstanceDescription_flowActionsRoleArn,
    systemInstanceDescription_s3BucketName,

    -- * SystemInstanceFilter
    SystemInstanceFilter (..),
    newSystemInstanceFilter,
    systemInstanceFilter_value,
    systemInstanceFilter_name,

    -- * SystemInstanceSummary
    SystemInstanceSummary (..),
    newSystemInstanceSummary,
    systemInstanceSummary_status,
    systemInstanceSummary_greengrassGroupName,
    systemInstanceSummary_arn,
    systemInstanceSummary_createdAt,
    systemInstanceSummary_greengrassGroupId,
    systemInstanceSummary_greengrassGroupVersionId,
    systemInstanceSummary_id,
    systemInstanceSummary_updatedAt,
    systemInstanceSummary_target,

    -- * SystemTemplateDescription
    SystemTemplateDescription (..),
    newSystemTemplateDescription,
    systemTemplateDescription_summary,
    systemTemplateDescription_definition,
    systemTemplateDescription_validatedNamespaceVersion,

    -- * SystemTemplateFilter
    SystemTemplateFilter (..),
    newSystemTemplateFilter,
    systemTemplateFilter_name,
    systemTemplateFilter_value,

    -- * SystemTemplateSummary
    SystemTemplateSummary (..),
    newSystemTemplateSummary,
    systemTemplateSummary_arn,
    systemTemplateSummary_createdAt,
    systemTemplateSummary_revisionNumber,
    systemTemplateSummary_id,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Thing
    Thing (..),
    newThing,
    thing_thingArn,
    thing_thingName,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTThingsGraph.Types.DefinitionDocument
import Network.AWS.IoTThingsGraph.Types.DefinitionLanguage
import Network.AWS.IoTThingsGraph.Types.DependencyRevision
import Network.AWS.IoTThingsGraph.Types.DeploymentTarget
import Network.AWS.IoTThingsGraph.Types.EntityDescription
import Network.AWS.IoTThingsGraph.Types.EntityFilter
import Network.AWS.IoTThingsGraph.Types.EntityFilterName
import Network.AWS.IoTThingsGraph.Types.EntityType
import Network.AWS.IoTThingsGraph.Types.FlowExecutionEventType
import Network.AWS.IoTThingsGraph.Types.FlowExecutionMessage
import Network.AWS.IoTThingsGraph.Types.FlowExecutionStatus
import Network.AWS.IoTThingsGraph.Types.FlowExecutionSummary
import Network.AWS.IoTThingsGraph.Types.FlowTemplateDescription
import Network.AWS.IoTThingsGraph.Types.FlowTemplateFilter
import Network.AWS.IoTThingsGraph.Types.FlowTemplateFilterName
import Network.AWS.IoTThingsGraph.Types.FlowTemplateSummary
import Network.AWS.IoTThingsGraph.Types.MetricsConfiguration
import Network.AWS.IoTThingsGraph.Types.NamespaceDeletionStatus
import Network.AWS.IoTThingsGraph.Types.NamespaceDeletionStatusErrorCodes
import Network.AWS.IoTThingsGraph.Types.SystemInstanceDeploymentStatus
import Network.AWS.IoTThingsGraph.Types.SystemInstanceDescription
import Network.AWS.IoTThingsGraph.Types.SystemInstanceFilter
import Network.AWS.IoTThingsGraph.Types.SystemInstanceFilterName
import Network.AWS.IoTThingsGraph.Types.SystemInstanceSummary
import Network.AWS.IoTThingsGraph.Types.SystemTemplateDescription
import Network.AWS.IoTThingsGraph.Types.SystemTemplateFilter
import Network.AWS.IoTThingsGraph.Types.SystemTemplateFilterName
import Network.AWS.IoTThingsGraph.Types.SystemTemplateSummary
import Network.AWS.IoTThingsGraph.Types.Tag
import Network.AWS.IoTThingsGraph.Types.Thing
import Network.AWS.IoTThingsGraph.Types.UploadStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- |
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- |
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- |
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- |
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"

-- |
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- |
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- |
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
