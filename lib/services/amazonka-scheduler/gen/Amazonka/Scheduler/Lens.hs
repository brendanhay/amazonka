{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Scheduler.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Lens
  ( -- * Operations

    -- ** CreateSchedule
    createSchedule_clientToken,
    createSchedule_description,
    createSchedule_endDate,
    createSchedule_groupName,
    createSchedule_kmsKeyArn,
    createSchedule_scheduleExpressionTimezone,
    createSchedule_startDate,
    createSchedule_state,
    createSchedule_flexibleTimeWindow,
    createSchedule_name,
    createSchedule_scheduleExpression,
    createSchedule_target,
    createScheduleResponse_httpStatus,
    createScheduleResponse_scheduleArn,

    -- ** CreateScheduleGroup
    createScheduleGroup_clientToken,
    createScheduleGroup_tags,
    createScheduleGroup_name,
    createScheduleGroupResponse_httpStatus,
    createScheduleGroupResponse_scheduleGroupArn,

    -- ** DeleteSchedule
    deleteSchedule_clientToken,
    deleteSchedule_groupName,
    deleteSchedule_name,
    deleteScheduleResponse_httpStatus,

    -- ** DeleteScheduleGroup
    deleteScheduleGroup_clientToken,
    deleteScheduleGroup_name,
    deleteScheduleGroupResponse_httpStatus,

    -- ** GetSchedule
    getSchedule_groupName,
    getSchedule_name,
    getScheduleResponse_arn,
    getScheduleResponse_creationDate,
    getScheduleResponse_description,
    getScheduleResponse_endDate,
    getScheduleResponse_flexibleTimeWindow,
    getScheduleResponse_groupName,
    getScheduleResponse_kmsKeyArn,
    getScheduleResponse_lastModificationDate,
    getScheduleResponse_name,
    getScheduleResponse_scheduleExpression,
    getScheduleResponse_scheduleExpressionTimezone,
    getScheduleResponse_startDate,
    getScheduleResponse_state,
    getScheduleResponse_target,
    getScheduleResponse_httpStatus,

    -- ** GetScheduleGroup
    getScheduleGroup_name,
    getScheduleGroupResponse_arn,
    getScheduleGroupResponse_creationDate,
    getScheduleGroupResponse_lastModificationDate,
    getScheduleGroupResponse_name,
    getScheduleGroupResponse_state,
    getScheduleGroupResponse_httpStatus,

    -- ** ListScheduleGroups
    listScheduleGroups_maxResults,
    listScheduleGroups_namePrefix,
    listScheduleGroups_nextToken,
    listScheduleGroupsResponse_nextToken,
    listScheduleGroupsResponse_httpStatus,
    listScheduleGroupsResponse_scheduleGroups,

    -- ** ListSchedules
    listSchedules_groupName,
    listSchedules_maxResults,
    listSchedules_namePrefix,
    listSchedules_nextToken,
    listSchedules_state,
    listSchedulesResponse_nextToken,
    listSchedulesResponse_httpStatus,
    listSchedulesResponse_schedules,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateSchedule
    updateSchedule_clientToken,
    updateSchedule_description,
    updateSchedule_endDate,
    updateSchedule_groupName,
    updateSchedule_kmsKeyArn,
    updateSchedule_scheduleExpressionTimezone,
    updateSchedule_startDate,
    updateSchedule_state,
    updateSchedule_flexibleTimeWindow,
    updateSchedule_name,
    updateSchedule_scheduleExpression,
    updateSchedule_target,
    updateScheduleResponse_httpStatus,
    updateScheduleResponse_scheduleArn,

    -- * Types

    -- ** AwsVpcConfiguration
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_subnets,

    -- ** CapacityProviderStrategyItem
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- ** DeadLetterConfig
    deadLetterConfig_arn,

    -- ** EcsParameters
    ecsParameters_capacityProviderStrategy,
    ecsParameters_enableECSManagedTags,
    ecsParameters_enableExecuteCommand,
    ecsParameters_group,
    ecsParameters_launchType,
    ecsParameters_networkConfiguration,
    ecsParameters_placementConstraints,
    ecsParameters_placementStrategy,
    ecsParameters_platformVersion,
    ecsParameters_propagateTags,
    ecsParameters_referenceId,
    ecsParameters_tags,
    ecsParameters_taskCount,
    ecsParameters_taskDefinitionArn,

    -- ** EventBridgeParameters
    eventBridgeParameters_detailType,
    eventBridgeParameters_source,

    -- ** FlexibleTimeWindow
    flexibleTimeWindow_maximumWindowInMinutes,
    flexibleTimeWindow_mode,

    -- ** KinesisParameters
    kinesisParameters_partitionKey,

    -- ** NetworkConfiguration
    networkConfiguration_awsvpcConfiguration,

    -- ** PlacementConstraint
    placementConstraint_expression,
    placementConstraint_type,

    -- ** PlacementStrategy
    placementStrategy_field,
    placementStrategy_type,

    -- ** RetryPolicy
    retryPolicy_maximumEventAgeInSeconds,
    retryPolicy_maximumRetryAttempts,

    -- ** SageMakerPipelineParameter
    sageMakerPipelineParameter_name,
    sageMakerPipelineParameter_value,

    -- ** SageMakerPipelineParameters
    sageMakerPipelineParameters_pipelineParameterList,

    -- ** ScheduleGroupSummary
    scheduleGroupSummary_arn,
    scheduleGroupSummary_creationDate,
    scheduleGroupSummary_lastModificationDate,
    scheduleGroupSummary_name,
    scheduleGroupSummary_state,

    -- ** ScheduleSummary
    scheduleSummary_arn,
    scheduleSummary_creationDate,
    scheduleSummary_groupName,
    scheduleSummary_lastModificationDate,
    scheduleSummary_name,
    scheduleSummary_state,
    scheduleSummary_target,

    -- ** SqsParameters
    sqsParameters_messageGroupId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Target
    target_deadLetterConfig,
    target_ecsParameters,
    target_eventBridgeParameters,
    target_input,
    target_kinesisParameters,
    target_retryPolicy,
    target_sageMakerPipelineParameters,
    target_sqsParameters,
    target_arn,
    target_roleArn,

    -- ** TargetSummary
    targetSummary_arn,
  )
where

import Amazonka.Scheduler.CreateSchedule
import Amazonka.Scheduler.CreateScheduleGroup
import Amazonka.Scheduler.DeleteSchedule
import Amazonka.Scheduler.DeleteScheduleGroup
import Amazonka.Scheduler.GetSchedule
import Amazonka.Scheduler.GetScheduleGroup
import Amazonka.Scheduler.ListScheduleGroups
import Amazonka.Scheduler.ListSchedules
import Amazonka.Scheduler.ListTagsForResource
import Amazonka.Scheduler.TagResource
import Amazonka.Scheduler.Types.AwsVpcConfiguration
import Amazonka.Scheduler.Types.CapacityProviderStrategyItem
import Amazonka.Scheduler.Types.DeadLetterConfig
import Amazonka.Scheduler.Types.EcsParameters
import Amazonka.Scheduler.Types.EventBridgeParameters
import Amazonka.Scheduler.Types.FlexibleTimeWindow
import Amazonka.Scheduler.Types.KinesisParameters
import Amazonka.Scheduler.Types.NetworkConfiguration
import Amazonka.Scheduler.Types.PlacementConstraint
import Amazonka.Scheduler.Types.PlacementStrategy
import Amazonka.Scheduler.Types.RetryPolicy
import Amazonka.Scheduler.Types.SageMakerPipelineParameter
import Amazonka.Scheduler.Types.SageMakerPipelineParameters
import Amazonka.Scheduler.Types.ScheduleGroupSummary
import Amazonka.Scheduler.Types.ScheduleSummary
import Amazonka.Scheduler.Types.SqsParameters
import Amazonka.Scheduler.Types.Tag
import Amazonka.Scheduler.Types.Target
import Amazonka.Scheduler.Types.TargetSummary
import Amazonka.Scheduler.UntagResource
import Amazonka.Scheduler.UpdateSchedule
