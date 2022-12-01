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
    createSchedule_scheduleExpressionTimezone,
    createSchedule_clientToken,
    createSchedule_endDate,
    createSchedule_state,
    createSchedule_groupName,
    createSchedule_description,
    createSchedule_kmsKeyArn,
    createSchedule_startDate,
    createSchedule_flexibleTimeWindow,
    createSchedule_name,
    createSchedule_scheduleExpression,
    createSchedule_target,
    createScheduleResponse_httpStatus,
    createScheduleResponse_scheduleArn,

    -- ** CreateScheduleGroup
    createScheduleGroup_tags,
    createScheduleGroup_clientToken,
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
    getScheduleResponse_scheduleExpressionTimezone,
    getScheduleResponse_name,
    getScheduleResponse_endDate,
    getScheduleResponse_arn,
    getScheduleResponse_state,
    getScheduleResponse_creationDate,
    getScheduleResponse_target,
    getScheduleResponse_groupName,
    getScheduleResponse_description,
    getScheduleResponse_kmsKeyArn,
    getScheduleResponse_scheduleExpression,
    getScheduleResponse_flexibleTimeWindow,
    getScheduleResponse_startDate,
    getScheduleResponse_lastModificationDate,
    getScheduleResponse_httpStatus,

    -- ** GetScheduleGroup
    getScheduleGroup_name,
    getScheduleGroupResponse_name,
    getScheduleGroupResponse_arn,
    getScheduleGroupResponse_state,
    getScheduleGroupResponse_creationDate,
    getScheduleGroupResponse_lastModificationDate,
    getScheduleGroupResponse_httpStatus,

    -- ** ListScheduleGroups
    listScheduleGroups_nextToken,
    listScheduleGroups_maxResults,
    listScheduleGroups_namePrefix,
    listScheduleGroupsResponse_nextToken,
    listScheduleGroupsResponse_httpStatus,
    listScheduleGroupsResponse_scheduleGroups,

    -- ** ListSchedules
    listSchedules_nextToken,
    listSchedules_state,
    listSchedules_groupName,
    listSchedules_maxResults,
    listSchedules_namePrefix,
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
    updateSchedule_scheduleExpressionTimezone,
    updateSchedule_clientToken,
    updateSchedule_endDate,
    updateSchedule_state,
    updateSchedule_groupName,
    updateSchedule_description,
    updateSchedule_kmsKeyArn,
    updateSchedule_startDate,
    updateSchedule_flexibleTimeWindow,
    updateSchedule_name,
    updateSchedule_scheduleExpression,
    updateSchedule_target,
    updateScheduleResponse_httpStatus,
    updateScheduleResponse_scheduleArn,

    -- * Types

    -- ** AwsVpcConfiguration
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_subnets,

    -- ** CapacityProviderStrategyItem
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- ** DeadLetterConfig
    deadLetterConfig_arn,

    -- ** EcsParameters
    ecsParameters_tags,
    ecsParameters_placementStrategy,
    ecsParameters_networkConfiguration,
    ecsParameters_enableExecuteCommand,
    ecsParameters_capacityProviderStrategy,
    ecsParameters_placementConstraints,
    ecsParameters_propagateTags,
    ecsParameters_referenceId,
    ecsParameters_launchType,
    ecsParameters_platformVersion,
    ecsParameters_enableECSManagedTags,
    ecsParameters_group,
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
    placementConstraint_type,
    placementConstraint_expression,

    -- ** PlacementStrategy
    placementStrategy_type,
    placementStrategy_field,

    -- ** RetryPolicy
    retryPolicy_maximumEventAgeInSeconds,
    retryPolicy_maximumRetryAttempts,

    -- ** SageMakerPipelineParameter
    sageMakerPipelineParameter_name,
    sageMakerPipelineParameter_value,

    -- ** SageMakerPipelineParameters
    sageMakerPipelineParameters_pipelineParameterList,

    -- ** ScheduleGroupSummary
    scheduleGroupSummary_name,
    scheduleGroupSummary_arn,
    scheduleGroupSummary_state,
    scheduleGroupSummary_creationDate,
    scheduleGroupSummary_lastModificationDate,

    -- ** ScheduleSummary
    scheduleSummary_name,
    scheduleSummary_arn,
    scheduleSummary_state,
    scheduleSummary_creationDate,
    scheduleSummary_target,
    scheduleSummary_groupName,
    scheduleSummary_lastModificationDate,

    -- ** SqsParameters
    sqsParameters_messageGroupId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Target
    target_kinesisParameters,
    target_sageMakerPipelineParameters,
    target_input,
    target_sqsParameters,
    target_ecsParameters,
    target_retryPolicy,
    target_eventBridgeParameters,
    target_deadLetterConfig,
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
