{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeStarNotifications.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarNotifications.Lens
  ( -- * Operations

    -- ** CreateNotificationRule
    createNotificationRule_clientRequestToken,
    createNotificationRule_status,
    createNotificationRule_tags,
    createNotificationRule_name,
    createNotificationRule_eventTypeIds,
    createNotificationRule_resource,
    createNotificationRule_targets,
    createNotificationRule_detailType,
    createNotificationRuleResponse_arn,
    createNotificationRuleResponse_httpStatus,

    -- ** DeleteNotificationRule
    deleteNotificationRule_arn,
    deleteNotificationRuleResponse_arn,
    deleteNotificationRuleResponse_httpStatus,

    -- ** DeleteTarget
    deleteTarget_forceUnsubscribeAll,
    deleteTarget_targetAddress,
    deleteTargetResponse_httpStatus,

    -- ** DescribeNotificationRule
    describeNotificationRule_arn,
    describeNotificationRuleResponse_createdBy,
    describeNotificationRuleResponse_createdTimestamp,
    describeNotificationRuleResponse_detailType,
    describeNotificationRuleResponse_eventTypes,
    describeNotificationRuleResponse_lastModifiedTimestamp,
    describeNotificationRuleResponse_name,
    describeNotificationRuleResponse_resource,
    describeNotificationRuleResponse_status,
    describeNotificationRuleResponse_tags,
    describeNotificationRuleResponse_targets,
    describeNotificationRuleResponse_httpStatus,
    describeNotificationRuleResponse_arn,

    -- ** ListEventTypes
    listEventTypes_filters,
    listEventTypes_maxResults,
    listEventTypes_nextToken,
    listEventTypesResponse_eventTypes,
    listEventTypesResponse_nextToken,
    listEventTypesResponse_httpStatus,

    -- ** ListNotificationRules
    listNotificationRules_filters,
    listNotificationRules_maxResults,
    listNotificationRules_nextToken,
    listNotificationRulesResponse_nextToken,
    listNotificationRulesResponse_notificationRules,
    listNotificationRulesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_arn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTargets
    listTargets_filters,
    listTargets_maxResults,
    listTargets_nextToken,
    listTargetsResponse_nextToken,
    listTargetsResponse_targets,
    listTargetsResponse_httpStatus,

    -- ** Subscribe
    subscribe_clientRequestToken,
    subscribe_arn,
    subscribe_target,
    subscribeResponse_arn,
    subscribeResponse_httpStatus,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,
    tagResourceResponse_tags,
    tagResourceResponse_httpStatus,

    -- ** Unsubscribe
    unsubscribe_arn,
    unsubscribe_targetAddress,
    unsubscribeResponse_httpStatus,
    unsubscribeResponse_arn,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateNotificationRule
    updateNotificationRule_detailType,
    updateNotificationRule_eventTypeIds,
    updateNotificationRule_name,
    updateNotificationRule_status,
    updateNotificationRule_targets,
    updateNotificationRule_arn,
    updateNotificationRuleResponse_httpStatus,

    -- * Types

    -- ** EventTypeSummary
    eventTypeSummary_eventTypeId,
    eventTypeSummary_eventTypeName,
    eventTypeSummary_resourceType,
    eventTypeSummary_serviceName,

    -- ** ListEventTypesFilter
    listEventTypesFilter_name,
    listEventTypesFilter_value,

    -- ** ListNotificationRulesFilter
    listNotificationRulesFilter_name,
    listNotificationRulesFilter_value,

    -- ** ListTargetsFilter
    listTargetsFilter_name,
    listTargetsFilter_value,

    -- ** NotificationRuleSummary
    notificationRuleSummary_arn,
    notificationRuleSummary_id,

    -- ** Target
    target_targetAddress,
    target_targetType,

    -- ** TargetSummary
    targetSummary_targetAddress,
    targetSummary_targetStatus,
    targetSummary_targetType,
  )
where

import Amazonka.CodeStarNotifications.CreateNotificationRule
import Amazonka.CodeStarNotifications.DeleteNotificationRule
import Amazonka.CodeStarNotifications.DeleteTarget
import Amazonka.CodeStarNotifications.DescribeNotificationRule
import Amazonka.CodeStarNotifications.ListEventTypes
import Amazonka.CodeStarNotifications.ListNotificationRules
import Amazonka.CodeStarNotifications.ListTagsForResource
import Amazonka.CodeStarNotifications.ListTargets
import Amazonka.CodeStarNotifications.Subscribe
import Amazonka.CodeStarNotifications.TagResource
import Amazonka.CodeStarNotifications.Types.EventTypeSummary
import Amazonka.CodeStarNotifications.Types.ListEventTypesFilter
import Amazonka.CodeStarNotifications.Types.ListNotificationRulesFilter
import Amazonka.CodeStarNotifications.Types.ListTargetsFilter
import Amazonka.CodeStarNotifications.Types.NotificationRuleSummary
import Amazonka.CodeStarNotifications.Types.Target
import Amazonka.CodeStarNotifications.Types.TargetSummary
import Amazonka.CodeStarNotifications.Unsubscribe
import Amazonka.CodeStarNotifications.UntagResource
import Amazonka.CodeStarNotifications.UpdateNotificationRule
