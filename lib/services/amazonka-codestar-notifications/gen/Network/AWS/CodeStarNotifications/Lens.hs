{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStarNotifications.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStarNotifications.Lens
  ( -- * Operations

    -- ** CreateNotificationRule
    createNotificationRule_status,
    createNotificationRule_clientRequestToken,
    createNotificationRule_tags,
    createNotificationRule_name,
    createNotificationRule_eventTypeIds,
    createNotificationRule_resource,
    createNotificationRule_targets,
    createNotificationRule_detailType,
    createNotificationRuleResponse_arn,
    createNotificationRuleResponse_httpStatus,

    -- ** UpdateNotificationRule
    updateNotificationRule_status,
    updateNotificationRule_eventTypeIds,
    updateNotificationRule_detailType,
    updateNotificationRule_name,
    updateNotificationRule_targets,
    updateNotificationRule_arn,
    updateNotificationRuleResponse_httpStatus,

    -- ** DeleteNotificationRule
    deleteNotificationRule_arn,
    deleteNotificationRuleResponse_arn,
    deleteNotificationRuleResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_arn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListEventTypes
    listEventTypes_filters,
    listEventTypes_nextToken,
    listEventTypes_maxResults,
    listEventTypesResponse_eventTypes,
    listEventTypesResponse_nextToken,
    listEventTypesResponse_httpStatus,

    -- ** DeleteTarget
    deleteTarget_forceUnsubscribeAll,
    deleteTarget_targetAddress,
    deleteTargetResponse_httpStatus,

    -- ** ListNotificationRules
    listNotificationRules_filters,
    listNotificationRules_nextToken,
    listNotificationRules_maxResults,
    listNotificationRulesResponse_nextToken,
    listNotificationRulesResponse_notificationRules,
    listNotificationRulesResponse_httpStatus,

    -- ** ListTargets
    listTargets_filters,
    listTargets_nextToken,
    listTargets_maxResults,
    listTargetsResponse_nextToken,
    listTargetsResponse_targets,
    listTargetsResponse_httpStatus,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,
    tagResourceResponse_tags,
    tagResourceResponse_httpStatus,

    -- ** Subscribe
    subscribe_clientRequestToken,
    subscribe_arn,
    subscribe_target,
    subscribeResponse_arn,
    subscribeResponse_httpStatus,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** Unsubscribe
    unsubscribe_arn,
    unsubscribe_targetAddress,
    unsubscribeResponse_httpStatus,
    unsubscribeResponse_arn,

    -- ** DescribeNotificationRule
    describeNotificationRule_arn,
    describeNotificationRuleResponse_status,
    describeNotificationRuleResponse_eventTypes,
    describeNotificationRuleResponse_lastModifiedTimestamp,
    describeNotificationRuleResponse_createdBy,
    describeNotificationRuleResponse_detailType,
    describeNotificationRuleResponse_name,
    describeNotificationRuleResponse_targets,
    describeNotificationRuleResponse_resource,
    describeNotificationRuleResponse_createdTimestamp,
    describeNotificationRuleResponse_tags,
    describeNotificationRuleResponse_httpStatus,
    describeNotificationRuleResponse_arn,

    -- * Types

    -- ** EventTypeSummary
    eventTypeSummary_resourceType,
    eventTypeSummary_eventTypeName,
    eventTypeSummary_eventTypeId,
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
    target_targetType,
    target_targetAddress,

    -- ** TargetSummary
    targetSummary_targetType,
    targetSummary_targetAddress,
    targetSummary_targetStatus,
  )
where

import Network.AWS.CodeStarNotifications.CreateNotificationRule
import Network.AWS.CodeStarNotifications.DeleteNotificationRule
import Network.AWS.CodeStarNotifications.DeleteTarget
import Network.AWS.CodeStarNotifications.DescribeNotificationRule
import Network.AWS.CodeStarNotifications.ListEventTypes
import Network.AWS.CodeStarNotifications.ListNotificationRules
import Network.AWS.CodeStarNotifications.ListTagsForResource
import Network.AWS.CodeStarNotifications.ListTargets
import Network.AWS.CodeStarNotifications.Subscribe
import Network.AWS.CodeStarNotifications.TagResource
import Network.AWS.CodeStarNotifications.Types.EventTypeSummary
import Network.AWS.CodeStarNotifications.Types.ListEventTypesFilter
import Network.AWS.CodeStarNotifications.Types.ListNotificationRulesFilter
import Network.AWS.CodeStarNotifications.Types.ListTargetsFilter
import Network.AWS.CodeStarNotifications.Types.NotificationRuleSummary
import Network.AWS.CodeStarNotifications.Types.Target
import Network.AWS.CodeStarNotifications.Types.TargetSummary
import Network.AWS.CodeStarNotifications.Unsubscribe
import Network.AWS.CodeStarNotifications.UntagResource
import Network.AWS.CodeStarNotifications.UpdateNotificationRule
