{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CodeStarNotifications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-10-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This AWS CodeStar Notifications API Reference provides descriptions and
-- usage examples of the operations and data types for the AWS CodeStar
-- Notifications API. You can use the AWS CodeStar Notifications API to
-- work with the following objects:
--
-- Notification rules, by calling the following:
--
-- -   CreateNotificationRule, which creates a notification rule for a
--     resource in your account.
--
-- -   DeleteNotificationRule, which deletes a notification rule.
--
-- -   DescribeNotificationRule, which provides information about a
--     notification rule.
--
-- -   ListNotificationRules, which lists the notification rules associated
--     with your account.
--
-- -   UpdateNotificationRule, which changes the name, events, or targets
--     associated with a notification rule.
--
-- -   Subscribe, which subscribes a target to a notification rule.
--
-- -   Unsubscribe, which removes a target from a notification rule.
--
-- Targets, by calling the following:
--
-- -   DeleteTarget, which removes a notification rule target from a
--     notification rule.
--
-- -   ListTargets, which lists the targets associated with a notification
--     rule.
--
-- Events, by calling the following:
--
-- -   ListEventTypes, which lists the event types you can include in a
--     notification rule.
--
-- Tags, by calling the following:
--
-- -   ListTagsForResource, which lists the tags already associated with a
--     notification rule in your account.
--
-- -   TagResource, which associates a tag you provide with a notification
--     rule in your account.
--
-- -   UntagResource, which removes a tag from a notification rule in your
--     account.
--
-- For information about how to use AWS CodeStar Notifications, see the
-- <https://docs.aws.amazon.com/dtconsole/latest/userguide/what-is-dtconsole.html Amazon Web Services Developer Tools Console User Guide>.
module Amazonka.CodeStarNotifications
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ConfigurationException
    _ConfigurationException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateNotificationRule
    CreateNotificationRule (CreateNotificationRule'),
    newCreateNotificationRule,
    CreateNotificationRuleResponse (CreateNotificationRuleResponse'),
    newCreateNotificationRuleResponse,

    -- ** DeleteNotificationRule
    DeleteNotificationRule (DeleteNotificationRule'),
    newDeleteNotificationRule,
    DeleteNotificationRuleResponse (DeleteNotificationRuleResponse'),
    newDeleteNotificationRuleResponse,

    -- ** DeleteTarget
    DeleteTarget (DeleteTarget'),
    newDeleteTarget,
    DeleteTargetResponse (DeleteTargetResponse'),
    newDeleteTargetResponse,

    -- ** DescribeNotificationRule
    DescribeNotificationRule (DescribeNotificationRule'),
    newDescribeNotificationRule,
    DescribeNotificationRuleResponse (DescribeNotificationRuleResponse'),
    newDescribeNotificationRuleResponse,

    -- ** ListEventTypes (Paginated)
    ListEventTypes (ListEventTypes'),
    newListEventTypes,
    ListEventTypesResponse (ListEventTypesResponse'),
    newListEventTypesResponse,

    -- ** ListNotificationRules (Paginated)
    ListNotificationRules (ListNotificationRules'),
    newListNotificationRules,
    ListNotificationRulesResponse (ListNotificationRulesResponse'),
    newListNotificationRulesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTargets (Paginated)
    ListTargets (ListTargets'),
    newListTargets,
    ListTargetsResponse (ListTargetsResponse'),
    newListTargetsResponse,

    -- ** Subscribe
    Subscribe (Subscribe'),
    newSubscribe,
    SubscribeResponse (SubscribeResponse'),
    newSubscribeResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** Unsubscribe
    Unsubscribe (Unsubscribe'),
    newUnsubscribe,
    UnsubscribeResponse (UnsubscribeResponse'),
    newUnsubscribeResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateNotificationRule
    UpdateNotificationRule (UpdateNotificationRule'),
    newUpdateNotificationRule,
    UpdateNotificationRuleResponse (UpdateNotificationRuleResponse'),
    newUpdateNotificationRuleResponse,

    -- * Types

    -- ** DetailType
    DetailType (..),

    -- ** ListEventTypesFilterName
    ListEventTypesFilterName (..),

    -- ** ListNotificationRulesFilterName
    ListNotificationRulesFilterName (..),

    -- ** ListTargetsFilterName
    ListTargetsFilterName (..),

    -- ** NotificationRuleStatus
    NotificationRuleStatus (..),

    -- ** TargetStatus
    TargetStatus (..),

    -- ** EventTypeSummary
    EventTypeSummary (EventTypeSummary'),
    newEventTypeSummary,

    -- ** ListEventTypesFilter
    ListEventTypesFilter (ListEventTypesFilter'),
    newListEventTypesFilter,

    -- ** ListNotificationRulesFilter
    ListNotificationRulesFilter (ListNotificationRulesFilter'),
    newListNotificationRulesFilter,

    -- ** ListTargetsFilter
    ListTargetsFilter (ListTargetsFilter'),
    newListTargetsFilter,

    -- ** NotificationRuleSummary
    NotificationRuleSummary (NotificationRuleSummary'),
    newNotificationRuleSummary,

    -- ** Target
    Target (Target'),
    newTarget,

    -- ** TargetSummary
    TargetSummary (TargetSummary'),
    newTargetSummary,
  )
where

import Amazonka.CodeStarNotifications.CreateNotificationRule
import Amazonka.CodeStarNotifications.DeleteNotificationRule
import Amazonka.CodeStarNotifications.DeleteTarget
import Amazonka.CodeStarNotifications.DescribeNotificationRule
import Amazonka.CodeStarNotifications.Lens
import Amazonka.CodeStarNotifications.ListEventTypes
import Amazonka.CodeStarNotifications.ListNotificationRules
import Amazonka.CodeStarNotifications.ListTagsForResource
import Amazonka.CodeStarNotifications.ListTargets
import Amazonka.CodeStarNotifications.Subscribe
import Amazonka.CodeStarNotifications.TagResource
import Amazonka.CodeStarNotifications.Types
import Amazonka.CodeStarNotifications.Unsubscribe
import Amazonka.CodeStarNotifications.UntagResource
import Amazonka.CodeStarNotifications.UpdateNotificationRule
import Amazonka.CodeStarNotifications.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodeStarNotifications'.

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
