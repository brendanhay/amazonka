{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.RBin
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-06-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the /Recycle Bin API Reference/. This documentation provides
-- descriptions and syntax for each of the actions and data types in
-- Recycle Bin.
--
-- Recycle Bin is a resource recovery feature that enables you to restore
-- accidentally deleted snapshots and EBS-backed AMIs. When using Recycle
-- Bin, if your resources are deleted, they are retained in the Recycle Bin
-- for a time period that you specify.
--
-- You can restore a resource from the Recycle Bin at any time before its
-- retention period expires. After you restore a resource from the Recycle
-- Bin, the resource is removed from the Recycle Bin, and you can then use
-- it in the same way you use any other resource of that type in your
-- account. If the retention period expires and the resource is not
-- restored, the resource is permanently deleted from the Recycle Bin and
-- is no longer available for recovery. For more information about Recycle
-- Bin, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshot-recycle-bin.html Recycle Bin>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.RBin
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateRule
    CreateRule (CreateRule'),
    newCreateRule,
    CreateRuleResponse (CreateRuleResponse'),
    newCreateRuleResponse,

    -- ** DeleteRule
    DeleteRule (DeleteRule'),
    newDeleteRule,
    DeleteRuleResponse (DeleteRuleResponse'),
    newDeleteRuleResponse,

    -- ** GetRule
    GetRule (GetRule'),
    newGetRule,
    GetRuleResponse (GetRuleResponse'),
    newGetRuleResponse,

    -- ** ListRules (Paginated)
    ListRules (ListRules'),
    newListRules,
    ListRulesResponse (ListRulesResponse'),
    newListRulesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** LockRule
    LockRule (LockRule'),
    newLockRule,
    LockRuleResponse (LockRuleResponse'),
    newLockRuleResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UnlockRule
    UnlockRule (UnlockRule'),
    newUnlockRule,
    UnlockRuleResponse (UnlockRuleResponse'),
    newUnlockRuleResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateRule
    UpdateRule (UpdateRule'),
    newUpdateRule,
    UpdateRuleResponse (UpdateRuleResponse'),
    newUpdateRuleResponse,

    -- * Types

    -- ** LockState
    LockState (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** RetentionPeriodUnit
    RetentionPeriodUnit (..),

    -- ** RuleStatus
    RuleStatus (..),

    -- ** UnlockDelayUnit
    UnlockDelayUnit (..),

    -- ** LockConfiguration
    LockConfiguration (LockConfiguration'),
    newLockConfiguration,

    -- ** ResourceTag
    ResourceTag (ResourceTag'),
    newResourceTag,

    -- ** RetentionPeriod
    RetentionPeriod (RetentionPeriod'),
    newRetentionPeriod,

    -- ** RuleSummary
    RuleSummary (RuleSummary'),
    newRuleSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UnlockDelay
    UnlockDelay (UnlockDelay'),
    newUnlockDelay,
  )
where

import Amazonka.RBin.CreateRule
import Amazonka.RBin.DeleteRule
import Amazonka.RBin.GetRule
import Amazonka.RBin.Lens
import Amazonka.RBin.ListRules
import Amazonka.RBin.ListTagsForResource
import Amazonka.RBin.LockRule
import Amazonka.RBin.TagResource
import Amazonka.RBin.Types
import Amazonka.RBin.UnlockRule
import Amazonka.RBin.UntagResource
import Amazonka.RBin.UpdateRule
import Amazonka.RBin.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'RBin'.

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
