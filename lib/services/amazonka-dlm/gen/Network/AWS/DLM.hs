{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.DLM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-01-12@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Data Lifecycle Manager
--
-- With Amazon Data Lifecycle Manager, you can manage the lifecycle of your
-- Amazon Web Services resources. You create lifecycle policies, which are
-- used to automate operations on the specified resources.
--
-- Amazon DLM supports Amazon EBS volumes and snapshots. For information
-- about using Amazon DLM with Amazon EBS, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshot-lifecycle.html Automating the Amazon EBS Snapshot Lifecycle>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.DLM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteLifecyclePolicy
    DeleteLifecyclePolicy (DeleteLifecyclePolicy'),
    newDeleteLifecyclePolicy,
    DeleteLifecyclePolicyResponse (DeleteLifecyclePolicyResponse'),
    newDeleteLifecyclePolicyResponse,

    -- ** UpdateLifecyclePolicy
    UpdateLifecyclePolicy (UpdateLifecyclePolicy'),
    newUpdateLifecyclePolicy,
    UpdateLifecyclePolicyResponse (UpdateLifecyclePolicyResponse'),
    newUpdateLifecyclePolicyResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateLifecyclePolicy
    CreateLifecyclePolicy (CreateLifecyclePolicy'),
    newCreateLifecyclePolicy,
    CreateLifecyclePolicyResponse (CreateLifecyclePolicyResponse'),
    newCreateLifecyclePolicyResponse,

    -- ** GetLifecyclePolicy
    GetLifecyclePolicy (GetLifecyclePolicy'),
    newGetLifecyclePolicy,
    GetLifecyclePolicyResponse (GetLifecyclePolicyResponse'),
    newGetLifecyclePolicyResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetLifecyclePolicies
    GetLifecyclePolicies (GetLifecyclePolicies'),
    newGetLifecyclePolicies,
    GetLifecyclePoliciesResponse (GetLifecyclePoliciesResponse'),
    newGetLifecyclePoliciesResponse,

    -- * Types

    -- ** EventSourceValues
    EventSourceValues (..),

    -- ** EventTypeValues
    EventTypeValues (..),

    -- ** GettablePolicyStateValues
    GettablePolicyStateValues (..),

    -- ** IntervalUnitValues
    IntervalUnitValues (..),

    -- ** LocationValues
    LocationValues (..),

    -- ** PolicyTypeValues
    PolicyTypeValues (..),

    -- ** ResourceLocationValues
    ResourceLocationValues (..),

    -- ** ResourceTypeValues
    ResourceTypeValues (..),

    -- ** RetentionIntervalUnitValues
    RetentionIntervalUnitValues (..),

    -- ** SettablePolicyStateValues
    SettablePolicyStateValues (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** CreateRule
    CreateRule (CreateRule'),
    newCreateRule,

    -- ** CrossRegionCopyAction
    CrossRegionCopyAction (CrossRegionCopyAction'),
    newCrossRegionCopyAction,

    -- ** CrossRegionCopyDeprecateRule
    CrossRegionCopyDeprecateRule (CrossRegionCopyDeprecateRule'),
    newCrossRegionCopyDeprecateRule,

    -- ** CrossRegionCopyRetainRule
    CrossRegionCopyRetainRule (CrossRegionCopyRetainRule'),
    newCrossRegionCopyRetainRule,

    -- ** CrossRegionCopyRule
    CrossRegionCopyRule (CrossRegionCopyRule'),
    newCrossRegionCopyRule,

    -- ** DeprecateRule
    DeprecateRule (DeprecateRule'),
    newDeprecateRule,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (EncryptionConfiguration'),
    newEncryptionConfiguration,

    -- ** EventParameters
    EventParameters (EventParameters'),
    newEventParameters,

    -- ** EventSource
    EventSource (EventSource'),
    newEventSource,

    -- ** FastRestoreRule
    FastRestoreRule (FastRestoreRule'),
    newFastRestoreRule,

    -- ** LifecyclePolicy
    LifecyclePolicy (LifecyclePolicy'),
    newLifecyclePolicy,

    -- ** LifecyclePolicySummary
    LifecyclePolicySummary (LifecyclePolicySummary'),
    newLifecyclePolicySummary,

    -- ** Parameters
    Parameters (Parameters'),
    newParameters,

    -- ** PolicyDetails
    PolicyDetails (PolicyDetails'),
    newPolicyDetails,

    -- ** RetainRule
    RetainRule (RetainRule'),
    newRetainRule,

    -- ** Schedule
    Schedule (Schedule'),
    newSchedule,

    -- ** ShareRule
    ShareRule (ShareRule'),
    newShareRule,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.DLM.CreateLifecyclePolicy
import Network.AWS.DLM.DeleteLifecyclePolicy
import Network.AWS.DLM.GetLifecyclePolicies
import Network.AWS.DLM.GetLifecyclePolicy
import Network.AWS.DLM.Lens
import Network.AWS.DLM.ListTagsForResource
import Network.AWS.DLM.TagResource
import Network.AWS.DLM.Types
import Network.AWS.DLM.UntagResource
import Network.AWS.DLM.UpdateLifecyclePolicy
import Network.AWS.DLM.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DLM'.

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
