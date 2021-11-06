{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SSMContacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-05-03@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Systems Manager Incident Manager is an incident management console
-- designed to help users mitigate and recover from incidents affecting
-- their Amazon Web Services-hosted applications. An incident is any
-- unplanned interruption or reduction in quality of services.
--
-- Incident Manager increases incident resolution by notifying responders
-- of impact, highlighting relevant troubleshooting data, and providing
-- collaboration tools to get services back up and running. To achieve the
-- primary goal of reducing the time-to-resolution of critical incidents,
-- Incident Manager automates response plans and enables responder team
-- escalation.
module Amazonka.SSMContacts
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** DataEncryptionException
    _DataEncryptionException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListPagesByEngagement (Paginated)
    ListPagesByEngagement (ListPagesByEngagement'),
    newListPagesByEngagement,
    ListPagesByEngagementResponse (ListPagesByEngagementResponse'),
    newListPagesByEngagementResponse,

    -- ** ListEngagements (Paginated)
    ListEngagements (ListEngagements'),
    newListEngagements,
    ListEngagementsResponse (ListEngagementsResponse'),
    newListEngagementsResponse,

    -- ** ListContactChannels (Paginated)
    ListContactChannels (ListContactChannels'),
    newListContactChannels,
    ListContactChannelsResponse (ListContactChannelsResponse'),
    newListContactChannelsResponse,

    -- ** ActivateContactChannel
    ActivateContactChannel (ActivateContactChannel'),
    newActivateContactChannel,
    ActivateContactChannelResponse (ActivateContactChannelResponse'),
    newActivateContactChannelResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartEngagement
    StartEngagement (StartEngagement'),
    newStartEngagement,
    StartEngagementResponse (StartEngagementResponse'),
    newStartEngagementResponse,

    -- ** DeactivateContactChannel
    DeactivateContactChannel (DeactivateContactChannel'),
    newDeactivateContactChannel,
    DeactivateContactChannelResponse (DeactivateContactChannelResponse'),
    newDeactivateContactChannelResponse,

    -- ** AcceptPage
    AcceptPage (AcceptPage'),
    newAcceptPage,
    AcceptPageResponse (AcceptPageResponse'),
    newAcceptPageResponse,

    -- ** ListPageReceipts (Paginated)
    ListPageReceipts (ListPageReceipts'),
    newListPageReceipts,
    ListPageReceiptsResponse (ListPageReceiptsResponse'),
    newListPageReceiptsResponse,

    -- ** GetContact
    GetContact (GetContact'),
    newGetContact,
    GetContactResponse (GetContactResponse'),
    newGetContactResponse,

    -- ** DescribePage
    DescribePage (DescribePage'),
    newDescribePage,
    DescribePageResponse (DescribePageResponse'),
    newDescribePageResponse,

    -- ** DeleteContact
    DeleteContact (DeleteContact'),
    newDeleteContact,
    DeleteContactResponse (DeleteContactResponse'),
    newDeleteContactResponse,

    -- ** UpdateContact
    UpdateContact (UpdateContact'),
    newUpdateContact,
    UpdateContactResponse (UpdateContactResponse'),
    newUpdateContactResponse,

    -- ** CreateContact
    CreateContact (CreateContact'),
    newCreateContact,
    CreateContactResponse (CreateContactResponse'),
    newCreateContactResponse,

    -- ** CreateContactChannel
    CreateContactChannel (CreateContactChannel'),
    newCreateContactChannel,
    CreateContactChannelResponse (CreateContactChannelResponse'),
    newCreateContactChannelResponse,

    -- ** DeleteContactChannel
    DeleteContactChannel (DeleteContactChannel'),
    newDeleteContactChannel,
    DeleteContactChannelResponse (DeleteContactChannelResponse'),
    newDeleteContactChannelResponse,

    -- ** UpdateContactChannel
    UpdateContactChannel (UpdateContactChannel'),
    newUpdateContactChannel,
    UpdateContactChannelResponse (UpdateContactChannelResponse'),
    newUpdateContactChannelResponse,

    -- ** GetContactChannel
    GetContactChannel (GetContactChannel'),
    newGetContactChannel,
    GetContactChannelResponse (GetContactChannelResponse'),
    newGetContactChannelResponse,

    -- ** SendActivationCode
    SendActivationCode (SendActivationCode'),
    newSendActivationCode,
    SendActivationCodeResponse (SendActivationCodeResponse'),
    newSendActivationCodeResponse,

    -- ** StopEngagement
    StopEngagement (StopEngagement'),
    newStopEngagement,
    StopEngagementResponse (StopEngagementResponse'),
    newStopEngagementResponse,

    -- ** DescribeEngagement
    DescribeEngagement (DescribeEngagement'),
    newDescribeEngagement,
    DescribeEngagementResponse (DescribeEngagementResponse'),
    newDescribeEngagementResponse,

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

    -- ** GetContactPolicy
    GetContactPolicy (GetContactPolicy'),
    newGetContactPolicy,
    GetContactPolicyResponse (GetContactPolicyResponse'),
    newGetContactPolicyResponse,

    -- ** PutContactPolicy
    PutContactPolicy (PutContactPolicy'),
    newPutContactPolicy,
    PutContactPolicyResponse (PutContactPolicyResponse'),
    newPutContactPolicyResponse,

    -- ** ListContacts (Paginated)
    ListContacts (ListContacts'),
    newListContacts,
    ListContactsResponse (ListContactsResponse'),
    newListContactsResponse,

    -- ** ListPagesByContact (Paginated)
    ListPagesByContact (ListPagesByContact'),
    newListPagesByContact,
    ListPagesByContactResponse (ListPagesByContactResponse'),
    newListPagesByContactResponse,

    -- * Types

    -- ** AcceptCodeValidation
    AcceptCodeValidation (..),

    -- ** AcceptType
    AcceptType (..),

    -- ** ActivationStatus
    ActivationStatus (..),

    -- ** ChannelType
    ChannelType (..),

    -- ** ContactType
    ContactType (..),

    -- ** ReceiptType
    ReceiptType (..),

    -- ** ChannelTargetInfo
    ChannelTargetInfo (ChannelTargetInfo'),
    newChannelTargetInfo,

    -- ** Contact
    Contact (Contact'),
    newContact,

    -- ** ContactChannel
    ContactChannel (ContactChannel'),
    newContactChannel,

    -- ** ContactChannelAddress
    ContactChannelAddress (ContactChannelAddress'),
    newContactChannelAddress,

    -- ** ContactTargetInfo
    ContactTargetInfo (ContactTargetInfo'),
    newContactTargetInfo,

    -- ** Engagement
    Engagement (Engagement'),
    newEngagement,

    -- ** Page
    Page (Page'),
    newPage,

    -- ** Plan
    Plan (Plan'),
    newPlan,

    -- ** Receipt
    Receipt (Receipt'),
    newReceipt,

    -- ** Stage
    Stage (Stage'),
    newStage,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Target
    Target (Target'),
    newTarget,

    -- ** TimeRange
    TimeRange (TimeRange'),
    newTimeRange,
  )
where

import Amazonka.SSMContacts.AcceptPage
import Amazonka.SSMContacts.ActivateContactChannel
import Amazonka.SSMContacts.CreateContact
import Amazonka.SSMContacts.CreateContactChannel
import Amazonka.SSMContacts.DeactivateContactChannel
import Amazonka.SSMContacts.DeleteContact
import Amazonka.SSMContacts.DeleteContactChannel
import Amazonka.SSMContacts.DescribeEngagement
import Amazonka.SSMContacts.DescribePage
import Amazonka.SSMContacts.GetContact
import Amazonka.SSMContacts.GetContactChannel
import Amazonka.SSMContacts.GetContactPolicy
import Amazonka.SSMContacts.Lens
import Amazonka.SSMContacts.ListContactChannels
import Amazonka.SSMContacts.ListContacts
import Amazonka.SSMContacts.ListEngagements
import Amazonka.SSMContacts.ListPageReceipts
import Amazonka.SSMContacts.ListPagesByContact
import Amazonka.SSMContacts.ListPagesByEngagement
import Amazonka.SSMContacts.ListTagsForResource
import Amazonka.SSMContacts.PutContactPolicy
import Amazonka.SSMContacts.SendActivationCode
import Amazonka.SSMContacts.StartEngagement
import Amazonka.SSMContacts.StopEngagement
import Amazonka.SSMContacts.TagResource
import Amazonka.SSMContacts.Types
import Amazonka.SSMContacts.UntagResource
import Amazonka.SSMContacts.UpdateContact
import Amazonka.SSMContacts.UpdateContactChannel
import Amazonka.SSMContacts.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SSMContacts'.

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
