{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.SSMContacts
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
module Network.AWS.SSMContacts
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

import Network.AWS.SSMContacts.AcceptPage
import Network.AWS.SSMContacts.ActivateContactChannel
import Network.AWS.SSMContacts.CreateContact
import Network.AWS.SSMContacts.CreateContactChannel
import Network.AWS.SSMContacts.DeactivateContactChannel
import Network.AWS.SSMContacts.DeleteContact
import Network.AWS.SSMContacts.DeleteContactChannel
import Network.AWS.SSMContacts.DescribeEngagement
import Network.AWS.SSMContacts.DescribePage
import Network.AWS.SSMContacts.GetContact
import Network.AWS.SSMContacts.GetContactChannel
import Network.AWS.SSMContacts.GetContactPolicy
import Network.AWS.SSMContacts.Lens
import Network.AWS.SSMContacts.ListContactChannels
import Network.AWS.SSMContacts.ListContacts
import Network.AWS.SSMContacts.ListEngagements
import Network.AWS.SSMContacts.ListPageReceipts
import Network.AWS.SSMContacts.ListPagesByContact
import Network.AWS.SSMContacts.ListPagesByEngagement
import Network.AWS.SSMContacts.ListTagsForResource
import Network.AWS.SSMContacts.PutContactPolicy
import Network.AWS.SSMContacts.SendActivationCode
import Network.AWS.SSMContacts.StartEngagement
import Network.AWS.SSMContacts.StopEngagement
import Network.AWS.SSMContacts.TagResource
import Network.AWS.SSMContacts.Types
import Network.AWS.SSMContacts.UntagResource
import Network.AWS.SSMContacts.UpdateContact
import Network.AWS.SSMContacts.UpdateContactChannel
import Network.AWS.SSMContacts.Waiters

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
