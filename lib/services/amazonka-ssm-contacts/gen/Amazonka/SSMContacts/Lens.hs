{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMContacts.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Lens
  ( -- * Operations

    -- ** ListPagesByEngagement
    listPagesByEngagement_nextToken,
    listPagesByEngagement_maxResults,
    listPagesByEngagement_engagementId,
    listPagesByEngagementResponse_nextToken,
    listPagesByEngagementResponse_httpStatus,
    listPagesByEngagementResponse_pages,

    -- ** ListEngagements
    listEngagements_timeRangeValue,
    listEngagements_nextToken,
    listEngagements_incidentId,
    listEngagements_maxResults,
    listEngagementsResponse_nextToken,
    listEngagementsResponse_httpStatus,
    listEngagementsResponse_engagements,

    -- ** ListContactChannels
    listContactChannels_nextToken,
    listContactChannels_maxResults,
    listContactChannels_contactId,
    listContactChannelsResponse_nextToken,
    listContactChannelsResponse_httpStatus,
    listContactChannelsResponse_contactChannels,

    -- ** ActivateContactChannel
    activateContactChannel_contactChannelId,
    activateContactChannel_activationCode,
    activateContactChannelResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartEngagement
    startEngagement_idempotencyToken,
    startEngagement_publicSubject,
    startEngagement_publicContent,
    startEngagement_incidentId,
    startEngagement_contactId,
    startEngagement_sender,
    startEngagement_subject,
    startEngagement_content,
    startEngagementResponse_httpStatus,
    startEngagementResponse_engagementArn,

    -- ** DeactivateContactChannel
    deactivateContactChannel_contactChannelId,
    deactivateContactChannelResponse_httpStatus,

    -- ** AcceptPage
    acceptPage_note,
    acceptPage_contactChannelId,
    acceptPage_acceptCodeValidation,
    acceptPage_pageId,
    acceptPage_acceptType,
    acceptPage_acceptCode,
    acceptPageResponse_httpStatus,

    -- ** ListPageReceipts
    listPageReceipts_nextToken,
    listPageReceipts_maxResults,
    listPageReceipts_pageId,
    listPageReceiptsResponse_nextToken,
    listPageReceiptsResponse_receipts,
    listPageReceiptsResponse_httpStatus,

    -- ** GetContact
    getContact_contactId,
    getContactResponse_displayName,
    getContactResponse_httpStatus,
    getContactResponse_contactArn,
    getContactResponse_alias,
    getContactResponse_type,
    getContactResponse_plan,

    -- ** DescribePage
    describePage_pageId,
    describePageResponse_readTime,
    describePageResponse_publicSubject,
    describePageResponse_publicContent,
    describePageResponse_deliveryTime,
    describePageResponse_incidentId,
    describePageResponse_sentTime,
    describePageResponse_httpStatus,
    describePageResponse_pageArn,
    describePageResponse_engagementArn,
    describePageResponse_contactArn,
    describePageResponse_sender,
    describePageResponse_subject,
    describePageResponse_content,

    -- ** DeleteContact
    deleteContact_contactId,
    deleteContactResponse_httpStatus,

    -- ** UpdateContact
    updateContact_plan,
    updateContact_displayName,
    updateContact_contactId,
    updateContactResponse_httpStatus,

    -- ** CreateContact
    createContact_idempotencyToken,
    createContact_displayName,
    createContact_tags,
    createContact_alias,
    createContact_type,
    createContact_plan,
    createContactResponse_httpStatus,
    createContactResponse_contactArn,

    -- ** CreateContactChannel
    createContactChannel_idempotencyToken,
    createContactChannel_deferActivation,
    createContactChannel_contactId,
    createContactChannel_name,
    createContactChannel_type,
    createContactChannel_deliveryAddress,
    createContactChannelResponse_httpStatus,
    createContactChannelResponse_contactChannelArn,

    -- ** DeleteContactChannel
    deleteContactChannel_contactChannelId,
    deleteContactChannelResponse_httpStatus,

    -- ** UpdateContactChannel
    updateContactChannel_name,
    updateContactChannel_deliveryAddress,
    updateContactChannel_contactChannelId,
    updateContactChannelResponse_httpStatus,

    -- ** GetContactChannel
    getContactChannel_contactChannelId,
    getContactChannelResponse_activationStatus,
    getContactChannelResponse_httpStatus,
    getContactChannelResponse_contactArn,
    getContactChannelResponse_contactChannelArn,
    getContactChannelResponse_name,
    getContactChannelResponse_type,
    getContactChannelResponse_deliveryAddress,

    -- ** SendActivationCode
    sendActivationCode_contactChannelId,
    sendActivationCodeResponse_httpStatus,

    -- ** StopEngagement
    stopEngagement_reason,
    stopEngagement_engagementId,
    stopEngagementResponse_httpStatus,

    -- ** DescribeEngagement
    describeEngagement_engagementId,
    describeEngagementResponse_publicSubject,
    describeEngagementResponse_startTime,
    describeEngagementResponse_publicContent,
    describeEngagementResponse_stopTime,
    describeEngagementResponse_incidentId,
    describeEngagementResponse_httpStatus,
    describeEngagementResponse_contactArn,
    describeEngagementResponse_engagementArn,
    describeEngagementResponse_sender,
    describeEngagementResponse_subject,
    describeEngagementResponse_content,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetContactPolicy
    getContactPolicy_contactArn,
    getContactPolicyResponse_contactArn,
    getContactPolicyResponse_policy,
    getContactPolicyResponse_httpStatus,

    -- ** PutContactPolicy
    putContactPolicy_contactArn,
    putContactPolicy_policy,
    putContactPolicyResponse_httpStatus,

    -- ** ListContacts
    listContacts_aliasPrefix,
    listContacts_nextToken,
    listContacts_type,
    listContacts_maxResults,
    listContactsResponse_nextToken,
    listContactsResponse_contacts,
    listContactsResponse_httpStatus,

    -- ** ListPagesByContact
    listPagesByContact_nextToken,
    listPagesByContact_maxResults,
    listPagesByContact_contactId,
    listPagesByContactResponse_nextToken,
    listPagesByContactResponse_httpStatus,
    listPagesByContactResponse_pages,

    -- * Types

    -- ** ChannelTargetInfo
    channelTargetInfo_retryIntervalInMinutes,
    channelTargetInfo_contactChannelId,

    -- ** Contact
    contact_displayName,
    contact_contactArn,
    contact_alias,
    contact_type,

    -- ** ContactChannel
    contactChannel_type,
    contactChannel_contactChannelArn,
    contactChannel_contactArn,
    contactChannel_name,
    contactChannel_deliveryAddress,
    contactChannel_activationStatus,

    -- ** ContactChannelAddress
    contactChannelAddress_simpleAddress,

    -- ** ContactTargetInfo
    contactTargetInfo_contactId,
    contactTargetInfo_isEssential,

    -- ** Engagement
    engagement_startTime,
    engagement_stopTime,
    engagement_incidentId,
    engagement_engagementArn,
    engagement_contactArn,
    engagement_sender,

    -- ** Page
    page_readTime,
    page_deliveryTime,
    page_incidentId,
    page_sentTime,
    page_pageArn,
    page_engagementArn,
    page_contactArn,
    page_sender,

    -- ** Plan
    plan_stages,

    -- ** Receipt
    receipt_receiptInfo,
    receipt_contactChannelArn,
    receipt_receiptType,
    receipt_receiptTime,

    -- ** Stage
    stage_durationInMinutes,
    stage_targets,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** Target
    target_channelTargetInfo,
    target_contactTargetInfo,

    -- ** TimeRange
    timeRange_startTime,
    timeRange_endTime,
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
import Amazonka.SSMContacts.Types.ChannelTargetInfo
import Amazonka.SSMContacts.Types.Contact
import Amazonka.SSMContacts.Types.ContactChannel
import Amazonka.SSMContacts.Types.ContactChannelAddress
import Amazonka.SSMContacts.Types.ContactTargetInfo
import Amazonka.SSMContacts.Types.Engagement
import Amazonka.SSMContacts.Types.Page
import Amazonka.SSMContacts.Types.Plan
import Amazonka.SSMContacts.Types.Receipt
import Amazonka.SSMContacts.Types.Stage
import Amazonka.SSMContacts.Types.Tag
import Amazonka.SSMContacts.Types.Target
import Amazonka.SSMContacts.Types.TimeRange
import Amazonka.SSMContacts.UntagResource
import Amazonka.SSMContacts.UpdateContact
import Amazonka.SSMContacts.UpdateContactChannel
