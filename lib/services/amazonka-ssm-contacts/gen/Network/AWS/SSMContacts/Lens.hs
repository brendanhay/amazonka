{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSMContacts.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMContacts.Lens
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
import Network.AWS.SSMContacts.Types.ChannelTargetInfo
import Network.AWS.SSMContacts.Types.Contact
import Network.AWS.SSMContacts.Types.ContactChannel
import Network.AWS.SSMContacts.Types.ContactChannelAddress
import Network.AWS.SSMContacts.Types.ContactTargetInfo
import Network.AWS.SSMContacts.Types.Engagement
import Network.AWS.SSMContacts.Types.Page
import Network.AWS.SSMContacts.Types.Plan
import Network.AWS.SSMContacts.Types.Receipt
import Network.AWS.SSMContacts.Types.Stage
import Network.AWS.SSMContacts.Types.Tag
import Network.AWS.SSMContacts.Types.Target
import Network.AWS.SSMContacts.Types.TimeRange
import Network.AWS.SSMContacts.UntagResource
import Network.AWS.SSMContacts.UpdateContact
import Network.AWS.SSMContacts.UpdateContactChannel
