{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMContacts.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Lens
  ( -- * Operations

    -- ** AcceptPage
    acceptPage_acceptCodeValidation,
    acceptPage_contactChannelId,
    acceptPage_note,
    acceptPage_pageId,
    acceptPage_acceptType,
    acceptPage_acceptCode,
    acceptPageResponse_httpStatus,

    -- ** ActivateContactChannel
    activateContactChannel_contactChannelId,
    activateContactChannel_activationCode,
    activateContactChannelResponse_httpStatus,

    -- ** CreateContact
    createContact_displayName,
    createContact_idempotencyToken,
    createContact_tags,
    createContact_alias,
    createContact_type,
    createContact_plan,
    createContactResponse_httpStatus,
    createContactResponse_contactArn,

    -- ** CreateContactChannel
    createContactChannel_deferActivation,
    createContactChannel_idempotencyToken,
    createContactChannel_contactId,
    createContactChannel_name,
    createContactChannel_type,
    createContactChannel_deliveryAddress,
    createContactChannelResponse_httpStatus,
    createContactChannelResponse_contactChannelArn,

    -- ** CreateRotation
    createRotation_idempotencyToken,
    createRotation_startTime,
    createRotation_tags,
    createRotation_name,
    createRotation_contactIds,
    createRotation_timeZoneId,
    createRotation_recurrence,
    createRotationResponse_httpStatus,
    createRotationResponse_rotationArn,

    -- ** CreateRotationOverride
    createRotationOverride_idempotencyToken,
    createRotationOverride_rotationId,
    createRotationOverride_newContactIds,
    createRotationOverride_startTime,
    createRotationOverride_endTime,
    createRotationOverrideResponse_httpStatus,
    createRotationOverrideResponse_rotationOverrideId,

    -- ** DeactivateContactChannel
    deactivateContactChannel_contactChannelId,
    deactivateContactChannelResponse_httpStatus,

    -- ** DeleteContact
    deleteContact_contactId,
    deleteContactResponse_httpStatus,

    -- ** DeleteContactChannel
    deleteContactChannel_contactChannelId,
    deleteContactChannelResponse_httpStatus,

    -- ** DeleteRotation
    deleteRotation_rotationId,
    deleteRotationResponse_httpStatus,

    -- ** DeleteRotationOverride
    deleteRotationOverride_rotationId,
    deleteRotationOverride_rotationOverrideId,
    deleteRotationOverrideResponse_httpStatus,

    -- ** DescribeEngagement
    describeEngagement_engagementId,
    describeEngagementResponse_incidentId,
    describeEngagementResponse_publicContent,
    describeEngagementResponse_publicSubject,
    describeEngagementResponse_startTime,
    describeEngagementResponse_stopTime,
    describeEngagementResponse_httpStatus,
    describeEngagementResponse_contactArn,
    describeEngagementResponse_engagementArn,
    describeEngagementResponse_sender,
    describeEngagementResponse_subject,
    describeEngagementResponse_content,

    -- ** DescribePage
    describePage_pageId,
    describePageResponse_deliveryTime,
    describePageResponse_incidentId,
    describePageResponse_publicContent,
    describePageResponse_publicSubject,
    describePageResponse_readTime,
    describePageResponse_sentTime,
    describePageResponse_httpStatus,
    describePageResponse_pageArn,
    describePageResponse_engagementArn,
    describePageResponse_contactArn,
    describePageResponse_sender,
    describePageResponse_subject,
    describePageResponse_content,

    -- ** GetContact
    getContact_contactId,
    getContactResponse_displayName,
    getContactResponse_httpStatus,
    getContactResponse_contactArn,
    getContactResponse_alias,
    getContactResponse_type,
    getContactResponse_plan,

    -- ** GetContactChannel
    getContactChannel_contactChannelId,
    getContactChannelResponse_activationStatus,
    getContactChannelResponse_httpStatus,
    getContactChannelResponse_contactArn,
    getContactChannelResponse_contactChannelArn,
    getContactChannelResponse_name,
    getContactChannelResponse_type,
    getContactChannelResponse_deliveryAddress,

    -- ** GetContactPolicy
    getContactPolicy_contactArn,
    getContactPolicyResponse_contactArn,
    getContactPolicyResponse_policy,
    getContactPolicyResponse_httpStatus,

    -- ** GetRotation
    getRotation_rotationId,
    getRotationResponse_httpStatus,
    getRotationResponse_rotationArn,
    getRotationResponse_name,
    getRotationResponse_contactIds,
    getRotationResponse_startTime,
    getRotationResponse_timeZoneId,
    getRotationResponse_recurrence,

    -- ** GetRotationOverride
    getRotationOverride_rotationId,
    getRotationOverride_rotationOverrideId,
    getRotationOverrideResponse_createTime,
    getRotationOverrideResponse_endTime,
    getRotationOverrideResponse_newContactIds,
    getRotationOverrideResponse_rotationArn,
    getRotationOverrideResponse_rotationOverrideId,
    getRotationOverrideResponse_startTime,
    getRotationOverrideResponse_httpStatus,

    -- ** ListContactChannels
    listContactChannels_maxResults,
    listContactChannels_nextToken,
    listContactChannels_contactId,
    listContactChannelsResponse_nextToken,
    listContactChannelsResponse_httpStatus,
    listContactChannelsResponse_contactChannels,

    -- ** ListContacts
    listContacts_aliasPrefix,
    listContacts_maxResults,
    listContacts_nextToken,
    listContacts_type,
    listContactsResponse_contacts,
    listContactsResponse_nextToken,
    listContactsResponse_httpStatus,

    -- ** ListEngagements
    listEngagements_incidentId,
    listEngagements_maxResults,
    listEngagements_nextToken,
    listEngagements_timeRangeValue,
    listEngagementsResponse_nextToken,
    listEngagementsResponse_httpStatus,
    listEngagementsResponse_engagements,

    -- ** ListPageReceipts
    listPageReceipts_maxResults,
    listPageReceipts_nextToken,
    listPageReceipts_pageId,
    listPageReceiptsResponse_nextToken,
    listPageReceiptsResponse_receipts,
    listPageReceiptsResponse_httpStatus,

    -- ** ListPageResolutions
    listPageResolutions_nextToken,
    listPageResolutions_pageId,
    listPageResolutionsResponse_nextToken,
    listPageResolutionsResponse_httpStatus,
    listPageResolutionsResponse_pageResolutions,

    -- ** ListPagesByContact
    listPagesByContact_maxResults,
    listPagesByContact_nextToken,
    listPagesByContact_contactId,
    listPagesByContactResponse_nextToken,
    listPagesByContactResponse_httpStatus,
    listPagesByContactResponse_pages,

    -- ** ListPagesByEngagement
    listPagesByEngagement_maxResults,
    listPagesByEngagement_nextToken,
    listPagesByEngagement_engagementId,
    listPagesByEngagementResponse_nextToken,
    listPagesByEngagementResponse_httpStatus,
    listPagesByEngagementResponse_pages,

    -- ** ListPreviewRotationShifts
    listPreviewRotationShifts_maxResults,
    listPreviewRotationShifts_nextToken,
    listPreviewRotationShifts_overrides,
    listPreviewRotationShifts_rotationStartTime,
    listPreviewRotationShifts_startTime,
    listPreviewRotationShifts_endTime,
    listPreviewRotationShifts_members,
    listPreviewRotationShifts_timeZoneId,
    listPreviewRotationShifts_recurrence,
    listPreviewRotationShiftsResponse_nextToken,
    listPreviewRotationShiftsResponse_rotationShifts,
    listPreviewRotationShiftsResponse_httpStatus,

    -- ** ListRotationOverrides
    listRotationOverrides_maxResults,
    listRotationOverrides_nextToken,
    listRotationOverrides_rotationId,
    listRotationOverrides_startTime,
    listRotationOverrides_endTime,
    listRotationOverridesResponse_nextToken,
    listRotationOverridesResponse_rotationOverrides,
    listRotationOverridesResponse_httpStatus,

    -- ** ListRotationShifts
    listRotationShifts_maxResults,
    listRotationShifts_nextToken,
    listRotationShifts_startTime,
    listRotationShifts_rotationId,
    listRotationShifts_endTime,
    listRotationShiftsResponse_nextToken,
    listRotationShiftsResponse_rotationShifts,
    listRotationShiftsResponse_httpStatus,

    -- ** ListRotations
    listRotations_maxResults,
    listRotations_nextToken,
    listRotations_rotationNamePrefix,
    listRotationsResponse_nextToken,
    listRotationsResponse_httpStatus,
    listRotationsResponse_rotations,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutContactPolicy
    putContactPolicy_contactArn,
    putContactPolicy_policy,
    putContactPolicyResponse_httpStatus,

    -- ** SendActivationCode
    sendActivationCode_contactChannelId,
    sendActivationCodeResponse_httpStatus,

    -- ** StartEngagement
    startEngagement_idempotencyToken,
    startEngagement_incidentId,
    startEngagement_publicContent,
    startEngagement_publicSubject,
    startEngagement_contactId,
    startEngagement_sender,
    startEngagement_subject,
    startEngagement_content,
    startEngagementResponse_httpStatus,
    startEngagementResponse_engagementArn,

    -- ** StopEngagement
    stopEngagement_reason,
    stopEngagement_engagementId,
    stopEngagementResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateContact
    updateContact_displayName,
    updateContact_plan,
    updateContact_contactId,
    updateContactResponse_httpStatus,

    -- ** UpdateContactChannel
    updateContactChannel_deliveryAddress,
    updateContactChannel_name,
    updateContactChannel_contactChannelId,
    updateContactChannelResponse_httpStatus,

    -- ** UpdateRotation
    updateRotation_contactIds,
    updateRotation_startTime,
    updateRotation_timeZoneId,
    updateRotation_rotationId,
    updateRotation_recurrence,
    updateRotationResponse_httpStatus,

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

    -- ** CoverageTime
    coverageTime_end,
    coverageTime_start,

    -- ** Engagement
    engagement_incidentId,
    engagement_startTime,
    engagement_stopTime,
    engagement_engagementArn,
    engagement_contactArn,
    engagement_sender,

    -- ** HandOffTime
    handOffTime_hourOfDay,
    handOffTime_minuteOfHour,

    -- ** MonthlySetting
    monthlySetting_dayOfMonth,
    monthlySetting_handOffTime,

    -- ** Page
    page_deliveryTime,
    page_incidentId,
    page_readTime,
    page_sentTime,
    page_pageArn,
    page_engagementArn,
    page_contactArn,
    page_sender,

    -- ** Plan
    plan_rotationIds,
    plan_stages,

    -- ** PreviewOverride
    previewOverride_endTime,
    previewOverride_newMembers,
    previewOverride_startTime,

    -- ** Receipt
    receipt_contactChannelArn,
    receipt_receiptInfo,
    receipt_receiptType,
    receipt_receiptTime,

    -- ** RecurrenceSettings
    recurrenceSettings_dailySettings,
    recurrenceSettings_monthlySettings,
    recurrenceSettings_shiftCoverages,
    recurrenceSettings_weeklySettings,
    recurrenceSettings_numberOfOnCalls,
    recurrenceSettings_recurrenceMultiplier,

    -- ** ResolutionContact
    resolutionContact_stageIndex,
    resolutionContact_contactArn,
    resolutionContact_type,

    -- ** Rotation
    rotation_contactIds,
    rotation_recurrence,
    rotation_startTime,
    rotation_timeZoneId,
    rotation_rotationArn,
    rotation_name,

    -- ** RotationOverride
    rotationOverride_rotationOverrideId,
    rotationOverride_newContactIds,
    rotationOverride_startTime,
    rotationOverride_endTime,
    rotationOverride_createTime,

    -- ** RotationShift
    rotationShift_contactIds,
    rotationShift_shiftDetails,
    rotationShift_type,
    rotationShift_startTime,
    rotationShift_endTime,

    -- ** ShiftDetails
    shiftDetails_overriddenContactIds,

    -- ** Stage
    stage_durationInMinutes,
    stage_targets,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Target
    target_channelTargetInfo,
    target_contactTargetInfo,

    -- ** TimeRange
    timeRange_endTime,
    timeRange_startTime,

    -- ** WeeklySetting
    weeklySetting_dayOfWeek,
    weeklySetting_handOffTime,
  )
where

import Amazonka.SSMContacts.AcceptPage
import Amazonka.SSMContacts.ActivateContactChannel
import Amazonka.SSMContacts.CreateContact
import Amazonka.SSMContacts.CreateContactChannel
import Amazonka.SSMContacts.CreateRotation
import Amazonka.SSMContacts.CreateRotationOverride
import Amazonka.SSMContacts.DeactivateContactChannel
import Amazonka.SSMContacts.DeleteContact
import Amazonka.SSMContacts.DeleteContactChannel
import Amazonka.SSMContacts.DeleteRotation
import Amazonka.SSMContacts.DeleteRotationOverride
import Amazonka.SSMContacts.DescribeEngagement
import Amazonka.SSMContacts.DescribePage
import Amazonka.SSMContacts.GetContact
import Amazonka.SSMContacts.GetContactChannel
import Amazonka.SSMContacts.GetContactPolicy
import Amazonka.SSMContacts.GetRotation
import Amazonka.SSMContacts.GetRotationOverride
import Amazonka.SSMContacts.ListContactChannels
import Amazonka.SSMContacts.ListContacts
import Amazonka.SSMContacts.ListEngagements
import Amazonka.SSMContacts.ListPageReceipts
import Amazonka.SSMContacts.ListPageResolutions
import Amazonka.SSMContacts.ListPagesByContact
import Amazonka.SSMContacts.ListPagesByEngagement
import Amazonka.SSMContacts.ListPreviewRotationShifts
import Amazonka.SSMContacts.ListRotationOverrides
import Amazonka.SSMContacts.ListRotationShifts
import Amazonka.SSMContacts.ListRotations
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
import Amazonka.SSMContacts.Types.CoverageTime
import Amazonka.SSMContacts.Types.Engagement
import Amazonka.SSMContacts.Types.HandOffTime
import Amazonka.SSMContacts.Types.MonthlySetting
import Amazonka.SSMContacts.Types.Page
import Amazonka.SSMContacts.Types.Plan
import Amazonka.SSMContacts.Types.PreviewOverride
import Amazonka.SSMContacts.Types.Receipt
import Amazonka.SSMContacts.Types.RecurrenceSettings
import Amazonka.SSMContacts.Types.ResolutionContact
import Amazonka.SSMContacts.Types.Rotation
import Amazonka.SSMContacts.Types.RotationOverride
import Amazonka.SSMContacts.Types.RotationShift
import Amazonka.SSMContacts.Types.ShiftDetails
import Amazonka.SSMContacts.Types.Stage
import Amazonka.SSMContacts.Types.Tag
import Amazonka.SSMContacts.Types.Target
import Amazonka.SSMContacts.Types.TimeRange
import Amazonka.SSMContacts.Types.WeeklySetting
import Amazonka.SSMContacts.UntagResource
import Amazonka.SSMContacts.UpdateContact
import Amazonka.SSMContacts.UpdateContactChannel
import Amazonka.SSMContacts.UpdateRotation
