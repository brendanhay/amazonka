{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectCampaigns.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Lens
  ( -- * Operations

    -- ** CreateCampaign
    createCampaign_tags,
    createCampaign_connectInstanceId,
    createCampaign_dialerConfig,
    createCampaign_name,
    createCampaign_outboundCallConfig,
    createCampaignResponse_arn,
    createCampaignResponse_id,
    createCampaignResponse_tags,
    createCampaignResponse_httpStatus,

    -- ** DeleteCampaign
    deleteCampaign_id,

    -- ** DeleteConnectInstanceConfig
    deleteConnectInstanceConfig_connectInstanceId,

    -- ** DeleteInstanceOnboardingJob
    deleteInstanceOnboardingJob_connectInstanceId,

    -- ** DescribeCampaign
    describeCampaign_id,
    describeCampaignResponse_campaign,
    describeCampaignResponse_httpStatus,

    -- ** GetCampaignState
    getCampaignState_id,
    getCampaignStateResponse_state,
    getCampaignStateResponse_httpStatus,

    -- ** GetCampaignStateBatch
    getCampaignStateBatch_campaignIds,
    getCampaignStateBatchResponse_failedRequests,
    getCampaignStateBatchResponse_successfulRequests,
    getCampaignStateBatchResponse_httpStatus,

    -- ** GetConnectInstanceConfig
    getConnectInstanceConfig_connectInstanceId,
    getConnectInstanceConfigResponse_connectInstanceConfig,
    getConnectInstanceConfigResponse_httpStatus,

    -- ** GetInstanceOnboardingJobStatus
    getInstanceOnboardingJobStatus_connectInstanceId,
    getInstanceOnboardingJobStatusResponse_connectInstanceOnboardingJobStatus,
    getInstanceOnboardingJobStatusResponse_httpStatus,

    -- ** ListCampaigns
    listCampaigns_filters,
    listCampaigns_maxResults,
    listCampaigns_nextToken,
    listCampaignsResponse_campaignSummaryList,
    listCampaignsResponse_nextToken,
    listCampaignsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_arn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PauseCampaign
    pauseCampaign_id,

    -- ** PutDialRequestBatch
    putDialRequestBatch_dialRequests,
    putDialRequestBatch_id,
    putDialRequestBatchResponse_failedRequests,
    putDialRequestBatchResponse_successfulRequests,
    putDialRequestBatchResponse_httpStatus,

    -- ** ResumeCampaign
    resumeCampaign_id,

    -- ** StartCampaign
    startCampaign_id,

    -- ** StartInstanceOnboardingJob
    startInstanceOnboardingJob_connectInstanceId,
    startInstanceOnboardingJob_encryptionConfig,
    startInstanceOnboardingJobResponse_connectInstanceOnboardingJobStatus,
    startInstanceOnboardingJobResponse_httpStatus,

    -- ** StopCampaign
    stopCampaign_id,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,

    -- ** UpdateCampaignDialerConfig
    updateCampaignDialerConfig_dialerConfig,
    updateCampaignDialerConfig_id,

    -- ** UpdateCampaignName
    updateCampaignName_id,
    updateCampaignName_name,

    -- ** UpdateCampaignOutboundCallConfig
    updateCampaignOutboundCallConfig_answerMachineDetectionConfig,
    updateCampaignOutboundCallConfig_connectContactFlowId,
    updateCampaignOutboundCallConfig_connectSourcePhoneNumber,
    updateCampaignOutboundCallConfig_id,

    -- * Types

    -- ** AnswerMachineDetectionConfig
    answerMachineDetectionConfig_enableAnswerMachineDetection,

    -- ** Campaign
    campaign_tags,
    campaign_arn,
    campaign_connectInstanceId,
    campaign_dialerConfig,
    campaign_id,
    campaign_name,
    campaign_outboundCallConfig,

    -- ** CampaignFilters
    campaignFilters_instanceIdFilter,

    -- ** CampaignSummary
    campaignSummary_arn,
    campaignSummary_connectInstanceId,
    campaignSummary_id,
    campaignSummary_name,

    -- ** DialRequest
    dialRequest_attributes,
    dialRequest_clientToken,
    dialRequest_expirationTime,
    dialRequest_phoneNumber,

    -- ** DialerConfig
    dialerConfig_predictiveDialerConfig,
    dialerConfig_progressiveDialerConfig,

    -- ** EncryptionConfig
    encryptionConfig_encryptionType,
    encryptionConfig_keyArn,
    encryptionConfig_enabled,

    -- ** FailedCampaignStateResponse
    failedCampaignStateResponse_campaignId,
    failedCampaignStateResponse_failureCode,

    -- ** FailedRequest
    failedRequest_clientToken,
    failedRequest_failureCode,
    failedRequest_id,

    -- ** InstanceConfig
    instanceConfig_connectInstanceId,
    instanceConfig_encryptionConfig,
    instanceConfig_serviceLinkedRoleArn,

    -- ** InstanceIdFilter
    instanceIdFilter_operator,
    instanceIdFilter_value,

    -- ** InstanceOnboardingJobStatus
    instanceOnboardingJobStatus_failureCode,
    instanceOnboardingJobStatus_connectInstanceId,
    instanceOnboardingJobStatus_status,

    -- ** OutboundCallConfig
    outboundCallConfig_answerMachineDetectionConfig,
    outboundCallConfig_connectSourcePhoneNumber,
    outboundCallConfig_connectContactFlowId,
    outboundCallConfig_connectQueueId,

    -- ** PredictiveDialerConfig
    predictiveDialerConfig_bandwidthAllocation,

    -- ** ProgressiveDialerConfig
    progressiveDialerConfig_bandwidthAllocation,

    -- ** SuccessfulCampaignStateResponse
    successfulCampaignStateResponse_campaignId,
    successfulCampaignStateResponse_state,

    -- ** SuccessfulRequest
    successfulRequest_clientToken,
    successfulRequest_id,
  )
where

import Amazonka.ConnectCampaigns.CreateCampaign
import Amazonka.ConnectCampaigns.DeleteCampaign
import Amazonka.ConnectCampaigns.DeleteConnectInstanceConfig
import Amazonka.ConnectCampaigns.DeleteInstanceOnboardingJob
import Amazonka.ConnectCampaigns.DescribeCampaign
import Amazonka.ConnectCampaigns.GetCampaignState
import Amazonka.ConnectCampaigns.GetCampaignStateBatch
import Amazonka.ConnectCampaigns.GetConnectInstanceConfig
import Amazonka.ConnectCampaigns.GetInstanceOnboardingJobStatus
import Amazonka.ConnectCampaigns.ListCampaigns
import Amazonka.ConnectCampaigns.ListTagsForResource
import Amazonka.ConnectCampaigns.PauseCampaign
import Amazonka.ConnectCampaigns.PutDialRequestBatch
import Amazonka.ConnectCampaigns.ResumeCampaign
import Amazonka.ConnectCampaigns.StartCampaign
import Amazonka.ConnectCampaigns.StartInstanceOnboardingJob
import Amazonka.ConnectCampaigns.StopCampaign
import Amazonka.ConnectCampaigns.TagResource
import Amazonka.ConnectCampaigns.Types.AnswerMachineDetectionConfig
import Amazonka.ConnectCampaigns.Types.Campaign
import Amazonka.ConnectCampaigns.Types.CampaignFilters
import Amazonka.ConnectCampaigns.Types.CampaignSummary
import Amazonka.ConnectCampaigns.Types.DialRequest
import Amazonka.ConnectCampaigns.Types.DialerConfig
import Amazonka.ConnectCampaigns.Types.EncryptionConfig
import Amazonka.ConnectCampaigns.Types.FailedCampaignStateResponse
import Amazonka.ConnectCampaigns.Types.FailedRequest
import Amazonka.ConnectCampaigns.Types.InstanceConfig
import Amazonka.ConnectCampaigns.Types.InstanceIdFilter
import Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobStatus
import Amazonka.ConnectCampaigns.Types.OutboundCallConfig
import Amazonka.ConnectCampaigns.Types.PredictiveDialerConfig
import Amazonka.ConnectCampaigns.Types.ProgressiveDialerConfig
import Amazonka.ConnectCampaigns.Types.SuccessfulCampaignStateResponse
import Amazonka.ConnectCampaigns.Types.SuccessfulRequest
import Amazonka.ConnectCampaigns.UntagResource
import Amazonka.ConnectCampaigns.UpdateCampaignDialerConfig
import Amazonka.ConnectCampaigns.UpdateCampaignName
import Amazonka.ConnectCampaigns.UpdateCampaignOutboundCallConfig
