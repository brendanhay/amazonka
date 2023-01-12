{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VoiceId.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Lens
  ( -- * Operations

    -- ** CreateDomain
    createDomain_clientToken,
    createDomain_description,
    createDomain_tags,
    createDomain_name,
    createDomain_serverSideEncryptionConfiguration,
    createDomainResponse_domain,
    createDomainResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainId,

    -- ** DeleteFraudster
    deleteFraudster_domainId,
    deleteFraudster_fraudsterId,

    -- ** DeleteSpeaker
    deleteSpeaker_domainId,
    deleteSpeaker_speakerId,

    -- ** DescribeDomain
    describeDomain_domainId,
    describeDomainResponse_domain,
    describeDomainResponse_httpStatus,

    -- ** DescribeFraudster
    describeFraudster_domainId,
    describeFraudster_fraudsterId,
    describeFraudsterResponse_fraudster,
    describeFraudsterResponse_httpStatus,

    -- ** DescribeFraudsterRegistrationJob
    describeFraudsterRegistrationJob_domainId,
    describeFraudsterRegistrationJob_jobId,
    describeFraudsterRegistrationJobResponse_job,
    describeFraudsterRegistrationJobResponse_httpStatus,

    -- ** DescribeSpeaker
    describeSpeaker_domainId,
    describeSpeaker_speakerId,
    describeSpeakerResponse_speaker,
    describeSpeakerResponse_httpStatus,

    -- ** DescribeSpeakerEnrollmentJob
    describeSpeakerEnrollmentJob_domainId,
    describeSpeakerEnrollmentJob_jobId,
    describeSpeakerEnrollmentJobResponse_job,
    describeSpeakerEnrollmentJobResponse_httpStatus,

    -- ** EvaluateSession
    evaluateSession_domainId,
    evaluateSession_sessionNameOrId,
    evaluateSessionResponse_authenticationResult,
    evaluateSessionResponse_domainId,
    evaluateSessionResponse_fraudDetectionResult,
    evaluateSessionResponse_sessionId,
    evaluateSessionResponse_sessionName,
    evaluateSessionResponse_streamingStatus,
    evaluateSessionResponse_httpStatus,

    -- ** ListDomains
    listDomains_maxResults,
    listDomains_nextToken,
    listDomainsResponse_domainSummaries,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,

    -- ** ListFraudsterRegistrationJobs
    listFraudsterRegistrationJobs_jobStatus,
    listFraudsterRegistrationJobs_maxResults,
    listFraudsterRegistrationJobs_nextToken,
    listFraudsterRegistrationJobs_domainId,
    listFraudsterRegistrationJobsResponse_jobSummaries,
    listFraudsterRegistrationJobsResponse_nextToken,
    listFraudsterRegistrationJobsResponse_httpStatus,

    -- ** ListSpeakerEnrollmentJobs
    listSpeakerEnrollmentJobs_jobStatus,
    listSpeakerEnrollmentJobs_maxResults,
    listSpeakerEnrollmentJobs_nextToken,
    listSpeakerEnrollmentJobs_domainId,
    listSpeakerEnrollmentJobsResponse_jobSummaries,
    listSpeakerEnrollmentJobsResponse_nextToken,
    listSpeakerEnrollmentJobsResponse_httpStatus,

    -- ** ListSpeakers
    listSpeakers_maxResults,
    listSpeakers_nextToken,
    listSpeakers_domainId,
    listSpeakersResponse_nextToken,
    listSpeakersResponse_speakerSummaries,
    listSpeakersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** OptOutSpeaker
    optOutSpeaker_domainId,
    optOutSpeaker_speakerId,
    optOutSpeakerResponse_speaker,
    optOutSpeakerResponse_httpStatus,

    -- ** StartFraudsterRegistrationJob
    startFraudsterRegistrationJob_clientToken,
    startFraudsterRegistrationJob_jobName,
    startFraudsterRegistrationJob_registrationConfig,
    startFraudsterRegistrationJob_dataAccessRoleArn,
    startFraudsterRegistrationJob_domainId,
    startFraudsterRegistrationJob_inputDataConfig,
    startFraudsterRegistrationJob_outputDataConfig,
    startFraudsterRegistrationJobResponse_job,
    startFraudsterRegistrationJobResponse_httpStatus,

    -- ** StartSpeakerEnrollmentJob
    startSpeakerEnrollmentJob_clientToken,
    startSpeakerEnrollmentJob_enrollmentConfig,
    startSpeakerEnrollmentJob_jobName,
    startSpeakerEnrollmentJob_dataAccessRoleArn,
    startSpeakerEnrollmentJob_domainId,
    startSpeakerEnrollmentJob_inputDataConfig,
    startSpeakerEnrollmentJob_outputDataConfig,
    startSpeakerEnrollmentJobResponse_job,
    startSpeakerEnrollmentJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDomain
    updateDomain_description,
    updateDomain_domainId,
    updateDomain_name,
    updateDomain_serverSideEncryptionConfiguration,
    updateDomainResponse_domain,
    updateDomainResponse_httpStatus,

    -- * Types

    -- ** AuthenticationConfiguration
    authenticationConfiguration_acceptanceThreshold,

    -- ** AuthenticationResult
    authenticationResult_audioAggregationEndedAt,
    authenticationResult_audioAggregationStartedAt,
    authenticationResult_authenticationResultId,
    authenticationResult_configuration,
    authenticationResult_customerSpeakerId,
    authenticationResult_decision,
    authenticationResult_generatedSpeakerId,
    authenticationResult_score,

    -- ** Domain
    domain_arn,
    domain_createdAt,
    domain_description,
    domain_domainId,
    domain_domainStatus,
    domain_name,
    domain_serverSideEncryptionConfiguration,
    domain_serverSideEncryptionUpdateDetails,
    domain_updatedAt,

    -- ** DomainSummary
    domainSummary_arn,
    domainSummary_createdAt,
    domainSummary_description,
    domainSummary_domainId,
    domainSummary_domainStatus,
    domainSummary_name,
    domainSummary_serverSideEncryptionConfiguration,
    domainSummary_serverSideEncryptionUpdateDetails,
    domainSummary_updatedAt,

    -- ** EnrollmentConfig
    enrollmentConfig_existingEnrollmentAction,
    enrollmentConfig_fraudDetectionConfig,

    -- ** EnrollmentJobFraudDetectionConfig
    enrollmentJobFraudDetectionConfig_fraudDetectionAction,
    enrollmentJobFraudDetectionConfig_riskThreshold,

    -- ** FailureDetails
    failureDetails_message,
    failureDetails_statusCode,

    -- ** FraudDetectionConfiguration
    fraudDetectionConfiguration_riskThreshold,

    -- ** FraudDetectionResult
    fraudDetectionResult_audioAggregationEndedAt,
    fraudDetectionResult_audioAggregationStartedAt,
    fraudDetectionResult_configuration,
    fraudDetectionResult_decision,
    fraudDetectionResult_fraudDetectionResultId,
    fraudDetectionResult_reasons,
    fraudDetectionResult_riskDetails,

    -- ** FraudRiskDetails
    fraudRiskDetails_knownFraudsterRisk,
    fraudRiskDetails_voiceSpoofingRisk,

    -- ** Fraudster
    fraudster_createdAt,
    fraudster_domainId,
    fraudster_generatedFraudsterId,

    -- ** FraudsterRegistrationJob
    fraudsterRegistrationJob_createdAt,
    fraudsterRegistrationJob_dataAccessRoleArn,
    fraudsterRegistrationJob_domainId,
    fraudsterRegistrationJob_endedAt,
    fraudsterRegistrationJob_failureDetails,
    fraudsterRegistrationJob_inputDataConfig,
    fraudsterRegistrationJob_jobId,
    fraudsterRegistrationJob_jobName,
    fraudsterRegistrationJob_jobProgress,
    fraudsterRegistrationJob_jobStatus,
    fraudsterRegistrationJob_outputDataConfig,
    fraudsterRegistrationJob_registrationConfig,

    -- ** FraudsterRegistrationJobSummary
    fraudsterRegistrationJobSummary_createdAt,
    fraudsterRegistrationJobSummary_domainId,
    fraudsterRegistrationJobSummary_endedAt,
    fraudsterRegistrationJobSummary_failureDetails,
    fraudsterRegistrationJobSummary_jobId,
    fraudsterRegistrationJobSummary_jobName,
    fraudsterRegistrationJobSummary_jobProgress,
    fraudsterRegistrationJobSummary_jobStatus,

    -- ** InputDataConfig
    inputDataConfig_s3Uri,

    -- ** JobProgress
    jobProgress_percentComplete,

    -- ** KnownFraudsterRisk
    knownFraudsterRisk_generatedFraudsterId,
    knownFraudsterRisk_riskScore,

    -- ** OutputDataConfig
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3Uri,

    -- ** RegistrationConfig
    registrationConfig_duplicateRegistrationAction,
    registrationConfig_fraudsterSimilarityThreshold,

    -- ** ServerSideEncryptionConfiguration
    serverSideEncryptionConfiguration_kmsKeyId,

    -- ** ServerSideEncryptionUpdateDetails
    serverSideEncryptionUpdateDetails_message,
    serverSideEncryptionUpdateDetails_oldKmsKeyId,
    serverSideEncryptionUpdateDetails_updateStatus,

    -- ** Speaker
    speaker_createdAt,
    speaker_customerSpeakerId,
    speaker_domainId,
    speaker_generatedSpeakerId,
    speaker_lastAccessedAt,
    speaker_status,
    speaker_updatedAt,

    -- ** SpeakerEnrollmentJob
    speakerEnrollmentJob_createdAt,
    speakerEnrollmentJob_dataAccessRoleArn,
    speakerEnrollmentJob_domainId,
    speakerEnrollmentJob_endedAt,
    speakerEnrollmentJob_enrollmentConfig,
    speakerEnrollmentJob_failureDetails,
    speakerEnrollmentJob_inputDataConfig,
    speakerEnrollmentJob_jobId,
    speakerEnrollmentJob_jobName,
    speakerEnrollmentJob_jobProgress,
    speakerEnrollmentJob_jobStatus,
    speakerEnrollmentJob_outputDataConfig,

    -- ** SpeakerEnrollmentJobSummary
    speakerEnrollmentJobSummary_createdAt,
    speakerEnrollmentJobSummary_domainId,
    speakerEnrollmentJobSummary_endedAt,
    speakerEnrollmentJobSummary_failureDetails,
    speakerEnrollmentJobSummary_jobId,
    speakerEnrollmentJobSummary_jobName,
    speakerEnrollmentJobSummary_jobProgress,
    speakerEnrollmentJobSummary_jobStatus,

    -- ** SpeakerSummary
    speakerSummary_createdAt,
    speakerSummary_customerSpeakerId,
    speakerSummary_domainId,
    speakerSummary_generatedSpeakerId,
    speakerSummary_lastAccessedAt,
    speakerSummary_status,
    speakerSummary_updatedAt,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** VoiceSpoofingRisk
    voiceSpoofingRisk_riskScore,
  )
where

import Amazonka.VoiceId.CreateDomain
import Amazonka.VoiceId.DeleteDomain
import Amazonka.VoiceId.DeleteFraudster
import Amazonka.VoiceId.DeleteSpeaker
import Amazonka.VoiceId.DescribeDomain
import Amazonka.VoiceId.DescribeFraudster
import Amazonka.VoiceId.DescribeFraudsterRegistrationJob
import Amazonka.VoiceId.DescribeSpeaker
import Amazonka.VoiceId.DescribeSpeakerEnrollmentJob
import Amazonka.VoiceId.EvaluateSession
import Amazonka.VoiceId.ListDomains
import Amazonka.VoiceId.ListFraudsterRegistrationJobs
import Amazonka.VoiceId.ListSpeakerEnrollmentJobs
import Amazonka.VoiceId.ListSpeakers
import Amazonka.VoiceId.ListTagsForResource
import Amazonka.VoiceId.OptOutSpeaker
import Amazonka.VoiceId.StartFraudsterRegistrationJob
import Amazonka.VoiceId.StartSpeakerEnrollmentJob
import Amazonka.VoiceId.TagResource
import Amazonka.VoiceId.Types.AuthenticationConfiguration
import Amazonka.VoiceId.Types.AuthenticationResult
import Amazonka.VoiceId.Types.Domain
import Amazonka.VoiceId.Types.DomainSummary
import Amazonka.VoiceId.Types.EnrollmentConfig
import Amazonka.VoiceId.Types.EnrollmentJobFraudDetectionConfig
import Amazonka.VoiceId.Types.FailureDetails
import Amazonka.VoiceId.Types.FraudDetectionConfiguration
import Amazonka.VoiceId.Types.FraudDetectionResult
import Amazonka.VoiceId.Types.FraudRiskDetails
import Amazonka.VoiceId.Types.Fraudster
import Amazonka.VoiceId.Types.FraudsterRegistrationJob
import Amazonka.VoiceId.Types.FraudsterRegistrationJobSummary
import Amazonka.VoiceId.Types.InputDataConfig
import Amazonka.VoiceId.Types.JobProgress
import Amazonka.VoiceId.Types.KnownFraudsterRisk
import Amazonka.VoiceId.Types.OutputDataConfig
import Amazonka.VoiceId.Types.RegistrationConfig
import Amazonka.VoiceId.Types.ServerSideEncryptionConfiguration
import Amazonka.VoiceId.Types.ServerSideEncryptionUpdateDetails
import Amazonka.VoiceId.Types.Speaker
import Amazonka.VoiceId.Types.SpeakerEnrollmentJob
import Amazonka.VoiceId.Types.SpeakerEnrollmentJobSummary
import Amazonka.VoiceId.Types.SpeakerSummary
import Amazonka.VoiceId.Types.Tag
import Amazonka.VoiceId.Types.VoiceSpoofingRisk
import Amazonka.VoiceId.UntagResource
import Amazonka.VoiceId.UpdateDomain
