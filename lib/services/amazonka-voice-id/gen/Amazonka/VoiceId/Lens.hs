{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VoiceId.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Lens
  ( -- * Operations

    -- ** CreateDomain
    createDomain_tags,
    createDomain_clientToken,
    createDomain_description,
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
    evaluateSessionResponse_fraudDetectionResult,
    evaluateSessionResponse_streamingStatus,
    evaluateSessionResponse_sessionId,
    evaluateSessionResponse_domainId,
    evaluateSessionResponse_sessionName,
    evaluateSessionResponse_httpStatus,

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxResults,
    listDomainsResponse_nextToken,
    listDomainsResponse_domainSummaries,
    listDomainsResponse_httpStatus,

    -- ** ListFraudsterRegistrationJobs
    listFraudsterRegistrationJobs_nextToken,
    listFraudsterRegistrationJobs_jobStatus,
    listFraudsterRegistrationJobs_maxResults,
    listFraudsterRegistrationJobs_domainId,
    listFraudsterRegistrationJobsResponse_nextToken,
    listFraudsterRegistrationJobsResponse_jobSummaries,
    listFraudsterRegistrationJobsResponse_httpStatus,

    -- ** ListSpeakerEnrollmentJobs
    listSpeakerEnrollmentJobs_nextToken,
    listSpeakerEnrollmentJobs_jobStatus,
    listSpeakerEnrollmentJobs_maxResults,
    listSpeakerEnrollmentJobs_domainId,
    listSpeakerEnrollmentJobsResponse_nextToken,
    listSpeakerEnrollmentJobsResponse_jobSummaries,
    listSpeakerEnrollmentJobsResponse_httpStatus,

    -- ** ListSpeakers
    listSpeakers_nextToken,
    listSpeakers_maxResults,
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
    startFraudsterRegistrationJob_registrationConfig,
    startFraudsterRegistrationJob_jobName,
    startFraudsterRegistrationJob_dataAccessRoleArn,
    startFraudsterRegistrationJob_domainId,
    startFraudsterRegistrationJob_inputDataConfig,
    startFraudsterRegistrationJob_outputDataConfig,
    startFraudsterRegistrationJobResponse_job,
    startFraudsterRegistrationJobResponse_httpStatus,

    -- ** StartSpeakerEnrollmentJob
    startSpeakerEnrollmentJob_enrollmentConfig,
    startSpeakerEnrollmentJob_clientToken,
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
    authenticationResult_authenticationResultId,
    authenticationResult_score,
    authenticationResult_configuration,
    authenticationResult_decision,
    authenticationResult_customerSpeakerId,
    authenticationResult_generatedSpeakerId,
    authenticationResult_audioAggregationStartedAt,

    -- ** Domain
    domain_name,
    domain_serverSideEncryptionConfiguration,
    domain_arn,
    domain_description,
    domain_domainStatus,
    domain_domainId,
    domain_createdAt,
    domain_updatedAt,

    -- ** DomainSummary
    domainSummary_name,
    domainSummary_serverSideEncryptionConfiguration,
    domainSummary_arn,
    domainSummary_description,
    domainSummary_domainStatus,
    domainSummary_domainId,
    domainSummary_createdAt,
    domainSummary_updatedAt,

    -- ** EnrollmentConfig
    enrollmentConfig_fraudDetectionConfig,
    enrollmentConfig_existingEnrollmentAction,

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
    fraudDetectionResult_fraudDetectionResultId,
    fraudDetectionResult_riskDetails,
    fraudDetectionResult_configuration,
    fraudDetectionResult_decision,
    fraudDetectionResult_audioAggregationStartedAt,
    fraudDetectionResult_reasons,

    -- ** FraudRiskDetails
    fraudRiskDetails_knownFraudsterRisk,

    -- ** Fraudster
    fraudster_generatedFraudsterId,
    fraudster_domainId,
    fraudster_createdAt,

    -- ** FraudsterRegistrationJob
    fraudsterRegistrationJob_outputDataConfig,
    fraudsterRegistrationJob_jobStatus,
    fraudsterRegistrationJob_registrationConfig,
    fraudsterRegistrationJob_jobName,
    fraudsterRegistrationJob_endedAt,
    fraudsterRegistrationJob_jobId,
    fraudsterRegistrationJob_dataAccessRoleArn,
    fraudsterRegistrationJob_failureDetails,
    fraudsterRegistrationJob_domainId,
    fraudsterRegistrationJob_jobProgress,
    fraudsterRegistrationJob_inputDataConfig,
    fraudsterRegistrationJob_createdAt,

    -- ** FraudsterRegistrationJobSummary
    fraudsterRegistrationJobSummary_jobStatus,
    fraudsterRegistrationJobSummary_jobName,
    fraudsterRegistrationJobSummary_endedAt,
    fraudsterRegistrationJobSummary_jobId,
    fraudsterRegistrationJobSummary_failureDetails,
    fraudsterRegistrationJobSummary_domainId,
    fraudsterRegistrationJobSummary_jobProgress,
    fraudsterRegistrationJobSummary_createdAt,

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

    -- ** Speaker
    speaker_status,
    speaker_customerSpeakerId,
    speaker_generatedSpeakerId,
    speaker_domainId,
    speaker_createdAt,
    speaker_updatedAt,

    -- ** SpeakerEnrollmentJob
    speakerEnrollmentJob_outputDataConfig,
    speakerEnrollmentJob_enrollmentConfig,
    speakerEnrollmentJob_jobStatus,
    speakerEnrollmentJob_jobName,
    speakerEnrollmentJob_endedAt,
    speakerEnrollmentJob_jobId,
    speakerEnrollmentJob_dataAccessRoleArn,
    speakerEnrollmentJob_failureDetails,
    speakerEnrollmentJob_domainId,
    speakerEnrollmentJob_jobProgress,
    speakerEnrollmentJob_inputDataConfig,
    speakerEnrollmentJob_createdAt,

    -- ** SpeakerEnrollmentJobSummary
    speakerEnrollmentJobSummary_jobStatus,
    speakerEnrollmentJobSummary_jobName,
    speakerEnrollmentJobSummary_endedAt,
    speakerEnrollmentJobSummary_jobId,
    speakerEnrollmentJobSummary_failureDetails,
    speakerEnrollmentJobSummary_domainId,
    speakerEnrollmentJobSummary_jobProgress,
    speakerEnrollmentJobSummary_createdAt,

    -- ** SpeakerSummary
    speakerSummary_status,
    speakerSummary_customerSpeakerId,
    speakerSummary_generatedSpeakerId,
    speakerSummary_domainId,
    speakerSummary_createdAt,
    speakerSummary_updatedAt,

    -- ** Tag
    tag_key,
    tag_value,
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
import Amazonka.VoiceId.Types.Speaker
import Amazonka.VoiceId.Types.SpeakerEnrollmentJob
import Amazonka.VoiceId.Types.SpeakerEnrollmentJobSummary
import Amazonka.VoiceId.Types.SpeakerSummary
import Amazonka.VoiceId.Types.Tag
import Amazonka.VoiceId.UntagResource
import Amazonka.VoiceId.UpdateDomain
