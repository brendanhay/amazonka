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

    -- ** DescribeFraudsterRegistrationJob
    describeFraudsterRegistrationJob_domainId,
    describeFraudsterRegistrationJob_jobId,
    describeFraudsterRegistrationJobResponse_job,
    describeFraudsterRegistrationJobResponse_httpStatus,

    -- ** DeleteSpeaker
    deleteSpeaker_domainId,
    deleteSpeaker_speakerId,

    -- ** ListSpeakers
    listSpeakers_nextToken,
    listSpeakers_maxResults,
    listSpeakers_domainId,
    listSpeakersResponse_speakerSummaries,
    listSpeakersResponse_nextToken,
    listSpeakersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** EvaluateSession
    evaluateSession_domainId,
    evaluateSession_sessionNameOrId,
    evaluateSessionResponse_fraudDetectionResult,
    evaluateSessionResponse_streamingStatus,
    evaluateSessionResponse_authenticationResult,
    evaluateSessionResponse_domainId,
    evaluateSessionResponse_sessionId,
    evaluateSessionResponse_sessionName,
    evaluateSessionResponse_httpStatus,

    -- ** DescribeSpeakerEnrollmentJob
    describeSpeakerEnrollmentJob_domainId,
    describeSpeakerEnrollmentJob_jobId,
    describeSpeakerEnrollmentJobResponse_job,
    describeSpeakerEnrollmentJobResponse_httpStatus,

    -- ** DeleteFraudster
    deleteFraudster_domainId,
    deleteFraudster_fraudsterId,

    -- ** ListFraudsterRegistrationJobs
    listFraudsterRegistrationJobs_nextToken,
    listFraudsterRegistrationJobs_jobStatus,
    listFraudsterRegistrationJobs_maxResults,
    listFraudsterRegistrationJobs_domainId,
    listFraudsterRegistrationJobsResponse_nextToken,
    listFraudsterRegistrationJobsResponse_jobSummaries,
    listFraudsterRegistrationJobsResponse_httpStatus,

    -- ** DescribeFraudster
    describeFraudster_domainId,
    describeFraudster_fraudsterId,
    describeFraudsterResponse_fraudster,
    describeFraudsterResponse_httpStatus,

    -- ** ListSpeakerEnrollmentJobs
    listSpeakerEnrollmentJobs_nextToken,
    listSpeakerEnrollmentJobs_jobStatus,
    listSpeakerEnrollmentJobs_maxResults,
    listSpeakerEnrollmentJobs_domainId,
    listSpeakerEnrollmentJobsResponse_nextToken,
    listSpeakerEnrollmentJobsResponse_jobSummaries,
    listSpeakerEnrollmentJobsResponse_httpStatus,

    -- ** CreateDomain
    createDomain_clientToken,
    createDomain_description,
    createDomain_tags,
    createDomain_name,
    createDomain_serverSideEncryptionConfiguration,
    createDomainResponse_domain,
    createDomainResponse_httpStatus,

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

    -- ** DescribeDomain
    describeDomain_domainId,
    describeDomainResponse_domain,
    describeDomainResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** StartSpeakerEnrollmentJob
    startSpeakerEnrollmentJob_clientToken,
    startSpeakerEnrollmentJob_jobName,
    startSpeakerEnrollmentJob_enrollmentConfig,
    startSpeakerEnrollmentJob_dataAccessRoleArn,
    startSpeakerEnrollmentJob_domainId,
    startSpeakerEnrollmentJob_inputDataConfig,
    startSpeakerEnrollmentJob_outputDataConfig,
    startSpeakerEnrollmentJobResponse_job,
    startSpeakerEnrollmentJobResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** OptOutSpeaker
    optOutSpeaker_domainId,
    optOutSpeaker_speakerId,
    optOutSpeakerResponse_speaker,
    optOutSpeakerResponse_httpStatus,

    -- ** DescribeSpeaker
    describeSpeaker_domainId,
    describeSpeaker_speakerId,
    describeSpeakerResponse_speaker,
    describeSpeakerResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_domainId,

    -- ** UpdateDomain
    updateDomain_description,
    updateDomain_domainId,
    updateDomain_name,
    updateDomain_serverSideEncryptionConfiguration,
    updateDomainResponse_domain,
    updateDomainResponse_httpStatus,

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxResults,
    listDomainsResponse_nextToken,
    listDomainsResponse_domainSummaries,
    listDomainsResponse_httpStatus,

    -- * Types

    -- ** AuthenticationConfiguration
    authenticationConfiguration_acceptanceThreshold,

    -- ** AuthenticationResult
    authenticationResult_customerSpeakerId,
    authenticationResult_score,
    authenticationResult_authenticationResultId,
    authenticationResult_decision,
    authenticationResult_configuration,
    authenticationResult_audioAggregationStartedAt,
    authenticationResult_generatedSpeakerId,
    authenticationResult_audioAggregationEndedAt,

    -- ** Domain
    domain_domainStatus,
    domain_arn,
    domain_createdAt,
    domain_name,
    domain_domainId,
    domain_updatedAt,
    domain_description,
    domain_serverSideEncryptionConfiguration,

    -- ** DomainSummary
    domainSummary_domainStatus,
    domainSummary_arn,
    domainSummary_createdAt,
    domainSummary_name,
    domainSummary_domainId,
    domainSummary_updatedAt,
    domainSummary_description,
    domainSummary_serverSideEncryptionConfiguration,

    -- ** EnrollmentConfig
    enrollmentConfig_fraudDetectionConfig,
    enrollmentConfig_existingEnrollmentAction,

    -- ** EnrollmentJobFraudDetectionConfig
    enrollmentJobFraudDetectionConfig_riskThreshold,
    enrollmentJobFraudDetectionConfig_fraudDetectionAction,

    -- ** FailureDetails
    failureDetails_message,
    failureDetails_statusCode,

    -- ** FraudDetectionConfiguration
    fraudDetectionConfiguration_riskThreshold,

    -- ** FraudDetectionResult
    fraudDetectionResult_reasons,
    fraudDetectionResult_riskDetails,
    fraudDetectionResult_fraudDetectionResultId,
    fraudDetectionResult_decision,
    fraudDetectionResult_configuration,
    fraudDetectionResult_audioAggregationStartedAt,
    fraudDetectionResult_audioAggregationEndedAt,

    -- ** FraudRiskDetails
    fraudRiskDetails_knownFraudsterRisk,

    -- ** Fraudster
    fraudster_createdAt,
    fraudster_generatedFraudsterId,
    fraudster_domainId,

    -- ** FraudsterRegistrationJob
    fraudsterRegistrationJob_failureDetails,
    fraudsterRegistrationJob_jobId,
    fraudsterRegistrationJob_createdAt,
    fraudsterRegistrationJob_jobName,
    fraudsterRegistrationJob_endedAt,
    fraudsterRegistrationJob_inputDataConfig,
    fraudsterRegistrationJob_jobProgress,
    fraudsterRegistrationJob_outputDataConfig,
    fraudsterRegistrationJob_dataAccessRoleArn,
    fraudsterRegistrationJob_domainId,
    fraudsterRegistrationJob_jobStatus,
    fraudsterRegistrationJob_registrationConfig,

    -- ** FraudsterRegistrationJobSummary
    fraudsterRegistrationJobSummary_failureDetails,
    fraudsterRegistrationJobSummary_jobId,
    fraudsterRegistrationJobSummary_createdAt,
    fraudsterRegistrationJobSummary_jobName,
    fraudsterRegistrationJobSummary_endedAt,
    fraudsterRegistrationJobSummary_jobProgress,
    fraudsterRegistrationJobSummary_domainId,
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

    -- ** Speaker
    speaker_status,
    speaker_customerSpeakerId,
    speaker_createdAt,
    speaker_domainId,
    speaker_updatedAt,
    speaker_generatedSpeakerId,

    -- ** SpeakerEnrollmentJob
    speakerEnrollmentJob_failureDetails,
    speakerEnrollmentJob_jobId,
    speakerEnrollmentJob_createdAt,
    speakerEnrollmentJob_jobName,
    speakerEnrollmentJob_endedAt,
    speakerEnrollmentJob_enrollmentConfig,
    speakerEnrollmentJob_inputDataConfig,
    speakerEnrollmentJob_jobProgress,
    speakerEnrollmentJob_outputDataConfig,
    speakerEnrollmentJob_dataAccessRoleArn,
    speakerEnrollmentJob_domainId,
    speakerEnrollmentJob_jobStatus,

    -- ** SpeakerEnrollmentJobSummary
    speakerEnrollmentJobSummary_failureDetails,
    speakerEnrollmentJobSummary_jobId,
    speakerEnrollmentJobSummary_createdAt,
    speakerEnrollmentJobSummary_jobName,
    speakerEnrollmentJobSummary_endedAt,
    speakerEnrollmentJobSummary_jobProgress,
    speakerEnrollmentJobSummary_domainId,
    speakerEnrollmentJobSummary_jobStatus,

    -- ** SpeakerSummary
    speakerSummary_status,
    speakerSummary_customerSpeakerId,
    speakerSummary_createdAt,
    speakerSummary_domainId,
    speakerSummary_updatedAt,
    speakerSummary_generatedSpeakerId,

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
