{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.VoiceId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-09-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Connect Voice ID provides real-time caller authentication and
-- fraud screening. This guide describes the APIs used for this service.
module Amazonka.VoiceId
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** DeleteFraudster
    DeleteFraudster (DeleteFraudster'),
    newDeleteFraudster,
    DeleteFraudsterResponse (DeleteFraudsterResponse'),
    newDeleteFraudsterResponse,

    -- ** DeleteSpeaker
    DeleteSpeaker (DeleteSpeaker'),
    newDeleteSpeaker,
    DeleteSpeakerResponse (DeleteSpeakerResponse'),
    newDeleteSpeakerResponse,

    -- ** DescribeDomain
    DescribeDomain (DescribeDomain'),
    newDescribeDomain,
    DescribeDomainResponse (DescribeDomainResponse'),
    newDescribeDomainResponse,

    -- ** DescribeFraudster
    DescribeFraudster (DescribeFraudster'),
    newDescribeFraudster,
    DescribeFraudsterResponse (DescribeFraudsterResponse'),
    newDescribeFraudsterResponse,

    -- ** DescribeFraudsterRegistrationJob
    DescribeFraudsterRegistrationJob (DescribeFraudsterRegistrationJob'),
    newDescribeFraudsterRegistrationJob,
    DescribeFraudsterRegistrationJobResponse (DescribeFraudsterRegistrationJobResponse'),
    newDescribeFraudsterRegistrationJobResponse,

    -- ** DescribeSpeaker
    DescribeSpeaker (DescribeSpeaker'),
    newDescribeSpeaker,
    DescribeSpeakerResponse (DescribeSpeakerResponse'),
    newDescribeSpeakerResponse,

    -- ** DescribeSpeakerEnrollmentJob
    DescribeSpeakerEnrollmentJob (DescribeSpeakerEnrollmentJob'),
    newDescribeSpeakerEnrollmentJob,
    DescribeSpeakerEnrollmentJobResponse (DescribeSpeakerEnrollmentJobResponse'),
    newDescribeSpeakerEnrollmentJobResponse,

    -- ** EvaluateSession
    EvaluateSession (EvaluateSession'),
    newEvaluateSession,
    EvaluateSessionResponse (EvaluateSessionResponse'),
    newEvaluateSessionResponse,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** ListFraudsterRegistrationJobs (Paginated)
    ListFraudsterRegistrationJobs (ListFraudsterRegistrationJobs'),
    newListFraudsterRegistrationJobs,
    ListFraudsterRegistrationJobsResponse (ListFraudsterRegistrationJobsResponse'),
    newListFraudsterRegistrationJobsResponse,

    -- ** ListSpeakerEnrollmentJobs (Paginated)
    ListSpeakerEnrollmentJobs (ListSpeakerEnrollmentJobs'),
    newListSpeakerEnrollmentJobs,
    ListSpeakerEnrollmentJobsResponse (ListSpeakerEnrollmentJobsResponse'),
    newListSpeakerEnrollmentJobsResponse,

    -- ** ListSpeakers (Paginated)
    ListSpeakers (ListSpeakers'),
    newListSpeakers,
    ListSpeakersResponse (ListSpeakersResponse'),
    newListSpeakersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** OptOutSpeaker
    OptOutSpeaker (OptOutSpeaker'),
    newOptOutSpeaker,
    OptOutSpeakerResponse (OptOutSpeakerResponse'),
    newOptOutSpeakerResponse,

    -- ** StartFraudsterRegistrationJob
    StartFraudsterRegistrationJob (StartFraudsterRegistrationJob'),
    newStartFraudsterRegistrationJob,
    StartFraudsterRegistrationJobResponse (StartFraudsterRegistrationJobResponse'),
    newStartFraudsterRegistrationJobResponse,

    -- ** StartSpeakerEnrollmentJob
    StartSpeakerEnrollmentJob (StartSpeakerEnrollmentJob'),
    newStartSpeakerEnrollmentJob,
    StartSpeakerEnrollmentJobResponse (StartSpeakerEnrollmentJobResponse'),
    newStartSpeakerEnrollmentJobResponse,

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

    -- ** UpdateDomain
    UpdateDomain (UpdateDomain'),
    newUpdateDomain,
    UpdateDomainResponse (UpdateDomainResponse'),
    newUpdateDomainResponse,

    -- * Types

    -- ** AuthenticationDecision
    AuthenticationDecision (..),

    -- ** DomainStatus
    DomainStatus (..),

    -- ** DuplicateRegistrationAction
    DuplicateRegistrationAction (..),

    -- ** ExistingEnrollmentAction
    ExistingEnrollmentAction (..),

    -- ** FraudDetectionAction
    FraudDetectionAction (..),

    -- ** FraudDetectionDecision
    FraudDetectionDecision (..),

    -- ** FraudDetectionReason
    FraudDetectionReason (..),

    -- ** FraudsterRegistrationJobStatus
    FraudsterRegistrationJobStatus (..),

    -- ** ServerSideEncryptionUpdateStatus
    ServerSideEncryptionUpdateStatus (..),

    -- ** SpeakerEnrollmentJobStatus
    SpeakerEnrollmentJobStatus (..),

    -- ** SpeakerStatus
    SpeakerStatus (..),

    -- ** StreamingStatus
    StreamingStatus (..),

    -- ** AuthenticationConfiguration
    AuthenticationConfiguration (AuthenticationConfiguration'),
    newAuthenticationConfiguration,

    -- ** AuthenticationResult
    AuthenticationResult (AuthenticationResult'),
    newAuthenticationResult,

    -- ** Domain
    Domain (Domain'),
    newDomain,

    -- ** DomainSummary
    DomainSummary (DomainSummary'),
    newDomainSummary,

    -- ** EnrollmentConfig
    EnrollmentConfig (EnrollmentConfig'),
    newEnrollmentConfig,

    -- ** EnrollmentJobFraudDetectionConfig
    EnrollmentJobFraudDetectionConfig (EnrollmentJobFraudDetectionConfig'),
    newEnrollmentJobFraudDetectionConfig,

    -- ** FailureDetails
    FailureDetails (FailureDetails'),
    newFailureDetails,

    -- ** FraudDetectionConfiguration
    FraudDetectionConfiguration (FraudDetectionConfiguration'),
    newFraudDetectionConfiguration,

    -- ** FraudDetectionResult
    FraudDetectionResult (FraudDetectionResult'),
    newFraudDetectionResult,

    -- ** FraudRiskDetails
    FraudRiskDetails (FraudRiskDetails'),
    newFraudRiskDetails,

    -- ** Fraudster
    Fraudster (Fraudster'),
    newFraudster,

    -- ** FraudsterRegistrationJob
    FraudsterRegistrationJob (FraudsterRegistrationJob'),
    newFraudsterRegistrationJob,

    -- ** FraudsterRegistrationJobSummary
    FraudsterRegistrationJobSummary (FraudsterRegistrationJobSummary'),
    newFraudsterRegistrationJobSummary,

    -- ** InputDataConfig
    InputDataConfig (InputDataConfig'),
    newInputDataConfig,

    -- ** JobProgress
    JobProgress (JobProgress'),
    newJobProgress,

    -- ** KnownFraudsterRisk
    KnownFraudsterRisk (KnownFraudsterRisk'),
    newKnownFraudsterRisk,

    -- ** OutputDataConfig
    OutputDataConfig (OutputDataConfig'),
    newOutputDataConfig,

    -- ** RegistrationConfig
    RegistrationConfig (RegistrationConfig'),
    newRegistrationConfig,

    -- ** ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (ServerSideEncryptionConfiguration'),
    newServerSideEncryptionConfiguration,

    -- ** ServerSideEncryptionUpdateDetails
    ServerSideEncryptionUpdateDetails (ServerSideEncryptionUpdateDetails'),
    newServerSideEncryptionUpdateDetails,

    -- ** Speaker
    Speaker (Speaker'),
    newSpeaker,

    -- ** SpeakerEnrollmentJob
    SpeakerEnrollmentJob (SpeakerEnrollmentJob'),
    newSpeakerEnrollmentJob,

    -- ** SpeakerEnrollmentJobSummary
    SpeakerEnrollmentJobSummary (SpeakerEnrollmentJobSummary'),
    newSpeakerEnrollmentJobSummary,

    -- ** SpeakerSummary
    SpeakerSummary (SpeakerSummary'),
    newSpeakerSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** VoiceSpoofingRisk
    VoiceSpoofingRisk (VoiceSpoofingRisk'),
    newVoiceSpoofingRisk,
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
import Amazonka.VoiceId.Lens
import Amazonka.VoiceId.ListDomains
import Amazonka.VoiceId.ListFraudsterRegistrationJobs
import Amazonka.VoiceId.ListSpeakerEnrollmentJobs
import Amazonka.VoiceId.ListSpeakers
import Amazonka.VoiceId.ListTagsForResource
import Amazonka.VoiceId.OptOutSpeaker
import Amazonka.VoiceId.StartFraudsterRegistrationJob
import Amazonka.VoiceId.StartSpeakerEnrollmentJob
import Amazonka.VoiceId.TagResource
import Amazonka.VoiceId.Types
import Amazonka.VoiceId.UntagResource
import Amazonka.VoiceId.UpdateDomain
import Amazonka.VoiceId.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'VoiceId'.

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
