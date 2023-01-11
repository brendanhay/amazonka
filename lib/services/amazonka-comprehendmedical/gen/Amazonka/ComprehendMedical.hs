{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ComprehendMedical
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-10-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Comprehend Medical; extracts structured information from unstructured
-- clinical text. Use these actions to gain insight in your documents.
module Amazonka.ComprehendMedical
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidEncodingException
    _InvalidEncodingException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TextSizeLimitExceededException
    _TextSizeLimitExceededException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeEntitiesDetectionV2Job
    DescribeEntitiesDetectionV2Job (DescribeEntitiesDetectionV2Job'),
    newDescribeEntitiesDetectionV2Job,
    DescribeEntitiesDetectionV2JobResponse (DescribeEntitiesDetectionV2JobResponse'),
    newDescribeEntitiesDetectionV2JobResponse,

    -- ** DescribeICD10CMInferenceJob
    DescribeICD10CMInferenceJob (DescribeICD10CMInferenceJob'),
    newDescribeICD10CMInferenceJob,
    DescribeICD10CMInferenceJobResponse (DescribeICD10CMInferenceJobResponse'),
    newDescribeICD10CMInferenceJobResponse,

    -- ** DescribePHIDetectionJob
    DescribePHIDetectionJob (DescribePHIDetectionJob'),
    newDescribePHIDetectionJob,
    DescribePHIDetectionJobResponse (DescribePHIDetectionJobResponse'),
    newDescribePHIDetectionJobResponse,

    -- ** DescribeRxNormInferenceJob
    DescribeRxNormInferenceJob (DescribeRxNormInferenceJob'),
    newDescribeRxNormInferenceJob,
    DescribeRxNormInferenceJobResponse (DescribeRxNormInferenceJobResponse'),
    newDescribeRxNormInferenceJobResponse,

    -- ** DescribeSNOMEDCTInferenceJob
    DescribeSNOMEDCTInferenceJob (DescribeSNOMEDCTInferenceJob'),
    newDescribeSNOMEDCTInferenceJob,
    DescribeSNOMEDCTInferenceJobResponse (DescribeSNOMEDCTInferenceJobResponse'),
    newDescribeSNOMEDCTInferenceJobResponse,

    -- ** DetectEntitiesV2
    DetectEntitiesV2 (DetectEntitiesV2'),
    newDetectEntitiesV2,
    DetectEntitiesV2Response (DetectEntitiesV2Response'),
    newDetectEntitiesV2Response,

    -- ** DetectPHI
    DetectPHI (DetectPHI'),
    newDetectPHI,
    DetectPHIResponse (DetectPHIResponse'),
    newDetectPHIResponse,

    -- ** InferICD10CM
    InferICD10CM (InferICD10CM'),
    newInferICD10CM,
    InferICD10CMResponse (InferICD10CMResponse'),
    newInferICD10CMResponse,

    -- ** InferRxNorm
    InferRxNorm (InferRxNorm'),
    newInferRxNorm,
    InferRxNormResponse (InferRxNormResponse'),
    newInferRxNormResponse,

    -- ** InferSNOMEDCT
    InferSNOMEDCT (InferSNOMEDCT'),
    newInferSNOMEDCT,
    InferSNOMEDCTResponse (InferSNOMEDCTResponse'),
    newInferSNOMEDCTResponse,

    -- ** ListEntitiesDetectionV2Jobs
    ListEntitiesDetectionV2Jobs (ListEntitiesDetectionV2Jobs'),
    newListEntitiesDetectionV2Jobs,
    ListEntitiesDetectionV2JobsResponse (ListEntitiesDetectionV2JobsResponse'),
    newListEntitiesDetectionV2JobsResponse,

    -- ** ListICD10CMInferenceJobs
    ListICD10CMInferenceJobs (ListICD10CMInferenceJobs'),
    newListICD10CMInferenceJobs,
    ListICD10CMInferenceJobsResponse (ListICD10CMInferenceJobsResponse'),
    newListICD10CMInferenceJobsResponse,

    -- ** ListPHIDetectionJobs
    ListPHIDetectionJobs (ListPHIDetectionJobs'),
    newListPHIDetectionJobs,
    ListPHIDetectionJobsResponse (ListPHIDetectionJobsResponse'),
    newListPHIDetectionJobsResponse,

    -- ** ListRxNormInferenceJobs
    ListRxNormInferenceJobs (ListRxNormInferenceJobs'),
    newListRxNormInferenceJobs,
    ListRxNormInferenceJobsResponse (ListRxNormInferenceJobsResponse'),
    newListRxNormInferenceJobsResponse,

    -- ** ListSNOMEDCTInferenceJobs
    ListSNOMEDCTInferenceJobs (ListSNOMEDCTInferenceJobs'),
    newListSNOMEDCTInferenceJobs,
    ListSNOMEDCTInferenceJobsResponse (ListSNOMEDCTInferenceJobsResponse'),
    newListSNOMEDCTInferenceJobsResponse,

    -- ** StartEntitiesDetectionV2Job
    StartEntitiesDetectionV2Job (StartEntitiesDetectionV2Job'),
    newStartEntitiesDetectionV2Job,
    StartEntitiesDetectionV2JobResponse (StartEntitiesDetectionV2JobResponse'),
    newStartEntitiesDetectionV2JobResponse,

    -- ** StartICD10CMInferenceJob
    StartICD10CMInferenceJob (StartICD10CMInferenceJob'),
    newStartICD10CMInferenceJob,
    StartICD10CMInferenceJobResponse (StartICD10CMInferenceJobResponse'),
    newStartICD10CMInferenceJobResponse,

    -- ** StartPHIDetectionJob
    StartPHIDetectionJob (StartPHIDetectionJob'),
    newStartPHIDetectionJob,
    StartPHIDetectionJobResponse (StartPHIDetectionJobResponse'),
    newStartPHIDetectionJobResponse,

    -- ** StartRxNormInferenceJob
    StartRxNormInferenceJob (StartRxNormInferenceJob'),
    newStartRxNormInferenceJob,
    StartRxNormInferenceJobResponse (StartRxNormInferenceJobResponse'),
    newStartRxNormInferenceJobResponse,

    -- ** StartSNOMEDCTInferenceJob
    StartSNOMEDCTInferenceJob (StartSNOMEDCTInferenceJob'),
    newStartSNOMEDCTInferenceJob,
    StartSNOMEDCTInferenceJobResponse (StartSNOMEDCTInferenceJobResponse'),
    newStartSNOMEDCTInferenceJobResponse,

    -- ** StopEntitiesDetectionV2Job
    StopEntitiesDetectionV2Job (StopEntitiesDetectionV2Job'),
    newStopEntitiesDetectionV2Job,
    StopEntitiesDetectionV2JobResponse (StopEntitiesDetectionV2JobResponse'),
    newStopEntitiesDetectionV2JobResponse,

    -- ** StopICD10CMInferenceJob
    StopICD10CMInferenceJob (StopICD10CMInferenceJob'),
    newStopICD10CMInferenceJob,
    StopICD10CMInferenceJobResponse (StopICD10CMInferenceJobResponse'),
    newStopICD10CMInferenceJobResponse,

    -- ** StopPHIDetectionJob
    StopPHIDetectionJob (StopPHIDetectionJob'),
    newStopPHIDetectionJob,
    StopPHIDetectionJobResponse (StopPHIDetectionJobResponse'),
    newStopPHIDetectionJobResponse,

    -- ** StopRxNormInferenceJob
    StopRxNormInferenceJob (StopRxNormInferenceJob'),
    newStopRxNormInferenceJob,
    StopRxNormInferenceJobResponse (StopRxNormInferenceJobResponse'),
    newStopRxNormInferenceJobResponse,

    -- ** StopSNOMEDCTInferenceJob
    StopSNOMEDCTInferenceJob (StopSNOMEDCTInferenceJob'),
    newStopSNOMEDCTInferenceJob,
    StopSNOMEDCTInferenceJobResponse (StopSNOMEDCTInferenceJobResponse'),
    newStopSNOMEDCTInferenceJobResponse,

    -- * Types

    -- ** AttributeName
    AttributeName (..),

    -- ** EntitySubType
    EntitySubType (..),

    -- ** EntityType
    EntityType (..),

    -- ** ICD10CMAttributeType
    ICD10CMAttributeType (..),

    -- ** ICD10CMEntityCategory
    ICD10CMEntityCategory (..),

    -- ** ICD10CMEntityType
    ICD10CMEntityType (..),

    -- ** ICD10CMRelationshipType
    ICD10CMRelationshipType (..),

    -- ** ICD10CMTraitName
    ICD10CMTraitName (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** LanguageCode
    LanguageCode (..),

    -- ** RelationshipType
    RelationshipType (..),

    -- ** RxNormAttributeType
    RxNormAttributeType (..),

    -- ** RxNormEntityCategory
    RxNormEntityCategory (..),

    -- ** RxNormEntityType
    RxNormEntityType (..),

    -- ** RxNormTraitName
    RxNormTraitName (..),

    -- ** SNOMEDCTAttributeType
    SNOMEDCTAttributeType (..),

    -- ** SNOMEDCTEntityCategory
    SNOMEDCTEntityCategory (..),

    -- ** SNOMEDCTEntityType
    SNOMEDCTEntityType (..),

    -- ** SNOMEDCTRelationshipType
    SNOMEDCTRelationshipType (..),

    -- ** SNOMEDCTTraitName
    SNOMEDCTTraitName (..),

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** Characters
    Characters (Characters'),
    newCharacters,

    -- ** ComprehendMedicalAsyncJobFilter
    ComprehendMedicalAsyncJobFilter (ComprehendMedicalAsyncJobFilter'),
    newComprehendMedicalAsyncJobFilter,

    -- ** ComprehendMedicalAsyncJobProperties
    ComprehendMedicalAsyncJobProperties (ComprehendMedicalAsyncJobProperties'),
    newComprehendMedicalAsyncJobProperties,

    -- ** Entity
    Entity (Entity'),
    newEntity,

    -- ** ICD10CMAttribute
    ICD10CMAttribute (ICD10CMAttribute'),
    newICD10CMAttribute,

    -- ** ICD10CMConcept
    ICD10CMConcept (ICD10CMConcept'),
    newICD10CMConcept,

    -- ** ICD10CMEntity
    ICD10CMEntity (ICD10CMEntity'),
    newICD10CMEntity,

    -- ** ICD10CMTrait
    ICD10CMTrait (ICD10CMTrait'),
    newICD10CMTrait,

    -- ** InputDataConfig
    InputDataConfig (InputDataConfig'),
    newInputDataConfig,

    -- ** OutputDataConfig
    OutputDataConfig (OutputDataConfig'),
    newOutputDataConfig,

    -- ** RxNormAttribute
    RxNormAttribute (RxNormAttribute'),
    newRxNormAttribute,

    -- ** RxNormConcept
    RxNormConcept (RxNormConcept'),
    newRxNormConcept,

    -- ** RxNormEntity
    RxNormEntity (RxNormEntity'),
    newRxNormEntity,

    -- ** RxNormTrait
    RxNormTrait (RxNormTrait'),
    newRxNormTrait,

    -- ** SNOMEDCTAttribute
    SNOMEDCTAttribute (SNOMEDCTAttribute'),
    newSNOMEDCTAttribute,

    -- ** SNOMEDCTConcept
    SNOMEDCTConcept (SNOMEDCTConcept'),
    newSNOMEDCTConcept,

    -- ** SNOMEDCTDetails
    SNOMEDCTDetails (SNOMEDCTDetails'),
    newSNOMEDCTDetails,

    -- ** SNOMEDCTEntity
    SNOMEDCTEntity (SNOMEDCTEntity'),
    newSNOMEDCTEntity,

    -- ** SNOMEDCTTrait
    SNOMEDCTTrait (SNOMEDCTTrait'),
    newSNOMEDCTTrait,

    -- ** Trait
    Trait (Trait'),
    newTrait,

    -- ** UnmappedAttribute
    UnmappedAttribute (UnmappedAttribute'),
    newUnmappedAttribute,
  )
where

import Amazonka.ComprehendMedical.DescribeEntitiesDetectionV2Job
import Amazonka.ComprehendMedical.DescribeICD10CMInferenceJob
import Amazonka.ComprehendMedical.DescribePHIDetectionJob
import Amazonka.ComprehendMedical.DescribeRxNormInferenceJob
import Amazonka.ComprehendMedical.DescribeSNOMEDCTInferenceJob
import Amazonka.ComprehendMedical.DetectEntitiesV2
import Amazonka.ComprehendMedical.DetectPHI
import Amazonka.ComprehendMedical.InferICD10CM
import Amazonka.ComprehendMedical.InferRxNorm
import Amazonka.ComprehendMedical.InferSNOMEDCT
import Amazonka.ComprehendMedical.Lens
import Amazonka.ComprehendMedical.ListEntitiesDetectionV2Jobs
import Amazonka.ComprehendMedical.ListICD10CMInferenceJobs
import Amazonka.ComprehendMedical.ListPHIDetectionJobs
import Amazonka.ComprehendMedical.ListRxNormInferenceJobs
import Amazonka.ComprehendMedical.ListSNOMEDCTInferenceJobs
import Amazonka.ComprehendMedical.StartEntitiesDetectionV2Job
import Amazonka.ComprehendMedical.StartICD10CMInferenceJob
import Amazonka.ComprehendMedical.StartPHIDetectionJob
import Amazonka.ComprehendMedical.StartRxNormInferenceJob
import Amazonka.ComprehendMedical.StartSNOMEDCTInferenceJob
import Amazonka.ComprehendMedical.StopEntitiesDetectionV2Job
import Amazonka.ComprehendMedical.StopICD10CMInferenceJob
import Amazonka.ComprehendMedical.StopPHIDetectionJob
import Amazonka.ComprehendMedical.StopRxNormInferenceJob
import Amazonka.ComprehendMedical.StopSNOMEDCTInferenceJob
import Amazonka.ComprehendMedical.Types
import Amazonka.ComprehendMedical.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ComprehendMedical'.

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
