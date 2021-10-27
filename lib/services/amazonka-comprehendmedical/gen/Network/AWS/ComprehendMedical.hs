{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.ComprehendMedical
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-10-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Comprehend Medical extracts structured information from
-- unstructured clinical text. Use these actions to gain insight in your
-- documents.
module Network.AWS.ComprehendMedical
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InvalidEncodingException
    _InvalidEncodingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TextSizeLimitExceededException
    _TextSizeLimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeEntitiesDetectionV2Job
    DescribeEntitiesDetectionV2Job (DescribeEntitiesDetectionV2Job'),
    newDescribeEntitiesDetectionV2Job,
    DescribeEntitiesDetectionV2JobResponse (DescribeEntitiesDetectionV2JobResponse'),
    newDescribeEntitiesDetectionV2JobResponse,

    -- ** DescribePHIDetectionJob
    DescribePHIDetectionJob (DescribePHIDetectionJob'),
    newDescribePHIDetectionJob,
    DescribePHIDetectionJobResponse (DescribePHIDetectionJobResponse'),
    newDescribePHIDetectionJobResponse,

    -- ** ListICD10CMInferenceJobs
    ListICD10CMInferenceJobs (ListICD10CMInferenceJobs'),
    newListICD10CMInferenceJobs,
    ListICD10CMInferenceJobsResponse (ListICD10CMInferenceJobsResponse'),
    newListICD10CMInferenceJobsResponse,

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

    -- ** StartICD10CMInferenceJob
    StartICD10CMInferenceJob (StartICD10CMInferenceJob'),
    newStartICD10CMInferenceJob,
    StartICD10CMInferenceJobResponse (StartICD10CMInferenceJobResponse'),
    newStartICD10CMInferenceJobResponse,

    -- ** StartRxNormInferenceJob
    StartRxNormInferenceJob (StartRxNormInferenceJob'),
    newStartRxNormInferenceJob,
    StartRxNormInferenceJobResponse (StartRxNormInferenceJobResponse'),
    newStartRxNormInferenceJobResponse,

    -- ** ListPHIDetectionJobs
    ListPHIDetectionJobs (ListPHIDetectionJobs'),
    newListPHIDetectionJobs,
    ListPHIDetectionJobsResponse (ListPHIDetectionJobsResponse'),
    newListPHIDetectionJobsResponse,

    -- ** DescribeICD10CMInferenceJob
    DescribeICD10CMInferenceJob (DescribeICD10CMInferenceJob'),
    newDescribeICD10CMInferenceJob,
    DescribeICD10CMInferenceJobResponse (DescribeICD10CMInferenceJobResponse'),
    newDescribeICD10CMInferenceJobResponse,

    -- ** StartPHIDetectionJob
    StartPHIDetectionJob (StartPHIDetectionJob'),
    newStartPHIDetectionJob,
    StartPHIDetectionJobResponse (StartPHIDetectionJobResponse'),
    newStartPHIDetectionJobResponse,

    -- ** StopEntitiesDetectionV2Job
    StopEntitiesDetectionV2Job (StopEntitiesDetectionV2Job'),
    newStopEntitiesDetectionV2Job,
    StopEntitiesDetectionV2JobResponse (StopEntitiesDetectionV2JobResponse'),
    newStopEntitiesDetectionV2JobResponse,

    -- ** DescribeRxNormInferenceJob
    DescribeRxNormInferenceJob (DescribeRxNormInferenceJob'),
    newDescribeRxNormInferenceJob,
    DescribeRxNormInferenceJobResponse (DescribeRxNormInferenceJobResponse'),
    newDescribeRxNormInferenceJobResponse,

    -- ** StopICD10CMInferenceJob
    StopICD10CMInferenceJob (StopICD10CMInferenceJob'),
    newStopICD10CMInferenceJob,
    StopICD10CMInferenceJobResponse (StopICD10CMInferenceJobResponse'),
    newStopICD10CMInferenceJobResponse,

    -- ** ListEntitiesDetectionV2Jobs
    ListEntitiesDetectionV2Jobs (ListEntitiesDetectionV2Jobs'),
    newListEntitiesDetectionV2Jobs,
    ListEntitiesDetectionV2JobsResponse (ListEntitiesDetectionV2JobsResponse'),
    newListEntitiesDetectionV2JobsResponse,

    -- ** StopRxNormInferenceJob
    StopRxNormInferenceJob (StopRxNormInferenceJob'),
    newStopRxNormInferenceJob,
    StopRxNormInferenceJobResponse (StopRxNormInferenceJobResponse'),
    newStopRxNormInferenceJobResponse,

    -- ** DetectPHI
    DetectPHI (DetectPHI'),
    newDetectPHI,
    DetectPHIResponse (DetectPHIResponse'),
    newDetectPHIResponse,

    -- ** DetectEntitiesV2
    DetectEntitiesV2 (DetectEntitiesV2'),
    newDetectEntitiesV2,
    DetectEntitiesV2Response (DetectEntitiesV2Response'),
    newDetectEntitiesV2Response,

    -- ** StopPHIDetectionJob
    StopPHIDetectionJob (StopPHIDetectionJob'),
    newStopPHIDetectionJob,
    StopPHIDetectionJobResponse (StopPHIDetectionJobResponse'),
    newStopPHIDetectionJobResponse,

    -- ** StartEntitiesDetectionV2Job
    StartEntitiesDetectionV2Job (StartEntitiesDetectionV2Job'),
    newStartEntitiesDetectionV2Job,
    StartEntitiesDetectionV2JobResponse (StartEntitiesDetectionV2JobResponse'),
    newStartEntitiesDetectionV2JobResponse,

    -- ** ListRxNormInferenceJobs
    ListRxNormInferenceJobs (ListRxNormInferenceJobs'),
    newListRxNormInferenceJobs,
    ListRxNormInferenceJobsResponse (ListRxNormInferenceJobsResponse'),
    newListRxNormInferenceJobsResponse,

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

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

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

    -- ** Trait
    Trait (Trait'),
    newTrait,

    -- ** UnmappedAttribute
    UnmappedAttribute (UnmappedAttribute'),
    newUnmappedAttribute,
  )
where

import Network.AWS.ComprehendMedical.DescribeEntitiesDetectionV2Job
import Network.AWS.ComprehendMedical.DescribeICD10CMInferenceJob
import Network.AWS.ComprehendMedical.DescribePHIDetectionJob
import Network.AWS.ComprehendMedical.DescribeRxNormInferenceJob
import Network.AWS.ComprehendMedical.DetectEntitiesV2
import Network.AWS.ComprehendMedical.DetectPHI
import Network.AWS.ComprehendMedical.InferICD10CM
import Network.AWS.ComprehendMedical.InferRxNorm
import Network.AWS.ComprehendMedical.Lens
import Network.AWS.ComprehendMedical.ListEntitiesDetectionV2Jobs
import Network.AWS.ComprehendMedical.ListICD10CMInferenceJobs
import Network.AWS.ComprehendMedical.ListPHIDetectionJobs
import Network.AWS.ComprehendMedical.ListRxNormInferenceJobs
import Network.AWS.ComprehendMedical.StartEntitiesDetectionV2Job
import Network.AWS.ComprehendMedical.StartICD10CMInferenceJob
import Network.AWS.ComprehendMedical.StartPHIDetectionJob
import Network.AWS.ComprehendMedical.StartRxNormInferenceJob
import Network.AWS.ComprehendMedical.StopEntitiesDetectionV2Job
import Network.AWS.ComprehendMedical.StopICD10CMInferenceJob
import Network.AWS.ComprehendMedical.StopPHIDetectionJob
import Network.AWS.ComprehendMedical.StopRxNormInferenceJob
import Network.AWS.ComprehendMedical.Types
import Network.AWS.ComprehendMedical.Waiters

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
