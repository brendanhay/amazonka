{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ComprehendMedical.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _TextSizeLimitExceededException,
    _ValidationException,
    _InvalidEncodingException,
    _TooManyRequestsException,
    _InvalidRequestException,

    -- * AttributeName
    AttributeName (..),

    -- * EntitySubType
    EntitySubType (..),

    -- * EntityType
    EntityType (..),

    -- * ICD10CMAttributeType
    ICD10CMAttributeType (..),

    -- * ICD10CMEntityCategory
    ICD10CMEntityCategory (..),

    -- * ICD10CMEntityType
    ICD10CMEntityType (..),

    -- * ICD10CMRelationshipType
    ICD10CMRelationshipType (..),

    -- * ICD10CMTraitName
    ICD10CMTraitName (..),

    -- * JobStatus
    JobStatus (..),

    -- * LanguageCode
    LanguageCode (..),

    -- * RelationshipType
    RelationshipType (..),

    -- * RxNormAttributeType
    RxNormAttributeType (..),

    -- * RxNormEntityCategory
    RxNormEntityCategory (..),

    -- * RxNormEntityType
    RxNormEntityType (..),

    -- * RxNormTraitName
    RxNormTraitName (..),

    -- * SNOMEDCTAttributeType
    SNOMEDCTAttributeType (..),

    -- * SNOMEDCTEntityCategory
    SNOMEDCTEntityCategory (..),

    -- * SNOMEDCTEntityType
    SNOMEDCTEntityType (..),

    -- * SNOMEDCTRelationshipType
    SNOMEDCTRelationshipType (..),

    -- * SNOMEDCTTraitName
    SNOMEDCTTraitName (..),

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_beginOffset,
    attribute_relationshipScore,
    attribute_type,
    attribute_traits,
    attribute_score,
    attribute_id,
    attribute_endOffset,
    attribute_relationshipType,
    attribute_category,
    attribute_text,

    -- * Characters
    Characters (..),
    newCharacters,
    characters_originalTextCharacters,

    -- * ComprehendMedicalAsyncJobFilter
    ComprehendMedicalAsyncJobFilter (..),
    newComprehendMedicalAsyncJobFilter,
    comprehendMedicalAsyncJobFilter_jobStatus,
    comprehendMedicalAsyncJobFilter_jobName,
    comprehendMedicalAsyncJobFilter_submitTimeBefore,
    comprehendMedicalAsyncJobFilter_submitTimeAfter,

    -- * ComprehendMedicalAsyncJobProperties
    ComprehendMedicalAsyncJobProperties (..),
    newComprehendMedicalAsyncJobProperties,
    comprehendMedicalAsyncJobProperties_outputDataConfig,
    comprehendMedicalAsyncJobProperties_message,
    comprehendMedicalAsyncJobProperties_jobStatus,
    comprehendMedicalAsyncJobProperties_expirationTime,
    comprehendMedicalAsyncJobProperties_jobName,
    comprehendMedicalAsyncJobProperties_submitTime,
    comprehendMedicalAsyncJobProperties_kmsKey,
    comprehendMedicalAsyncJobProperties_jobId,
    comprehendMedicalAsyncJobProperties_modelVersion,
    comprehendMedicalAsyncJobProperties_dataAccessRoleArn,
    comprehendMedicalAsyncJobProperties_endTime,
    comprehendMedicalAsyncJobProperties_languageCode,
    comprehendMedicalAsyncJobProperties_manifestFilePath,
    comprehendMedicalAsyncJobProperties_inputDataConfig,

    -- * Entity
    Entity (..),
    newEntity,
    entity_beginOffset,
    entity_type,
    entity_traits,
    entity_score,
    entity_id,
    entity_endOffset,
    entity_category,
    entity_attributes,
    entity_text,

    -- * ICD10CMAttribute
    ICD10CMAttribute (..),
    newICD10CMAttribute,
    iCD10CMAttribute_beginOffset,
    iCD10CMAttribute_relationshipScore,
    iCD10CMAttribute_type,
    iCD10CMAttribute_traits,
    iCD10CMAttribute_score,
    iCD10CMAttribute_id,
    iCD10CMAttribute_endOffset,
    iCD10CMAttribute_relationshipType,
    iCD10CMAttribute_category,
    iCD10CMAttribute_text,

    -- * ICD10CMConcept
    ICD10CMConcept (..),
    newICD10CMConcept,
    iCD10CMConcept_code,
    iCD10CMConcept_score,
    iCD10CMConcept_description,

    -- * ICD10CMEntity
    ICD10CMEntity (..),
    newICD10CMEntity,
    iCD10CMEntity_beginOffset,
    iCD10CMEntity_type,
    iCD10CMEntity_traits,
    iCD10CMEntity_score,
    iCD10CMEntity_id,
    iCD10CMEntity_endOffset,
    iCD10CMEntity_category,
    iCD10CMEntity_attributes,
    iCD10CMEntity_text,
    iCD10CMEntity_iCD10CMConcepts,

    -- * ICD10CMTrait
    ICD10CMTrait (..),
    newICD10CMTrait,
    iCD10CMTrait_name,
    iCD10CMTrait_score,

    -- * InputDataConfig
    InputDataConfig (..),
    newInputDataConfig,
    inputDataConfig_s3Key,
    inputDataConfig_s3Bucket,

    -- * OutputDataConfig
    OutputDataConfig (..),
    newOutputDataConfig,
    outputDataConfig_s3Key,
    outputDataConfig_s3Bucket,

    -- * RxNormAttribute
    RxNormAttribute (..),
    newRxNormAttribute,
    rxNormAttribute_beginOffset,
    rxNormAttribute_relationshipScore,
    rxNormAttribute_type,
    rxNormAttribute_traits,
    rxNormAttribute_score,
    rxNormAttribute_id,
    rxNormAttribute_endOffset,
    rxNormAttribute_text,

    -- * RxNormConcept
    RxNormConcept (..),
    newRxNormConcept,
    rxNormConcept_code,
    rxNormConcept_score,
    rxNormConcept_description,

    -- * RxNormEntity
    RxNormEntity (..),
    newRxNormEntity,
    rxNormEntity_beginOffset,
    rxNormEntity_type,
    rxNormEntity_rxNormConcepts,
    rxNormEntity_traits,
    rxNormEntity_score,
    rxNormEntity_id,
    rxNormEntity_endOffset,
    rxNormEntity_category,
    rxNormEntity_attributes,
    rxNormEntity_text,

    -- * RxNormTrait
    RxNormTrait (..),
    newRxNormTrait,
    rxNormTrait_name,
    rxNormTrait_score,

    -- * SNOMEDCTAttribute
    SNOMEDCTAttribute (..),
    newSNOMEDCTAttribute,
    sNOMEDCTAttribute_beginOffset,
    sNOMEDCTAttribute_relationshipScore,
    sNOMEDCTAttribute_type,
    sNOMEDCTAttribute_traits,
    sNOMEDCTAttribute_score,
    sNOMEDCTAttribute_id,
    sNOMEDCTAttribute_endOffset,
    sNOMEDCTAttribute_relationshipType,
    sNOMEDCTAttribute_category,
    sNOMEDCTAttribute_text,
    sNOMEDCTAttribute_sNOMEDCTConcepts,

    -- * SNOMEDCTConcept
    SNOMEDCTConcept (..),
    newSNOMEDCTConcept,
    sNOMEDCTConcept_code,
    sNOMEDCTConcept_score,
    sNOMEDCTConcept_description,

    -- * SNOMEDCTDetails
    SNOMEDCTDetails (..),
    newSNOMEDCTDetails,
    sNOMEDCTDetails_edition,
    sNOMEDCTDetails_versionDate,
    sNOMEDCTDetails_language,

    -- * SNOMEDCTEntity
    SNOMEDCTEntity (..),
    newSNOMEDCTEntity,
    sNOMEDCTEntity_beginOffset,
    sNOMEDCTEntity_type,
    sNOMEDCTEntity_traits,
    sNOMEDCTEntity_score,
    sNOMEDCTEntity_id,
    sNOMEDCTEntity_endOffset,
    sNOMEDCTEntity_category,
    sNOMEDCTEntity_attributes,
    sNOMEDCTEntity_text,
    sNOMEDCTEntity_sNOMEDCTConcepts,

    -- * SNOMEDCTTrait
    SNOMEDCTTrait (..),
    newSNOMEDCTTrait,
    sNOMEDCTTrait_name,
    sNOMEDCTTrait_score,

    -- * Trait
    Trait (..),
    newTrait,
    trait_name,
    trait_score,

    -- * UnmappedAttribute
    UnmappedAttribute (..),
    newUnmappedAttribute,
    unmappedAttribute_type,
    unmappedAttribute_attribute,
  )
where

import Amazonka.ComprehendMedical.Types.Attribute
import Amazonka.ComprehendMedical.Types.AttributeName
import Amazonka.ComprehendMedical.Types.Characters
import Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobFilter
import Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobProperties
import Amazonka.ComprehendMedical.Types.Entity
import Amazonka.ComprehendMedical.Types.EntitySubType
import Amazonka.ComprehendMedical.Types.EntityType
import Amazonka.ComprehendMedical.Types.ICD10CMAttribute
import Amazonka.ComprehendMedical.Types.ICD10CMAttributeType
import Amazonka.ComprehendMedical.Types.ICD10CMConcept
import Amazonka.ComprehendMedical.Types.ICD10CMEntity
import Amazonka.ComprehendMedical.Types.ICD10CMEntityCategory
import Amazonka.ComprehendMedical.Types.ICD10CMEntityType
import Amazonka.ComprehendMedical.Types.ICD10CMRelationshipType
import Amazonka.ComprehendMedical.Types.ICD10CMTrait
import Amazonka.ComprehendMedical.Types.ICD10CMTraitName
import Amazonka.ComprehendMedical.Types.InputDataConfig
import Amazonka.ComprehendMedical.Types.JobStatus
import Amazonka.ComprehendMedical.Types.LanguageCode
import Amazonka.ComprehendMedical.Types.OutputDataConfig
import Amazonka.ComprehendMedical.Types.RelationshipType
import Amazonka.ComprehendMedical.Types.RxNormAttribute
import Amazonka.ComprehendMedical.Types.RxNormAttributeType
import Amazonka.ComprehendMedical.Types.RxNormConcept
import Amazonka.ComprehendMedical.Types.RxNormEntity
import Amazonka.ComprehendMedical.Types.RxNormEntityCategory
import Amazonka.ComprehendMedical.Types.RxNormEntityType
import Amazonka.ComprehendMedical.Types.RxNormTrait
import Amazonka.ComprehendMedical.Types.RxNormTraitName
import Amazonka.ComprehendMedical.Types.SNOMEDCTAttribute
import Amazonka.ComprehendMedical.Types.SNOMEDCTAttributeType
import Amazonka.ComprehendMedical.Types.SNOMEDCTConcept
import Amazonka.ComprehendMedical.Types.SNOMEDCTDetails
import Amazonka.ComprehendMedical.Types.SNOMEDCTEntity
import Amazonka.ComprehendMedical.Types.SNOMEDCTEntityCategory
import Amazonka.ComprehendMedical.Types.SNOMEDCTEntityType
import Amazonka.ComprehendMedical.Types.SNOMEDCTRelationshipType
import Amazonka.ComprehendMedical.Types.SNOMEDCTTrait
import Amazonka.ComprehendMedical.Types.SNOMEDCTTraitName
import Amazonka.ComprehendMedical.Types.Trait
import Amazonka.ComprehendMedical.Types.UnmappedAttribute
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-10-30@ of the Amazon Comprehend Medical SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ComprehendMedical",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "comprehendmedical",
      Core.signingName = "comprehendmedical",
      Core.version = "2018-10-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ComprehendMedical",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | An internal server error occurred. Retry your request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The Comprehend Medical; service is temporarily unavailable. Please wait
-- and then retry your request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The resource identified by the specified Amazon Resource Name (ARN) was
-- not found. Check the ARN and try your request again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The size of the text you submitted exceeds the size limit. Reduce the
-- size of the text or use a smaller document and then retry your request.
_TextSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TextSizeLimitExceededException"

-- | The filter that you specified for the operation is invalid. Check the
-- filter values that you entered and try your request again.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The input text was not in valid UTF-8 character encoding. Check your
-- text then retry your request.
_InvalidEncodingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEncodingException =
  Core._MatchServiceError
    defaultService
    "InvalidEncodingException"

-- | You have made too many requests within a short period of time. Wait for
-- a short time and then try your request again. Contact customer support
-- for more information about a service limit increase.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | The request that you made is invalid. Check your request to determine
-- why it\'s invalid and then retry the request.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
