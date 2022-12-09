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
    _InvalidEncodingException,
    _InvalidRequestException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,
    _TextSizeLimitExceededException,
    _TooManyRequestsException,
    _ValidationException,

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
    attribute_category,
    attribute_endOffset,
    attribute_id,
    attribute_relationshipScore,
    attribute_relationshipType,
    attribute_score,
    attribute_text,
    attribute_traits,
    attribute_type,

    -- * Characters
    Characters (..),
    newCharacters,
    characters_originalTextCharacters,

    -- * ComprehendMedicalAsyncJobFilter
    ComprehendMedicalAsyncJobFilter (..),
    newComprehendMedicalAsyncJobFilter,
    comprehendMedicalAsyncJobFilter_jobName,
    comprehendMedicalAsyncJobFilter_jobStatus,
    comprehendMedicalAsyncJobFilter_submitTimeAfter,
    comprehendMedicalAsyncJobFilter_submitTimeBefore,

    -- * ComprehendMedicalAsyncJobProperties
    ComprehendMedicalAsyncJobProperties (..),
    newComprehendMedicalAsyncJobProperties,
    comprehendMedicalAsyncJobProperties_dataAccessRoleArn,
    comprehendMedicalAsyncJobProperties_endTime,
    comprehendMedicalAsyncJobProperties_expirationTime,
    comprehendMedicalAsyncJobProperties_inputDataConfig,
    comprehendMedicalAsyncJobProperties_jobId,
    comprehendMedicalAsyncJobProperties_jobName,
    comprehendMedicalAsyncJobProperties_jobStatus,
    comprehendMedicalAsyncJobProperties_kmsKey,
    comprehendMedicalAsyncJobProperties_languageCode,
    comprehendMedicalAsyncJobProperties_manifestFilePath,
    comprehendMedicalAsyncJobProperties_message,
    comprehendMedicalAsyncJobProperties_modelVersion,
    comprehendMedicalAsyncJobProperties_outputDataConfig,
    comprehendMedicalAsyncJobProperties_submitTime,

    -- * Entity
    Entity (..),
    newEntity,
    entity_attributes,
    entity_beginOffset,
    entity_category,
    entity_endOffset,
    entity_id,
    entity_score,
    entity_text,
    entity_traits,
    entity_type,

    -- * ICD10CMAttribute
    ICD10CMAttribute (..),
    newICD10CMAttribute,
    iCD10CMAttribute_beginOffset,
    iCD10CMAttribute_category,
    iCD10CMAttribute_endOffset,
    iCD10CMAttribute_id,
    iCD10CMAttribute_relationshipScore,
    iCD10CMAttribute_relationshipType,
    iCD10CMAttribute_score,
    iCD10CMAttribute_text,
    iCD10CMAttribute_traits,
    iCD10CMAttribute_type,

    -- * ICD10CMConcept
    ICD10CMConcept (..),
    newICD10CMConcept,
    iCD10CMConcept_code,
    iCD10CMConcept_description,
    iCD10CMConcept_score,

    -- * ICD10CMEntity
    ICD10CMEntity (..),
    newICD10CMEntity,
    iCD10CMEntity_attributes,
    iCD10CMEntity_beginOffset,
    iCD10CMEntity_category,
    iCD10CMEntity_endOffset,
    iCD10CMEntity_iCD10CMConcepts,
    iCD10CMEntity_id,
    iCD10CMEntity_score,
    iCD10CMEntity_text,
    iCD10CMEntity_traits,
    iCD10CMEntity_type,

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
    rxNormAttribute_endOffset,
    rxNormAttribute_id,
    rxNormAttribute_relationshipScore,
    rxNormAttribute_score,
    rxNormAttribute_text,
    rxNormAttribute_traits,
    rxNormAttribute_type,

    -- * RxNormConcept
    RxNormConcept (..),
    newRxNormConcept,
    rxNormConcept_code,
    rxNormConcept_description,
    rxNormConcept_score,

    -- * RxNormEntity
    RxNormEntity (..),
    newRxNormEntity,
    rxNormEntity_attributes,
    rxNormEntity_beginOffset,
    rxNormEntity_category,
    rxNormEntity_endOffset,
    rxNormEntity_id,
    rxNormEntity_rxNormConcepts,
    rxNormEntity_score,
    rxNormEntity_text,
    rxNormEntity_traits,
    rxNormEntity_type,

    -- * RxNormTrait
    RxNormTrait (..),
    newRxNormTrait,
    rxNormTrait_name,
    rxNormTrait_score,

    -- * SNOMEDCTAttribute
    SNOMEDCTAttribute (..),
    newSNOMEDCTAttribute,
    sNOMEDCTAttribute_beginOffset,
    sNOMEDCTAttribute_category,
    sNOMEDCTAttribute_endOffset,
    sNOMEDCTAttribute_id,
    sNOMEDCTAttribute_relationshipScore,
    sNOMEDCTAttribute_relationshipType,
    sNOMEDCTAttribute_sNOMEDCTConcepts,
    sNOMEDCTAttribute_score,
    sNOMEDCTAttribute_text,
    sNOMEDCTAttribute_traits,
    sNOMEDCTAttribute_type,

    -- * SNOMEDCTConcept
    SNOMEDCTConcept (..),
    newSNOMEDCTConcept,
    sNOMEDCTConcept_code,
    sNOMEDCTConcept_description,
    sNOMEDCTConcept_score,

    -- * SNOMEDCTDetails
    SNOMEDCTDetails (..),
    newSNOMEDCTDetails,
    sNOMEDCTDetails_edition,
    sNOMEDCTDetails_language,
    sNOMEDCTDetails_versionDate,

    -- * SNOMEDCTEntity
    SNOMEDCTEntity (..),
    newSNOMEDCTEntity,
    sNOMEDCTEntity_attributes,
    sNOMEDCTEntity_beginOffset,
    sNOMEDCTEntity_category,
    sNOMEDCTEntity_endOffset,
    sNOMEDCTEntity_id,
    sNOMEDCTEntity_sNOMEDCTConcepts,
    sNOMEDCTEntity_score,
    sNOMEDCTEntity_text,
    sNOMEDCTEntity_traits,
    sNOMEDCTEntity_type,

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
    unmappedAttribute_attribute,
    unmappedAttribute_type,
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
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An internal server error occurred. Retry your request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The input text was not in valid UTF-8 character encoding. Check your
-- text then retry your request.
_InvalidEncodingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEncodingException =
  Core._MatchServiceError
    defaultService
    "InvalidEncodingException"

-- | The request that you made is invalid. Check your request to determine
-- why it\'s invalid and then retry the request.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The resource identified by the specified Amazon Resource Name (ARN) was
-- not found. Check the ARN and try your request again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The Comprehend Medical; service is temporarily unavailable. Please wait
-- and then retry your request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The size of the text you submitted exceeds the size limit. Reduce the
-- size of the text or use a smaller document and then retry your request.
_TextSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TextSizeLimitExceededException"

-- | You have made too many requests within a short period of time. Wait for
-- a short time and then try your request again. Contact customer support
-- for more information about a service limit increase.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | The filter that you specified for the operation is invalid. Check the
-- filter values that you entered and try your request again.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
