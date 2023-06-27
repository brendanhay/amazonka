{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VerifiedPermissions.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * Decision
    Decision (..),

    -- * OpenIdIssuer
    OpenIdIssuer (..),

    -- * PolicyType
    PolicyType (..),

    -- * ValidationMode
    ValidationMode (..),

    -- * ActionIdentifier
    ActionIdentifier (..),
    newActionIdentifier,
    actionIdentifier_actionType,
    actionIdentifier_actionId,

    -- * AttributeValue
    AttributeValue (..),
    newAttributeValue,
    attributeValue_boolean,
    attributeValue_entityIdentifier,
    attributeValue_long,
    attributeValue_record,
    attributeValue_set,
    attributeValue_string,

    -- * CognitoUserPoolConfiguration
    CognitoUserPoolConfiguration (..),
    newCognitoUserPoolConfiguration,
    cognitoUserPoolConfiguration_clientIds,
    cognitoUserPoolConfiguration_userPoolArn,

    -- * Configuration
    Configuration (..),
    newConfiguration,
    configuration_cognitoUserPoolConfiguration,

    -- * ContextDefinition
    ContextDefinition (..),
    newContextDefinition,
    contextDefinition_contextMap,

    -- * DeterminingPolicyItem
    DeterminingPolicyItem (..),
    newDeterminingPolicyItem,
    determiningPolicyItem_policyId,

    -- * EntitiesDefinition
    EntitiesDefinition (..),
    newEntitiesDefinition,
    entitiesDefinition_entityList,

    -- * EntityIdentifier
    EntityIdentifier (..),
    newEntityIdentifier,
    entityIdentifier_entityType,
    entityIdentifier_entityId,

    -- * EntityItem
    EntityItem (..),
    newEntityItem,
    entityItem_attributes,
    entityItem_parents,
    entityItem_identifier,

    -- * EntityReference
    EntityReference (..),
    newEntityReference,
    entityReference_identifier,
    entityReference_unspecified,

    -- * EvaluationErrorItem
    EvaluationErrorItem (..),
    newEvaluationErrorItem,
    evaluationErrorItem_errorDescription,

    -- * IdentitySourceDetails
    IdentitySourceDetails (..),
    newIdentitySourceDetails,
    identitySourceDetails_clientIds,
    identitySourceDetails_discoveryUrl,
    identitySourceDetails_openIdIssuer,
    identitySourceDetails_userPoolArn,

    -- * IdentitySourceFilter
    IdentitySourceFilter (..),
    newIdentitySourceFilter,
    identitySourceFilter_principalEntityType,

    -- * IdentitySourceItem
    IdentitySourceItem (..),
    newIdentitySourceItem,
    identitySourceItem_createdDate,
    identitySourceItem_details,
    identitySourceItem_identitySourceId,
    identitySourceItem_lastUpdatedDate,
    identitySourceItem_policyStoreId,
    identitySourceItem_principalEntityType,

    -- * IdentitySourceItemDetails
    IdentitySourceItemDetails (..),
    newIdentitySourceItemDetails,
    identitySourceItemDetails_clientIds,
    identitySourceItemDetails_discoveryUrl,
    identitySourceItemDetails_openIdIssuer,
    identitySourceItemDetails_userPoolArn,

    -- * PolicyDefinition
    PolicyDefinition (..),
    newPolicyDefinition,
    policyDefinition_static,
    policyDefinition_templateLinked,

    -- * PolicyDefinitionDetail
    PolicyDefinitionDetail (..),
    newPolicyDefinitionDetail,
    policyDefinitionDetail_static,
    policyDefinitionDetail_templateLinked,

    -- * PolicyDefinitionItem
    PolicyDefinitionItem (..),
    newPolicyDefinitionItem,
    policyDefinitionItem_static,
    policyDefinitionItem_templateLinked,

    -- * PolicyFilter
    PolicyFilter (..),
    newPolicyFilter,
    policyFilter_policyTemplateId,
    policyFilter_policyType,
    policyFilter_principal,
    policyFilter_resource,

    -- * PolicyItem
    PolicyItem (..),
    newPolicyItem,
    policyItem_principal,
    policyItem_resource,
    policyItem_policyStoreId,
    policyItem_policyId,
    policyItem_policyType,
    policyItem_definition,
    policyItem_createdDate,
    policyItem_lastUpdatedDate,

    -- * PolicyStoreItem
    PolicyStoreItem (..),
    newPolicyStoreItem,
    policyStoreItem_policyStoreId,
    policyStoreItem_arn,
    policyStoreItem_createdDate,

    -- * PolicyTemplateItem
    PolicyTemplateItem (..),
    newPolicyTemplateItem,
    policyTemplateItem_description,
    policyTemplateItem_policyStoreId,
    policyTemplateItem_policyTemplateId,
    policyTemplateItem_createdDate,
    policyTemplateItem_lastUpdatedDate,

    -- * SchemaDefinition
    SchemaDefinition (..),
    newSchemaDefinition,
    schemaDefinition_cedarJson,

    -- * StaticPolicyDefinition
    StaticPolicyDefinition (..),
    newStaticPolicyDefinition,
    staticPolicyDefinition_description,
    staticPolicyDefinition_statement,

    -- * StaticPolicyDefinitionDetail
    StaticPolicyDefinitionDetail (..),
    newStaticPolicyDefinitionDetail,
    staticPolicyDefinitionDetail_description,
    staticPolicyDefinitionDetail_statement,

    -- * StaticPolicyDefinitionItem
    StaticPolicyDefinitionItem (..),
    newStaticPolicyDefinitionItem,
    staticPolicyDefinitionItem_description,

    -- * TemplateLinkedPolicyDefinition
    TemplateLinkedPolicyDefinition (..),
    newTemplateLinkedPolicyDefinition,
    templateLinkedPolicyDefinition_principal,
    templateLinkedPolicyDefinition_resource,
    templateLinkedPolicyDefinition_policyTemplateId,

    -- * TemplateLinkedPolicyDefinitionDetail
    TemplateLinkedPolicyDefinitionDetail (..),
    newTemplateLinkedPolicyDefinitionDetail,
    templateLinkedPolicyDefinitionDetail_principal,
    templateLinkedPolicyDefinitionDetail_resource,
    templateLinkedPolicyDefinitionDetail_policyTemplateId,

    -- * TemplateLinkedPolicyDefinitionItem
    TemplateLinkedPolicyDefinitionItem (..),
    newTemplateLinkedPolicyDefinitionItem,
    templateLinkedPolicyDefinitionItem_principal,
    templateLinkedPolicyDefinitionItem_resource,
    templateLinkedPolicyDefinitionItem_policyTemplateId,

    -- * UpdateCognitoUserPoolConfiguration
    UpdateCognitoUserPoolConfiguration (..),
    newUpdateCognitoUserPoolConfiguration,
    updateCognitoUserPoolConfiguration_clientIds,
    updateCognitoUserPoolConfiguration_userPoolArn,

    -- * UpdateConfiguration
    UpdateConfiguration (..),
    newUpdateConfiguration,
    updateConfiguration_cognitoUserPoolConfiguration,

    -- * UpdatePolicyDefinition
    UpdatePolicyDefinition (..),
    newUpdatePolicyDefinition,
    updatePolicyDefinition_static,

    -- * UpdateStaticPolicyDefinition
    UpdateStaticPolicyDefinition (..),
    newUpdateStaticPolicyDefinition,
    updateStaticPolicyDefinition_description,
    updateStaticPolicyDefinition_statement,

    -- * ValidationSettings
    ValidationSettings (..),
    newValidationSettings,
    validationSettings_mode,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.VerifiedPermissions.Types.ActionIdentifier
import Amazonka.VerifiedPermissions.Types.AttributeValue
import Amazonka.VerifiedPermissions.Types.CognitoUserPoolConfiguration
import Amazonka.VerifiedPermissions.Types.Configuration
import Amazonka.VerifiedPermissions.Types.ContextDefinition
import Amazonka.VerifiedPermissions.Types.Decision
import Amazonka.VerifiedPermissions.Types.DeterminingPolicyItem
import Amazonka.VerifiedPermissions.Types.EntitiesDefinition
import Amazonka.VerifiedPermissions.Types.EntityIdentifier
import Amazonka.VerifiedPermissions.Types.EntityItem
import Amazonka.VerifiedPermissions.Types.EntityReference
import Amazonka.VerifiedPermissions.Types.EvaluationErrorItem
import Amazonka.VerifiedPermissions.Types.IdentitySourceDetails
import Amazonka.VerifiedPermissions.Types.IdentitySourceFilter
import Amazonka.VerifiedPermissions.Types.IdentitySourceItem
import Amazonka.VerifiedPermissions.Types.IdentitySourceItemDetails
import Amazonka.VerifiedPermissions.Types.OpenIdIssuer
import Amazonka.VerifiedPermissions.Types.PolicyDefinition
import Amazonka.VerifiedPermissions.Types.PolicyDefinitionDetail
import Amazonka.VerifiedPermissions.Types.PolicyDefinitionItem
import Amazonka.VerifiedPermissions.Types.PolicyFilter
import Amazonka.VerifiedPermissions.Types.PolicyItem
import Amazonka.VerifiedPermissions.Types.PolicyStoreItem
import Amazonka.VerifiedPermissions.Types.PolicyTemplateItem
import Amazonka.VerifiedPermissions.Types.PolicyType
import Amazonka.VerifiedPermissions.Types.SchemaDefinition
import Amazonka.VerifiedPermissions.Types.StaticPolicyDefinition
import Amazonka.VerifiedPermissions.Types.StaticPolicyDefinitionDetail
import Amazonka.VerifiedPermissions.Types.StaticPolicyDefinitionItem
import Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinition
import Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinitionDetail
import Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinitionItem
import Amazonka.VerifiedPermissions.Types.UpdateCognitoUserPoolConfiguration
import Amazonka.VerifiedPermissions.Types.UpdateConfiguration
import Amazonka.VerifiedPermissions.Types.UpdatePolicyDefinition
import Amazonka.VerifiedPermissions.Types.UpdateStaticPolicyDefinition
import Amazonka.VerifiedPermissions.Types.ValidationMode
import Amazonka.VerifiedPermissions.Types.ValidationSettings

-- | API version @2021-12-01@ of the Amazon Verified Permissions SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "VerifiedPermissions",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "verifiedpermissions",
      Core.signingName = "verifiedpermissions",
      Core.version = "2021-12-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "VerifiedPermissions",
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

-- | You don\'t have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request failed because another request to modify a resource occurred
-- at the same.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The request failed because of an internal error. Try your request again
-- later
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The request failed because it references a resource that doesn\'t exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request failed because it would cause a service quota to be
-- exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request failed because it exceeded a throttling quota.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request failed because one or more input parameters don\'t satisfy
-- their constraint requirements. The output is provided as a list of
-- fields and a reason for each field that isn\'t valid.
--
-- The possible reasons include the following:
--
-- -   __UnrecognizedEntityType__
--
--     The policy includes an entity type that isn\'t found in the schema.
--
-- -   __UnrecognizedActionId__
--
--     The policy includes an action id that isn\'t found in the schema.
--
-- -   __InvalidActionApplication__
--
--     The policy includes an action that, according to the schema,
--     doesn\'t support the specified principal and resource.
--
-- -   __UnexpectedType__
--
--     The policy included an operand that isn\'t a valid type for the
--     specified operation.
--
-- -   __IncompatibleTypes__
--
--     The types of elements included in a @set@, or the types of
--     expressions used in an @if...then...else@ clause aren\'t compatible
--     in this context.
--
-- -   __MissingAttribute__
--
--     The policy attempts to access a record or entity attribute that
--     isn\'t specified in the schema. Test for the existence of the
--     attribute first before attempting to access its value. For more
--     information, see the
--     <docs.cedarpolicy.comsyntax-operators.html#has-presence-of-attribute-test has (presence of attribute test) operator>
--     in the /Cedar Policy Language Guide/.
--
-- -   __UnsafeOptionalAttributeAccess__
--
--     The policy attempts to access a record or entity attribute that is
--     optional and isn\'t guaranteed to be present. Test for the existence
--     of the attribute first before attempting to access its value. For
--     more information, see the
--     <docs.cedarpolicy.comsyntax-operators.html#has-presence-of-attribute-test has (presence of attribute test) operator>
--     in the /Cedar Policy Language Guide/.
--
-- -   __ImpossiblePolicy__
--
--     Cedar has determined that a policy condition always evaluates to
--     false. If the policy is always false, it can never apply to any
--     query, and so it can never affect an authorization decision.
--
-- -   __WrongNumberArguments__
--
--     The policy references an extension type with the wrong number of
--     arguments.
--
-- -   __FunctionArgumentValidationError__
--
--     Cedar couldn\'t parse the argument passed to an extension type. For
--     example, a string that is to be parsed as an IPv4 address can
--     contain only digits and the period character.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
