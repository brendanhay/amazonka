{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VerifiedPermissions.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Lens
  ( -- * Operations

    -- ** CreateIdentitySource
    createIdentitySource_clientToken,
    createIdentitySource_principalEntityType,
    createIdentitySource_policyStoreId,
    createIdentitySource_configuration,
    createIdentitySourceResponse_httpStatus,
    createIdentitySourceResponse_createdDate,
    createIdentitySourceResponse_identitySourceId,
    createIdentitySourceResponse_lastUpdatedDate,
    createIdentitySourceResponse_policyStoreId,

    -- ** CreatePolicy
    createPolicy_clientToken,
    createPolicy_policyStoreId,
    createPolicy_definition,
    createPolicyResponse_principal,
    createPolicyResponse_resource,
    createPolicyResponse_httpStatus,
    createPolicyResponse_policyStoreId,
    createPolicyResponse_policyId,
    createPolicyResponse_policyType,
    createPolicyResponse_createdDate,
    createPolicyResponse_lastUpdatedDate,

    -- ** CreatePolicyStore
    createPolicyStore_clientToken,
    createPolicyStore_validationSettings,
    createPolicyStoreResponse_httpStatus,
    createPolicyStoreResponse_policyStoreId,
    createPolicyStoreResponse_arn,
    createPolicyStoreResponse_createdDate,
    createPolicyStoreResponse_lastUpdatedDate,

    -- ** CreatePolicyTemplate
    createPolicyTemplate_clientToken,
    createPolicyTemplate_description,
    createPolicyTemplate_policyStoreId,
    createPolicyTemplate_statement,
    createPolicyTemplateResponse_httpStatus,
    createPolicyTemplateResponse_policyStoreId,
    createPolicyTemplateResponse_policyTemplateId,
    createPolicyTemplateResponse_createdDate,
    createPolicyTemplateResponse_lastUpdatedDate,

    -- ** DeleteIdentitySource
    deleteIdentitySource_policyStoreId,
    deleteIdentitySource_identitySourceId,
    deleteIdentitySourceResponse_httpStatus,

    -- ** DeletePolicy
    deletePolicy_policyStoreId,
    deletePolicy_policyId,
    deletePolicyResponse_httpStatus,

    -- ** DeletePolicyStore
    deletePolicyStore_policyStoreId,
    deletePolicyStoreResponse_httpStatus,

    -- ** DeletePolicyTemplate
    deletePolicyTemplate_policyStoreId,
    deletePolicyTemplate_policyTemplateId,
    deletePolicyTemplateResponse_httpStatus,

    -- ** GetIdentitySource
    getIdentitySource_policyStoreId,
    getIdentitySource_identitySourceId,
    getIdentitySourceResponse_httpStatus,
    getIdentitySourceResponse_createdDate,
    getIdentitySourceResponse_details,
    getIdentitySourceResponse_identitySourceId,
    getIdentitySourceResponse_lastUpdatedDate,
    getIdentitySourceResponse_policyStoreId,
    getIdentitySourceResponse_principalEntityType,

    -- ** GetPolicy
    getPolicy_policyStoreId,
    getPolicy_policyId,
    getPolicyResponse_principal,
    getPolicyResponse_resource,
    getPolicyResponse_httpStatus,
    getPolicyResponse_policyStoreId,
    getPolicyResponse_policyId,
    getPolicyResponse_policyType,
    getPolicyResponse_definition,
    getPolicyResponse_createdDate,
    getPolicyResponse_lastUpdatedDate,

    -- ** GetPolicyStore
    getPolicyStore_policyStoreId,
    getPolicyStoreResponse_httpStatus,
    getPolicyStoreResponse_policyStoreId,
    getPolicyStoreResponse_arn,
    getPolicyStoreResponse_validationSettings,
    getPolicyStoreResponse_createdDate,
    getPolicyStoreResponse_lastUpdatedDate,

    -- ** GetPolicyTemplate
    getPolicyTemplate_policyStoreId,
    getPolicyTemplate_policyTemplateId,
    getPolicyTemplateResponse_description,
    getPolicyTemplateResponse_httpStatus,
    getPolicyTemplateResponse_policyStoreId,
    getPolicyTemplateResponse_policyTemplateId,
    getPolicyTemplateResponse_statement,
    getPolicyTemplateResponse_createdDate,
    getPolicyTemplateResponse_lastUpdatedDate,

    -- ** GetSchema
    getSchema_policyStoreId,
    getSchemaResponse_httpStatus,
    getSchemaResponse_policyStoreId,
    getSchemaResponse_schema,
    getSchemaResponse_createdDate,
    getSchemaResponse_lastUpdatedDate,

    -- ** IsAuthorized
    isAuthorized_action,
    isAuthorized_context,
    isAuthorized_entities,
    isAuthorized_principal,
    isAuthorized_resource,
    isAuthorized_policyStoreId,
    isAuthorizedResponse_httpStatus,
    isAuthorizedResponse_decision,
    isAuthorizedResponse_determiningPolicies,
    isAuthorizedResponse_errors,

    -- ** IsAuthorizedWithToken
    isAuthorizedWithToken_accessToken,
    isAuthorizedWithToken_action,
    isAuthorizedWithToken_context,
    isAuthorizedWithToken_entities,
    isAuthorizedWithToken_identityToken,
    isAuthorizedWithToken_resource,
    isAuthorizedWithToken_policyStoreId,
    isAuthorizedWithTokenResponse_httpStatus,
    isAuthorizedWithTokenResponse_decision,
    isAuthorizedWithTokenResponse_determiningPolicies,
    isAuthorizedWithTokenResponse_errors,

    -- ** ListIdentitySources
    listIdentitySources_filters,
    listIdentitySources_maxResults,
    listIdentitySources_nextToken,
    listIdentitySources_policyStoreId,
    listIdentitySourcesResponse_nextToken,
    listIdentitySourcesResponse_httpStatus,
    listIdentitySourcesResponse_identitySources,

    -- ** ListPolicies
    listPolicies_filter,
    listPolicies_maxResults,
    listPolicies_nextToken,
    listPolicies_policyStoreId,
    listPoliciesResponse_nextToken,
    listPoliciesResponse_httpStatus,
    listPoliciesResponse_policies,

    -- ** ListPolicyStores
    listPolicyStores_maxResults,
    listPolicyStores_nextToken,
    listPolicyStoresResponse_nextToken,
    listPolicyStoresResponse_httpStatus,
    listPolicyStoresResponse_policyStores,

    -- ** ListPolicyTemplates
    listPolicyTemplates_maxResults,
    listPolicyTemplates_nextToken,
    listPolicyTemplates_policyStoreId,
    listPolicyTemplatesResponse_nextToken,
    listPolicyTemplatesResponse_httpStatus,
    listPolicyTemplatesResponse_policyTemplates,

    -- ** PutSchema
    putSchema_policyStoreId,
    putSchema_definition,
    putSchemaResponse_httpStatus,
    putSchemaResponse_policyStoreId,
    putSchemaResponse_namespaces,
    putSchemaResponse_createdDate,
    putSchemaResponse_lastUpdatedDate,

    -- ** UpdateIdentitySource
    updateIdentitySource_principalEntityType,
    updateIdentitySource_policyStoreId,
    updateIdentitySource_identitySourceId,
    updateIdentitySource_updateConfiguration,
    updateIdentitySourceResponse_httpStatus,
    updateIdentitySourceResponse_createdDate,
    updateIdentitySourceResponse_identitySourceId,
    updateIdentitySourceResponse_lastUpdatedDate,
    updateIdentitySourceResponse_policyStoreId,

    -- ** UpdatePolicy
    updatePolicy_policyStoreId,
    updatePolicy_policyId,
    updatePolicy_definition,
    updatePolicyResponse_principal,
    updatePolicyResponse_resource,
    updatePolicyResponse_httpStatus,
    updatePolicyResponse_policyStoreId,
    updatePolicyResponse_policyId,
    updatePolicyResponse_policyType,
    updatePolicyResponse_createdDate,
    updatePolicyResponse_lastUpdatedDate,

    -- ** UpdatePolicyStore
    updatePolicyStore_policyStoreId,
    updatePolicyStore_validationSettings,
    updatePolicyStoreResponse_httpStatus,
    updatePolicyStoreResponse_policyStoreId,
    updatePolicyStoreResponse_arn,
    updatePolicyStoreResponse_createdDate,
    updatePolicyStoreResponse_lastUpdatedDate,

    -- ** UpdatePolicyTemplate
    updatePolicyTemplate_description,
    updatePolicyTemplate_policyStoreId,
    updatePolicyTemplate_policyTemplateId,
    updatePolicyTemplate_statement,
    updatePolicyTemplateResponse_httpStatus,
    updatePolicyTemplateResponse_policyStoreId,
    updatePolicyTemplateResponse_policyTemplateId,
    updatePolicyTemplateResponse_createdDate,
    updatePolicyTemplateResponse_lastUpdatedDate,

    -- * Types

    -- ** ActionIdentifier
    actionIdentifier_actionType,
    actionIdentifier_actionId,

    -- ** AttributeValue
    attributeValue_boolean,
    attributeValue_entityIdentifier,
    attributeValue_long,
    attributeValue_record,
    attributeValue_set,
    attributeValue_string,

    -- ** CognitoUserPoolConfiguration
    cognitoUserPoolConfiguration_clientIds,
    cognitoUserPoolConfiguration_userPoolArn,

    -- ** Configuration
    configuration_cognitoUserPoolConfiguration,

    -- ** ContextDefinition
    contextDefinition_contextMap,

    -- ** DeterminingPolicyItem
    determiningPolicyItem_policyId,

    -- ** EntitiesDefinition
    entitiesDefinition_entityList,

    -- ** EntityIdentifier
    entityIdentifier_entityType,
    entityIdentifier_entityId,

    -- ** EntityItem
    entityItem_attributes,
    entityItem_parents,
    entityItem_identifier,

    -- ** EntityReference
    entityReference_identifier,
    entityReference_unspecified,

    -- ** EvaluationErrorItem
    evaluationErrorItem_errorDescription,

    -- ** IdentitySourceDetails
    identitySourceDetails_clientIds,
    identitySourceDetails_discoveryUrl,
    identitySourceDetails_openIdIssuer,
    identitySourceDetails_userPoolArn,

    -- ** IdentitySourceFilter
    identitySourceFilter_principalEntityType,

    -- ** IdentitySourceItem
    identitySourceItem_createdDate,
    identitySourceItem_details,
    identitySourceItem_identitySourceId,
    identitySourceItem_lastUpdatedDate,
    identitySourceItem_policyStoreId,
    identitySourceItem_principalEntityType,

    -- ** IdentitySourceItemDetails
    identitySourceItemDetails_clientIds,
    identitySourceItemDetails_discoveryUrl,
    identitySourceItemDetails_openIdIssuer,
    identitySourceItemDetails_userPoolArn,

    -- ** PolicyDefinition
    policyDefinition_static,
    policyDefinition_templateLinked,

    -- ** PolicyDefinitionDetail
    policyDefinitionDetail_static,
    policyDefinitionDetail_templateLinked,

    -- ** PolicyDefinitionItem
    policyDefinitionItem_static,
    policyDefinitionItem_templateLinked,

    -- ** PolicyFilter
    policyFilter_policyTemplateId,
    policyFilter_policyType,
    policyFilter_principal,
    policyFilter_resource,

    -- ** PolicyItem
    policyItem_principal,
    policyItem_resource,
    policyItem_policyStoreId,
    policyItem_policyId,
    policyItem_policyType,
    policyItem_definition,
    policyItem_createdDate,
    policyItem_lastUpdatedDate,

    -- ** PolicyStoreItem
    policyStoreItem_policyStoreId,
    policyStoreItem_arn,
    policyStoreItem_createdDate,

    -- ** PolicyTemplateItem
    policyTemplateItem_description,
    policyTemplateItem_policyStoreId,
    policyTemplateItem_policyTemplateId,
    policyTemplateItem_createdDate,
    policyTemplateItem_lastUpdatedDate,

    -- ** SchemaDefinition
    schemaDefinition_cedarJson,

    -- ** StaticPolicyDefinition
    staticPolicyDefinition_description,
    staticPolicyDefinition_statement,

    -- ** StaticPolicyDefinitionDetail
    staticPolicyDefinitionDetail_description,
    staticPolicyDefinitionDetail_statement,

    -- ** StaticPolicyDefinitionItem
    staticPolicyDefinitionItem_description,

    -- ** TemplateLinkedPolicyDefinition
    templateLinkedPolicyDefinition_principal,
    templateLinkedPolicyDefinition_resource,
    templateLinkedPolicyDefinition_policyTemplateId,

    -- ** TemplateLinkedPolicyDefinitionDetail
    templateLinkedPolicyDefinitionDetail_principal,
    templateLinkedPolicyDefinitionDetail_resource,
    templateLinkedPolicyDefinitionDetail_policyTemplateId,

    -- ** TemplateLinkedPolicyDefinitionItem
    templateLinkedPolicyDefinitionItem_principal,
    templateLinkedPolicyDefinitionItem_resource,
    templateLinkedPolicyDefinitionItem_policyTemplateId,

    -- ** UpdateCognitoUserPoolConfiguration
    updateCognitoUserPoolConfiguration_clientIds,
    updateCognitoUserPoolConfiguration_userPoolArn,

    -- ** UpdateConfiguration
    updateConfiguration_cognitoUserPoolConfiguration,

    -- ** UpdatePolicyDefinition
    updatePolicyDefinition_static,

    -- ** UpdateStaticPolicyDefinition
    updateStaticPolicyDefinition_description,
    updateStaticPolicyDefinition_statement,

    -- ** ValidationSettings
    validationSettings_mode,
  )
where

import Amazonka.VerifiedPermissions.CreateIdentitySource
import Amazonka.VerifiedPermissions.CreatePolicy
import Amazonka.VerifiedPermissions.CreatePolicyStore
import Amazonka.VerifiedPermissions.CreatePolicyTemplate
import Amazonka.VerifiedPermissions.DeleteIdentitySource
import Amazonka.VerifiedPermissions.DeletePolicy
import Amazonka.VerifiedPermissions.DeletePolicyStore
import Amazonka.VerifiedPermissions.DeletePolicyTemplate
import Amazonka.VerifiedPermissions.GetIdentitySource
import Amazonka.VerifiedPermissions.GetPolicy
import Amazonka.VerifiedPermissions.GetPolicyStore
import Amazonka.VerifiedPermissions.GetPolicyTemplate
import Amazonka.VerifiedPermissions.GetSchema
import Amazonka.VerifiedPermissions.IsAuthorized
import Amazonka.VerifiedPermissions.IsAuthorizedWithToken
import Amazonka.VerifiedPermissions.ListIdentitySources
import Amazonka.VerifiedPermissions.ListPolicies
import Amazonka.VerifiedPermissions.ListPolicyStores
import Amazonka.VerifiedPermissions.ListPolicyTemplates
import Amazonka.VerifiedPermissions.PutSchema
import Amazonka.VerifiedPermissions.Types.ActionIdentifier
import Amazonka.VerifiedPermissions.Types.AttributeValue
import Amazonka.VerifiedPermissions.Types.CognitoUserPoolConfiguration
import Amazonka.VerifiedPermissions.Types.Configuration
import Amazonka.VerifiedPermissions.Types.ContextDefinition
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
import Amazonka.VerifiedPermissions.Types.PolicyDefinition
import Amazonka.VerifiedPermissions.Types.PolicyDefinitionDetail
import Amazonka.VerifiedPermissions.Types.PolicyDefinitionItem
import Amazonka.VerifiedPermissions.Types.PolicyFilter
import Amazonka.VerifiedPermissions.Types.PolicyItem
import Amazonka.VerifiedPermissions.Types.PolicyStoreItem
import Amazonka.VerifiedPermissions.Types.PolicyTemplateItem
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
import Amazonka.VerifiedPermissions.Types.ValidationSettings
import Amazonka.VerifiedPermissions.UpdateIdentitySource
import Amazonka.VerifiedPermissions.UpdatePolicy
import Amazonka.VerifiedPermissions.UpdatePolicyStore
import Amazonka.VerifiedPermissions.UpdatePolicyTemplate
