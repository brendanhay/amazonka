{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.VerifiedPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-12-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Verified Permissions is a permissions management service from
-- Amazon Web Services. You can use Verified Permissions to manage
-- permissions for your application, and authorize user access based on
-- those permissions. Using Verified Permissions, application developers
-- can grant access based on information about the users, resources, and
-- requested actions. You can also evaluate additional information like
-- group membership, attributes of the resources, and session context, such
-- as time of request and IP addresses. Verified Permissions manages these
-- permissions by letting you create and store authorization policies for
-- your applications, such as consumer-facing web sites and enterprise
-- business systems.
--
-- Verified Permissions uses Cedar as the policy language to express your
-- permission requirements. Cedar supports both role-based access control
-- (RBAC) and attribute-based access control (ABAC) authorization models.
--
-- For more information about configuring, administering, and using Amazon
-- Verified Permissions in your applications, see the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/userguide/ Amazon Verified Permissions User Guide>.
--
-- For more information about the Cedar policy language, see the
-- <docs.cedarpolicy.com Cedar Policy Language Guide>.
--
-- When you write Cedar policies that reference principals, resources and
-- actions, you can define the unique identifiers used for each of those
-- elements. We strongly recommend that you follow these best practices:
--
-- -   __Use values like universally unique identifiers (UUIDs) for all
--     principal and resource identifiers.__
--
--     For example, if user @jane@ leaves the company, and you later let
--     someone else use the name @jane@, then that new user automatically
--     gets access to everything granted by policies that still reference
--     @User::\"jane\"@. Cedar can’t distinguish between the new user and
--     the old. This applies to both principal and resource identifiers.
--     Always use identifiers that are guaranteed unique and never reused
--     to ensure that you don’t unintentionally grant access because of the
--     presence of an old identifier in a policy.
--
--     Where you use a UUID for an entity, we recommend that you follow it
--     with the \/\/ comment specifier and the ‘friendly’ name of your
--     entity. This helps to make your policies easier to understand. For
--     example: principal ==
--     User::\"a1b2c3d4-e5f6-a1b2-c3d4-EXAMPLE11111\", \/\/ alice
--
-- -   __Do not include personally identifying, confidential, or sensitive
--     information as part of the unique identifier for your principals or
--     resources.__ These identifiers are included in log entries shared in
--     CloudTrail trails.
--
-- Several operations return structures that appear similar, but have
-- different purposes. As new functionality is added to the product, the
-- structure used in a parameter of one operation might need to change in a
-- way that wouldn\'t make sense for the same parameter in a different
-- operation. To help you understand the purpose of each, the following
-- naming convention is used for the structures:
--
-- -   Parameter type structures that end in @Detail@ are used in @Get@
--     operations.
--
-- -   Parameter type structures that end in @Item@ are used in @List@
--     operations.
--
-- -   Parameter type structures that use neither suffix are used in the
--     mutating (create and update) operations.
module Amazonka.VerifiedPermissions
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

    -- ** CreateIdentitySource
    CreateIdentitySource (CreateIdentitySource'),
    newCreateIdentitySource,
    CreateIdentitySourceResponse (CreateIdentitySourceResponse'),
    newCreateIdentitySourceResponse,

    -- ** CreatePolicy
    CreatePolicy (CreatePolicy'),
    newCreatePolicy,
    CreatePolicyResponse (CreatePolicyResponse'),
    newCreatePolicyResponse,

    -- ** CreatePolicyStore
    CreatePolicyStore (CreatePolicyStore'),
    newCreatePolicyStore,
    CreatePolicyStoreResponse (CreatePolicyStoreResponse'),
    newCreatePolicyStoreResponse,

    -- ** CreatePolicyTemplate
    CreatePolicyTemplate (CreatePolicyTemplate'),
    newCreatePolicyTemplate,
    CreatePolicyTemplateResponse (CreatePolicyTemplateResponse'),
    newCreatePolicyTemplateResponse,

    -- ** DeleteIdentitySource
    DeleteIdentitySource (DeleteIdentitySource'),
    newDeleteIdentitySource,
    DeleteIdentitySourceResponse (DeleteIdentitySourceResponse'),
    newDeleteIdentitySourceResponse,

    -- ** DeletePolicy
    DeletePolicy (DeletePolicy'),
    newDeletePolicy,
    DeletePolicyResponse (DeletePolicyResponse'),
    newDeletePolicyResponse,

    -- ** DeletePolicyStore
    DeletePolicyStore (DeletePolicyStore'),
    newDeletePolicyStore,
    DeletePolicyStoreResponse (DeletePolicyStoreResponse'),
    newDeletePolicyStoreResponse,

    -- ** DeletePolicyTemplate
    DeletePolicyTemplate (DeletePolicyTemplate'),
    newDeletePolicyTemplate,
    DeletePolicyTemplateResponse (DeletePolicyTemplateResponse'),
    newDeletePolicyTemplateResponse,

    -- ** GetIdentitySource
    GetIdentitySource (GetIdentitySource'),
    newGetIdentitySource,
    GetIdentitySourceResponse (GetIdentitySourceResponse'),
    newGetIdentitySourceResponse,

    -- ** GetPolicy
    GetPolicy (GetPolicy'),
    newGetPolicy,
    GetPolicyResponse (GetPolicyResponse'),
    newGetPolicyResponse,

    -- ** GetPolicyStore
    GetPolicyStore (GetPolicyStore'),
    newGetPolicyStore,
    GetPolicyStoreResponse (GetPolicyStoreResponse'),
    newGetPolicyStoreResponse,

    -- ** GetPolicyTemplate
    GetPolicyTemplate (GetPolicyTemplate'),
    newGetPolicyTemplate,
    GetPolicyTemplateResponse (GetPolicyTemplateResponse'),
    newGetPolicyTemplateResponse,

    -- ** GetSchema
    GetSchema (GetSchema'),
    newGetSchema,
    GetSchemaResponse (GetSchemaResponse'),
    newGetSchemaResponse,

    -- ** IsAuthorized
    IsAuthorized (IsAuthorized'),
    newIsAuthorized,
    IsAuthorizedResponse (IsAuthorizedResponse'),
    newIsAuthorizedResponse,

    -- ** IsAuthorizedWithToken
    IsAuthorizedWithToken (IsAuthorizedWithToken'),
    newIsAuthorizedWithToken,
    IsAuthorizedWithTokenResponse (IsAuthorizedWithTokenResponse'),
    newIsAuthorizedWithTokenResponse,

    -- ** ListIdentitySources (Paginated)
    ListIdentitySources (ListIdentitySources'),
    newListIdentitySources,
    ListIdentitySourcesResponse (ListIdentitySourcesResponse'),
    newListIdentitySourcesResponse,

    -- ** ListPolicies (Paginated)
    ListPolicies (ListPolicies'),
    newListPolicies,
    ListPoliciesResponse (ListPoliciesResponse'),
    newListPoliciesResponse,

    -- ** ListPolicyStores (Paginated)
    ListPolicyStores (ListPolicyStores'),
    newListPolicyStores,
    ListPolicyStoresResponse (ListPolicyStoresResponse'),
    newListPolicyStoresResponse,

    -- ** ListPolicyTemplates (Paginated)
    ListPolicyTemplates (ListPolicyTemplates'),
    newListPolicyTemplates,
    ListPolicyTemplatesResponse (ListPolicyTemplatesResponse'),
    newListPolicyTemplatesResponse,

    -- ** PutSchema
    PutSchema (PutSchema'),
    newPutSchema,
    PutSchemaResponse (PutSchemaResponse'),
    newPutSchemaResponse,

    -- ** UpdateIdentitySource
    UpdateIdentitySource (UpdateIdentitySource'),
    newUpdateIdentitySource,
    UpdateIdentitySourceResponse (UpdateIdentitySourceResponse'),
    newUpdateIdentitySourceResponse,

    -- ** UpdatePolicy
    UpdatePolicy (UpdatePolicy'),
    newUpdatePolicy,
    UpdatePolicyResponse (UpdatePolicyResponse'),
    newUpdatePolicyResponse,

    -- ** UpdatePolicyStore
    UpdatePolicyStore (UpdatePolicyStore'),
    newUpdatePolicyStore,
    UpdatePolicyStoreResponse (UpdatePolicyStoreResponse'),
    newUpdatePolicyStoreResponse,

    -- ** UpdatePolicyTemplate
    UpdatePolicyTemplate (UpdatePolicyTemplate'),
    newUpdatePolicyTemplate,
    UpdatePolicyTemplateResponse (UpdatePolicyTemplateResponse'),
    newUpdatePolicyTemplateResponse,

    -- * Types

    -- ** Decision
    Decision (..),

    -- ** OpenIdIssuer
    OpenIdIssuer (..),

    -- ** PolicyType
    PolicyType (..),

    -- ** ValidationMode
    ValidationMode (..),

    -- ** ActionIdentifier
    ActionIdentifier (ActionIdentifier'),
    newActionIdentifier,

    -- ** AttributeValue
    AttributeValue (AttributeValue'),
    newAttributeValue,

    -- ** CognitoUserPoolConfiguration
    CognitoUserPoolConfiguration (CognitoUserPoolConfiguration'),
    newCognitoUserPoolConfiguration,

    -- ** Configuration
    Configuration (Configuration'),
    newConfiguration,

    -- ** ContextDefinition
    ContextDefinition (ContextDefinition'),
    newContextDefinition,

    -- ** DeterminingPolicyItem
    DeterminingPolicyItem (DeterminingPolicyItem'),
    newDeterminingPolicyItem,

    -- ** EntitiesDefinition
    EntitiesDefinition (EntitiesDefinition'),
    newEntitiesDefinition,

    -- ** EntityIdentifier
    EntityIdentifier (EntityIdentifier'),
    newEntityIdentifier,

    -- ** EntityItem
    EntityItem (EntityItem'),
    newEntityItem,

    -- ** EntityReference
    EntityReference (EntityReference'),
    newEntityReference,

    -- ** EvaluationErrorItem
    EvaluationErrorItem (EvaluationErrorItem'),
    newEvaluationErrorItem,

    -- ** IdentitySourceDetails
    IdentitySourceDetails (IdentitySourceDetails'),
    newIdentitySourceDetails,

    -- ** IdentitySourceFilter
    IdentitySourceFilter (IdentitySourceFilter'),
    newIdentitySourceFilter,

    -- ** IdentitySourceItem
    IdentitySourceItem (IdentitySourceItem'),
    newIdentitySourceItem,

    -- ** IdentitySourceItemDetails
    IdentitySourceItemDetails (IdentitySourceItemDetails'),
    newIdentitySourceItemDetails,

    -- ** PolicyDefinition
    PolicyDefinition (PolicyDefinition'),
    newPolicyDefinition,

    -- ** PolicyDefinitionDetail
    PolicyDefinitionDetail (PolicyDefinitionDetail'),
    newPolicyDefinitionDetail,

    -- ** PolicyDefinitionItem
    PolicyDefinitionItem (PolicyDefinitionItem'),
    newPolicyDefinitionItem,

    -- ** PolicyFilter
    PolicyFilter (PolicyFilter'),
    newPolicyFilter,

    -- ** PolicyItem
    PolicyItem (PolicyItem'),
    newPolicyItem,

    -- ** PolicyStoreItem
    PolicyStoreItem (PolicyStoreItem'),
    newPolicyStoreItem,

    -- ** PolicyTemplateItem
    PolicyTemplateItem (PolicyTemplateItem'),
    newPolicyTemplateItem,

    -- ** SchemaDefinition
    SchemaDefinition (SchemaDefinition'),
    newSchemaDefinition,

    -- ** StaticPolicyDefinition
    StaticPolicyDefinition (StaticPolicyDefinition'),
    newStaticPolicyDefinition,

    -- ** StaticPolicyDefinitionDetail
    StaticPolicyDefinitionDetail (StaticPolicyDefinitionDetail'),
    newStaticPolicyDefinitionDetail,

    -- ** StaticPolicyDefinitionItem
    StaticPolicyDefinitionItem (StaticPolicyDefinitionItem'),
    newStaticPolicyDefinitionItem,

    -- ** TemplateLinkedPolicyDefinition
    TemplateLinkedPolicyDefinition (TemplateLinkedPolicyDefinition'),
    newTemplateLinkedPolicyDefinition,

    -- ** TemplateLinkedPolicyDefinitionDetail
    TemplateLinkedPolicyDefinitionDetail (TemplateLinkedPolicyDefinitionDetail'),
    newTemplateLinkedPolicyDefinitionDetail,

    -- ** TemplateLinkedPolicyDefinitionItem
    TemplateLinkedPolicyDefinitionItem (TemplateLinkedPolicyDefinitionItem'),
    newTemplateLinkedPolicyDefinitionItem,

    -- ** UpdateCognitoUserPoolConfiguration
    UpdateCognitoUserPoolConfiguration (UpdateCognitoUserPoolConfiguration'),
    newUpdateCognitoUserPoolConfiguration,

    -- ** UpdateConfiguration
    UpdateConfiguration (UpdateConfiguration'),
    newUpdateConfiguration,

    -- ** UpdatePolicyDefinition
    UpdatePolicyDefinition (UpdatePolicyDefinition'),
    newUpdatePolicyDefinition,

    -- ** UpdateStaticPolicyDefinition
    UpdateStaticPolicyDefinition (UpdateStaticPolicyDefinition'),
    newUpdateStaticPolicyDefinition,

    -- ** ValidationSettings
    ValidationSettings (ValidationSettings'),
    newValidationSettings,
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
import Amazonka.VerifiedPermissions.Lens
import Amazonka.VerifiedPermissions.ListIdentitySources
import Amazonka.VerifiedPermissions.ListPolicies
import Amazonka.VerifiedPermissions.ListPolicyStores
import Amazonka.VerifiedPermissions.ListPolicyTemplates
import Amazonka.VerifiedPermissions.PutSchema
import Amazonka.VerifiedPermissions.Types
import Amazonka.VerifiedPermissions.UpdateIdentitySource
import Amazonka.VerifiedPermissions.UpdatePolicy
import Amazonka.VerifiedPermissions.UpdatePolicyStore
import Amazonka.VerifiedPermissions.UpdatePolicyTemplate
import Amazonka.VerifiedPermissions.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'VerifiedPermissions'.

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
