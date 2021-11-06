{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CustomerProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Connect Customer Profiles
--
-- Welcome to the Amazon Connect Customer Profiles API Reference. This
-- guide provides information about the Amazon Connect Customer Profiles
-- API, including supported operations, data types, parameters, and
-- schemas.
--
-- Amazon Connect Customer Profiles is a unified customer profile for your
-- contact center that has pre-built connectors powered by AppFlow that
-- make it easy to combine customer information from third party
-- applications, such as Salesforce (CRM), ServiceNow (ITSM), and your
-- enterprise resource planning (ERP), with contact history from your
-- Amazon Connect contact center.
--
-- If you\'re new to Amazon Connect , you might find it helpful to also
-- review the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/what-is-amazon-connect.html Amazon Connect Administrator Guide>.
module Amazonka.CustomerProfiles
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteProfileObjectType
    DeleteProfileObjectType (DeleteProfileObjectType'),
    newDeleteProfileObjectType,
    DeleteProfileObjectTypeResponse (DeleteProfileObjectTypeResponse'),
    newDeleteProfileObjectTypeResponse,

    -- ** ListIntegrations
    ListIntegrations (ListIntegrations'),
    newListIntegrations,
    ListIntegrationsResponse (ListIntegrationsResponse'),
    newListIntegrationsResponse,

    -- ** PutProfileObjectType
    PutProfileObjectType (PutProfileObjectType'),
    newPutProfileObjectType,
    PutProfileObjectTypeResponse (PutProfileObjectTypeResponse'),
    newPutProfileObjectTypeResponse,

    -- ** ListProfileObjects
    ListProfileObjects (ListProfileObjects'),
    newListProfileObjects,
    ListProfileObjectsResponse (ListProfileObjectsResponse'),
    newListProfileObjectsResponse,

    -- ** ListProfileObjectTypeTemplates
    ListProfileObjectTypeTemplates (ListProfileObjectTypeTemplates'),
    newListProfileObjectTypeTemplates,
    ListProfileObjectTypeTemplatesResponse (ListProfileObjectTypeTemplatesResponse'),
    newListProfileObjectTypeTemplatesResponse,

    -- ** DeleteProfile
    DeleteProfile (DeleteProfile'),
    newDeleteProfile,
    DeleteProfileResponse (DeleteProfileResponse'),
    newDeleteProfileResponse,

    -- ** UpdateProfile
    UpdateProfile (UpdateProfile'),
    newUpdateProfile,
    UpdateProfileResponse (UpdateProfileResponse'),
    newUpdateProfileResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** AddProfileKey
    AddProfileKey (AddProfileKey'),
    newAddProfileKey,
    AddProfileKeyResponse (AddProfileKeyResponse'),
    newAddProfileKeyResponse,

    -- ** GetProfileObjectTypeTemplate
    GetProfileObjectTypeTemplate (GetProfileObjectTypeTemplate'),
    newGetProfileObjectTypeTemplate,
    GetProfileObjectTypeTemplateResponse (GetProfileObjectTypeTemplateResponse'),
    newGetProfileObjectTypeTemplateResponse,

    -- ** GetIntegration
    GetIntegration (GetIntegration'),
    newGetIntegration,
    GetIntegrationResponse (GetIntegrationResponse'),
    newGetIntegrationResponse,

    -- ** GetDomain
    GetDomain (GetDomain'),
    newGetDomain,
    GetDomainResponse (GetDomainResponse'),
    newGetDomainResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** DeleteIntegration
    DeleteIntegration (DeleteIntegration'),
    newDeleteIntegration,
    DeleteIntegrationResponse (DeleteIntegrationResponse'),
    newDeleteIntegrationResponse,

    -- ** CreateProfile
    CreateProfile (CreateProfile'),
    newCreateProfile,
    CreateProfileResponse (CreateProfileResponse'),
    newCreateProfileResponse,

    -- ** PutProfileObject
    PutProfileObject (PutProfileObject'),
    newPutProfileObject,
    PutProfileObjectResponse (PutProfileObjectResponse'),
    newPutProfileObjectResponse,

    -- ** PutIntegration
    PutIntegration (PutIntegration'),
    newPutIntegration,
    PutIntegrationResponse (PutIntegrationResponse'),
    newPutIntegrationResponse,

    -- ** DeleteProfileObject
    DeleteProfileObject (DeleteProfileObject'),
    newDeleteProfileObject,
    DeleteProfileObjectResponse (DeleteProfileObjectResponse'),
    newDeleteProfileObjectResponse,

    -- ** ListProfileObjectTypes
    ListProfileObjectTypes (ListProfileObjectTypes'),
    newListProfileObjectTypes,
    ListProfileObjectTypesResponse (ListProfileObjectTypesResponse'),
    newListProfileObjectTypesResponse,

    -- ** DeleteProfileKey
    DeleteProfileKey (DeleteProfileKey'),
    newDeleteProfileKey,
    DeleteProfileKeyResponse (DeleteProfileKeyResponse'),
    newDeleteProfileKeyResponse,

    -- ** GetProfileObjectType
    GetProfileObjectType (GetProfileObjectType'),
    newGetProfileObjectType,
    GetProfileObjectTypeResponse (GetProfileObjectTypeResponse'),
    newGetProfileObjectTypeResponse,

    -- ** MergeProfiles
    MergeProfiles (MergeProfiles'),
    newMergeProfiles,
    MergeProfilesResponse (MergeProfilesResponse'),
    newMergeProfilesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetMatches
    GetMatches (GetMatches'),
    newGetMatches,
    GetMatchesResponse (GetMatchesResponse'),
    newGetMatchesResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** SearchProfiles
    SearchProfiles (SearchProfiles'),
    newSearchProfiles,
    SearchProfilesResponse (SearchProfilesResponse'),
    newSearchProfilesResponse,

    -- ** ListAccountIntegrations
    ListAccountIntegrations (ListAccountIntegrations'),
    newListAccountIntegrations,
    ListAccountIntegrationsResponse (ListAccountIntegrationsResponse'),
    newListAccountIntegrationsResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** UpdateDomain
    UpdateDomain (UpdateDomain'),
    newUpdateDomain,
    UpdateDomainResponse (UpdateDomainResponse'),
    newUpdateDomainResponse,

    -- ** ListDomains
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- * Types

    -- ** DataPullMode
    DataPullMode (..),

    -- ** FieldContentType
    FieldContentType (..),

    -- ** Gender
    Gender (..),

    -- ** MarketoConnectorOperator
    MarketoConnectorOperator (..),

    -- ** OperatorPropertiesKeys
    OperatorPropertiesKeys (..),

    -- ** PartyType
    PartyType (..),

    -- ** S3ConnectorOperator
    S3ConnectorOperator (..),

    -- ** SalesforceConnectorOperator
    SalesforceConnectorOperator (..),

    -- ** ServiceNowConnectorOperator
    ServiceNowConnectorOperator (..),

    -- ** SourceConnectorType
    SourceConnectorType (..),

    -- ** StandardIdentifier
    StandardIdentifier (..),

    -- ** TaskType
    TaskType (..),

    -- ** TriggerType
    TriggerType (..),

    -- ** ZendeskConnectorOperator
    ZendeskConnectorOperator (..),

    -- ** Address
    Address (Address'),
    newAddress,

    -- ** ConnectorOperator
    ConnectorOperator (ConnectorOperator'),
    newConnectorOperator,

    -- ** DomainStats
    DomainStats (DomainStats'),
    newDomainStats,

    -- ** FieldSourceProfileIds
    FieldSourceProfileIds (FieldSourceProfileIds'),
    newFieldSourceProfileIds,

    -- ** FlowDefinition
    FlowDefinition (FlowDefinition'),
    newFlowDefinition,

    -- ** IncrementalPullConfig
    IncrementalPullConfig (IncrementalPullConfig'),
    newIncrementalPullConfig,

    -- ** ListDomainItem
    ListDomainItem (ListDomainItem'),
    newListDomainItem,

    -- ** ListIntegrationItem
    ListIntegrationItem (ListIntegrationItem'),
    newListIntegrationItem,

    -- ** ListProfileObjectTypeItem
    ListProfileObjectTypeItem (ListProfileObjectTypeItem'),
    newListProfileObjectTypeItem,

    -- ** ListProfileObjectTypeTemplateItem
    ListProfileObjectTypeTemplateItem (ListProfileObjectTypeTemplateItem'),
    newListProfileObjectTypeTemplateItem,

    -- ** ListProfileObjectsItem
    ListProfileObjectsItem (ListProfileObjectsItem'),
    newListProfileObjectsItem,

    -- ** MarketoSourceProperties
    MarketoSourceProperties (MarketoSourceProperties'),
    newMarketoSourceProperties,

    -- ** MatchItem
    MatchItem (MatchItem'),
    newMatchItem,

    -- ** MatchingRequest
    MatchingRequest (MatchingRequest'),
    newMatchingRequest,

    -- ** MatchingResponse
    MatchingResponse (MatchingResponse'),
    newMatchingResponse,

    -- ** ObjectFilter
    ObjectFilter (ObjectFilter'),
    newObjectFilter,

    -- ** ObjectTypeField
    ObjectTypeField (ObjectTypeField'),
    newObjectTypeField,

    -- ** ObjectTypeKey
    ObjectTypeKey (ObjectTypeKey'),
    newObjectTypeKey,

    -- ** Profile
    Profile (Profile'),
    newProfile,

    -- ** S3SourceProperties
    S3SourceProperties (S3SourceProperties'),
    newS3SourceProperties,

    -- ** SalesforceSourceProperties
    SalesforceSourceProperties (SalesforceSourceProperties'),
    newSalesforceSourceProperties,

    -- ** ScheduledTriggerProperties
    ScheduledTriggerProperties (ScheduledTriggerProperties'),
    newScheduledTriggerProperties,

    -- ** ServiceNowSourceProperties
    ServiceNowSourceProperties (ServiceNowSourceProperties'),
    newServiceNowSourceProperties,

    -- ** SourceConnectorProperties
    SourceConnectorProperties (SourceConnectorProperties'),
    newSourceConnectorProperties,

    -- ** SourceFlowConfig
    SourceFlowConfig (SourceFlowConfig'),
    newSourceFlowConfig,

    -- ** Task
    Task (Task'),
    newTask,

    -- ** TriggerConfig
    TriggerConfig (TriggerConfig'),
    newTriggerConfig,

    -- ** TriggerProperties
    TriggerProperties (TriggerProperties'),
    newTriggerProperties,

    -- ** UpdateAddress
    UpdateAddress (UpdateAddress'),
    newUpdateAddress,

    -- ** ZendeskSourceProperties
    ZendeskSourceProperties (ZendeskSourceProperties'),
    newZendeskSourceProperties,
  )
where

import Amazonka.CustomerProfiles.AddProfileKey
import Amazonka.CustomerProfiles.CreateDomain
import Amazonka.CustomerProfiles.CreateProfile
import Amazonka.CustomerProfiles.DeleteDomain
import Amazonka.CustomerProfiles.DeleteIntegration
import Amazonka.CustomerProfiles.DeleteProfile
import Amazonka.CustomerProfiles.DeleteProfileKey
import Amazonka.CustomerProfiles.DeleteProfileObject
import Amazonka.CustomerProfiles.DeleteProfileObjectType
import Amazonka.CustomerProfiles.GetDomain
import Amazonka.CustomerProfiles.GetIntegration
import Amazonka.CustomerProfiles.GetMatches
import Amazonka.CustomerProfiles.GetProfileObjectType
import Amazonka.CustomerProfiles.GetProfileObjectTypeTemplate
import Amazonka.CustomerProfiles.Lens
import Amazonka.CustomerProfiles.ListAccountIntegrations
import Amazonka.CustomerProfiles.ListDomains
import Amazonka.CustomerProfiles.ListIntegrations
import Amazonka.CustomerProfiles.ListProfileObjectTypeTemplates
import Amazonka.CustomerProfiles.ListProfileObjectTypes
import Amazonka.CustomerProfiles.ListProfileObjects
import Amazonka.CustomerProfiles.ListTagsForResource
import Amazonka.CustomerProfiles.MergeProfiles
import Amazonka.CustomerProfiles.PutIntegration
import Amazonka.CustomerProfiles.PutProfileObject
import Amazonka.CustomerProfiles.PutProfileObjectType
import Amazonka.CustomerProfiles.SearchProfiles
import Amazonka.CustomerProfiles.TagResource
import Amazonka.CustomerProfiles.Types
import Amazonka.CustomerProfiles.UntagResource
import Amazonka.CustomerProfiles.UpdateDomain
import Amazonka.CustomerProfiles.UpdateProfile
import Amazonka.CustomerProfiles.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CustomerProfiles'.

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
