{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AmplifyUiBuilder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-08-11@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amplify UI Builder API provides a programmatic interface for
-- creating and configuring user interface (UI) component libraries and
-- themes for use in your Amplify applications. You can then connect these
-- UI components to an application\'s backend Amazon Web Services
-- resources.
--
-- You can also use the Amplify Studio visual designer to create UI
-- components and model data for an app. For more information, see
-- <https://docs.amplify.aws/console/adminui/intro Introduction> in the
-- /Amplify Docs/.
--
-- The Amplify Framework is a comprehensive set of SDKs, libraries, tools,
-- and documentation for client app development. For more information, see
-- the <https://docs.amplify.aws/ Amplify Framework>. For more information
-- about deploying an Amplify application to Amazon Web Services, see the
-- <https://docs.aws.amazon.com/amplify/latest/userguide/welcome.html Amplify User Guide>.
module Amazonka.AmplifyUiBuilder
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateComponent
    CreateComponent (CreateComponent'),
    newCreateComponent,
    CreateComponentResponse (CreateComponentResponse'),
    newCreateComponentResponse,

    -- ** CreateForm
    CreateForm (CreateForm'),
    newCreateForm,
    CreateFormResponse (CreateFormResponse'),
    newCreateFormResponse,

    -- ** CreateTheme
    CreateTheme (CreateTheme'),
    newCreateTheme,
    CreateThemeResponse (CreateThemeResponse'),
    newCreateThemeResponse,

    -- ** DeleteComponent
    DeleteComponent (DeleteComponent'),
    newDeleteComponent,
    DeleteComponentResponse (DeleteComponentResponse'),
    newDeleteComponentResponse,

    -- ** DeleteForm
    DeleteForm (DeleteForm'),
    newDeleteForm,
    DeleteFormResponse (DeleteFormResponse'),
    newDeleteFormResponse,

    -- ** DeleteTheme
    DeleteTheme (DeleteTheme'),
    newDeleteTheme,
    DeleteThemeResponse (DeleteThemeResponse'),
    newDeleteThemeResponse,

    -- ** ExchangeCodeForToken
    ExchangeCodeForToken (ExchangeCodeForToken'),
    newExchangeCodeForToken,
    ExchangeCodeForTokenResponse (ExchangeCodeForTokenResponse'),
    newExchangeCodeForTokenResponse,

    -- ** ExportComponents (Paginated)
    ExportComponents (ExportComponents'),
    newExportComponents,
    ExportComponentsResponse (ExportComponentsResponse'),
    newExportComponentsResponse,

    -- ** ExportForms (Paginated)
    ExportForms (ExportForms'),
    newExportForms,
    ExportFormsResponse (ExportFormsResponse'),
    newExportFormsResponse,

    -- ** ExportThemes (Paginated)
    ExportThemes (ExportThemes'),
    newExportThemes,
    ExportThemesResponse (ExportThemesResponse'),
    newExportThemesResponse,

    -- ** GetComponent
    GetComponent (GetComponent'),
    newGetComponent,
    GetComponentResponse (GetComponentResponse'),
    newGetComponentResponse,

    -- ** GetForm
    GetForm (GetForm'),
    newGetForm,
    GetFormResponse (GetFormResponse'),
    newGetFormResponse,

    -- ** GetMetadata
    GetMetadata (GetMetadata'),
    newGetMetadata,
    GetMetadataResponse (GetMetadataResponse'),
    newGetMetadataResponse,

    -- ** GetTheme
    GetTheme (GetTheme'),
    newGetTheme,
    GetThemeResponse (GetThemeResponse'),
    newGetThemeResponse,

    -- ** ListComponents (Paginated)
    ListComponents (ListComponents'),
    newListComponents,
    ListComponentsResponse (ListComponentsResponse'),
    newListComponentsResponse,

    -- ** ListForms (Paginated)
    ListForms (ListForms'),
    newListForms,
    ListFormsResponse (ListFormsResponse'),
    newListFormsResponse,

    -- ** ListThemes (Paginated)
    ListThemes (ListThemes'),
    newListThemes,
    ListThemesResponse (ListThemesResponse'),
    newListThemesResponse,

    -- ** PutMetadataFlag
    PutMetadataFlag (PutMetadataFlag'),
    newPutMetadataFlag,
    PutMetadataFlagResponse (PutMetadataFlagResponse'),
    newPutMetadataFlagResponse,

    -- ** RefreshToken
    RefreshToken (RefreshToken'),
    newRefreshToken,
    RefreshTokenResponse (RefreshTokenResponse'),
    newRefreshTokenResponse,

    -- ** UpdateComponent
    UpdateComponent (UpdateComponent'),
    newUpdateComponent,
    UpdateComponentResponse (UpdateComponentResponse'),
    newUpdateComponentResponse,

    -- ** UpdateForm
    UpdateForm (UpdateForm'),
    newUpdateForm,
    UpdateFormResponse (UpdateFormResponse'),
    newUpdateFormResponse,

    -- ** UpdateTheme
    UpdateTheme (UpdateTheme'),
    newUpdateTheme,
    UpdateThemeResponse (UpdateThemeResponse'),
    newUpdateThemeResponse,

    -- * Types

    -- ** FixedPosition
    FixedPosition (..),

    -- ** FormActionType
    FormActionType (..),

    -- ** FormButtonsPosition
    FormButtonsPosition (..),

    -- ** FormDataSourceType
    FormDataSourceType (..),

    -- ** SortDirection
    SortDirection (..),

    -- ** TokenProviders
    TokenProviders (..),

    -- ** ActionParameters
    ActionParameters (ActionParameters'),
    newActionParameters,

    -- ** Component
    Component (Component'),
    newComponent,

    -- ** ComponentBindingPropertiesValue
    ComponentBindingPropertiesValue (ComponentBindingPropertiesValue'),
    newComponentBindingPropertiesValue,

    -- ** ComponentBindingPropertiesValueProperties
    ComponentBindingPropertiesValueProperties (ComponentBindingPropertiesValueProperties'),
    newComponentBindingPropertiesValueProperties,

    -- ** ComponentChild
    ComponentChild (ComponentChild'),
    newComponentChild,

    -- ** ComponentConditionProperty
    ComponentConditionProperty (ComponentConditionProperty'),
    newComponentConditionProperty,

    -- ** ComponentDataConfiguration
    ComponentDataConfiguration (ComponentDataConfiguration'),
    newComponentDataConfiguration,

    -- ** ComponentEvent
    ComponentEvent (ComponentEvent'),
    newComponentEvent,

    -- ** ComponentProperty
    ComponentProperty (ComponentProperty'),
    newComponentProperty,

    -- ** ComponentPropertyBindingProperties
    ComponentPropertyBindingProperties (ComponentPropertyBindingProperties'),
    newComponentPropertyBindingProperties,

    -- ** ComponentSummary
    ComponentSummary (ComponentSummary'),
    newComponentSummary,

    -- ** ComponentVariant
    ComponentVariant (ComponentVariant'),
    newComponentVariant,

    -- ** CreateComponentData
    CreateComponentData (CreateComponentData'),
    newCreateComponentData,

    -- ** CreateFormData
    CreateFormData (CreateFormData'),
    newCreateFormData,

    -- ** CreateThemeData
    CreateThemeData (CreateThemeData'),
    newCreateThemeData,

    -- ** ExchangeCodeForTokenRequestBody
    ExchangeCodeForTokenRequestBody (ExchangeCodeForTokenRequestBody'),
    newExchangeCodeForTokenRequestBody,

    -- ** FieldConfig
    FieldConfig (FieldConfig'),
    newFieldConfig,

    -- ** FieldInputConfig
    FieldInputConfig (FieldInputConfig'),
    newFieldInputConfig,

    -- ** FieldPosition
    FieldPosition (FieldPosition'),
    newFieldPosition,

    -- ** FieldValidationConfiguration
    FieldValidationConfiguration (FieldValidationConfiguration'),
    newFieldValidationConfiguration,

    -- ** Form
    Form (Form'),
    newForm,

    -- ** FormBindingElement
    FormBindingElement (FormBindingElement'),
    newFormBindingElement,

    -- ** FormButton
    FormButton (FormButton'),
    newFormButton,

    -- ** FormCTA
    FormCTA (FormCTA'),
    newFormCTA,

    -- ** FormDataTypeConfig
    FormDataTypeConfig (FormDataTypeConfig'),
    newFormDataTypeConfig,

    -- ** FormInputValueProperty
    FormInputValueProperty (FormInputValueProperty'),
    newFormInputValueProperty,

    -- ** FormStyle
    FormStyle (FormStyle'),
    newFormStyle,

    -- ** FormStyleConfig
    FormStyleConfig (FormStyleConfig'),
    newFormStyleConfig,

    -- ** FormSummary
    FormSummary (FormSummary'),
    newFormSummary,

    -- ** MutationActionSetStateParameter
    MutationActionSetStateParameter (MutationActionSetStateParameter'),
    newMutationActionSetStateParameter,

    -- ** Predicate
    Predicate (Predicate'),
    newPredicate,

    -- ** PutMetadataFlagBody
    PutMetadataFlagBody (PutMetadataFlagBody'),
    newPutMetadataFlagBody,

    -- ** RefreshTokenRequestBody
    RefreshTokenRequestBody (RefreshTokenRequestBody'),
    newRefreshTokenRequestBody,

    -- ** SectionalElement
    SectionalElement (SectionalElement'),
    newSectionalElement,

    -- ** SortProperty
    SortProperty (SortProperty'),
    newSortProperty,

    -- ** Theme
    Theme (Theme'),
    newTheme,

    -- ** ThemeSummary
    ThemeSummary (ThemeSummary'),
    newThemeSummary,

    -- ** ThemeValue
    ThemeValue (ThemeValue'),
    newThemeValue,

    -- ** ThemeValues
    ThemeValues (ThemeValues'),
    newThemeValues,

    -- ** UpdateComponentData
    UpdateComponentData (UpdateComponentData'),
    newUpdateComponentData,

    -- ** UpdateFormData
    UpdateFormData (UpdateFormData'),
    newUpdateFormData,

    -- ** UpdateThemeData
    UpdateThemeData (UpdateThemeData'),
    newUpdateThemeData,

    -- ** ValueMapping
    ValueMapping (ValueMapping'),
    newValueMapping,

    -- ** ValueMappings
    ValueMappings (ValueMappings'),
    newValueMappings,
  )
where

import Amazonka.AmplifyUiBuilder.CreateComponent
import Amazonka.AmplifyUiBuilder.CreateForm
import Amazonka.AmplifyUiBuilder.CreateTheme
import Amazonka.AmplifyUiBuilder.DeleteComponent
import Amazonka.AmplifyUiBuilder.DeleteForm
import Amazonka.AmplifyUiBuilder.DeleteTheme
import Amazonka.AmplifyUiBuilder.ExchangeCodeForToken
import Amazonka.AmplifyUiBuilder.ExportComponents
import Amazonka.AmplifyUiBuilder.ExportForms
import Amazonka.AmplifyUiBuilder.ExportThemes
import Amazonka.AmplifyUiBuilder.GetComponent
import Amazonka.AmplifyUiBuilder.GetForm
import Amazonka.AmplifyUiBuilder.GetMetadata
import Amazonka.AmplifyUiBuilder.GetTheme
import Amazonka.AmplifyUiBuilder.Lens
import Amazonka.AmplifyUiBuilder.ListComponents
import Amazonka.AmplifyUiBuilder.ListForms
import Amazonka.AmplifyUiBuilder.ListThemes
import Amazonka.AmplifyUiBuilder.PutMetadataFlag
import Amazonka.AmplifyUiBuilder.RefreshToken
import Amazonka.AmplifyUiBuilder.Types
import Amazonka.AmplifyUiBuilder.UpdateComponent
import Amazonka.AmplifyUiBuilder.UpdateForm
import Amazonka.AmplifyUiBuilder.UpdateTheme
import Amazonka.AmplifyUiBuilder.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AmplifyUiBuilder'.

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
