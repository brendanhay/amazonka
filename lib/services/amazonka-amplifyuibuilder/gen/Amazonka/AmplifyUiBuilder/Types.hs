{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AmplifyUiBuilder.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerException,
    _InvalidParameterException,
    _ResourceConflictException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _UnauthorizedException,

    -- * FixedPosition
    FixedPosition (..),

    -- * FormActionType
    FormActionType (..),

    -- * FormButtonsPosition
    FormButtonsPosition (..),

    -- * FormDataSourceType
    FormDataSourceType (..),

    -- * SortDirection
    SortDirection (..),

    -- * TokenProviders
    TokenProviders (..),

    -- * ActionParameters
    ActionParameters (..),
    newActionParameters,
    actionParameters_anchor,
    actionParameters_fields,
    actionParameters_global,
    actionParameters_id,
    actionParameters_model,
    actionParameters_state,
    actionParameters_target,
    actionParameters_type,
    actionParameters_url,

    -- * Component
    Component (..),
    newComponent,
    component_children,
    component_collectionProperties,
    component_events,
    component_modifiedAt,
    component_schemaVersion,
    component_sourceId,
    component_tags,
    component_appId,
    component_bindingProperties,
    component_componentType,
    component_createdAt,
    component_environmentName,
    component_id,
    component_name,
    component_overrides,
    component_properties,
    component_variants,

    -- * ComponentBindingPropertiesValue
    ComponentBindingPropertiesValue (..),
    newComponentBindingPropertiesValue,
    componentBindingPropertiesValue_bindingProperties,
    componentBindingPropertiesValue_defaultValue,
    componentBindingPropertiesValue_type,

    -- * ComponentBindingPropertiesValueProperties
    ComponentBindingPropertiesValueProperties (..),
    newComponentBindingPropertiesValueProperties,
    componentBindingPropertiesValueProperties_bucket,
    componentBindingPropertiesValueProperties_defaultValue,
    componentBindingPropertiesValueProperties_field,
    componentBindingPropertiesValueProperties_key,
    componentBindingPropertiesValueProperties_model,
    componentBindingPropertiesValueProperties_predicates,
    componentBindingPropertiesValueProperties_slotName,
    componentBindingPropertiesValueProperties_userAttribute,

    -- * ComponentChild
    ComponentChild (..),
    newComponentChild,
    componentChild_children,
    componentChild_events,
    componentChild_sourceId,
    componentChild_componentType,
    componentChild_name,
    componentChild_properties,

    -- * ComponentConditionProperty
    ComponentConditionProperty (..),
    newComponentConditionProperty,
    componentConditionProperty_else,
    componentConditionProperty_field,
    componentConditionProperty_operand,
    componentConditionProperty_operandType,
    componentConditionProperty_operator,
    componentConditionProperty_property,
    componentConditionProperty_then,

    -- * ComponentDataConfiguration
    ComponentDataConfiguration (..),
    newComponentDataConfiguration,
    componentDataConfiguration_identifiers,
    componentDataConfiguration_predicate,
    componentDataConfiguration_sort,
    componentDataConfiguration_model,

    -- * ComponentEvent
    ComponentEvent (..),
    newComponentEvent,
    componentEvent_action,
    componentEvent_bindingEvent,
    componentEvent_parameters,

    -- * ComponentProperty
    ComponentProperty (..),
    newComponentProperty,
    componentProperty_bindingProperties,
    componentProperty_bindings,
    componentProperty_collectionBindingProperties,
    componentProperty_componentName,
    componentProperty_concat,
    componentProperty_condition,
    componentProperty_configured,
    componentProperty_defaultValue,
    componentProperty_event,
    componentProperty_importedValue,
    componentProperty_model,
    componentProperty_property,
    componentProperty_type,
    componentProperty_userAttribute,
    componentProperty_value,

    -- * ComponentPropertyBindingProperties
    ComponentPropertyBindingProperties (..),
    newComponentPropertyBindingProperties,
    componentPropertyBindingProperties_field,
    componentPropertyBindingProperties_property,

    -- * ComponentSummary
    ComponentSummary (..),
    newComponentSummary,
    componentSummary_appId,
    componentSummary_componentType,
    componentSummary_environmentName,
    componentSummary_id,
    componentSummary_name,

    -- * ComponentVariant
    ComponentVariant (..),
    newComponentVariant,
    componentVariant_overrides,
    componentVariant_variantValues,

    -- * CreateComponentData
    CreateComponentData (..),
    newCreateComponentData,
    createComponentData_children,
    createComponentData_collectionProperties,
    createComponentData_events,
    createComponentData_schemaVersion,
    createComponentData_sourceId,
    createComponentData_tags,
    createComponentData_bindingProperties,
    createComponentData_componentType,
    createComponentData_name,
    createComponentData_overrides,
    createComponentData_properties,
    createComponentData_variants,

    -- * CreateFormData
    CreateFormData (..),
    newCreateFormData,
    createFormData_cta,
    createFormData_tags,
    createFormData_dataType,
    createFormData_fields,
    createFormData_formActionType,
    createFormData_name,
    createFormData_schemaVersion,
    createFormData_sectionalElements,
    createFormData_style,

    -- * CreateThemeData
    CreateThemeData (..),
    newCreateThemeData,
    createThemeData_overrides,
    createThemeData_tags,
    createThemeData_name,
    createThemeData_values,

    -- * ExchangeCodeForTokenRequestBody
    ExchangeCodeForTokenRequestBody (..),
    newExchangeCodeForTokenRequestBody,
    exchangeCodeForTokenRequestBody_code,
    exchangeCodeForTokenRequestBody_redirectUri,

    -- * FieldConfig
    FieldConfig (..),
    newFieldConfig,
    fieldConfig_excluded,
    fieldConfig_inputType,
    fieldConfig_label,
    fieldConfig_position,
    fieldConfig_validations,

    -- * FieldInputConfig
    FieldInputConfig (..),
    newFieldInputConfig,
    fieldInputConfig_defaultChecked,
    fieldInputConfig_defaultCountryCode,
    fieldInputConfig_defaultValue,
    fieldInputConfig_descriptiveText,
    fieldInputConfig_isArray,
    fieldInputConfig_maxValue,
    fieldInputConfig_minValue,
    fieldInputConfig_name,
    fieldInputConfig_placeholder,
    fieldInputConfig_readOnly,
    fieldInputConfig_required,
    fieldInputConfig_step,
    fieldInputConfig_value,
    fieldInputConfig_valueMappings,
    fieldInputConfig_type,

    -- * FieldPosition
    FieldPosition (..),
    newFieldPosition,
    fieldPosition_below,
    fieldPosition_fixed,
    fieldPosition_rightOf,

    -- * FieldValidationConfiguration
    FieldValidationConfiguration (..),
    newFieldValidationConfiguration,
    fieldValidationConfiguration_numValues,
    fieldValidationConfiguration_strValues,
    fieldValidationConfiguration_validationMessage,
    fieldValidationConfiguration_type,

    -- * Form
    Form (..),
    newForm,
    form_cta,
    form_tags,
    form_appId,
    form_dataType,
    form_environmentName,
    form_fields,
    form_formActionType,
    form_id,
    form_name,
    form_schemaVersion,
    form_sectionalElements,
    form_style,

    -- * FormBindingElement
    FormBindingElement (..),
    newFormBindingElement,
    formBindingElement_element,
    formBindingElement_property,

    -- * FormButton
    FormButton (..),
    newFormButton,
    formButton_children,
    formButton_excluded,
    formButton_position,

    -- * FormCTA
    FormCTA (..),
    newFormCTA,
    formCTA_cancel,
    formCTA_clear,
    formCTA_position,
    formCTA_submit,

    -- * FormDataTypeConfig
    FormDataTypeConfig (..),
    newFormDataTypeConfig,
    formDataTypeConfig_dataSourceType,
    formDataTypeConfig_dataTypeName,

    -- * FormInputValueProperty
    FormInputValueProperty (..),
    newFormInputValueProperty,
    formInputValueProperty_value,

    -- * FormStyle
    FormStyle (..),
    newFormStyle,
    formStyle_horizontalGap,
    formStyle_outerPadding,
    formStyle_verticalGap,

    -- * FormStyleConfig
    FormStyleConfig (..),
    newFormStyleConfig,
    formStyleConfig_tokenReference,
    formStyleConfig_value,

    -- * FormSummary
    FormSummary (..),
    newFormSummary,
    formSummary_appId,
    formSummary_dataType,
    formSummary_environmentName,
    formSummary_formActionType,
    formSummary_id,
    formSummary_name,

    -- * MutationActionSetStateParameter
    MutationActionSetStateParameter (..),
    newMutationActionSetStateParameter,
    mutationActionSetStateParameter_componentName,
    mutationActionSetStateParameter_property,
    mutationActionSetStateParameter_set,

    -- * Predicate
    Predicate (..),
    newPredicate,
    predicate_and,
    predicate_field,
    predicate_operand,
    predicate_operator,
    predicate_or,

    -- * PutMetadataFlagBody
    PutMetadataFlagBody (..),
    newPutMetadataFlagBody,
    putMetadataFlagBody_newValue,

    -- * RefreshTokenRequestBody
    RefreshTokenRequestBody (..),
    newRefreshTokenRequestBody,
    refreshTokenRequestBody_token,

    -- * SectionalElement
    SectionalElement (..),
    newSectionalElement,
    sectionalElement_level,
    sectionalElement_orientation,
    sectionalElement_position,
    sectionalElement_text,
    sectionalElement_type,

    -- * SortProperty
    SortProperty (..),
    newSortProperty,
    sortProperty_direction,
    sortProperty_field,

    -- * Theme
    Theme (..),
    newTheme,
    theme_modifiedAt,
    theme_overrides,
    theme_tags,
    theme_appId,
    theme_createdAt,
    theme_environmentName,
    theme_id,
    theme_name,
    theme_values,

    -- * ThemeSummary
    ThemeSummary (..),
    newThemeSummary,
    themeSummary_appId,
    themeSummary_environmentName,
    themeSummary_id,
    themeSummary_name,

    -- * ThemeValue
    ThemeValue (..),
    newThemeValue,
    themeValue_children,
    themeValue_value,

    -- * ThemeValues
    ThemeValues (..),
    newThemeValues,
    themeValues_key,
    themeValues_value,

    -- * UpdateComponentData
    UpdateComponentData (..),
    newUpdateComponentData,
    updateComponentData_bindingProperties,
    updateComponentData_children,
    updateComponentData_collectionProperties,
    updateComponentData_componentType,
    updateComponentData_events,
    updateComponentData_id,
    updateComponentData_name,
    updateComponentData_overrides,
    updateComponentData_properties,
    updateComponentData_schemaVersion,
    updateComponentData_sourceId,
    updateComponentData_variants,

    -- * UpdateFormData
    UpdateFormData (..),
    newUpdateFormData,
    updateFormData_cta,
    updateFormData_dataType,
    updateFormData_fields,
    updateFormData_formActionType,
    updateFormData_name,
    updateFormData_schemaVersion,
    updateFormData_sectionalElements,
    updateFormData_style,

    -- * UpdateThemeData
    UpdateThemeData (..),
    newUpdateThemeData,
    updateThemeData_id,
    updateThemeData_name,
    updateThemeData_overrides,
    updateThemeData_values,

    -- * ValueMapping
    ValueMapping (..),
    newValueMapping,
    valueMapping_displayValue,
    valueMapping_value,

    -- * ValueMappings
    ValueMappings (..),
    newValueMappings,
    valueMappings_values,
  )
where

import Amazonka.AmplifyUiBuilder.Types.ActionParameters
import Amazonka.AmplifyUiBuilder.Types.Component
import Amazonka.AmplifyUiBuilder.Types.ComponentBindingPropertiesValue
import Amazonka.AmplifyUiBuilder.Types.ComponentBindingPropertiesValueProperties
import Amazonka.AmplifyUiBuilder.Types.ComponentChild
import Amazonka.AmplifyUiBuilder.Types.ComponentConditionProperty
import Amazonka.AmplifyUiBuilder.Types.ComponentDataConfiguration
import Amazonka.AmplifyUiBuilder.Types.ComponentEvent
import Amazonka.AmplifyUiBuilder.Types.ComponentProperty
import Amazonka.AmplifyUiBuilder.Types.ComponentPropertyBindingProperties
import Amazonka.AmplifyUiBuilder.Types.ComponentSummary
import Amazonka.AmplifyUiBuilder.Types.ComponentVariant
import Amazonka.AmplifyUiBuilder.Types.CreateComponentData
import Amazonka.AmplifyUiBuilder.Types.CreateFormData
import Amazonka.AmplifyUiBuilder.Types.CreateThemeData
import Amazonka.AmplifyUiBuilder.Types.ExchangeCodeForTokenRequestBody
import Amazonka.AmplifyUiBuilder.Types.FieldConfig
import Amazonka.AmplifyUiBuilder.Types.FieldInputConfig
import Amazonka.AmplifyUiBuilder.Types.FieldPosition
import Amazonka.AmplifyUiBuilder.Types.FieldValidationConfiguration
import Amazonka.AmplifyUiBuilder.Types.FixedPosition
import Amazonka.AmplifyUiBuilder.Types.Form
import Amazonka.AmplifyUiBuilder.Types.FormActionType
import Amazonka.AmplifyUiBuilder.Types.FormBindingElement
import Amazonka.AmplifyUiBuilder.Types.FormButton
import Amazonka.AmplifyUiBuilder.Types.FormButtonsPosition
import Amazonka.AmplifyUiBuilder.Types.FormCTA
import Amazonka.AmplifyUiBuilder.Types.FormDataSourceType
import Amazonka.AmplifyUiBuilder.Types.FormDataTypeConfig
import Amazonka.AmplifyUiBuilder.Types.FormInputValueProperty
import Amazonka.AmplifyUiBuilder.Types.FormStyle
import Amazonka.AmplifyUiBuilder.Types.FormStyleConfig
import Amazonka.AmplifyUiBuilder.Types.FormSummary
import Amazonka.AmplifyUiBuilder.Types.MutationActionSetStateParameter
import Amazonka.AmplifyUiBuilder.Types.Predicate
import Amazonka.AmplifyUiBuilder.Types.PutMetadataFlagBody
import Amazonka.AmplifyUiBuilder.Types.RefreshTokenRequestBody
import Amazonka.AmplifyUiBuilder.Types.SectionalElement
import Amazonka.AmplifyUiBuilder.Types.SortDirection
import Amazonka.AmplifyUiBuilder.Types.SortProperty
import Amazonka.AmplifyUiBuilder.Types.Theme
import Amazonka.AmplifyUiBuilder.Types.ThemeSummary
import Amazonka.AmplifyUiBuilder.Types.ThemeValue
import Amazonka.AmplifyUiBuilder.Types.ThemeValues
import Amazonka.AmplifyUiBuilder.Types.TokenProviders
import Amazonka.AmplifyUiBuilder.Types.UpdateComponentData
import Amazonka.AmplifyUiBuilder.Types.UpdateFormData
import Amazonka.AmplifyUiBuilder.Types.UpdateThemeData
import Amazonka.AmplifyUiBuilder.Types.ValueMapping
import Amazonka.AmplifyUiBuilder.Types.ValueMappings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-08-11@ of the Amazon Amplify UI Builder SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AmplifyUiBuilder",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "amplifyuibuilder",
      Core.signingName = "amplifyuibuilder",
      Core.version = "2021-08-11",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AmplifyUiBuilder",
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

-- | An internal error has occurred. Please retry your request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | An invalid or out-of-range value was supplied for the input parameter.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The resource specified in the request conflicts with an existing
-- resource.
_ResourceConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- | The requested resource does not exist, or access was denied.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You exceeded your service quota. Service quotas, also referred to as
-- limits, are the maximum number of service resources or operations for
-- your Amazon Web Services account.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | You don\'t have permission to perform this operation.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401
