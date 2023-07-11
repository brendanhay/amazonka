{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AmplifyUiBuilder.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Lens
  ( -- * Operations

    -- ** CreateComponent
    createComponent_clientToken,
    createComponent_appId,
    createComponent_componentToCreate,
    createComponent_environmentName,
    createComponentResponse_entity,
    createComponentResponse_httpStatus,

    -- ** CreateForm
    createForm_clientToken,
    createForm_appId,
    createForm_environmentName,
    createForm_formToCreate,
    createFormResponse_entity,
    createFormResponse_httpStatus,

    -- ** CreateTheme
    createTheme_clientToken,
    createTheme_appId,
    createTheme_environmentName,
    createTheme_themeToCreate,
    createThemeResponse_entity,
    createThemeResponse_httpStatus,

    -- ** DeleteComponent
    deleteComponent_appId,
    deleteComponent_environmentName,
    deleteComponent_id,

    -- ** DeleteForm
    deleteForm_appId,
    deleteForm_environmentName,
    deleteForm_id,

    -- ** DeleteTheme
    deleteTheme_appId,
    deleteTheme_environmentName,
    deleteTheme_id,

    -- ** ExchangeCodeForToken
    exchangeCodeForToken_provider,
    exchangeCodeForToken_request,
    exchangeCodeForTokenResponse_httpStatus,
    exchangeCodeForTokenResponse_accessToken,
    exchangeCodeForTokenResponse_expiresIn,
    exchangeCodeForTokenResponse_refreshToken,

    -- ** ExportComponents
    exportComponents_nextToken,
    exportComponents_appId,
    exportComponents_environmentName,
    exportComponentsResponse_nextToken,
    exportComponentsResponse_httpStatus,
    exportComponentsResponse_entities,

    -- ** ExportForms
    exportForms_nextToken,
    exportForms_appId,
    exportForms_environmentName,
    exportFormsResponse_nextToken,
    exportFormsResponse_httpStatus,
    exportFormsResponse_entities,

    -- ** ExportThemes
    exportThemes_nextToken,
    exportThemes_appId,
    exportThemes_environmentName,
    exportThemesResponse_nextToken,
    exportThemesResponse_httpStatus,
    exportThemesResponse_entities,

    -- ** GetComponent
    getComponent_appId,
    getComponent_environmentName,
    getComponent_id,
    getComponentResponse_component,
    getComponentResponse_httpStatus,

    -- ** GetForm
    getForm_appId,
    getForm_environmentName,
    getForm_id,
    getFormResponse_form,
    getFormResponse_httpStatus,

    -- ** GetMetadata
    getMetadata_appId,
    getMetadata_environmentName,
    getMetadataResponse_httpStatus,
    getMetadataResponse_features,

    -- ** GetTheme
    getTheme_appId,
    getTheme_environmentName,
    getTheme_id,
    getThemeResponse_theme,
    getThemeResponse_httpStatus,

    -- ** ListComponents
    listComponents_maxResults,
    listComponents_nextToken,
    listComponents_appId,
    listComponents_environmentName,
    listComponentsResponse_nextToken,
    listComponentsResponse_httpStatus,
    listComponentsResponse_entities,

    -- ** ListForms
    listForms_maxResults,
    listForms_nextToken,
    listForms_appId,
    listForms_environmentName,
    listFormsResponse_nextToken,
    listFormsResponse_httpStatus,
    listFormsResponse_entities,

    -- ** ListThemes
    listThemes_maxResults,
    listThemes_nextToken,
    listThemes_appId,
    listThemes_environmentName,
    listThemesResponse_nextToken,
    listThemesResponse_httpStatus,
    listThemesResponse_entities,

    -- ** PutMetadataFlag
    putMetadataFlag_appId,
    putMetadataFlag_body,
    putMetadataFlag_environmentName,
    putMetadataFlag_featureName,

    -- ** RefreshToken
    refreshToken_provider,
    refreshToken_refreshTokenBody,
    refreshTokenResponse_httpStatus,
    refreshTokenResponse_accessToken,
    refreshTokenResponse_expiresIn,

    -- ** UpdateComponent
    updateComponent_clientToken,
    updateComponent_appId,
    updateComponent_environmentName,
    updateComponent_id,
    updateComponent_updatedComponent,
    updateComponentResponse_entity,
    updateComponentResponse_httpStatus,

    -- ** UpdateForm
    updateForm_clientToken,
    updateForm_appId,
    updateForm_environmentName,
    updateForm_id,
    updateForm_updatedForm,
    updateFormResponse_entity,
    updateFormResponse_httpStatus,

    -- ** UpdateTheme
    updateTheme_clientToken,
    updateTheme_appId,
    updateTheme_environmentName,
    updateTheme_id,
    updateTheme_updatedTheme,
    updateThemeResponse_entity,
    updateThemeResponse_httpStatus,

    -- * Types

    -- ** ActionParameters
    actionParameters_anchor,
    actionParameters_fields,
    actionParameters_global,
    actionParameters_id,
    actionParameters_model,
    actionParameters_state,
    actionParameters_target,
    actionParameters_type,
    actionParameters_url,

    -- ** Component
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

    -- ** ComponentBindingPropertiesValue
    componentBindingPropertiesValue_bindingProperties,
    componentBindingPropertiesValue_defaultValue,
    componentBindingPropertiesValue_type,

    -- ** ComponentBindingPropertiesValueProperties
    componentBindingPropertiesValueProperties_bucket,
    componentBindingPropertiesValueProperties_defaultValue,
    componentBindingPropertiesValueProperties_field,
    componentBindingPropertiesValueProperties_key,
    componentBindingPropertiesValueProperties_model,
    componentBindingPropertiesValueProperties_predicates,
    componentBindingPropertiesValueProperties_slotName,
    componentBindingPropertiesValueProperties_userAttribute,

    -- ** ComponentChild
    componentChild_children,
    componentChild_events,
    componentChild_sourceId,
    componentChild_componentType,
    componentChild_name,
    componentChild_properties,

    -- ** ComponentConditionProperty
    componentConditionProperty_else,
    componentConditionProperty_field,
    componentConditionProperty_operand,
    componentConditionProperty_operandType,
    componentConditionProperty_operator,
    componentConditionProperty_property,
    componentConditionProperty_then,

    -- ** ComponentDataConfiguration
    componentDataConfiguration_identifiers,
    componentDataConfiguration_predicate,
    componentDataConfiguration_sort,
    componentDataConfiguration_model,

    -- ** ComponentEvent
    componentEvent_action,
    componentEvent_bindingEvent,
    componentEvent_parameters,

    -- ** ComponentProperty
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

    -- ** ComponentPropertyBindingProperties
    componentPropertyBindingProperties_field,
    componentPropertyBindingProperties_property,

    -- ** ComponentSummary
    componentSummary_appId,
    componentSummary_componentType,
    componentSummary_environmentName,
    componentSummary_id,
    componentSummary_name,

    -- ** ComponentVariant
    componentVariant_overrides,
    componentVariant_variantValues,

    -- ** CreateComponentData
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

    -- ** CreateFormData
    createFormData_cta,
    createFormData_tags,
    createFormData_dataType,
    createFormData_fields,
    createFormData_formActionType,
    createFormData_name,
    createFormData_schemaVersion,
    createFormData_sectionalElements,
    createFormData_style,

    -- ** CreateThemeData
    createThemeData_overrides,
    createThemeData_tags,
    createThemeData_name,
    createThemeData_values,

    -- ** ExchangeCodeForTokenRequestBody
    exchangeCodeForTokenRequestBody_code,
    exchangeCodeForTokenRequestBody_redirectUri,

    -- ** FieldConfig
    fieldConfig_excluded,
    fieldConfig_inputType,
    fieldConfig_label,
    fieldConfig_position,
    fieldConfig_validations,

    -- ** FieldInputConfig
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

    -- ** FieldPosition
    fieldPosition_below,
    fieldPosition_fixed,
    fieldPosition_rightOf,

    -- ** FieldValidationConfiguration
    fieldValidationConfiguration_numValues,
    fieldValidationConfiguration_strValues,
    fieldValidationConfiguration_validationMessage,
    fieldValidationConfiguration_type,

    -- ** Form
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

    -- ** FormBindingElement
    formBindingElement_element,
    formBindingElement_property,

    -- ** FormButton
    formButton_children,
    formButton_excluded,
    formButton_position,

    -- ** FormCTA
    formCTA_cancel,
    formCTA_clear,
    formCTA_position,
    formCTA_submit,

    -- ** FormDataTypeConfig
    formDataTypeConfig_dataSourceType,
    formDataTypeConfig_dataTypeName,

    -- ** FormInputValueProperty
    formInputValueProperty_value,

    -- ** FormStyle
    formStyle_horizontalGap,
    formStyle_outerPadding,
    formStyle_verticalGap,

    -- ** FormStyleConfig
    formStyleConfig_tokenReference,
    formStyleConfig_value,

    -- ** FormSummary
    formSummary_appId,
    formSummary_dataType,
    formSummary_environmentName,
    formSummary_formActionType,
    formSummary_id,
    formSummary_name,

    -- ** MutationActionSetStateParameter
    mutationActionSetStateParameter_componentName,
    mutationActionSetStateParameter_property,
    mutationActionSetStateParameter_set,

    -- ** Predicate
    predicate_and,
    predicate_field,
    predicate_operand,
    predicate_operator,
    predicate_or,

    -- ** PutMetadataFlagBody
    putMetadataFlagBody_newValue,

    -- ** RefreshTokenRequestBody
    refreshTokenRequestBody_token,

    -- ** SectionalElement
    sectionalElement_level,
    sectionalElement_orientation,
    sectionalElement_position,
    sectionalElement_text,
    sectionalElement_type,

    -- ** SortProperty
    sortProperty_direction,
    sortProperty_field,

    -- ** Theme
    theme_modifiedAt,
    theme_overrides,
    theme_tags,
    theme_appId,
    theme_createdAt,
    theme_environmentName,
    theme_id,
    theme_name,
    theme_values,

    -- ** ThemeSummary
    themeSummary_appId,
    themeSummary_environmentName,
    themeSummary_id,
    themeSummary_name,

    -- ** ThemeValue
    themeValue_children,
    themeValue_value,

    -- ** ThemeValues
    themeValues_key,
    themeValues_value,

    -- ** UpdateComponentData
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

    -- ** UpdateFormData
    updateFormData_cta,
    updateFormData_dataType,
    updateFormData_fields,
    updateFormData_formActionType,
    updateFormData_name,
    updateFormData_schemaVersion,
    updateFormData_sectionalElements,
    updateFormData_style,

    -- ** UpdateThemeData
    updateThemeData_id,
    updateThemeData_name,
    updateThemeData_overrides,
    updateThemeData_values,

    -- ** ValueMapping
    valueMapping_displayValue,
    valueMapping_value,

    -- ** ValueMappings
    valueMappings_values,
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
import Amazonka.AmplifyUiBuilder.ListComponents
import Amazonka.AmplifyUiBuilder.ListForms
import Amazonka.AmplifyUiBuilder.ListThemes
import Amazonka.AmplifyUiBuilder.PutMetadataFlag
import Amazonka.AmplifyUiBuilder.RefreshToken
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
import Amazonka.AmplifyUiBuilder.Types.Form
import Amazonka.AmplifyUiBuilder.Types.FormBindingElement
import Amazonka.AmplifyUiBuilder.Types.FormButton
import Amazonka.AmplifyUiBuilder.Types.FormCTA
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
import Amazonka.AmplifyUiBuilder.Types.SortProperty
import Amazonka.AmplifyUiBuilder.Types.Theme
import Amazonka.AmplifyUiBuilder.Types.ThemeSummary
import Amazonka.AmplifyUiBuilder.Types.ThemeValue
import Amazonka.AmplifyUiBuilder.Types.ThemeValues
import Amazonka.AmplifyUiBuilder.Types.UpdateComponentData
import Amazonka.AmplifyUiBuilder.Types.UpdateFormData
import Amazonka.AmplifyUiBuilder.Types.UpdateThemeData
import Amazonka.AmplifyUiBuilder.Types.ValueMapping
import Amazonka.AmplifyUiBuilder.Types.ValueMappings
import Amazonka.AmplifyUiBuilder.UpdateComponent
import Amazonka.AmplifyUiBuilder.UpdateForm
import Amazonka.AmplifyUiBuilder.UpdateTheme
