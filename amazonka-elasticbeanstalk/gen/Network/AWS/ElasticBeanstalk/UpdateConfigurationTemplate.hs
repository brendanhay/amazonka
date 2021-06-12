{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified configuration template to have the specified
-- properties or configuration option values.
--
-- If a property (for example, @ApplicationName@) is not provided, its
-- value remains unchanged. To clear such properties, specify an empty
-- string.
--
-- Related Topics
--
-- -   DescribeConfigurationOptions
module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
  ( -- * Creating a Request
    UpdateConfigurationTemplate (..),
    newUpdateConfigurationTemplate,

    -- * Request Lenses
    updateConfigurationTemplate_optionsToRemove,
    updateConfigurationTemplate_optionSettings,
    updateConfigurationTemplate_description,
    updateConfigurationTemplate_applicationName,
    updateConfigurationTemplate_templateName,

    -- * Destructuring the Response
    ConfigurationSettingsDescription (..),
    newConfigurationSettingsDescription,

    -- * Response Lenses
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_description,
    configurationSettingsDescription_applicationName,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The result message containing the options for the specified solution
-- stack.
--
-- /See:/ 'newUpdateConfigurationTemplate' smart constructor.
data UpdateConfigurationTemplate = UpdateConfigurationTemplate'
  { -- | A list of configuration options to remove from the configuration set.
    --
    -- Constraint: You can remove only @UserDefined@ configuration options.
    optionsToRemove :: Core.Maybe [OptionSpecification],
    -- | A list of configuration option settings to update with the new specified
    -- option value.
    optionSettings :: Core.Maybe [ConfigurationOptionSetting],
    -- | A new description for the configuration.
    description :: Core.Maybe Core.Text,
    -- | The name of the application associated with the configuration template
    -- to update.
    --
    -- If no application is found with this name, @UpdateConfigurationTemplate@
    -- returns an @InvalidParameterValue@ error.
    applicationName :: Core.Text,
    -- | The name of the configuration template to update.
    --
    -- If no configuration template is found with this name,
    -- @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionsToRemove', 'updateConfigurationTemplate_optionsToRemove' - A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
--
-- 'optionSettings', 'updateConfigurationTemplate_optionSettings' - A list of configuration option settings to update with the new specified
-- option value.
--
-- 'description', 'updateConfigurationTemplate_description' - A new description for the configuration.
--
-- 'applicationName', 'updateConfigurationTemplate_applicationName' - The name of the application associated with the configuration template
-- to update.
--
-- If no application is found with this name, @UpdateConfigurationTemplate@
-- returns an @InvalidParameterValue@ error.
--
-- 'templateName', 'updateConfigurationTemplate_templateName' - The name of the configuration template to update.
--
-- If no configuration template is found with this name,
-- @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
newUpdateConfigurationTemplate ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'templateName'
  Core.Text ->
  UpdateConfigurationTemplate
newUpdateConfigurationTemplate
  pApplicationName_
  pTemplateName_ =
    UpdateConfigurationTemplate'
      { optionsToRemove =
          Core.Nothing,
        optionSettings = Core.Nothing,
        description = Core.Nothing,
        applicationName = pApplicationName_,
        templateName = pTemplateName_
      }

-- | A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
updateConfigurationTemplate_optionsToRemove :: Lens.Lens' UpdateConfigurationTemplate (Core.Maybe [OptionSpecification])
updateConfigurationTemplate_optionsToRemove = Lens.lens (\UpdateConfigurationTemplate' {optionsToRemove} -> optionsToRemove) (\s@UpdateConfigurationTemplate' {} a -> s {optionsToRemove = a} :: UpdateConfigurationTemplate) Core.. Lens.mapping Lens._Coerce

-- | A list of configuration option settings to update with the new specified
-- option value.
updateConfigurationTemplate_optionSettings :: Lens.Lens' UpdateConfigurationTemplate (Core.Maybe [ConfigurationOptionSetting])
updateConfigurationTemplate_optionSettings = Lens.lens (\UpdateConfigurationTemplate' {optionSettings} -> optionSettings) (\s@UpdateConfigurationTemplate' {} a -> s {optionSettings = a} :: UpdateConfigurationTemplate) Core.. Lens.mapping Lens._Coerce

-- | A new description for the configuration.
updateConfigurationTemplate_description :: Lens.Lens' UpdateConfigurationTemplate (Core.Maybe Core.Text)
updateConfigurationTemplate_description = Lens.lens (\UpdateConfigurationTemplate' {description} -> description) (\s@UpdateConfigurationTemplate' {} a -> s {description = a} :: UpdateConfigurationTemplate)

-- | The name of the application associated with the configuration template
-- to update.
--
-- If no application is found with this name, @UpdateConfigurationTemplate@
-- returns an @InvalidParameterValue@ error.
updateConfigurationTemplate_applicationName :: Lens.Lens' UpdateConfigurationTemplate Core.Text
updateConfigurationTemplate_applicationName = Lens.lens (\UpdateConfigurationTemplate' {applicationName} -> applicationName) (\s@UpdateConfigurationTemplate' {} a -> s {applicationName = a} :: UpdateConfigurationTemplate)

-- | The name of the configuration template to update.
--
-- If no configuration template is found with this name,
-- @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
updateConfigurationTemplate_templateName :: Lens.Lens' UpdateConfigurationTemplate Core.Text
updateConfigurationTemplate_templateName = Lens.lens (\UpdateConfigurationTemplate' {templateName} -> templateName) (\s@UpdateConfigurationTemplate' {} a -> s {templateName = a} :: UpdateConfigurationTemplate)

instance Core.AWSRequest UpdateConfigurationTemplate where
  type
    AWSResponse UpdateConfigurationTemplate =
      ConfigurationSettingsDescription
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateConfigurationTemplateResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable UpdateConfigurationTemplate

instance Core.NFData UpdateConfigurationTemplate

instance Core.ToHeaders UpdateConfigurationTemplate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateConfigurationTemplate where
  toPath = Core.const "/"

instance Core.ToQuery UpdateConfigurationTemplate where
  toQuery UpdateConfigurationTemplate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateConfigurationTemplate" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "OptionsToRemove"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> optionsToRemove),
        "OptionSettings"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> optionSettings),
        "Description" Core.=: description,
        "ApplicationName" Core.=: applicationName,
        "TemplateName" Core.=: templateName
      ]
