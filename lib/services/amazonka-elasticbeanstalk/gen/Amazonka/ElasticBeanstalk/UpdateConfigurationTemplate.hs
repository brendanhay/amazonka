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
-- Module      : Amazonka.ElasticBeanstalk.UpdateConfigurationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ElasticBeanstalk.UpdateConfigurationTemplate
  ( -- * Creating a Request
    UpdateConfigurationTemplate (..),
    newUpdateConfigurationTemplate,

    -- * Request Lenses
    updateConfigurationTemplate_description,
    updateConfigurationTemplate_optionSettings,
    updateConfigurationTemplate_optionsToRemove,
    updateConfigurationTemplate_applicationName,
    updateConfigurationTemplate_templateName,

    -- * Destructuring the Response
    ConfigurationSettingsDescription (..),
    newConfigurationSettingsDescription,

    -- * Response Lenses
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_description,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_optionSettings,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_templateName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The result message containing the options for the specified solution
-- stack.
--
-- /See:/ 'newUpdateConfigurationTemplate' smart constructor.
data UpdateConfigurationTemplate = UpdateConfigurationTemplate'
  { -- | A new description for the configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of configuration option settings to update with the new specified
    -- option value.
    optionSettings :: Prelude.Maybe [ConfigurationOptionSetting],
    -- | A list of configuration options to remove from the configuration set.
    --
    -- Constraint: You can remove only @UserDefined@ configuration options.
    optionsToRemove :: Prelude.Maybe [OptionSpecification],
    -- | The name of the application associated with the configuration template
    -- to update.
    --
    -- If no application is found with this name, @UpdateConfigurationTemplate@
    -- returns an @InvalidParameterValue@ error.
    applicationName :: Prelude.Text,
    -- | The name of the configuration template to update.
    --
    -- If no configuration template is found with this name,
    -- @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateConfigurationTemplate_description' - A new description for the configuration.
--
-- 'optionSettings', 'updateConfigurationTemplate_optionSettings' - A list of configuration option settings to update with the new specified
-- option value.
--
-- 'optionsToRemove', 'updateConfigurationTemplate_optionsToRemove' - A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
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
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  UpdateConfigurationTemplate
newUpdateConfigurationTemplate
  pApplicationName_
  pTemplateName_ =
    UpdateConfigurationTemplate'
      { description =
          Prelude.Nothing,
        optionSettings = Prelude.Nothing,
        optionsToRemove = Prelude.Nothing,
        applicationName = pApplicationName_,
        templateName = pTemplateName_
      }

-- | A new description for the configuration.
updateConfigurationTemplate_description :: Lens.Lens' UpdateConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateConfigurationTemplate_description = Lens.lens (\UpdateConfigurationTemplate' {description} -> description) (\s@UpdateConfigurationTemplate' {} a -> s {description = a} :: UpdateConfigurationTemplate)

-- | A list of configuration option settings to update with the new specified
-- option value.
updateConfigurationTemplate_optionSettings :: Lens.Lens' UpdateConfigurationTemplate (Prelude.Maybe [ConfigurationOptionSetting])
updateConfigurationTemplate_optionSettings = Lens.lens (\UpdateConfigurationTemplate' {optionSettings} -> optionSettings) (\s@UpdateConfigurationTemplate' {} a -> s {optionSettings = a} :: UpdateConfigurationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | A list of configuration options to remove from the configuration set.
--
-- Constraint: You can remove only @UserDefined@ configuration options.
updateConfigurationTemplate_optionsToRemove :: Lens.Lens' UpdateConfigurationTemplate (Prelude.Maybe [OptionSpecification])
updateConfigurationTemplate_optionsToRemove = Lens.lens (\UpdateConfigurationTemplate' {optionsToRemove} -> optionsToRemove) (\s@UpdateConfigurationTemplate' {} a -> s {optionsToRemove = a} :: UpdateConfigurationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The name of the application associated with the configuration template
-- to update.
--
-- If no application is found with this name, @UpdateConfigurationTemplate@
-- returns an @InvalidParameterValue@ error.
updateConfigurationTemplate_applicationName :: Lens.Lens' UpdateConfigurationTemplate Prelude.Text
updateConfigurationTemplate_applicationName = Lens.lens (\UpdateConfigurationTemplate' {applicationName} -> applicationName) (\s@UpdateConfigurationTemplate' {} a -> s {applicationName = a} :: UpdateConfigurationTemplate)

-- | The name of the configuration template to update.
--
-- If no configuration template is found with this name,
-- @UpdateConfigurationTemplate@ returns an @InvalidParameterValue@ error.
updateConfigurationTemplate_templateName :: Lens.Lens' UpdateConfigurationTemplate Prelude.Text
updateConfigurationTemplate_templateName = Lens.lens (\UpdateConfigurationTemplate' {templateName} -> templateName) (\s@UpdateConfigurationTemplate' {} a -> s {templateName = a} :: UpdateConfigurationTemplate)

instance Core.AWSRequest UpdateConfigurationTemplate where
  type
    AWSResponse UpdateConfigurationTemplate =
      ConfigurationSettingsDescription
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateConfigurationTemplateResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable UpdateConfigurationTemplate where
  hashWithSalt _salt UpdateConfigurationTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` optionSettings
      `Prelude.hashWithSalt` optionsToRemove
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData UpdateConfigurationTemplate where
  rnf UpdateConfigurationTemplate' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf optionSettings
      `Prelude.seq` Prelude.rnf optionsToRemove
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders UpdateConfigurationTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateConfigurationTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateConfigurationTemplate where
  toQuery UpdateConfigurationTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "UpdateConfigurationTemplate" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Description" Data.=: description,
        "OptionSettings"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> optionSettings
            ),
        "OptionsToRemove"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> optionsToRemove
            ),
        "ApplicationName" Data.=: applicationName,
        "TemplateName" Data.=: templateName
      ]
