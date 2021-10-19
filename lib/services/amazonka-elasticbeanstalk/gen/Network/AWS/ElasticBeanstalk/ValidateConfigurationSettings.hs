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
-- Module      : Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Takes a set of configuration settings and either a configuration
-- template or environment, and determines whether those values are valid.
--
-- This action returns a list of messages indicating any errors or warnings
-- associated with the selection of option values.
module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
  ( -- * Creating a Request
    ValidateConfigurationSettings (..),
    newValidateConfigurationSettings,

    -- * Request Lenses
    validateConfigurationSettings_templateName,
    validateConfigurationSettings_environmentName,
    validateConfigurationSettings_applicationName,
    validateConfigurationSettings_optionSettings,

    -- * Destructuring the Response
    ValidateConfigurationSettingsResponse (..),
    newValidateConfigurationSettingsResponse,

    -- * Response Lenses
    validateConfigurationSettingsResponse_messages,
    validateConfigurationSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A list of validation messages for a specified configuration template.
--
-- /See:/ 'newValidateConfigurationSettings' smart constructor.
data ValidateConfigurationSettings = ValidateConfigurationSettings'
  { -- | The name of the configuration template to validate the settings against.
    --
    -- Condition: You cannot specify both this and an environment name.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment to validate the settings against.
    --
    -- Condition: You cannot specify both this and a configuration template
    -- name.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the application that the configuration template or
    -- environment belongs to.
    applicationName :: Prelude.Text,
    -- | A list of the options and desired values to evaluate.
    optionSettings :: [ConfigurationOptionSetting]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateConfigurationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'validateConfigurationSettings_templateName' - The name of the configuration template to validate the settings against.
--
-- Condition: You cannot specify both this and an environment name.
--
-- 'environmentName', 'validateConfigurationSettings_environmentName' - The name of the environment to validate the settings against.
--
-- Condition: You cannot specify both this and a configuration template
-- name.
--
-- 'applicationName', 'validateConfigurationSettings_applicationName' - The name of the application that the configuration template or
-- environment belongs to.
--
-- 'optionSettings', 'validateConfigurationSettings_optionSettings' - A list of the options and desired values to evaluate.
newValidateConfigurationSettings ::
  -- | 'applicationName'
  Prelude.Text ->
  ValidateConfigurationSettings
newValidateConfigurationSettings pApplicationName_ =
  ValidateConfigurationSettings'
    { templateName =
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      applicationName = pApplicationName_,
      optionSettings = Prelude.mempty
    }

-- | The name of the configuration template to validate the settings against.
--
-- Condition: You cannot specify both this and an environment name.
validateConfigurationSettings_templateName :: Lens.Lens' ValidateConfigurationSettings (Prelude.Maybe Prelude.Text)
validateConfigurationSettings_templateName = Lens.lens (\ValidateConfigurationSettings' {templateName} -> templateName) (\s@ValidateConfigurationSettings' {} a -> s {templateName = a} :: ValidateConfigurationSettings)

-- | The name of the environment to validate the settings against.
--
-- Condition: You cannot specify both this and a configuration template
-- name.
validateConfigurationSettings_environmentName :: Lens.Lens' ValidateConfigurationSettings (Prelude.Maybe Prelude.Text)
validateConfigurationSettings_environmentName = Lens.lens (\ValidateConfigurationSettings' {environmentName} -> environmentName) (\s@ValidateConfigurationSettings' {} a -> s {environmentName = a} :: ValidateConfigurationSettings)

-- | The name of the application that the configuration template or
-- environment belongs to.
validateConfigurationSettings_applicationName :: Lens.Lens' ValidateConfigurationSettings Prelude.Text
validateConfigurationSettings_applicationName = Lens.lens (\ValidateConfigurationSettings' {applicationName} -> applicationName) (\s@ValidateConfigurationSettings' {} a -> s {applicationName = a} :: ValidateConfigurationSettings)

-- | A list of the options and desired values to evaluate.
validateConfigurationSettings_optionSettings :: Lens.Lens' ValidateConfigurationSettings [ConfigurationOptionSetting]
validateConfigurationSettings_optionSettings = Lens.lens (\ValidateConfigurationSettings' {optionSettings} -> optionSettings) (\s@ValidateConfigurationSettings' {} a -> s {optionSettings = a} :: ValidateConfigurationSettings) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    ValidateConfigurationSettings
  where
  type
    AWSResponse ValidateConfigurationSettings =
      ValidateConfigurationSettingsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ValidateConfigurationSettingsResult"
      ( \s h x ->
          ValidateConfigurationSettingsResponse'
            Prelude.<$> ( x Core..@? "Messages" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ValidateConfigurationSettings

instance Prelude.NFData ValidateConfigurationSettings

instance Core.ToHeaders ValidateConfigurationSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ValidateConfigurationSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery ValidateConfigurationSettings where
  toQuery ValidateConfigurationSettings' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ValidateConfigurationSettings" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Core.=: templateName,
        "EnvironmentName" Core.=: environmentName,
        "ApplicationName" Core.=: applicationName,
        "OptionSettings"
          Core.=: Core.toQueryList "member" optionSettings
      ]

-- | Provides a list of validation messages.
--
-- /See:/ 'newValidateConfigurationSettingsResponse' smart constructor.
data ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse'
  { -- | A list of ValidationMessage.
    messages :: Prelude.Maybe [ValidationMessage],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateConfigurationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messages', 'validateConfigurationSettingsResponse_messages' - A list of ValidationMessage.
--
-- 'httpStatus', 'validateConfigurationSettingsResponse_httpStatus' - The response's http status code.
newValidateConfigurationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ValidateConfigurationSettingsResponse
newValidateConfigurationSettingsResponse pHttpStatus_ =
  ValidateConfigurationSettingsResponse'
    { messages =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ValidationMessage.
validateConfigurationSettingsResponse_messages :: Lens.Lens' ValidateConfigurationSettingsResponse (Prelude.Maybe [ValidationMessage])
validateConfigurationSettingsResponse_messages = Lens.lens (\ValidateConfigurationSettingsResponse' {messages} -> messages) (\s@ValidateConfigurationSettingsResponse' {} a -> s {messages = a} :: ValidateConfigurationSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
validateConfigurationSettingsResponse_httpStatus :: Lens.Lens' ValidateConfigurationSettingsResponse Prelude.Int
validateConfigurationSettingsResponse_httpStatus = Lens.lens (\ValidateConfigurationSettingsResponse' {httpStatus} -> httpStatus) (\s@ValidateConfigurationSettingsResponse' {} a -> s {httpStatus = a} :: ValidateConfigurationSettingsResponse)

instance
  Prelude.NFData
    ValidateConfigurationSettingsResponse
