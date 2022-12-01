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
-- Module      : Amazonka.ElasticBeanstalk.DescribeConfigurationSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the settings for the specified configuration
-- set, that is, either a configuration template or the configuration set
-- associated with a running environment.
--
-- When describing the settings for the configuration set associated with a
-- running environment, it is possible to receive two sets of setting
-- descriptions. One is the deployed configuration set, and the other is a
-- draft configuration of an environment that is either in the process of
-- deployment or that failed to deploy.
--
-- Related Topics
--
-- -   DeleteEnvironmentConfiguration
module Amazonka.ElasticBeanstalk.DescribeConfigurationSettings
  ( -- * Creating a Request
    DescribeConfigurationSettings (..),
    newDescribeConfigurationSettings,

    -- * Request Lenses
    describeConfigurationSettings_templateName,
    describeConfigurationSettings_environmentName,
    describeConfigurationSettings_applicationName,

    -- * Destructuring the Response
    DescribeConfigurationSettingsResponse (..),
    newDescribeConfigurationSettingsResponse,

    -- * Response Lenses
    describeConfigurationSettingsResponse_configurationSettings,
    describeConfigurationSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Result message containing all of the configuration settings for a
-- specified solution stack or configuration template.
--
-- /See:/ 'newDescribeConfigurationSettings' smart constructor.
data DescribeConfigurationSettings = DescribeConfigurationSettings'
  { -- | The name of the configuration template to describe.
    --
    -- Conditional: You must specify either this parameter or an
    -- EnvironmentName, but not both. If you specify both, AWS Elastic
    -- Beanstalk returns an @InvalidParameterCombination@ error. If you do not
    -- specify either, AWS Elastic Beanstalk returns a
    -- @MissingRequiredParameter@ error.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment to describe.
    --
    -- Condition: You must specify either this or a TemplateName, but not both.
    -- If you specify both, AWS Elastic Beanstalk returns an
    -- @InvalidParameterCombination@ error. If you do not specify either, AWS
    -- Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The application for the environment or configuration template.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'describeConfigurationSettings_templateName' - The name of the configuration template to describe.
--
-- Conditional: You must specify either this parameter or an
-- EnvironmentName, but not both. If you specify both, AWS Elastic
-- Beanstalk returns an @InvalidParameterCombination@ error. If you do not
-- specify either, AWS Elastic Beanstalk returns a
-- @MissingRequiredParameter@ error.
--
-- 'environmentName', 'describeConfigurationSettings_environmentName' - The name of the environment to describe.
--
-- Condition: You must specify either this or a TemplateName, but not both.
-- If you specify both, AWS Elastic Beanstalk returns an
-- @InvalidParameterCombination@ error. If you do not specify either, AWS
-- Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- 'applicationName', 'describeConfigurationSettings_applicationName' - The application for the environment or configuration template.
newDescribeConfigurationSettings ::
  -- | 'applicationName'
  Prelude.Text ->
  DescribeConfigurationSettings
newDescribeConfigurationSettings pApplicationName_ =
  DescribeConfigurationSettings'
    { templateName =
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | The name of the configuration template to describe.
--
-- Conditional: You must specify either this parameter or an
-- EnvironmentName, but not both. If you specify both, AWS Elastic
-- Beanstalk returns an @InvalidParameterCombination@ error. If you do not
-- specify either, AWS Elastic Beanstalk returns a
-- @MissingRequiredParameter@ error.
describeConfigurationSettings_templateName :: Lens.Lens' DescribeConfigurationSettings (Prelude.Maybe Prelude.Text)
describeConfigurationSettings_templateName = Lens.lens (\DescribeConfigurationSettings' {templateName} -> templateName) (\s@DescribeConfigurationSettings' {} a -> s {templateName = a} :: DescribeConfigurationSettings)

-- | The name of the environment to describe.
--
-- Condition: You must specify either this or a TemplateName, but not both.
-- If you specify both, AWS Elastic Beanstalk returns an
-- @InvalidParameterCombination@ error. If you do not specify either, AWS
-- Elastic Beanstalk returns @MissingRequiredParameter@ error.
describeConfigurationSettings_environmentName :: Lens.Lens' DescribeConfigurationSettings (Prelude.Maybe Prelude.Text)
describeConfigurationSettings_environmentName = Lens.lens (\DescribeConfigurationSettings' {environmentName} -> environmentName) (\s@DescribeConfigurationSettings' {} a -> s {environmentName = a} :: DescribeConfigurationSettings)

-- | The application for the environment or configuration template.
describeConfigurationSettings_applicationName :: Lens.Lens' DescribeConfigurationSettings Prelude.Text
describeConfigurationSettings_applicationName = Lens.lens (\DescribeConfigurationSettings' {applicationName} -> applicationName) (\s@DescribeConfigurationSettings' {} a -> s {applicationName = a} :: DescribeConfigurationSettings)

instance
  Core.AWSRequest
    DescribeConfigurationSettings
  where
  type
    AWSResponse DescribeConfigurationSettings =
      DescribeConfigurationSettingsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeConfigurationSettingsResult"
      ( \s h x ->
          DescribeConfigurationSettingsResponse'
            Prelude.<$> ( x Core..@? "ConfigurationSettings"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConfigurationSettings
  where
  hashWithSalt _salt DescribeConfigurationSettings' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData DescribeConfigurationSettings where
  rnf DescribeConfigurationSettings' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf applicationName

instance Core.ToHeaders DescribeConfigurationSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeConfigurationSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeConfigurationSettings where
  toQuery DescribeConfigurationSettings' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeConfigurationSettings" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Core.=: templateName,
        "EnvironmentName" Core.=: environmentName,
        "ApplicationName" Core.=: applicationName
      ]

-- | The results from a request to change the configuration settings of an
-- environment.
--
-- /See:/ 'newDescribeConfigurationSettingsResponse' smart constructor.
data DescribeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse'
  { -- | A list of ConfigurationSettingsDescription.
    configurationSettings :: Prelude.Maybe [ConfigurationSettingsDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSettings', 'describeConfigurationSettingsResponse_configurationSettings' - A list of ConfigurationSettingsDescription.
--
-- 'httpStatus', 'describeConfigurationSettingsResponse_httpStatus' - The response's http status code.
newDescribeConfigurationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationSettingsResponse
newDescribeConfigurationSettingsResponse pHttpStatus_ =
  DescribeConfigurationSettingsResponse'
    { configurationSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ConfigurationSettingsDescription.
describeConfigurationSettingsResponse_configurationSettings :: Lens.Lens' DescribeConfigurationSettingsResponse (Prelude.Maybe [ConfigurationSettingsDescription])
describeConfigurationSettingsResponse_configurationSettings = Lens.lens (\DescribeConfigurationSettingsResponse' {configurationSettings} -> configurationSettings) (\s@DescribeConfigurationSettingsResponse' {} a -> s {configurationSettings = a} :: DescribeConfigurationSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeConfigurationSettingsResponse_httpStatus :: Lens.Lens' DescribeConfigurationSettingsResponse Prelude.Int
describeConfigurationSettingsResponse_httpStatus = Lens.lens (\DescribeConfigurationSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationSettingsResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationSettingsResponse)

instance
  Prelude.NFData
    DescribeConfigurationSettingsResponse
  where
  rnf DescribeConfigurationSettingsResponse' {..} =
    Prelude.rnf configurationSettings
      `Prelude.seq` Prelude.rnf httpStatus
