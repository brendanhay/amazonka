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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    templateName :: Core.Maybe Core.Text,
    -- | The name of the environment to describe.
    --
    -- Condition: You must specify either this or a TemplateName, but not both.
    -- If you specify both, AWS Elastic Beanstalk returns an
    -- @InvalidParameterCombination@ error. If you do not specify either, AWS
    -- Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentName :: Core.Maybe Core.Text,
    -- | The application for the environment or configuration template.
    applicationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeConfigurationSettings
newDescribeConfigurationSettings pApplicationName_ =
  DescribeConfigurationSettings'
    { templateName =
        Core.Nothing,
      environmentName = Core.Nothing,
      applicationName = pApplicationName_
    }

-- | The name of the configuration template to describe.
--
-- Conditional: You must specify either this parameter or an
-- EnvironmentName, but not both. If you specify both, AWS Elastic
-- Beanstalk returns an @InvalidParameterCombination@ error. If you do not
-- specify either, AWS Elastic Beanstalk returns a
-- @MissingRequiredParameter@ error.
describeConfigurationSettings_templateName :: Lens.Lens' DescribeConfigurationSettings (Core.Maybe Core.Text)
describeConfigurationSettings_templateName = Lens.lens (\DescribeConfigurationSettings' {templateName} -> templateName) (\s@DescribeConfigurationSettings' {} a -> s {templateName = a} :: DescribeConfigurationSettings)

-- | The name of the environment to describe.
--
-- Condition: You must specify either this or a TemplateName, but not both.
-- If you specify both, AWS Elastic Beanstalk returns an
-- @InvalidParameterCombination@ error. If you do not specify either, AWS
-- Elastic Beanstalk returns @MissingRequiredParameter@ error.
describeConfigurationSettings_environmentName :: Lens.Lens' DescribeConfigurationSettings (Core.Maybe Core.Text)
describeConfigurationSettings_environmentName = Lens.lens (\DescribeConfigurationSettings' {environmentName} -> environmentName) (\s@DescribeConfigurationSettings' {} a -> s {environmentName = a} :: DescribeConfigurationSettings)

-- | The application for the environment or configuration template.
describeConfigurationSettings_applicationName :: Lens.Lens' DescribeConfigurationSettings Core.Text
describeConfigurationSettings_applicationName = Lens.lens (\DescribeConfigurationSettings' {applicationName} -> applicationName) (\s@DescribeConfigurationSettings' {} a -> s {applicationName = a} :: DescribeConfigurationSettings)

instance
  Core.AWSRequest
    DescribeConfigurationSettings
  where
  type
    AWSResponse DescribeConfigurationSettings =
      DescribeConfigurationSettingsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeConfigurationSettingsResult"
      ( \s h x ->
          DescribeConfigurationSettingsResponse'
            Core.<$> ( x Core..@? "ConfigurationSettings"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConfigurationSettings

instance Core.NFData DescribeConfigurationSettings

instance Core.ToHeaders DescribeConfigurationSettings where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeConfigurationSettings where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConfigurationSettings where
  toQuery DescribeConfigurationSettings' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeConfigurationSettings" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
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
    configurationSettings :: Core.Maybe [ConfigurationSettingsDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeConfigurationSettingsResponse
newDescribeConfigurationSettingsResponse pHttpStatus_ =
  DescribeConfigurationSettingsResponse'
    { configurationSettings =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ConfigurationSettingsDescription.
describeConfigurationSettingsResponse_configurationSettings :: Lens.Lens' DescribeConfigurationSettingsResponse (Core.Maybe [ConfigurationSettingsDescription])
describeConfigurationSettingsResponse_configurationSettings = Lens.lens (\DescribeConfigurationSettingsResponse' {configurationSettings} -> configurationSettings) (\s@DescribeConfigurationSettingsResponse' {} a -> s {configurationSettings = a} :: DescribeConfigurationSettingsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConfigurationSettingsResponse_httpStatus :: Lens.Lens' DescribeConfigurationSettingsResponse Core.Int
describeConfigurationSettingsResponse_httpStatus = Lens.lens (\DescribeConfigurationSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationSettingsResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationSettingsResponse)

instance
  Core.NFData
    DescribeConfigurationSettingsResponse
