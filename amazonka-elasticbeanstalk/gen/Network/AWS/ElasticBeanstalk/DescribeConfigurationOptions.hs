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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the configuration options that are used in a particular
-- configuration template or environment, or that a specified solution
-- stack defines. The description includes the values the options, their
-- default values, and an indication of the required action on a running
-- environment if an option value is changed.
module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
  ( -- * Creating a Request
    DescribeConfigurationOptions (..),
    newDescribeConfigurationOptions,

    -- * Request Lenses
    describeConfigurationOptions_templateName,
    describeConfigurationOptions_options,
    describeConfigurationOptions_solutionStackName,
    describeConfigurationOptions_environmentName,
    describeConfigurationOptions_platformArn,
    describeConfigurationOptions_applicationName,

    -- * Destructuring the Response
    DescribeConfigurationOptionsResponse (..),
    newDescribeConfigurationOptionsResponse,

    -- * Response Lenses
    describeConfigurationOptionsResponse_options,
    describeConfigurationOptionsResponse_solutionStackName,
    describeConfigurationOptionsResponse_platformArn,
    describeConfigurationOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Result message containing a list of application version descriptions.
--
-- /See:/ 'newDescribeConfigurationOptions' smart constructor.
data DescribeConfigurationOptions = DescribeConfigurationOptions'
  { -- | The name of the configuration template whose configuration options you
    -- want to describe.
    templateName :: Core.Maybe Core.Text,
    -- | If specified, restricts the descriptions to only the specified options.
    options :: Core.Maybe [OptionSpecification],
    -- | The name of the solution stack whose configuration options you want to
    -- describe.
    solutionStackName :: Core.Maybe Core.Text,
    -- | The name of the environment whose configuration options you want to
    -- describe.
    environmentName :: Core.Maybe Core.Text,
    -- | The ARN of the custom platform.
    platformArn :: Core.Maybe Core.Text,
    -- | The name of the application associated with the configuration template
    -- or environment. Only needed if you want to describe the configuration
    -- options associated with either the configuration template or
    -- environment.
    applicationName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigurationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'describeConfigurationOptions_templateName' - The name of the configuration template whose configuration options you
-- want to describe.
--
-- 'options', 'describeConfigurationOptions_options' - If specified, restricts the descriptions to only the specified options.
--
-- 'solutionStackName', 'describeConfigurationOptions_solutionStackName' - The name of the solution stack whose configuration options you want to
-- describe.
--
-- 'environmentName', 'describeConfigurationOptions_environmentName' - The name of the environment whose configuration options you want to
-- describe.
--
-- 'platformArn', 'describeConfigurationOptions_platformArn' - The ARN of the custom platform.
--
-- 'applicationName', 'describeConfigurationOptions_applicationName' - The name of the application associated with the configuration template
-- or environment. Only needed if you want to describe the configuration
-- options associated with either the configuration template or
-- environment.
newDescribeConfigurationOptions ::
  DescribeConfigurationOptions
newDescribeConfigurationOptions =
  DescribeConfigurationOptions'
    { templateName =
        Core.Nothing,
      options = Core.Nothing,
      solutionStackName = Core.Nothing,
      environmentName = Core.Nothing,
      platformArn = Core.Nothing,
      applicationName = Core.Nothing
    }

-- | The name of the configuration template whose configuration options you
-- want to describe.
describeConfigurationOptions_templateName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Core.Text)
describeConfigurationOptions_templateName = Lens.lens (\DescribeConfigurationOptions' {templateName} -> templateName) (\s@DescribeConfigurationOptions' {} a -> s {templateName = a} :: DescribeConfigurationOptions)

-- | If specified, restricts the descriptions to only the specified options.
describeConfigurationOptions_options :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe [OptionSpecification])
describeConfigurationOptions_options = Lens.lens (\DescribeConfigurationOptions' {options} -> options) (\s@DescribeConfigurationOptions' {} a -> s {options = a} :: DescribeConfigurationOptions) Core.. Lens.mapping Lens._Coerce

-- | The name of the solution stack whose configuration options you want to
-- describe.
describeConfigurationOptions_solutionStackName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Core.Text)
describeConfigurationOptions_solutionStackName = Lens.lens (\DescribeConfigurationOptions' {solutionStackName} -> solutionStackName) (\s@DescribeConfigurationOptions' {} a -> s {solutionStackName = a} :: DescribeConfigurationOptions)

-- | The name of the environment whose configuration options you want to
-- describe.
describeConfigurationOptions_environmentName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Core.Text)
describeConfigurationOptions_environmentName = Lens.lens (\DescribeConfigurationOptions' {environmentName} -> environmentName) (\s@DescribeConfigurationOptions' {} a -> s {environmentName = a} :: DescribeConfigurationOptions)

-- | The ARN of the custom platform.
describeConfigurationOptions_platformArn :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Core.Text)
describeConfigurationOptions_platformArn = Lens.lens (\DescribeConfigurationOptions' {platformArn} -> platformArn) (\s@DescribeConfigurationOptions' {} a -> s {platformArn = a} :: DescribeConfigurationOptions)

-- | The name of the application associated with the configuration template
-- or environment. Only needed if you want to describe the configuration
-- options associated with either the configuration template or
-- environment.
describeConfigurationOptions_applicationName :: Lens.Lens' DescribeConfigurationOptions (Core.Maybe Core.Text)
describeConfigurationOptions_applicationName = Lens.lens (\DescribeConfigurationOptions' {applicationName} -> applicationName) (\s@DescribeConfigurationOptions' {} a -> s {applicationName = a} :: DescribeConfigurationOptions)

instance Core.AWSRequest DescribeConfigurationOptions where
  type
    AWSResponse DescribeConfigurationOptions =
      DescribeConfigurationOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeConfigurationOptionsResult"
      ( \s h x ->
          DescribeConfigurationOptionsResponse'
            Core.<$> ( x Core..@? "Options" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "SolutionStackName")
            Core.<*> (x Core..@? "PlatformArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeConfigurationOptions

instance Core.NFData DescribeConfigurationOptions

instance Core.ToHeaders DescribeConfigurationOptions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeConfigurationOptions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConfigurationOptions where
  toQuery DescribeConfigurationOptions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeConfigurationOptions" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "TemplateName" Core.=: templateName,
        "Options"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> options),
        "SolutionStackName" Core.=: solutionStackName,
        "EnvironmentName" Core.=: environmentName,
        "PlatformArn" Core.=: platformArn,
        "ApplicationName" Core.=: applicationName
      ]

-- | Describes the settings for a specified configuration set.
--
-- /See:/ 'newDescribeConfigurationOptionsResponse' smart constructor.
data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse'
  { -- | A list of ConfigurationOptionDescription.
    options :: Core.Maybe [ConfigurationOptionDescription],
    -- | The name of the solution stack these configuration options belong to.
    solutionStackName :: Core.Maybe Core.Text,
    -- | The ARN of the platform version.
    platformArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigurationOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'describeConfigurationOptionsResponse_options' - A list of ConfigurationOptionDescription.
--
-- 'solutionStackName', 'describeConfigurationOptionsResponse_solutionStackName' - The name of the solution stack these configuration options belong to.
--
-- 'platformArn', 'describeConfigurationOptionsResponse_platformArn' - The ARN of the platform version.
--
-- 'httpStatus', 'describeConfigurationOptionsResponse_httpStatus' - The response's http status code.
newDescribeConfigurationOptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConfigurationOptionsResponse
newDescribeConfigurationOptionsResponse pHttpStatus_ =
  DescribeConfigurationOptionsResponse'
    { options =
        Core.Nothing,
      solutionStackName = Core.Nothing,
      platformArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ConfigurationOptionDescription.
describeConfigurationOptionsResponse_options :: Lens.Lens' DescribeConfigurationOptionsResponse (Core.Maybe [ConfigurationOptionDescription])
describeConfigurationOptionsResponse_options = Lens.lens (\DescribeConfigurationOptionsResponse' {options} -> options) (\s@DescribeConfigurationOptionsResponse' {} a -> s {options = a} :: DescribeConfigurationOptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The name of the solution stack these configuration options belong to.
describeConfigurationOptionsResponse_solutionStackName :: Lens.Lens' DescribeConfigurationOptionsResponse (Core.Maybe Core.Text)
describeConfigurationOptionsResponse_solutionStackName = Lens.lens (\DescribeConfigurationOptionsResponse' {solutionStackName} -> solutionStackName) (\s@DescribeConfigurationOptionsResponse' {} a -> s {solutionStackName = a} :: DescribeConfigurationOptionsResponse)

-- | The ARN of the platform version.
describeConfigurationOptionsResponse_platformArn :: Lens.Lens' DescribeConfigurationOptionsResponse (Core.Maybe Core.Text)
describeConfigurationOptionsResponse_platformArn = Lens.lens (\DescribeConfigurationOptionsResponse' {platformArn} -> platformArn) (\s@DescribeConfigurationOptionsResponse' {} a -> s {platformArn = a} :: DescribeConfigurationOptionsResponse)

-- | The response's http status code.
describeConfigurationOptionsResponse_httpStatus :: Lens.Lens' DescribeConfigurationOptionsResponse Core.Int
describeConfigurationOptionsResponse_httpStatus = Lens.lens (\DescribeConfigurationOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationOptionsResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationOptionsResponse)

instance
  Core.NFData
    DescribeConfigurationOptionsResponse
