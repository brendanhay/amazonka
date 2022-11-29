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
-- Module      : Amazonka.ElasticBeanstalk.DescribeConfigurationOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ElasticBeanstalk.DescribeConfigurationOptions
  ( -- * Creating a Request
    DescribeConfigurationOptions (..),
    newDescribeConfigurationOptions,

    -- * Request Lenses
    describeConfigurationOptions_templateName,
    describeConfigurationOptions_environmentName,
    describeConfigurationOptions_options,
    describeConfigurationOptions_solutionStackName,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Result message containing a list of application version descriptions.
--
-- /See:/ 'newDescribeConfigurationOptions' smart constructor.
data DescribeConfigurationOptions = DescribeConfigurationOptions'
  { -- | The name of the configuration template whose configuration options you
    -- want to describe.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment whose configuration options you want to
    -- describe.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | If specified, restricts the descriptions to only the specified options.
    options :: Prelude.Maybe [OptionSpecification],
    -- | The name of the solution stack whose configuration options you want to
    -- describe.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the custom platform.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the application associated with the configuration template
    -- or environment. Only needed if you want to describe the configuration
    -- options associated with either the configuration template or
    -- environment.
    applicationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'environmentName', 'describeConfigurationOptions_environmentName' - The name of the environment whose configuration options you want to
-- describe.
--
-- 'options', 'describeConfigurationOptions_options' - If specified, restricts the descriptions to only the specified options.
--
-- 'solutionStackName', 'describeConfigurationOptions_solutionStackName' - The name of the solution stack whose configuration options you want to
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
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      options = Prelude.Nothing,
      solutionStackName = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      applicationName = Prelude.Nothing
    }

-- | The name of the configuration template whose configuration options you
-- want to describe.
describeConfigurationOptions_templateName :: Lens.Lens' DescribeConfigurationOptions (Prelude.Maybe Prelude.Text)
describeConfigurationOptions_templateName = Lens.lens (\DescribeConfigurationOptions' {templateName} -> templateName) (\s@DescribeConfigurationOptions' {} a -> s {templateName = a} :: DescribeConfigurationOptions)

-- | The name of the environment whose configuration options you want to
-- describe.
describeConfigurationOptions_environmentName :: Lens.Lens' DescribeConfigurationOptions (Prelude.Maybe Prelude.Text)
describeConfigurationOptions_environmentName = Lens.lens (\DescribeConfigurationOptions' {environmentName} -> environmentName) (\s@DescribeConfigurationOptions' {} a -> s {environmentName = a} :: DescribeConfigurationOptions)

-- | If specified, restricts the descriptions to only the specified options.
describeConfigurationOptions_options :: Lens.Lens' DescribeConfigurationOptions (Prelude.Maybe [OptionSpecification])
describeConfigurationOptions_options = Lens.lens (\DescribeConfigurationOptions' {options} -> options) (\s@DescribeConfigurationOptions' {} a -> s {options = a} :: DescribeConfigurationOptions) Prelude.. Lens.mapping Lens.coerced

-- | The name of the solution stack whose configuration options you want to
-- describe.
describeConfigurationOptions_solutionStackName :: Lens.Lens' DescribeConfigurationOptions (Prelude.Maybe Prelude.Text)
describeConfigurationOptions_solutionStackName = Lens.lens (\DescribeConfigurationOptions' {solutionStackName} -> solutionStackName) (\s@DescribeConfigurationOptions' {} a -> s {solutionStackName = a} :: DescribeConfigurationOptions)

-- | The ARN of the custom platform.
describeConfigurationOptions_platformArn :: Lens.Lens' DescribeConfigurationOptions (Prelude.Maybe Prelude.Text)
describeConfigurationOptions_platformArn = Lens.lens (\DescribeConfigurationOptions' {platformArn} -> platformArn) (\s@DescribeConfigurationOptions' {} a -> s {platformArn = a} :: DescribeConfigurationOptions)

-- | The name of the application associated with the configuration template
-- or environment. Only needed if you want to describe the configuration
-- options associated with either the configuration template or
-- environment.
describeConfigurationOptions_applicationName :: Lens.Lens' DescribeConfigurationOptions (Prelude.Maybe Prelude.Text)
describeConfigurationOptions_applicationName = Lens.lens (\DescribeConfigurationOptions' {applicationName} -> applicationName) (\s@DescribeConfigurationOptions' {} a -> s {applicationName = a} :: DescribeConfigurationOptions)

instance Core.AWSRequest DescribeConfigurationOptions where
  type
    AWSResponse DescribeConfigurationOptions =
      DescribeConfigurationOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeConfigurationOptionsResult"
      ( \s h x ->
          DescribeConfigurationOptionsResponse'
            Prelude.<$> ( x Core..@? "Options" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "SolutionStackName")
            Prelude.<*> (x Core..@? "PlatformArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConfigurationOptions
  where
  hashWithSalt _salt DescribeConfigurationOptions' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` solutionStackName
      `Prelude.hashWithSalt` platformArn
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData DescribeConfigurationOptions where
  rnf DescribeConfigurationOptions' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf solutionStackName
      `Prelude.seq` Prelude.rnf platformArn
      `Prelude.seq` Prelude.rnf applicationName

instance Core.ToHeaders DescribeConfigurationOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeConfigurationOptions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeConfigurationOptions where
  toQuery DescribeConfigurationOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeConfigurationOptions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Core.=: templateName,
        "EnvironmentName" Core.=: environmentName,
        "Options"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> options),
        "SolutionStackName" Core.=: solutionStackName,
        "PlatformArn" Core.=: platformArn,
        "ApplicationName" Core.=: applicationName
      ]

-- | Describes the settings for a specified configuration set.
--
-- /See:/ 'newDescribeConfigurationOptionsResponse' smart constructor.
data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse'
  { -- | A list of ConfigurationOptionDescription.
    options :: Prelude.Maybe [ConfigurationOptionDescription],
    -- | The name of the solution stack these configuration options belong to.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the platform version.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeConfigurationOptionsResponse
newDescribeConfigurationOptionsResponse pHttpStatus_ =
  DescribeConfigurationOptionsResponse'
    { options =
        Prelude.Nothing,
      solutionStackName = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ConfigurationOptionDescription.
describeConfigurationOptionsResponse_options :: Lens.Lens' DescribeConfigurationOptionsResponse (Prelude.Maybe [ConfigurationOptionDescription])
describeConfigurationOptionsResponse_options = Lens.lens (\DescribeConfigurationOptionsResponse' {options} -> options) (\s@DescribeConfigurationOptionsResponse' {} a -> s {options = a} :: DescribeConfigurationOptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the solution stack these configuration options belong to.
describeConfigurationOptionsResponse_solutionStackName :: Lens.Lens' DescribeConfigurationOptionsResponse (Prelude.Maybe Prelude.Text)
describeConfigurationOptionsResponse_solutionStackName = Lens.lens (\DescribeConfigurationOptionsResponse' {solutionStackName} -> solutionStackName) (\s@DescribeConfigurationOptionsResponse' {} a -> s {solutionStackName = a} :: DescribeConfigurationOptionsResponse)

-- | The ARN of the platform version.
describeConfigurationOptionsResponse_platformArn :: Lens.Lens' DescribeConfigurationOptionsResponse (Prelude.Maybe Prelude.Text)
describeConfigurationOptionsResponse_platformArn = Lens.lens (\DescribeConfigurationOptionsResponse' {platformArn} -> platformArn) (\s@DescribeConfigurationOptionsResponse' {} a -> s {platformArn = a} :: DescribeConfigurationOptionsResponse)

-- | The response's http status code.
describeConfigurationOptionsResponse_httpStatus :: Lens.Lens' DescribeConfigurationOptionsResponse Prelude.Int
describeConfigurationOptionsResponse_httpStatus = Lens.lens (\DescribeConfigurationOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationOptionsResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationOptionsResponse)

instance
  Prelude.NFData
    DescribeConfigurationOptionsResponse
  where
  rnf DescribeConfigurationOptionsResponse' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf solutionStackName
      `Prelude.seq` Prelude.rnf platformArn
      `Prelude.seq` Prelude.rnf httpStatus
