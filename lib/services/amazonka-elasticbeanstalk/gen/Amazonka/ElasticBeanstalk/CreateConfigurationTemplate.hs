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
-- Module      : Amazonka.ElasticBeanstalk.CreateConfigurationTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Elastic Beanstalk configuration template, associated with
-- a specific Elastic Beanstalk application. You define application
-- configuration settings in a configuration template. You can then use the
-- configuration template to deploy different versions of the application
-- with the same configuration settings.
--
-- Templates aren\'t associated with any environment. The @EnvironmentName@
-- response element is always @null@.
--
-- Related Topics
--
-- -   DescribeConfigurationOptions
--
-- -   DescribeConfigurationSettings
--
-- -   ListAvailableSolutionStacks
module Amazonka.ElasticBeanstalk.CreateConfigurationTemplate
  ( -- * Creating a Request
    CreateConfigurationTemplate (..),
    newCreateConfigurationTemplate,

    -- * Request Lenses
    createConfigurationTemplate_tags,
    createConfigurationTemplate_sourceConfiguration,
    createConfigurationTemplate_description,
    createConfigurationTemplate_solutionStackName,
    createConfigurationTemplate_platformArn,
    createConfigurationTemplate_environmentId,
    createConfigurationTemplate_optionSettings,
    createConfigurationTemplate_applicationName,
    createConfigurationTemplate_templateName,

    -- * Destructuring the Response
    ConfigurationSettingsDescription (..),
    newConfigurationSettingsDescription,

    -- * Response Lenses
    configurationSettingsDescription_deploymentStatus,
    configurationSettingsDescription_templateName,
    configurationSettingsDescription_environmentName,
    configurationSettingsDescription_description,
    configurationSettingsDescription_solutionStackName,
    configurationSettingsDescription_dateUpdated,
    configurationSettingsDescription_dateCreated,
    configurationSettingsDescription_platformArn,
    configurationSettingsDescription_applicationName,
    configurationSettingsDescription_optionSettings,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to create a configuration template.
--
-- /See:/ 'newCreateConfigurationTemplate' smart constructor.
data CreateConfigurationTemplate = CreateConfigurationTemplate'
  { -- | Specifies the tags applied to the configuration template.
    tags :: Prelude.Maybe [Tag],
    -- | An Elastic Beanstalk configuration template to base this one on. If
    -- specified, Elastic Beanstalk uses the configuration values from the
    -- specified configuration template to create a new configuration.
    --
    -- Values specified in @OptionSettings@ override any values obtained from
    -- the @SourceConfiguration@.
    --
    -- You must specify @SourceConfiguration@ if you don\'t specify
    -- @PlatformArn@, @EnvironmentId@, or @SolutionStackName@.
    --
    -- Constraint: If both solution stack name and source configuration are
    -- specified, the solution stack of the source configuration template must
    -- match the specified solution stack name.
    sourceConfiguration :: Prelude.Maybe SourceConfiguration,
    -- | An optional description for this configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of an Elastic Beanstalk solution stack (platform version) that
    -- this configuration uses. For example,
    -- @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@. A solution stack
    -- specifies the operating system, runtime, and application server for a
    -- configuration template. It also determines the set of configuration
    -- options as well as the possible and default values. For more
    -- information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms>
    -- in the /AWS Elastic Beanstalk Developer Guide/.
    --
    -- You must specify @SolutionStackName@ if you don\'t specify
    -- @PlatformArn@, @EnvironmentId@, or @SourceConfiguration@.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html ListAvailableSolutionStacks>
    -- API to obtain a list of available solution stacks.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the custom platform. For more
    -- information, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms>
    -- in the /AWS Elastic Beanstalk Developer Guide/.
    --
    -- If you specify @PlatformArn@, then don\'t specify @SolutionStackName@.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of an environment whose settings you want to use to create the
    -- configuration template. You must specify @EnvironmentId@ if you don\'t
    -- specify @PlatformArn@, @SolutionStackName@, or @SourceConfiguration@.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Option values for the Elastic Beanstalk configuration, such as the
    -- instance type. If specified, these values override the values obtained
    -- from the solution stack or the source configuration template. For a
    -- complete list of Elastic Beanstalk configuration options, see
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values>
    -- in the /AWS Elastic Beanstalk Developer Guide/.
    optionSettings :: Prelude.Maybe [ConfigurationOptionSetting],
    -- | The name of the Elastic Beanstalk application to associate with this
    -- configuration template.
    applicationName :: Prelude.Text,
    -- | The name of the configuration template.
    --
    -- Constraint: This name must be unique per application.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createConfigurationTemplate_tags' - Specifies the tags applied to the configuration template.
--
-- 'sourceConfiguration', 'createConfigurationTemplate_sourceConfiguration' - An Elastic Beanstalk configuration template to base this one on. If
-- specified, Elastic Beanstalk uses the configuration values from the
-- specified configuration template to create a new configuration.
--
-- Values specified in @OptionSettings@ override any values obtained from
-- the @SourceConfiguration@.
--
-- You must specify @SourceConfiguration@ if you don\'t specify
-- @PlatformArn@, @EnvironmentId@, or @SolutionStackName@.
--
-- Constraint: If both solution stack name and source configuration are
-- specified, the solution stack of the source configuration template must
-- match the specified solution stack name.
--
-- 'description', 'createConfigurationTemplate_description' - An optional description for this configuration.
--
-- 'solutionStackName', 'createConfigurationTemplate_solutionStackName' - The name of an Elastic Beanstalk solution stack (platform version) that
-- this configuration uses. For example,
-- @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@. A solution stack
-- specifies the operating system, runtime, and application server for a
-- configuration template. It also determines the set of configuration
-- options as well as the possible and default values. For more
-- information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- You must specify @SolutionStackName@ if you don\'t specify
-- @PlatformArn@, @EnvironmentId@, or @SourceConfiguration@.
--
-- Use the
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html ListAvailableSolutionStacks>
-- API to obtain a list of available solution stacks.
--
-- 'platformArn', 'createConfigurationTemplate_platformArn' - The Amazon Resource Name (ARN) of the custom platform. For more
-- information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- If you specify @PlatformArn@, then don\'t specify @SolutionStackName@.
--
-- 'environmentId', 'createConfigurationTemplate_environmentId' - The ID of an environment whose settings you want to use to create the
-- configuration template. You must specify @EnvironmentId@ if you don\'t
-- specify @PlatformArn@, @SolutionStackName@, or @SourceConfiguration@.
--
-- 'optionSettings', 'createConfigurationTemplate_optionSettings' - Option values for the Elastic Beanstalk configuration, such as the
-- instance type. If specified, these values override the values obtained
-- from the solution stack or the source configuration template. For a
-- complete list of Elastic Beanstalk configuration options, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- 'applicationName', 'createConfigurationTemplate_applicationName' - The name of the Elastic Beanstalk application to associate with this
-- configuration template.
--
-- 'templateName', 'createConfigurationTemplate_templateName' - The name of the configuration template.
--
-- Constraint: This name must be unique per application.
newCreateConfigurationTemplate ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  CreateConfigurationTemplate
newCreateConfigurationTemplate
  pApplicationName_
  pTemplateName_ =
    CreateConfigurationTemplate'
      { tags =
          Prelude.Nothing,
        sourceConfiguration = Prelude.Nothing,
        description = Prelude.Nothing,
        solutionStackName = Prelude.Nothing,
        platformArn = Prelude.Nothing,
        environmentId = Prelude.Nothing,
        optionSettings = Prelude.Nothing,
        applicationName = pApplicationName_,
        templateName = pTemplateName_
      }

-- | Specifies the tags applied to the configuration template.
createConfigurationTemplate_tags :: Lens.Lens' CreateConfigurationTemplate (Prelude.Maybe [Tag])
createConfigurationTemplate_tags = Lens.lens (\CreateConfigurationTemplate' {tags} -> tags) (\s@CreateConfigurationTemplate' {} a -> s {tags = a} :: CreateConfigurationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | An Elastic Beanstalk configuration template to base this one on. If
-- specified, Elastic Beanstalk uses the configuration values from the
-- specified configuration template to create a new configuration.
--
-- Values specified in @OptionSettings@ override any values obtained from
-- the @SourceConfiguration@.
--
-- You must specify @SourceConfiguration@ if you don\'t specify
-- @PlatformArn@, @EnvironmentId@, or @SolutionStackName@.
--
-- Constraint: If both solution stack name and source configuration are
-- specified, the solution stack of the source configuration template must
-- match the specified solution stack name.
createConfigurationTemplate_sourceConfiguration :: Lens.Lens' CreateConfigurationTemplate (Prelude.Maybe SourceConfiguration)
createConfigurationTemplate_sourceConfiguration = Lens.lens (\CreateConfigurationTemplate' {sourceConfiguration} -> sourceConfiguration) (\s@CreateConfigurationTemplate' {} a -> s {sourceConfiguration = a} :: CreateConfigurationTemplate)

-- | An optional description for this configuration.
createConfigurationTemplate_description :: Lens.Lens' CreateConfigurationTemplate (Prelude.Maybe Prelude.Text)
createConfigurationTemplate_description = Lens.lens (\CreateConfigurationTemplate' {description} -> description) (\s@CreateConfigurationTemplate' {} a -> s {description = a} :: CreateConfigurationTemplate)

-- | The name of an Elastic Beanstalk solution stack (platform version) that
-- this configuration uses. For example,
-- @64bit Amazon Linux 2013.09 running Tomcat 7 Java 7@. A solution stack
-- specifies the operating system, runtime, and application server for a
-- configuration template. It also determines the set of configuration
-- options as well as the possible and default values. For more
-- information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/concepts.platforms.html Supported Platforms>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- You must specify @SolutionStackName@ if you don\'t specify
-- @PlatformArn@, @EnvironmentId@, or @SourceConfiguration@.
--
-- Use the
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_ListAvailableSolutionStacks.html ListAvailableSolutionStacks>
-- API to obtain a list of available solution stacks.
createConfigurationTemplate_solutionStackName :: Lens.Lens' CreateConfigurationTemplate (Prelude.Maybe Prelude.Text)
createConfigurationTemplate_solutionStackName = Lens.lens (\CreateConfigurationTemplate' {solutionStackName} -> solutionStackName) (\s@CreateConfigurationTemplate' {} a -> s {solutionStackName = a} :: CreateConfigurationTemplate)

-- | The Amazon Resource Name (ARN) of the custom platform. For more
-- information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/custom-platforms.html Custom Platforms>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- If you specify @PlatformArn@, then don\'t specify @SolutionStackName@.
createConfigurationTemplate_platformArn :: Lens.Lens' CreateConfigurationTemplate (Prelude.Maybe Prelude.Text)
createConfigurationTemplate_platformArn = Lens.lens (\CreateConfigurationTemplate' {platformArn} -> platformArn) (\s@CreateConfigurationTemplate' {} a -> s {platformArn = a} :: CreateConfigurationTemplate)

-- | The ID of an environment whose settings you want to use to create the
-- configuration template. You must specify @EnvironmentId@ if you don\'t
-- specify @PlatformArn@, @SolutionStackName@, or @SourceConfiguration@.
createConfigurationTemplate_environmentId :: Lens.Lens' CreateConfigurationTemplate (Prelude.Maybe Prelude.Text)
createConfigurationTemplate_environmentId = Lens.lens (\CreateConfigurationTemplate' {environmentId} -> environmentId) (\s@CreateConfigurationTemplate' {} a -> s {environmentId = a} :: CreateConfigurationTemplate)

-- | Option values for the Elastic Beanstalk configuration, such as the
-- instance type. If specified, these values override the values obtained
-- from the solution stack or the source configuration template. For a
-- complete list of Elastic Beanstalk configuration options, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values>
-- in the /AWS Elastic Beanstalk Developer Guide/.
createConfigurationTemplate_optionSettings :: Lens.Lens' CreateConfigurationTemplate (Prelude.Maybe [ConfigurationOptionSetting])
createConfigurationTemplate_optionSettings = Lens.lens (\CreateConfigurationTemplate' {optionSettings} -> optionSettings) (\s@CreateConfigurationTemplate' {} a -> s {optionSettings = a} :: CreateConfigurationTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Elastic Beanstalk application to associate with this
-- configuration template.
createConfigurationTemplate_applicationName :: Lens.Lens' CreateConfigurationTemplate Prelude.Text
createConfigurationTemplate_applicationName = Lens.lens (\CreateConfigurationTemplate' {applicationName} -> applicationName) (\s@CreateConfigurationTemplate' {} a -> s {applicationName = a} :: CreateConfigurationTemplate)

-- | The name of the configuration template.
--
-- Constraint: This name must be unique per application.
createConfigurationTemplate_templateName :: Lens.Lens' CreateConfigurationTemplate Prelude.Text
createConfigurationTemplate_templateName = Lens.lens (\CreateConfigurationTemplate' {templateName} -> templateName) (\s@CreateConfigurationTemplate' {} a -> s {templateName = a} :: CreateConfigurationTemplate)

instance Core.AWSRequest CreateConfigurationTemplate where
  type
    AWSResponse CreateConfigurationTemplate =
      ConfigurationSettingsDescription
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateConfigurationTemplateResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable CreateConfigurationTemplate where
  hashWithSalt _salt CreateConfigurationTemplate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` solutionStackName
      `Prelude.hashWithSalt` platformArn
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` optionSettings
      `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData CreateConfigurationTemplate where
  rnf CreateConfigurationTemplate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf solutionStackName
      `Prelude.seq` Prelude.rnf platformArn
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf optionSettings
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf templateName

instance Core.ToHeaders CreateConfigurationTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateConfigurationTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateConfigurationTemplate where
  toQuery CreateConfigurationTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateConfigurationTemplate" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "SourceConfiguration" Core.=: sourceConfiguration,
        "Description" Core.=: description,
        "SolutionStackName" Core.=: solutionStackName,
        "PlatformArn" Core.=: platformArn,
        "EnvironmentId" Core.=: environmentId,
        "OptionSettings"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> optionSettings
            ),
        "ApplicationName" Core.=: applicationName,
        "TemplateName" Core.=: templateName
      ]
