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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the environment description, deploys a new application version,
-- updates the configuration settings to an entirely new configuration
-- template, or updates select configuration option values in the running
-- environment.
--
-- Attempting to update both the release and configuration is not allowed
-- and AWS Elastic Beanstalk returns an @InvalidParameterCombination@
-- error.
--
-- When updating the configuration settings to a new template or individual
-- settings, a draft configuration is created and
-- DescribeConfigurationSettings for this environment returns two setting
-- descriptions with different @DeploymentStatus@ values.
module Network.AWS.ElasticBeanstalk.UpdateEnvironment
  ( -- * Creating a Request
    UpdateEnvironment (..),
    newUpdateEnvironment,

    -- * Request Lenses
    updateEnvironment_templateName,
    updateEnvironment_environmentId,
    updateEnvironment_solutionStackName,
    updateEnvironment_groupName,
    updateEnvironment_optionsToRemove,
    updateEnvironment_platformArn,
    updateEnvironment_environmentName,
    updateEnvironment_versionLabel,
    updateEnvironment_optionSettings,
    updateEnvironment_description,
    updateEnvironment_applicationName,
    updateEnvironment_tier,

    -- * Destructuring the Response
    EnvironmentDescription (..),
    newEnvironmentDescription,

    -- * Response Lenses
    environmentDescription_abortableOperationInProgress,
    environmentDescription_templateName,
    environmentDescription_status,
    environmentDescription_dateCreated,
    environmentDescription_environmentLinks,
    environmentDescription_environmentId,
    environmentDescription_solutionStackName,
    environmentDescription_platformArn,
    environmentDescription_environmentName,
    environmentDescription_health,
    environmentDescription_versionLabel,
    environmentDescription_cname,
    environmentDescription_dateUpdated,
    environmentDescription_resources,
    environmentDescription_description,
    environmentDescription_endpointURL,
    environmentDescription_healthStatus,
    environmentDescription_environmentArn,
    environmentDescription_applicationName,
    environmentDescription_tier,
    environmentDescription_operationsRole,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an environment.
--
-- /See:/ 'newUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | If this parameter is specified, AWS Elastic Beanstalk deploys this
    -- configuration template to the environment. If no such configuration
    -- template is found, AWS Elastic Beanstalk returns an
    -- @InvalidParameterValue@ error.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment to update.
    --
    -- If no environment with this ID exists, AWS Elastic Beanstalk returns an
    -- @InvalidParameterValue@ error.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | This specifies the platform version that the environment will run after
    -- the environment is updated.
    solutionStackName :: Prelude.Maybe Prelude.Text,
    -- | The name of the group to which the target environment belongs. Specify a
    -- group name only if the environment\'s name is specified in an
    -- environment manifest and not with the environment name or environment ID
    -- parameters. See
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
    -- for details.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | A list of custom user-defined configuration options to remove from the
    -- configuration set for this environment.
    optionsToRemove :: Prelude.Maybe [OptionSpecification],
    -- | The ARN of the platform, if used.
    platformArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment to update. If no environment with this name
    -- exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If
    -- you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
    -- application version to the environment. If no such application version
    -- is found, returns an @InvalidParameterValue@ error.
    versionLabel :: Prelude.Maybe Prelude.Text,
    -- | If specified, AWS Elastic Beanstalk updates the configuration set
    -- associated with the running environment and sets the specified
    -- configuration options to the requested value.
    optionSettings :: Prelude.Maybe [ConfigurationOptionSetting],
    -- | If this parameter is specified, AWS Elastic Beanstalk updates the
    -- description of this environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the application with which the environment is associated.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | This specifies the tier to use to update the environment.
    --
    -- Condition: At this time, if you change the tier version, name, or type,
    -- AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
    tier :: Prelude.Maybe EnvironmentTier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'updateEnvironment_templateName' - If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
--
-- 'environmentId', 'updateEnvironment_environmentId' - The ID of the environment to update.
--
-- If no environment with this ID exists, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'solutionStackName', 'updateEnvironment_solutionStackName' - This specifies the platform version that the environment will run after
-- the environment is updated.
--
-- 'groupName', 'updateEnvironment_groupName' - The name of the group to which the target environment belongs. Specify a
-- group name only if the environment\'s name is specified in an
-- environment manifest and not with the environment name or environment ID
-- parameters. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
--
-- 'optionsToRemove', 'updateEnvironment_optionsToRemove' - A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
--
-- 'platformArn', 'updateEnvironment_platformArn' - The ARN of the platform, if used.
--
-- 'environmentName', 'updateEnvironment_environmentName' - The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'versionLabel', 'updateEnvironment_versionLabel' - If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version
-- is found, returns an @InvalidParameterValue@ error.
--
-- 'optionSettings', 'updateEnvironment_optionSettings' - If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
--
-- 'description', 'updateEnvironment_description' - If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
--
-- 'applicationName', 'updateEnvironment_applicationName' - The name of the application with which the environment is associated.
--
-- 'tier', 'updateEnvironment_tier' - This specifies the tier to use to update the environment.
--
-- Condition: At this time, if you change the tier version, name, or type,
-- AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
newUpdateEnvironment ::
  UpdateEnvironment
newUpdateEnvironment =
  UpdateEnvironment'
    { templateName = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      solutionStackName = Prelude.Nothing,
      groupName = Prelude.Nothing,
      optionsToRemove = Prelude.Nothing,
      platformArn = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      versionLabel = Prelude.Nothing,
      optionSettings = Prelude.Nothing,
      description = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      tier = Prelude.Nothing
    }

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
updateEnvironment_templateName :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_templateName = Lens.lens (\UpdateEnvironment' {templateName} -> templateName) (\s@UpdateEnvironment' {} a -> s {templateName = a} :: UpdateEnvironment)

-- | The ID of the environment to update.
--
-- If no environment with this ID exists, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
updateEnvironment_environmentId :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_environmentId = Lens.lens (\UpdateEnvironment' {environmentId} -> environmentId) (\s@UpdateEnvironment' {} a -> s {environmentId = a} :: UpdateEnvironment)

-- | This specifies the platform version that the environment will run after
-- the environment is updated.
updateEnvironment_solutionStackName :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_solutionStackName = Lens.lens (\UpdateEnvironment' {solutionStackName} -> solutionStackName) (\s@UpdateEnvironment' {} a -> s {solutionStackName = a} :: UpdateEnvironment)

-- | The name of the group to which the target environment belongs. Specify a
-- group name only if the environment\'s name is specified in an
-- environment manifest and not with the environment name or environment ID
-- parameters. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
updateEnvironment_groupName :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_groupName = Lens.lens (\UpdateEnvironment' {groupName} -> groupName) (\s@UpdateEnvironment' {} a -> s {groupName = a} :: UpdateEnvironment)

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
updateEnvironment_optionsToRemove :: Lens.Lens' UpdateEnvironment (Prelude.Maybe [OptionSpecification])
updateEnvironment_optionsToRemove = Lens.lens (\UpdateEnvironment' {optionsToRemove} -> optionsToRemove) (\s@UpdateEnvironment' {} a -> s {optionsToRemove = a} :: UpdateEnvironment) Prelude.. Lens.mapping Lens._Coerce

-- | The ARN of the platform, if used.
updateEnvironment_platformArn :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_platformArn = Lens.lens (\UpdateEnvironment' {platformArn} -> platformArn) (\s@UpdateEnvironment' {} a -> s {platformArn = a} :: UpdateEnvironment)

-- | The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
updateEnvironment_environmentName :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_environmentName = Lens.lens (\UpdateEnvironment' {environmentName} -> environmentName) (\s@UpdateEnvironment' {} a -> s {environmentName = a} :: UpdateEnvironment)

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version
-- is found, returns an @InvalidParameterValue@ error.
updateEnvironment_versionLabel :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_versionLabel = Lens.lens (\UpdateEnvironment' {versionLabel} -> versionLabel) (\s@UpdateEnvironment' {} a -> s {versionLabel = a} :: UpdateEnvironment)

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
updateEnvironment_optionSettings :: Lens.Lens' UpdateEnvironment (Prelude.Maybe [ConfigurationOptionSetting])
updateEnvironment_optionSettings = Lens.lens (\UpdateEnvironment' {optionSettings} -> optionSettings) (\s@UpdateEnvironment' {} a -> s {optionSettings = a} :: UpdateEnvironment) Prelude.. Lens.mapping Lens._Coerce

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
updateEnvironment_description :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_description = Lens.lens (\UpdateEnvironment' {description} -> description) (\s@UpdateEnvironment' {} a -> s {description = a} :: UpdateEnvironment)

-- | The name of the application with which the environment is associated.
updateEnvironment_applicationName :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_applicationName = Lens.lens (\UpdateEnvironment' {applicationName} -> applicationName) (\s@UpdateEnvironment' {} a -> s {applicationName = a} :: UpdateEnvironment)

-- | This specifies the tier to use to update the environment.
--
-- Condition: At this time, if you change the tier version, name, or type,
-- AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
updateEnvironment_tier :: Lens.Lens' UpdateEnvironment (Prelude.Maybe EnvironmentTier)
updateEnvironment_tier = Lens.lens (\UpdateEnvironment' {tier} -> tier) (\s@UpdateEnvironment' {} a -> s {tier = a} :: UpdateEnvironment)

instance Core.AWSRequest UpdateEnvironment where
  type
    AWSResponse UpdateEnvironment =
      EnvironmentDescription
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateEnvironmentResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable UpdateEnvironment

instance Prelude.NFData UpdateEnvironment

instance Core.ToHeaders UpdateEnvironment where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UpdateEnvironment where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateEnvironment where
  toQuery UpdateEnvironment' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("UpdateEnvironment" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Core.=: templateName,
        "EnvironmentId" Core.=: environmentId,
        "SolutionStackName" Core.=: solutionStackName,
        "GroupName" Core.=: groupName,
        "OptionsToRemove"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> optionsToRemove
            ),
        "PlatformArn" Core.=: platformArn,
        "EnvironmentName" Core.=: environmentName,
        "VersionLabel" Core.=: versionLabel,
        "OptionSettings"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> optionSettings
            ),
        "Description" Core.=: description,
        "ApplicationName" Core.=: applicationName,
        "Tier" Core.=: tier
      ]
