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
    updateEnvironment_groupName,
    updateEnvironment_solutionStackName,
    updateEnvironment_environmentId,
    updateEnvironment_optionsToRemove,
    updateEnvironment_environmentName,
    updateEnvironment_platformArn,
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
    environmentDescription_solutionStackName,
    environmentDescription_environmentId,
    environmentDescription_environmentName,
    environmentDescription_platformArn,
    environmentDescription_versionLabel,
    environmentDescription_health,
    environmentDescription_cname,
    environmentDescription_resources,
    environmentDescription_dateUpdated,
    environmentDescription_description,
    environmentDescription_healthStatus,
    environmentDescription_environmentArn,
    environmentDescription_endpointURL,
    environmentDescription_applicationName,
    environmentDescription_tier,
    environmentDescription_operationsRole,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
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
    templateName :: Core.Maybe Core.Text,
    -- | The name of the group to which the target environment belongs. Specify a
    -- group name only if the environment\'s name is specified in an
    -- environment manifest and not with the environment name or environment ID
    -- parameters. See
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
    -- for details.
    groupName :: Core.Maybe Core.Text,
    -- | This specifies the platform version that the environment will run after
    -- the environment is updated.
    solutionStackName :: Core.Maybe Core.Text,
    -- | The ID of the environment to update.
    --
    -- If no environment with this ID exists, AWS Elastic Beanstalk returns an
    -- @InvalidParameterValue@ error.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Core.Maybe Core.Text,
    -- | A list of custom user-defined configuration options to remove from the
    -- configuration set for this environment.
    optionsToRemove :: Core.Maybe [OptionSpecification],
    -- | The name of the environment to update. If no environment with this name
    -- exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If
    -- you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentName :: Core.Maybe Core.Text,
    -- | The ARN of the platform, if used.
    platformArn :: Core.Maybe Core.Text,
    -- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
    -- application version to the environment. If no such application version
    -- is found, returns an @InvalidParameterValue@ error.
    versionLabel :: Core.Maybe Core.Text,
    -- | If specified, AWS Elastic Beanstalk updates the configuration set
    -- associated with the running environment and sets the specified
    -- configuration options to the requested value.
    optionSettings :: Core.Maybe [ConfigurationOptionSetting],
    -- | If this parameter is specified, AWS Elastic Beanstalk updates the
    -- description of this environment.
    description :: Core.Maybe Core.Text,
    -- | The name of the application with which the environment is associated.
    applicationName :: Core.Maybe Core.Text,
    -- | This specifies the tier to use to update the environment.
    --
    -- Condition: At this time, if you change the tier version, name, or type,
    -- AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
    tier :: Core.Maybe EnvironmentTier
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'groupName', 'updateEnvironment_groupName' - The name of the group to which the target environment belongs. Specify a
-- group name only if the environment\'s name is specified in an
-- environment manifest and not with the environment name or environment ID
-- parameters. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
--
-- 'solutionStackName', 'updateEnvironment_solutionStackName' - This specifies the platform version that the environment will run after
-- the environment is updated.
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
-- 'optionsToRemove', 'updateEnvironment_optionsToRemove' - A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
--
-- 'environmentName', 'updateEnvironment_environmentName' - The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'platformArn', 'updateEnvironment_platformArn' - The ARN of the platform, if used.
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
    { templateName = Core.Nothing,
      groupName = Core.Nothing,
      solutionStackName = Core.Nothing,
      environmentId = Core.Nothing,
      optionsToRemove = Core.Nothing,
      environmentName = Core.Nothing,
      platformArn = Core.Nothing,
      versionLabel = Core.Nothing,
      optionSettings = Core.Nothing,
      description = Core.Nothing,
      applicationName = Core.Nothing,
      tier = Core.Nothing
    }

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this
-- configuration template to the environment. If no such configuration
-- template is found, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
updateEnvironment_templateName :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_templateName = Lens.lens (\UpdateEnvironment' {templateName} -> templateName) (\s@UpdateEnvironment' {} a -> s {templateName = a} :: UpdateEnvironment)

-- | The name of the group to which the target environment belongs. Specify a
-- group name only if the environment\'s name is specified in an
-- environment manifest and not with the environment name or environment ID
-- parameters. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
updateEnvironment_groupName :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_groupName = Lens.lens (\UpdateEnvironment' {groupName} -> groupName) (\s@UpdateEnvironment' {} a -> s {groupName = a} :: UpdateEnvironment)

-- | This specifies the platform version that the environment will run after
-- the environment is updated.
updateEnvironment_solutionStackName :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_solutionStackName = Lens.lens (\UpdateEnvironment' {solutionStackName} -> solutionStackName) (\s@UpdateEnvironment' {} a -> s {solutionStackName = a} :: UpdateEnvironment)

-- | The ID of the environment to update.
--
-- If no environment with this ID exists, AWS Elastic Beanstalk returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
updateEnvironment_environmentId :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_environmentId = Lens.lens (\UpdateEnvironment' {environmentId} -> environmentId) (\s@UpdateEnvironment' {} a -> s {environmentId = a} :: UpdateEnvironment)

-- | A list of custom user-defined configuration options to remove from the
-- configuration set for this environment.
updateEnvironment_optionsToRemove :: Lens.Lens' UpdateEnvironment (Core.Maybe [OptionSpecification])
updateEnvironment_optionsToRemove = Lens.lens (\UpdateEnvironment' {optionsToRemove} -> optionsToRemove) (\s@UpdateEnvironment' {} a -> s {optionsToRemove = a} :: UpdateEnvironment) Core.. Lens.mapping Lens._Coerce

-- | The name of the environment to update. If no environment with this name
-- exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
updateEnvironment_environmentName :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_environmentName = Lens.lens (\UpdateEnvironment' {environmentName} -> environmentName) (\s@UpdateEnvironment' {} a -> s {environmentName = a} :: UpdateEnvironment)

-- | The ARN of the platform, if used.
updateEnvironment_platformArn :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_platformArn = Lens.lens (\UpdateEnvironment' {platformArn} -> platformArn) (\s@UpdateEnvironment' {} a -> s {platformArn = a} :: UpdateEnvironment)

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named
-- application version to the environment. If no such application version
-- is found, returns an @InvalidParameterValue@ error.
updateEnvironment_versionLabel :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_versionLabel = Lens.lens (\UpdateEnvironment' {versionLabel} -> versionLabel) (\s@UpdateEnvironment' {} a -> s {versionLabel = a} :: UpdateEnvironment)

-- | If specified, AWS Elastic Beanstalk updates the configuration set
-- associated with the running environment and sets the specified
-- configuration options to the requested value.
updateEnvironment_optionSettings :: Lens.Lens' UpdateEnvironment (Core.Maybe [ConfigurationOptionSetting])
updateEnvironment_optionSettings = Lens.lens (\UpdateEnvironment' {optionSettings} -> optionSettings) (\s@UpdateEnvironment' {} a -> s {optionSettings = a} :: UpdateEnvironment) Core.. Lens.mapping Lens._Coerce

-- | If this parameter is specified, AWS Elastic Beanstalk updates the
-- description of this environment.
updateEnvironment_description :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_description = Lens.lens (\UpdateEnvironment' {description} -> description) (\s@UpdateEnvironment' {} a -> s {description = a} :: UpdateEnvironment)

-- | The name of the application with which the environment is associated.
updateEnvironment_applicationName :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_applicationName = Lens.lens (\UpdateEnvironment' {applicationName} -> applicationName) (\s@UpdateEnvironment' {} a -> s {applicationName = a} :: UpdateEnvironment)

-- | This specifies the tier to use to update the environment.
--
-- Condition: At this time, if you change the tier version, name, or type,
-- AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
updateEnvironment_tier :: Lens.Lens' UpdateEnvironment (Core.Maybe EnvironmentTier)
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

instance Core.Hashable UpdateEnvironment

instance Core.NFData UpdateEnvironment

instance Core.ToHeaders UpdateEnvironment where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateEnvironment where
  toPath = Core.const "/"

instance Core.ToQuery UpdateEnvironment where
  toQuery UpdateEnvironment' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateEnvironment" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "TemplateName" Core.=: templateName,
        "GroupName" Core.=: groupName,
        "SolutionStackName" Core.=: solutionStackName,
        "EnvironmentId" Core.=: environmentId,
        "OptionsToRemove"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> optionsToRemove),
        "EnvironmentName" Core.=: environmentName,
        "PlatformArn" Core.=: platformArn,
        "VersionLabel" Core.=: versionLabel,
        "OptionSettings"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> optionSettings),
        "Description" Core.=: description,
        "ApplicationName" Core.=: applicationName,
        "Tier" Core.=: tier
      ]
