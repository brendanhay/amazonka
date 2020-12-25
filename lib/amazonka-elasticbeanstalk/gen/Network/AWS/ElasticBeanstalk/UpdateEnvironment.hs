{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.UpdateEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the environment description, deploys a new application version, updates the configuration settings to an entirely new configuration template, or updates select configuration option values in the running environment.
--
-- Attempting to update both the release and configuration is not allowed and AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error.
-- When updating the configuration settings to a new template or individual settings, a draft configuration is created and 'DescribeConfigurationSettings' for this environment returns two setting descriptions with different @DeploymentStatus@ values.
module Network.AWS.ElasticBeanstalk.UpdateEnvironment
  ( -- * Creating a request
    UpdateEnvironment (..),
    mkUpdateEnvironment,

    -- ** Request lenses
    ueApplicationName,
    ueDescription,
    ueEnvironmentId,
    ueEnvironmentName,
    ueGroupName,
    ueOptionSettings,
    ueOptionsToRemove,
    uePlatformArn,
    ueSolutionStackName,
    ueTemplateName,
    ueTier,
    ueVersionLabel,

    -- * Destructuring the response
    Types.EnvironmentDescription (..),
    Types.mkEnvironmentDescription,

    -- ** Response lenses
    Types.eAbortableOperationInProgress,
    Types.eApplicationName,
    Types.eCNAME,
    Types.eDateCreated,
    Types.eDateUpdated,
    Types.eDescription,
    Types.eEndpointURL,
    Types.eEnvironmentArn,
    Types.eEnvironmentId,
    Types.eEnvironmentLinks,
    Types.eEnvironmentName,
    Types.eHealth,
    Types.eHealthStatus,
    Types.eOperationsRole,
    Types.ePlatformArn,
    Types.eResources,
    Types.eSolutionStackName,
    Types.eStatus,
    Types.eTemplateName,
    Types.eTier,
    Types.eVersionLabel,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an environment.
--
-- /See:/ 'mkUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | The name of the application with which the environment is associated.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | If this parameter is specified, AWS Elastic Beanstalk updates the description of this environment.
    description :: Core.Maybe Types.Description,
    -- | The ID of the environment to update.
    --
    -- If no environment with this ID exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
    -- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentId :: Core.Maybe Types.EnvironmentId,
    -- | The name of the environment to update. If no environment with this name exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
    environmentName :: Core.Maybe Types.EnvironmentName,
    -- | The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name or environment ID parameters. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
    groupName :: Core.Maybe Types.GroupName,
    -- | If specified, AWS Elastic Beanstalk updates the configuration set associated with the running environment and sets the specified configuration options to the requested value.
    optionSettings :: Core.Maybe [Types.ConfigurationOptionSetting],
    -- | A list of custom user-defined configuration options to remove from the configuration set for this environment.
    optionsToRemove :: Core.Maybe [Types.OptionSpecification],
    -- | The ARN of the platform, if used.
    platformArn :: Core.Maybe Types.PlatformArn,
    -- | This specifies the platform version that the environment will run after the environment is updated.
    solutionStackName :: Core.Maybe Types.SolutionStackName,
    -- | If this parameter is specified, AWS Elastic Beanstalk deploys this configuration template to the environment. If no such configuration template is found, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
    templateName :: Core.Maybe Types.ConfigurationTemplateName,
    -- | This specifies the tier to use to update the environment.
    --
    -- Condition: At this time, if you change the tier version, name, or type, AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
    tier :: Core.Maybe Types.EnvironmentTier,
    -- | If this parameter is specified, AWS Elastic Beanstalk deploys the named application version to the environment. If no such application version is found, returns an @InvalidParameterValue@ error.
    versionLabel :: Core.Maybe Types.VersionLabel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEnvironment' value with any optional fields omitted.
mkUpdateEnvironment ::
  UpdateEnvironment
mkUpdateEnvironment =
  UpdateEnvironment'
    { applicationName = Core.Nothing,
      description = Core.Nothing,
      environmentId = Core.Nothing,
      environmentName = Core.Nothing,
      groupName = Core.Nothing,
      optionSettings = Core.Nothing,
      optionsToRemove = Core.Nothing,
      platformArn = Core.Nothing,
      solutionStackName = Core.Nothing,
      templateName = Core.Nothing,
      tier = Core.Nothing,
      versionLabel = Core.Nothing
    }

-- | The name of the application with which the environment is associated.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueApplicationName :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.ApplicationName)
ueApplicationName = Lens.field @"applicationName"
{-# DEPRECATED ueApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | If this parameter is specified, AWS Elastic Beanstalk updates the description of this environment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDescription :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.Description)
ueDescription = Lens.field @"description"
{-# DEPRECATED ueDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the environment to update.
--
-- If no environment with this ID exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEnvironmentId :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.EnvironmentId)
ueEnvironmentId = Lens.field @"environmentId"
{-# DEPRECATED ueEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The name of the environment to update. If no environment with this name exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEnvironmentName :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.EnvironmentName)
ueEnvironmentName = Lens.field @"environmentName"
{-# DEPRECATED ueEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name or environment ID parameters. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueGroupName :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.GroupName)
ueGroupName = Lens.field @"groupName"
{-# DEPRECATED ueGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | If specified, AWS Elastic Beanstalk updates the configuration set associated with the running environment and sets the specified configuration options to the requested value.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueOptionSettings :: Lens.Lens' UpdateEnvironment (Core.Maybe [Types.ConfigurationOptionSetting])
ueOptionSettings = Lens.field @"optionSettings"
{-# DEPRECATED ueOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | A list of custom user-defined configuration options to remove from the configuration set for this environment.
--
-- /Note:/ Consider using 'optionsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueOptionsToRemove :: Lens.Lens' UpdateEnvironment (Core.Maybe [Types.OptionSpecification])
ueOptionsToRemove = Lens.field @"optionsToRemove"
{-# DEPRECATED ueOptionsToRemove "Use generic-lens or generic-optics with 'optionsToRemove' instead." #-}

-- | The ARN of the platform, if used.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uePlatformArn :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.PlatformArn)
uePlatformArn = Lens.field @"platformArn"
{-# DEPRECATED uePlatformArn "Use generic-lens or generic-optics with 'platformArn' instead." #-}

-- | This specifies the platform version that the environment will run after the environment is updated.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueSolutionStackName :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.SolutionStackName)
ueSolutionStackName = Lens.field @"solutionStackName"
{-# DEPRECATED ueSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this configuration template to the environment. If no such configuration template is found, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueTemplateName :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.ConfigurationTemplateName)
ueTemplateName = Lens.field @"templateName"
{-# DEPRECATED ueTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | This specifies the tier to use to update the environment.
--
-- Condition: At this time, if you change the tier version, name, or type, AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueTier :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.EnvironmentTier)
ueTier = Lens.field @"tier"
{-# DEPRECATED ueTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named application version to the environment. If no such application version is found, returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueVersionLabel :: Lens.Lens' UpdateEnvironment (Core.Maybe Types.VersionLabel)
ueVersionLabel = Lens.field @"versionLabel"
{-# DEPRECATED ueVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

instance Core.AWSRequest UpdateEnvironment where
  type Rs UpdateEnvironment = Types.EnvironmentDescription
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UpdateEnvironment")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ApplicationName" Core.<$> applicationName)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "EnvironmentId" Core.<$> environmentId)
                Core.<> (Core.toQueryValue "EnvironmentName" Core.<$> environmentName)
                Core.<> (Core.toQueryValue "GroupName" Core.<$> groupName)
                Core.<> ( Core.toQueryValue
                            "OptionSettings"
                            (Core.toQueryList "member" Core.<$> optionSettings)
                        )
                Core.<> ( Core.toQueryValue
                            "OptionsToRemove"
                            (Core.toQueryList "member" Core.<$> optionsToRemove)
                        )
                Core.<> (Core.toQueryValue "PlatformArn" Core.<$> platformArn)
                Core.<> (Core.toQueryValue "SolutionStackName" Core.<$> solutionStackName)
                Core.<> (Core.toQueryValue "TemplateName" Core.<$> templateName)
                Core.<> (Core.toQueryValue "Tier" Core.<$> tier)
                Core.<> (Core.toQueryValue "VersionLabel" Core.<$> versionLabel)
            )
      }
  response =
    Response.receiveXMLWrapper
      "UpdateEnvironmentResult"
      (\s h x -> Core.parseXML x)
