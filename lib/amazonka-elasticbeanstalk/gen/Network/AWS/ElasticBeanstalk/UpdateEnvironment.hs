{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ueTemplateName,
    ueOptionsToRemove,
    ueOptionSettings,
    ueVersionLabel,
    uePlatformARN,
    ueTier,
    ueEnvironmentName,
    ueApplicationName,
    ueSolutionStackName,
    ueEnvironmentId,
    ueGroupName,
    ueDescription,

    -- * Destructuring the response
    EnvironmentDescription (..),
    mkEnvironmentDescription,

    -- ** Response lenses
    eStatus,
    eCNAME,
    eTemplateName,
    eAbortableOperationInProgress,
    eEndpointURL,
    eResources,
    eDateUpdated,
    eDateCreated,
    eHealth,
    eVersionLabel,
    eOperationsRole,
    ePlatformARN,
    eTier,
    eEnvironmentName,
    eApplicationName,
    eEnvironmentARN,
    eSolutionStackName,
    eEnvironmentId,
    eHealthStatus,
    eEnvironmentLinks,
    eDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to update an environment.
--
-- /See:/ 'mkUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { templateName ::
      Lude.Maybe Lude.Text,
    optionsToRemove :: Lude.Maybe [OptionSpecification],
    optionSettings ::
      Lude.Maybe [ConfigurationOptionSetting],
    versionLabel :: Lude.Maybe Lude.Text,
    platformARN :: Lude.Maybe Lude.Text,
    tier :: Lude.Maybe EnvironmentTier,
    environmentName :: Lude.Maybe Lude.Text,
    applicationName :: Lude.Maybe Lude.Text,
    solutionStackName :: Lude.Maybe Lude.Text,
    environmentId :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEnvironment' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application with which the environment is associated.
-- * 'description' - If this parameter is specified, AWS Elastic Beanstalk updates the description of this environment.
-- * 'environmentId' - The ID of the environment to update.
--
-- If no environment with this ID exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'environmentName' - The name of the environment to update. If no environment with this name exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
-- * 'groupName' - The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name or environment ID parameters. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
-- * 'optionSettings' - If specified, AWS Elastic Beanstalk updates the configuration set associated with the running environment and sets the specified configuration options to the requested value.
-- * 'optionsToRemove' - A list of custom user-defined configuration options to remove from the configuration set for this environment.
-- * 'platformARN' - The ARN of the platform, if used.
-- * 'solutionStackName' - This specifies the platform version that the environment will run after the environment is updated.
-- * 'templateName' - If this parameter is specified, AWS Elastic Beanstalk deploys this configuration template to the environment. If no such configuration template is found, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
-- * 'tier' - This specifies the tier to use to update the environment.
--
-- Condition: At this time, if you change the tier version, name, or type, AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
-- * 'versionLabel' - If this parameter is specified, AWS Elastic Beanstalk deploys the named application version to the environment. If no such application version is found, returns an @InvalidParameterValue@ error.
mkUpdateEnvironment ::
  UpdateEnvironment
mkUpdateEnvironment =
  UpdateEnvironment'
    { templateName = Lude.Nothing,
      optionsToRemove = Lude.Nothing,
      optionSettings = Lude.Nothing,
      versionLabel = Lude.Nothing,
      platformARN = Lude.Nothing,
      tier = Lude.Nothing,
      environmentName = Lude.Nothing,
      applicationName = Lude.Nothing,
      solutionStackName = Lude.Nothing,
      environmentId = Lude.Nothing,
      groupName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | If this parameter is specified, AWS Elastic Beanstalk deploys this configuration template to the environment. If no such configuration template is found, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueTemplateName :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
ueTemplateName = Lens.lens (templateName :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: UpdateEnvironment)
{-# DEPRECATED ueTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | A list of custom user-defined configuration options to remove from the configuration set for this environment.
--
-- /Note:/ Consider using 'optionsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueOptionsToRemove :: Lens.Lens' UpdateEnvironment (Lude.Maybe [OptionSpecification])
ueOptionsToRemove = Lens.lens (optionsToRemove :: UpdateEnvironment -> Lude.Maybe [OptionSpecification]) (\s a -> s {optionsToRemove = a} :: UpdateEnvironment)
{-# DEPRECATED ueOptionsToRemove "Use generic-lens or generic-optics with 'optionsToRemove' instead." #-}

-- | If specified, AWS Elastic Beanstalk updates the configuration set associated with the running environment and sets the specified configuration options to the requested value.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueOptionSettings :: Lens.Lens' UpdateEnvironment (Lude.Maybe [ConfigurationOptionSetting])
ueOptionSettings = Lens.lens (optionSettings :: UpdateEnvironment -> Lude.Maybe [ConfigurationOptionSetting]) (\s a -> s {optionSettings = a} :: UpdateEnvironment)
{-# DEPRECATED ueOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | If this parameter is specified, AWS Elastic Beanstalk deploys the named application version to the environment. If no such application version is found, returns an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueVersionLabel :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
ueVersionLabel = Lens.lens (versionLabel :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: UpdateEnvironment)
{-# DEPRECATED ueVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | The ARN of the platform, if used.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uePlatformARN :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
uePlatformARN = Lens.lens (platformARN :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: UpdateEnvironment)
{-# DEPRECATED uePlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | This specifies the tier to use to update the environment.
--
-- Condition: At this time, if you change the tier version, name, or type, AWS Elastic Beanstalk returns @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'tier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueTier :: Lens.Lens' UpdateEnvironment (Lude.Maybe EnvironmentTier)
ueTier = Lens.lens (tier :: UpdateEnvironment -> Lude.Maybe EnvironmentTier) (\s a -> s {tier = a} :: UpdateEnvironment)
{-# DEPRECATED ueTier "Use generic-lens or generic-optics with 'tier' instead." #-}

-- | The name of the environment to update. If no environment with this name exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEnvironmentName :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
ueEnvironmentName = Lens.lens (environmentName :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: UpdateEnvironment)
{-# DEPRECATED ueEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the application with which the environment is associated.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueApplicationName :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
ueApplicationName = Lens.lens (applicationName :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: UpdateEnvironment)
{-# DEPRECATED ueApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | This specifies the platform version that the environment will run after the environment is updated.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueSolutionStackName :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
ueSolutionStackName = Lens.lens (solutionStackName :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {solutionStackName = a} :: UpdateEnvironment)
{-# DEPRECATED ueSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | The ID of the environment to update.
--
-- If no environment with this ID exists, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
-- Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEnvironmentId :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
ueEnvironmentId = Lens.lens (environmentId :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: UpdateEnvironment)
{-# DEPRECATED ueEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The name of the group to which the target environment belongs. Specify a group name only if the environment's name is specified in an environment manifest and not with the environment name or environment ID parameters. See <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueGroupName :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
ueGroupName = Lens.lens (groupName :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: UpdateEnvironment)
{-# DEPRECATED ueGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | If this parameter is specified, AWS Elastic Beanstalk updates the description of this environment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueDescription :: Lens.Lens' UpdateEnvironment (Lude.Maybe Lude.Text)
ueDescription = Lens.lens (description :: UpdateEnvironment -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateEnvironment)
{-# DEPRECATED ueDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateEnvironment where
  type Rs UpdateEnvironment = EnvironmentDescription
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "UpdateEnvironmentResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders UpdateEnvironment where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateEnvironment where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateEnvironment where
  toQuery UpdateEnvironment' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateEnvironment" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "TemplateName" Lude.=: templateName,
        "OptionsToRemove"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> optionsToRemove),
        "OptionSettings"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> optionSettings),
        "VersionLabel" Lude.=: versionLabel,
        "PlatformArn" Lude.=: platformARN,
        "Tier" Lude.=: tier,
        "EnvironmentName" Lude.=: environmentName,
        "ApplicationName" Lude.=: applicationName,
        "SolutionStackName" Lude.=: solutionStackName,
        "EnvironmentId" Lude.=: environmentId,
        "GroupName" Lude.=: groupName,
        "Description" Lude.=: description
      ]
