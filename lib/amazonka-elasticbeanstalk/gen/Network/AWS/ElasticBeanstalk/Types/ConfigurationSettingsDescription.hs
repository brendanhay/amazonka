{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
  ( ConfigurationSettingsDescription (..),

    -- * Smart constructor
    mkConfigurationSettingsDescription,

    -- * Lenses
    csdTemplateName,
    csdOptionSettings,
    csdDateUpdated,
    csdDateCreated,
    csdPlatformARN,
    csdEnvironmentName,
    csdApplicationName,
    csdDeploymentStatus,
    csdSolutionStackName,
    csdDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the settings for a configuration set.
--
-- /See:/ 'mkConfigurationSettingsDescription' smart constructor.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription'
  { -- | If not @null@ , the name of the configuration template for this configuration set.
    templateName :: Lude.Maybe Lude.Text,
    -- | A list of the configuration options and their values in this configuration set.
    optionSettings :: Lude.Maybe [ConfigurationOptionSetting],
    -- | The date (in UTC time) when this configuration set was last modified.
    dateUpdated :: Lude.Maybe Lude.DateTime,
    -- | The date (in UTC time) when this configuration set was created.
    dateCreated :: Lude.Maybe Lude.DateTime,
    -- | The ARN of the platform version.
    platformARN :: Lude.Maybe Lude.Text,
    -- | If not @null@ , the name of the environment for this configuration set.
    environmentName :: Lude.Maybe Lude.Text,
    -- | The name of the application associated with this configuration set.
    applicationName :: Lude.Maybe Lude.Text,
    -- | If this configuration set is associated with an environment, the @DeploymentStatus@ parameter indicates the deployment status of this configuration set:
    --
    --
    --     * @null@ : This configuration is not associated with a running environment.
    --
    --
    --     * @pending@ : This is a draft configuration that is not deployed to the associated environment but is in the process of deploying.
    --
    --
    --     * @deployed@ : This is the configuration that is currently deployed to the associated running environment.
    --
    --
    --     * @failed@ : This is a draft configuration that failed to successfully deploy.
    deploymentStatus :: Lude.Maybe ConfigurationDeploymentStatus,
    -- | The name of the solution stack this configuration set uses.
    solutionStackName :: Lude.Maybe Lude.Text,
    -- | Describes this configuration set.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationSettingsDescription' with the minimum fields required to make a request.
--
-- * 'templateName' - If not @null@ , the name of the configuration template for this configuration set.
-- * 'optionSettings' - A list of the configuration options and their values in this configuration set.
-- * 'dateUpdated' - The date (in UTC time) when this configuration set was last modified.
-- * 'dateCreated' - The date (in UTC time) when this configuration set was created.
-- * 'platformARN' - The ARN of the platform version.
-- * 'environmentName' - If not @null@ , the name of the environment for this configuration set.
-- * 'applicationName' - The name of the application associated with this configuration set.
-- * 'deploymentStatus' - If this configuration set is associated with an environment, the @DeploymentStatus@ parameter indicates the deployment status of this configuration set:
--
--
--     * @null@ : This configuration is not associated with a running environment.
--
--
--     * @pending@ : This is a draft configuration that is not deployed to the associated environment but is in the process of deploying.
--
--
--     * @deployed@ : This is the configuration that is currently deployed to the associated running environment.
--
--
--     * @failed@ : This is a draft configuration that failed to successfully deploy.
--
--
-- * 'solutionStackName' - The name of the solution stack this configuration set uses.
-- * 'description' - Describes this configuration set.
mkConfigurationSettingsDescription ::
  ConfigurationSettingsDescription
mkConfigurationSettingsDescription =
  ConfigurationSettingsDescription'
    { templateName = Lude.Nothing,
      optionSettings = Lude.Nothing,
      dateUpdated = Lude.Nothing,
      dateCreated = Lude.Nothing,
      platformARN = Lude.Nothing,
      environmentName = Lude.Nothing,
      applicationName = Lude.Nothing,
      deploymentStatus = Lude.Nothing,
      solutionStackName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | If not @null@ , the name of the configuration template for this configuration set.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdTemplateName :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe Lude.Text)
csdTemplateName = Lens.lens (templateName :: ConfigurationSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | A list of the configuration options and their values in this configuration set.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdOptionSettings :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe [ConfigurationOptionSetting])
csdOptionSettings = Lens.lens (optionSettings :: ConfigurationSettingsDescription -> Lude.Maybe [ConfigurationOptionSetting]) (\s a -> s {optionSettings = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | The date (in UTC time) when this configuration set was last modified.
--
-- /Note:/ Consider using 'dateUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDateUpdated :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe Lude.DateTime)
csdDateUpdated = Lens.lens (dateUpdated :: ConfigurationSettingsDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {dateUpdated = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdDateUpdated "Use generic-lens or generic-optics with 'dateUpdated' instead." #-}

-- | The date (in UTC time) when this configuration set was created.
--
-- /Note:/ Consider using 'dateCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDateCreated :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe Lude.DateTime)
csdDateCreated = Lens.lens (dateCreated :: ConfigurationSettingsDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {dateCreated = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdDateCreated "Use generic-lens or generic-optics with 'dateCreated' instead." #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdPlatformARN :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe Lude.Text)
csdPlatformARN = Lens.lens (platformARN :: ConfigurationSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

-- | If not @null@ , the name of the environment for this configuration set.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdEnvironmentName :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe Lude.Text)
csdEnvironmentName = Lens.lens (environmentName :: ConfigurationSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The name of the application associated with this configuration set.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdApplicationName :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe Lude.Text)
csdApplicationName = Lens.lens (applicationName :: ConfigurationSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | If this configuration set is associated with an environment, the @DeploymentStatus@ parameter indicates the deployment status of this configuration set:
--
--
--     * @null@ : This configuration is not associated with a running environment.
--
--
--     * @pending@ : This is a draft configuration that is not deployed to the associated environment but is in the process of deploying.
--
--
--     * @deployed@ : This is the configuration that is currently deployed to the associated running environment.
--
--
--     * @failed@ : This is a draft configuration that failed to successfully deploy.
--
--
--
-- /Note:/ Consider using 'deploymentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDeploymentStatus :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe ConfigurationDeploymentStatus)
csdDeploymentStatus = Lens.lens (deploymentStatus :: ConfigurationSettingsDescription -> Lude.Maybe ConfigurationDeploymentStatus) (\s a -> s {deploymentStatus = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdDeploymentStatus "Use generic-lens or generic-optics with 'deploymentStatus' instead." #-}

-- | The name of the solution stack this configuration set uses.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdSolutionStackName :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe Lude.Text)
csdSolutionStackName = Lens.lens (solutionStackName :: ConfigurationSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {solutionStackName = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdSolutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead." #-}

-- | Describes this configuration set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDescription :: Lens.Lens' ConfigurationSettingsDescription (Lude.Maybe Lude.Text)
csdDescription = Lens.lens (description :: ConfigurationSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ConfigurationSettingsDescription)
{-# DEPRECATED csdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ConfigurationSettingsDescription where
  parseXML x =
    ConfigurationSettingsDescription'
      Lude.<$> (x Lude..@? "TemplateName")
      Lude.<*> ( x Lude..@? "OptionSettings" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "DateUpdated")
      Lude.<*> (x Lude..@? "DateCreated")
      Lude.<*> (x Lude..@? "PlatformArn")
      Lude.<*> (x Lude..@? "EnvironmentName")
      Lude.<*> (x Lude..@? "ApplicationName")
      Lude.<*> (x Lude..@? "DeploymentStatus")
      Lude.<*> (x Lude..@? "SolutionStackName")
      Lude.<*> (x Lude..@? "Description")
