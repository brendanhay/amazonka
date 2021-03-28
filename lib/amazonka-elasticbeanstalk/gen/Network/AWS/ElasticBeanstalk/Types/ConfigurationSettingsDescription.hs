{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
  ( ConfigurationSettingsDescription (..)
  -- * Smart constructor
  , mkConfigurationSettingsDescription
  -- * Lenses
  , csdApplicationName
  , csdDateCreated
  , csdDateUpdated
  , csdDeploymentStatus
  , csdDescription
  , csdEnvironmentName
  , csdOptionSettings
  , csdPlatformArn
  , csdSolutionStackName
  , csdTemplateName
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Description as Types
import qualified Network.AWS.ElasticBeanstalk.Types.EnvironmentName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformArn as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SolutionStackName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.TemplateName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the settings for a configuration set.
--
-- /See:/ 'mkConfigurationSettingsDescription' smart constructor.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription'
  { applicationName :: Core.Maybe Types.ApplicationName
    -- ^ The name of the application associated with this configuration set.
  , dateCreated :: Core.Maybe Core.UTCTime
    -- ^ The date (in UTC time) when this configuration set was created.
  , dateUpdated :: Core.Maybe Core.UTCTime
    -- ^ The date (in UTC time) when this configuration set was last modified.
  , deploymentStatus :: Core.Maybe Types.ConfigurationDeploymentStatus
    -- ^ If this configuration set is associated with an environment, the @DeploymentStatus@ parameter indicates the deployment status of this configuration set: 
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
  , description :: Core.Maybe Types.Description
    -- ^ Describes this configuration set.
  , environmentName :: Core.Maybe Types.EnvironmentName
    -- ^ If not @null@ , the name of the environment for this configuration set. 
  , optionSettings :: Core.Maybe [Types.ConfigurationOptionSetting]
    -- ^ A list of the configuration options and their values in this configuration set.
  , platformArn :: Core.Maybe Types.PlatformArn
    -- ^ The ARN of the platform version.
  , solutionStackName :: Core.Maybe Types.SolutionStackName
    -- ^ The name of the solution stack this configuration set uses.
  , templateName :: Core.Maybe Types.TemplateName
    -- ^ If not @null@ , the name of the configuration template for this configuration set. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ConfigurationSettingsDescription' value with any optional fields omitted.
mkConfigurationSettingsDescription
    :: ConfigurationSettingsDescription
mkConfigurationSettingsDescription
  = ConfigurationSettingsDescription'{applicationName = Core.Nothing,
                                      dateCreated = Core.Nothing, dateUpdated = Core.Nothing,
                                      deploymentStatus = Core.Nothing, description = Core.Nothing,
                                      environmentName = Core.Nothing, optionSettings = Core.Nothing,
                                      platformArn = Core.Nothing, solutionStackName = Core.Nothing,
                                      templateName = Core.Nothing}

-- | The name of the application associated with this configuration set.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdApplicationName :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe Types.ApplicationName)
csdApplicationName = Lens.field @"applicationName"
{-# INLINEABLE csdApplicationName #-}
{-# DEPRECATED applicationName "Use generic-lens or generic-optics with 'applicationName' instead"  #-}

-- | The date (in UTC time) when this configuration set was created.
--
-- /Note:/ Consider using 'dateCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDateCreated :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe Core.UTCTime)
csdDateCreated = Lens.field @"dateCreated"
{-# INLINEABLE csdDateCreated #-}
{-# DEPRECATED dateCreated "Use generic-lens or generic-optics with 'dateCreated' instead"  #-}

-- | The date (in UTC time) when this configuration set was last modified.
--
-- /Note:/ Consider using 'dateUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDateUpdated :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe Core.UTCTime)
csdDateUpdated = Lens.field @"dateUpdated"
{-# INLINEABLE csdDateUpdated #-}
{-# DEPRECATED dateUpdated "Use generic-lens or generic-optics with 'dateUpdated' instead"  #-}

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
csdDeploymentStatus :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe Types.ConfigurationDeploymentStatus)
csdDeploymentStatus = Lens.field @"deploymentStatus"
{-# INLINEABLE csdDeploymentStatus #-}
{-# DEPRECATED deploymentStatus "Use generic-lens or generic-optics with 'deploymentStatus' instead"  #-}

-- | Describes this configuration set.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdDescription :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe Types.Description)
csdDescription = Lens.field @"description"
{-# INLINEABLE csdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | If not @null@ , the name of the environment for this configuration set. 
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdEnvironmentName :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe Types.EnvironmentName)
csdEnvironmentName = Lens.field @"environmentName"
{-# INLINEABLE csdEnvironmentName #-}
{-# DEPRECATED environmentName "Use generic-lens or generic-optics with 'environmentName' instead"  #-}

-- | A list of the configuration options and their values in this configuration set.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdOptionSettings :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe [Types.ConfigurationOptionSetting])
csdOptionSettings = Lens.field @"optionSettings"
{-# INLINEABLE csdOptionSettings #-}
{-# DEPRECATED optionSettings "Use generic-lens or generic-optics with 'optionSettings' instead"  #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdPlatformArn :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe Types.PlatformArn)
csdPlatformArn = Lens.field @"platformArn"
{-# INLINEABLE csdPlatformArn #-}
{-# DEPRECATED platformArn "Use generic-lens or generic-optics with 'platformArn' instead"  #-}

-- | The name of the solution stack this configuration set uses.
--
-- /Note:/ Consider using 'solutionStackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdSolutionStackName :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe Types.SolutionStackName)
csdSolutionStackName = Lens.field @"solutionStackName"
{-# INLINEABLE csdSolutionStackName #-}
{-# DEPRECATED solutionStackName "Use generic-lens or generic-optics with 'solutionStackName' instead"  #-}

-- | If not @null@ , the name of the configuration template for this configuration set. 
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdTemplateName :: Lens.Lens' ConfigurationSettingsDescription (Core.Maybe Types.TemplateName)
csdTemplateName = Lens.field @"templateName"
{-# INLINEABLE csdTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

instance Core.FromXML ConfigurationSettingsDescription where
        parseXML x
          = ConfigurationSettingsDescription' Core.<$>
              (x Core..@? "ApplicationName") Core.<*> x Core..@? "DateCreated"
                Core.<*> x Core..@? "DateUpdated"
                Core.<*> x Core..@? "DeploymentStatus"
                Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "EnvironmentName"
                Core.<*>
                x Core..@? "OptionSettings" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "PlatformArn"
                Core.<*> x Core..@? "SolutionStackName"
                Core.<*> x Core..@? "TemplateName"
