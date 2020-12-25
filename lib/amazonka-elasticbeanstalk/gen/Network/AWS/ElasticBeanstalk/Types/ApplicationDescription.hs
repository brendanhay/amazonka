{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
  ( ApplicationDescription (..),

    -- * Smart constructor
    mkApplicationDescription,

    -- * Lenses
    adApplicationArn,
    adApplicationName,
    adConfigurationTemplates,
    adDateCreated,
    adDateUpdated,
    adDescription,
    adResourceLifecycleConfig,
    adVersions,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationArn as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ConfigurationTemplateName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Description as Types
import qualified Network.AWS.ElasticBeanstalk.Types.VersionLabel as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the properties of an application.
--
-- /See:/ 'mkApplicationDescription' smart constructor.
data ApplicationDescription = ApplicationDescription'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationArn :: Core.Maybe Types.ApplicationArn,
    -- | The name of the application.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | The names of the configuration templates associated with this application.
    configurationTemplates :: Core.Maybe [Types.ConfigurationTemplateName],
    -- | The date when the application was created.
    dateCreated :: Core.Maybe Core.UTCTime,
    -- | The date when the application was last modified.
    dateUpdated :: Core.Maybe Core.UTCTime,
    -- | User-defined description of the application.
    description :: Core.Maybe Types.Description,
    -- | The lifecycle settings for the application.
    resourceLifecycleConfig :: Core.Maybe Types.ApplicationResourceLifecycleConfig,
    -- | The names of the versions for this application.
    versions :: Core.Maybe [Types.VersionLabel]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ApplicationDescription' value with any optional fields omitted.
mkApplicationDescription ::
  ApplicationDescription
mkApplicationDescription =
  ApplicationDescription'
    { applicationArn = Core.Nothing,
      applicationName = Core.Nothing,
      configurationTemplates = Core.Nothing,
      dateCreated = Core.Nothing,
      dateUpdated = Core.Nothing,
      description = Core.Nothing,
      resourceLifecycleConfig = Core.Nothing,
      versions = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationArn :: Lens.Lens' ApplicationDescription (Core.Maybe Types.ApplicationArn)
adApplicationArn = Lens.field @"applicationArn"
{-# DEPRECATED adApplicationArn "Use generic-lens or generic-optics with 'applicationArn' instead." #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationName :: Lens.Lens' ApplicationDescription (Core.Maybe Types.ApplicationName)
adApplicationName = Lens.field @"applicationName"
{-# DEPRECATED adApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The names of the configuration templates associated with this application.
--
-- /Note:/ Consider using 'configurationTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adConfigurationTemplates :: Lens.Lens' ApplicationDescription (Core.Maybe [Types.ConfigurationTemplateName])
adConfigurationTemplates = Lens.field @"configurationTemplates"
{-# DEPRECATED adConfigurationTemplates "Use generic-lens or generic-optics with 'configurationTemplates' instead." #-}

-- | The date when the application was created.
--
-- /Note:/ Consider using 'dateCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDateCreated :: Lens.Lens' ApplicationDescription (Core.Maybe Core.UTCTime)
adDateCreated = Lens.field @"dateCreated"
{-# DEPRECATED adDateCreated "Use generic-lens or generic-optics with 'dateCreated' instead." #-}

-- | The date when the application was last modified.
--
-- /Note:/ Consider using 'dateUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDateUpdated :: Lens.Lens' ApplicationDescription (Core.Maybe Core.UTCTime)
adDateUpdated = Lens.field @"dateUpdated"
{-# DEPRECATED adDateUpdated "Use generic-lens or generic-optics with 'dateUpdated' instead." #-}

-- | User-defined description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDescription :: Lens.Lens' ApplicationDescription (Core.Maybe Types.Description)
adDescription = Lens.field @"description"
{-# DEPRECATED adDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The lifecycle settings for the application.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adResourceLifecycleConfig :: Lens.Lens' ApplicationDescription (Core.Maybe Types.ApplicationResourceLifecycleConfig)
adResourceLifecycleConfig = Lens.field @"resourceLifecycleConfig"
{-# DEPRECATED adResourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead." #-}

-- | The names of the versions for this application.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adVersions :: Lens.Lens' ApplicationDescription (Core.Maybe [Types.VersionLabel])
adVersions = Lens.field @"versions"
{-# DEPRECATED adVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

instance Core.FromXML ApplicationDescription where
  parseXML x =
    ApplicationDescription'
      Core.<$> (x Core..@? "ApplicationArn")
      Core.<*> (x Core..@? "ApplicationName")
      Core.<*> ( x Core..@? "ConfigurationTemplates"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "DateCreated")
      Core.<*> (x Core..@? "DateUpdated")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "ResourceLifecycleConfig")
      Core.<*> (x Core..@? "Versions" Core..<@> Core.parseXMLList "member")
