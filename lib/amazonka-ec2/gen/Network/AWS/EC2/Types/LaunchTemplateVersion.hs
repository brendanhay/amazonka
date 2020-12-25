{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateVersion
  ( LaunchTemplateVersion (..),

    -- * Smart constructor
    mkLaunchTemplateVersion,

    -- * Lenses
    ltvCreateTime,
    ltvCreatedBy,
    ltvDefaultVersion,
    ltvLaunchTemplateData,
    ltvLaunchTemplateId,
    ltvLaunchTemplateName,
    ltvVersionDescription,
    ltvVersionNumber,
  )
where

import qualified Network.AWS.EC2.Types.LaunchTemplateName as Types
import qualified Network.AWS.EC2.Types.ResponseLaunchTemplateData as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.VersionDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template version.
--
-- /See:/ 'mkLaunchTemplateVersion' smart constructor.
data LaunchTemplateVersion = LaunchTemplateVersion'
  { -- | The time the version was created.
    createTime :: Core.Maybe Core.UTCTime,
    -- | The principal that created the version.
    createdBy :: Core.Maybe Types.String,
    -- | Indicates whether the version is the default version.
    defaultVersion :: Core.Maybe Core.Bool,
    -- | Information about the launch template.
    launchTemplateData :: Core.Maybe Types.ResponseLaunchTemplateData,
    -- | The ID of the launch template.
    launchTemplateId :: Core.Maybe Types.String,
    -- | The name of the launch template.
    launchTemplateName :: Core.Maybe Types.LaunchTemplateName,
    -- | The description for the version.
    versionDescription :: Core.Maybe Types.VersionDescription,
    -- | The version number.
    versionNumber :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LaunchTemplateVersion' value with any optional fields omitted.
mkLaunchTemplateVersion ::
  LaunchTemplateVersion
mkLaunchTemplateVersion =
  LaunchTemplateVersion'
    { createTime = Core.Nothing,
      createdBy = Core.Nothing,
      defaultVersion = Core.Nothing,
      launchTemplateData = Core.Nothing,
      launchTemplateId = Core.Nothing,
      launchTemplateName = Core.Nothing,
      versionDescription = Core.Nothing,
      versionNumber = Core.Nothing
    }

-- | The time the version was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvCreateTime :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.UTCTime)
ltvCreateTime = Lens.field @"createTime"
{-# DEPRECATED ltvCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The principal that created the version.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvCreatedBy :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Types.String)
ltvCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED ltvCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | Indicates whether the version is the default version.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvDefaultVersion :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Bool)
ltvDefaultVersion = Lens.field @"defaultVersion"
{-# DEPRECATED ltvDefaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead." #-}

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvLaunchTemplateData :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Types.ResponseLaunchTemplateData)
ltvLaunchTemplateData = Lens.field @"launchTemplateData"
{-# DEPRECATED ltvLaunchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead." #-}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvLaunchTemplateId :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Types.String)
ltvLaunchTemplateId = Lens.field @"launchTemplateId"
{-# DEPRECATED ltvLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvLaunchTemplateName :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Types.LaunchTemplateName)
ltvLaunchTemplateName = Lens.field @"launchTemplateName"
{-# DEPRECATED ltvLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The description for the version.
--
-- /Note:/ Consider using 'versionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvVersionDescription :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Types.VersionDescription)
ltvVersionDescription = Lens.field @"versionDescription"
{-# DEPRECATED ltvVersionDescription "Use generic-lens or generic-optics with 'versionDescription' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvVersionNumber :: Lens.Lens' LaunchTemplateVersion (Core.Maybe Core.Integer)
ltvVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED ltvVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Core.FromXML LaunchTemplateVersion where
  parseXML x =
    LaunchTemplateVersion'
      Core.<$> (x Core..@? "createTime")
      Core.<*> (x Core..@? "createdBy")
      Core.<*> (x Core..@? "defaultVersion")
      Core.<*> (x Core..@? "launchTemplateData")
      Core.<*> (x Core..@? "launchTemplateId")
      Core.<*> (x Core..@? "launchTemplateName")
      Core.<*> (x Core..@? "versionDescription")
      Core.<*> (x Core..@? "versionNumber")
