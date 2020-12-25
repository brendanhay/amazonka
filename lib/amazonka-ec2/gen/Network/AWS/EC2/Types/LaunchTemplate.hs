{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplate
  ( LaunchTemplate (..),

    -- * Smart constructor
    mkLaunchTemplate,

    -- * Lenses
    ltCreateTime,
    ltCreatedBy,
    ltDefaultVersionNumber,
    ltLatestVersionNumber,
    ltLaunchTemplateId,
    ltLaunchTemplateName,
    ltTags,
  )
where

import qualified Network.AWS.EC2.Types.LaunchTemplateName as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template.
--
-- /See:/ 'mkLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { -- | The time launch template was created.
    createTime :: Core.Maybe Core.UTCTime,
    -- | The principal that created the launch template.
    createdBy :: Core.Maybe Types.String,
    -- | The version number of the default version of the launch template.
    defaultVersionNumber :: Core.Maybe Core.Integer,
    -- | The version number of the latest version of the launch template.
    latestVersionNumber :: Core.Maybe Core.Integer,
    -- | The ID of the launch template.
    launchTemplateId :: Core.Maybe Types.String,
    -- | The name of the launch template.
    launchTemplateName :: Core.Maybe Types.LaunchTemplateName,
    -- | The tags for the launch template.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LaunchTemplate' value with any optional fields omitted.
mkLaunchTemplate ::
  LaunchTemplate
mkLaunchTemplate =
  LaunchTemplate'
    { createTime = Core.Nothing,
      createdBy = Core.Nothing,
      defaultVersionNumber = Core.Nothing,
      latestVersionNumber = Core.Nothing,
      launchTemplateId = Core.Nothing,
      launchTemplateName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The time launch template was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCreateTime :: Lens.Lens' LaunchTemplate (Core.Maybe Core.UTCTime)
ltCreateTime = Lens.field @"createTime"
{-# DEPRECATED ltCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The principal that created the launch template.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCreatedBy :: Lens.Lens' LaunchTemplate (Core.Maybe Types.String)
ltCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED ltCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The version number of the default version of the launch template.
--
-- /Note:/ Consider using 'defaultVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDefaultVersionNumber :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Integer)
ltDefaultVersionNumber = Lens.field @"defaultVersionNumber"
{-# DEPRECATED ltDefaultVersionNumber "Use generic-lens or generic-optics with 'defaultVersionNumber' instead." #-}

-- | The version number of the latest version of the launch template.
--
-- /Note:/ Consider using 'latestVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLatestVersionNumber :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Integer)
ltLatestVersionNumber = Lens.field @"latestVersionNumber"
{-# DEPRECATED ltLatestVersionNumber "Use generic-lens or generic-optics with 'latestVersionNumber' instead." #-}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchTemplateId :: Lens.Lens' LaunchTemplate (Core.Maybe Types.String)
ltLaunchTemplateId = Lens.field @"launchTemplateId"
{-# DEPRECATED ltLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchTemplateName :: Lens.Lens' LaunchTemplate (Core.Maybe Types.LaunchTemplateName)
ltLaunchTemplateName = Lens.field @"launchTemplateName"
{-# DEPRECATED ltLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The tags for the launch template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTags :: Lens.Lens' LaunchTemplate (Core.Maybe [Types.Tag])
ltTags = Lens.field @"tags"
{-# DEPRECATED ltTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML LaunchTemplate where
  parseXML x =
    LaunchTemplate'
      Core.<$> (x Core..@? "createTime")
      Core.<*> (x Core..@? "createdBy")
      Core.<*> (x Core..@? "defaultVersionNumber")
      Core.<*> (x Core..@? "latestVersionNumber")
      Core.<*> (x Core..@? "launchTemplateId")
      Core.<*> (x Core..@? "launchTemplateName")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
