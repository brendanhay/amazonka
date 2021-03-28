{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplate
  ( LaunchTemplate (..)
  -- * Smart constructor
  , mkLaunchTemplate
  -- * Lenses
  , ltCreateTime
  , ltCreatedBy
  , ltDefaultVersionNumber
  , ltLatestVersionNumber
  , ltLaunchTemplateId
  , ltLaunchTemplateName
  , ltTags
  ) where

import qualified Network.AWS.EC2.Types.LaunchTemplateName as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch template.
--
-- /See:/ 'mkLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { createTime :: Core.Maybe Core.UTCTime
    -- ^ The time launch template was created.
  , createdBy :: Core.Maybe Core.Text
    -- ^ The principal that created the launch template. 
  , defaultVersionNumber :: Core.Maybe Core.Integer
    -- ^ The version number of the default version of the launch template.
  , latestVersionNumber :: Core.Maybe Core.Integer
    -- ^ The version number of the latest version of the launch template.
  , launchTemplateId :: Core.Maybe Core.Text
    -- ^ The ID of the launch template.
  , launchTemplateName :: Core.Maybe Types.LaunchTemplateName
    -- ^ The name of the launch template.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the launch template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LaunchTemplate' value with any optional fields omitted.
mkLaunchTemplate
    :: LaunchTemplate
mkLaunchTemplate
  = LaunchTemplate'{createTime = Core.Nothing,
                    createdBy = Core.Nothing, defaultVersionNumber = Core.Nothing,
                    latestVersionNumber = Core.Nothing,
                    launchTemplateId = Core.Nothing, launchTemplateName = Core.Nothing,
                    tags = Core.Nothing}

-- | The time launch template was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCreateTime :: Lens.Lens' LaunchTemplate (Core.Maybe Core.UTCTime)
ltCreateTime = Lens.field @"createTime"
{-# INLINEABLE ltCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The principal that created the launch template. 
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCreatedBy :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Text)
ltCreatedBy = Lens.field @"createdBy"
{-# INLINEABLE ltCreatedBy #-}
{-# DEPRECATED createdBy "Use generic-lens or generic-optics with 'createdBy' instead"  #-}

-- | The version number of the default version of the launch template.
--
-- /Note:/ Consider using 'defaultVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDefaultVersionNumber :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Integer)
ltDefaultVersionNumber = Lens.field @"defaultVersionNumber"
{-# INLINEABLE ltDefaultVersionNumber #-}
{-# DEPRECATED defaultVersionNumber "Use generic-lens or generic-optics with 'defaultVersionNumber' instead"  #-}

-- | The version number of the latest version of the launch template.
--
-- /Note:/ Consider using 'latestVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLatestVersionNumber :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Integer)
ltLatestVersionNumber = Lens.field @"latestVersionNumber"
{-# INLINEABLE ltLatestVersionNumber #-}
{-# DEPRECATED latestVersionNumber "Use generic-lens or generic-optics with 'latestVersionNumber' instead"  #-}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchTemplateId :: Lens.Lens' LaunchTemplate (Core.Maybe Core.Text)
ltLaunchTemplateId = Lens.field @"launchTemplateId"
{-# INLINEABLE ltLaunchTemplateId #-}
{-# DEPRECATED launchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead"  #-}

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchTemplateName :: Lens.Lens' LaunchTemplate (Core.Maybe Types.LaunchTemplateName)
ltLaunchTemplateName = Lens.field @"launchTemplateName"
{-# INLINEABLE ltLaunchTemplateName #-}
{-# DEPRECATED launchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead"  #-}

-- | The tags for the launch template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTags :: Lens.Lens' LaunchTemplate (Core.Maybe [Types.Tag])
ltTags = Lens.field @"tags"
{-# INLINEABLE ltTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML LaunchTemplate where
        parseXML x
          = LaunchTemplate' Core.<$>
              (x Core..@? "createTime") Core.<*> x Core..@? "createdBy" Core.<*>
                x Core..@? "defaultVersionNumber"
                Core.<*> x Core..@? "latestVersionNumber"
                Core.<*> x Core..@? "launchTemplateId"
                Core.<*> x Core..@? "launchTemplateName"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
