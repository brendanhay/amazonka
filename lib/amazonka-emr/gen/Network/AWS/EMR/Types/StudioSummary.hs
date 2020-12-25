{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StudioSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StudioSummary
  ( StudioSummary (..),

    -- * Smart constructor
    mkStudioSummary,

    -- * Lenses
    ssCreationTime,
    ssDescription,
    ssName,
    ssStudioId,
    ssUrl,
    ssVpcId,
  )
where

import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details for an Amazon EMR Studio, including ID, Name, VPC, and Description. The details do not include subnets, IAM roles, security groups, or tags associated with the Studio.
--
-- /See:/ 'mkStudioSummary' smart constructor.
data StudioSummary = StudioSummary'
  { -- | The time when the Amazon EMR Studio was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The detailed description of the EMR Studio.
    description :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The name of the Amazon EMR Studio.
    name :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The unique access URL of the Amazon EMR Studio.
    url :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The ID of the Virtual Private Cloud (Amazon VPC) associated with the Amazon EMR Studio.
    vpcId :: Core.Maybe Types.XmlStringMaxLen256
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StudioSummary' value with any optional fields omitted.
mkStudioSummary ::
  StudioSummary
mkStudioSummary =
  StudioSummary'
    { creationTime = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing,
      studioId = Core.Nothing,
      url = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The time when the Amazon EMR Studio was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCreationTime :: Lens.Lens' StudioSummary (Core.Maybe Core.NominalDiffTime)
ssCreationTime = Lens.field @"creationTime"
{-# DEPRECATED ssCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The detailed description of the EMR Studio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDescription :: Lens.Lens' StudioSummary (Core.Maybe Types.XmlStringMaxLen256)
ssDescription = Lens.field @"description"
{-# DEPRECATED ssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' StudioSummary (Core.Maybe Types.XmlStringMaxLen256)
ssName = Lens.field @"name"
{-# DEPRECATED ssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStudioId :: Lens.Lens' StudioSummary (Core.Maybe Types.XmlStringMaxLen256)
ssStudioId = Lens.field @"studioId"
{-# DEPRECATED ssStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | The unique access URL of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssUrl :: Lens.Lens' StudioSummary (Core.Maybe Types.XmlStringMaxLen256)
ssUrl = Lens.field @"url"
{-# DEPRECATED ssUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The ID of the Virtual Private Cloud (Amazon VPC) associated with the Amazon EMR Studio.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssVpcId :: Lens.Lens' StudioSummary (Core.Maybe Types.XmlStringMaxLen256)
ssVpcId = Lens.field @"vpcId"
{-# DEPRECATED ssVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromJSON StudioSummary where
  parseJSON =
    Core.withObject "StudioSummary" Core.$
      \x ->
        StudioSummary'
          Core.<$> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "StudioId")
          Core.<*> (x Core..:? "Url")
          Core.<*> (x Core..:? "VpcId")
