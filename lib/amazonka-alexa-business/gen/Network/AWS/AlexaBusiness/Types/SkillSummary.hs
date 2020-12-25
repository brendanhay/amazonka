{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillSummary
  ( SkillSummary (..),

    -- * Smart constructor
    mkSkillSummary,

    -- * Lenses
    ssEnablementType,
    ssSkillId,
    ssSkillName,
    ssSkillType,
    ssSupportsLinking,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.EnablementType as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillId as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillName as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The summary of skills.
--
-- /See:/ 'mkSkillSummary' smart constructor.
data SkillSummary = SkillSummary'
  { -- | Whether the skill is enabled under the user's account, or if it requires linking to be used.
    enablementType :: Core.Maybe Types.EnablementType,
    -- | The ARN of the skill summary.
    skillId :: Core.Maybe Types.SkillId,
    -- | The name of the skill.
    skillName :: Core.Maybe Types.SkillName,
    -- | Whether the skill is publicly available or is a private skill.
    skillType :: Core.Maybe Types.SkillType,
    -- | Linking support for a skill.
    supportsLinking :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SkillSummary' value with any optional fields omitted.
mkSkillSummary ::
  SkillSummary
mkSkillSummary =
  SkillSummary'
    { enablementType = Core.Nothing,
      skillId = Core.Nothing,
      skillName = Core.Nothing,
      skillType = Core.Nothing,
      supportsLinking = Core.Nothing
    }

-- | Whether the skill is enabled under the user's account, or if it requires linking to be used.
--
-- /Note:/ Consider using 'enablementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssEnablementType :: Lens.Lens' SkillSummary (Core.Maybe Types.EnablementType)
ssEnablementType = Lens.field @"enablementType"
{-# DEPRECATED ssEnablementType "Use generic-lens or generic-optics with 'enablementType' instead." #-}

-- | The ARN of the skill summary.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSkillId :: Lens.Lens' SkillSummary (Core.Maybe Types.SkillId)
ssSkillId = Lens.field @"skillId"
{-# DEPRECATED ssSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

-- | The name of the skill.
--
-- /Note:/ Consider using 'skillName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSkillName :: Lens.Lens' SkillSummary (Core.Maybe Types.SkillName)
ssSkillName = Lens.field @"skillName"
{-# DEPRECATED ssSkillName "Use generic-lens or generic-optics with 'skillName' instead." #-}

-- | Whether the skill is publicly available or is a private skill.
--
-- /Note:/ Consider using 'skillType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSkillType :: Lens.Lens' SkillSummary (Core.Maybe Types.SkillType)
ssSkillType = Lens.field @"skillType"
{-# DEPRECATED ssSkillType "Use generic-lens or generic-optics with 'skillType' instead." #-}

-- | Linking support for a skill.
--
-- /Note:/ Consider using 'supportsLinking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSupportsLinking :: Lens.Lens' SkillSummary (Core.Maybe Core.Bool)
ssSupportsLinking = Lens.field @"supportsLinking"
{-# DEPRECATED ssSupportsLinking "Use generic-lens or generic-optics with 'supportsLinking' instead." #-}

instance Core.FromJSON SkillSummary where
  parseJSON =
    Core.withObject "SkillSummary" Core.$
      \x ->
        SkillSummary'
          Core.<$> (x Core..:? "EnablementType")
          Core.<*> (x Core..:? "SkillId")
          Core.<*> (x Core..:? "SkillName")
          Core.<*> (x Core..:? "SkillType")
          Core.<*> (x Core..:? "SupportsLinking")
