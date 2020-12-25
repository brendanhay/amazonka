{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillGroup
  ( SkillGroup (..),

    -- * Smart constructor
    mkSkillGroup,

    -- * Lenses
    sgDescription,
    sgSkillGroupArn,
    sgSkillGroupName,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillGroupDescription as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A skill group with attributes.
--
-- /See:/ 'mkSkillGroup' smart constructor.
data SkillGroup = SkillGroup'
  { -- | The description of a skill group.
    description :: Core.Maybe Types.SkillGroupDescription,
    -- | The ARN of a skill group.
    skillGroupArn :: Core.Maybe Types.Arn,
    -- | The name of a skill group.
    skillGroupName :: Core.Maybe Types.SkillGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SkillGroup' value with any optional fields omitted.
mkSkillGroup ::
  SkillGroup
mkSkillGroup =
  SkillGroup'
    { description = Core.Nothing,
      skillGroupArn = Core.Nothing,
      skillGroupName = Core.Nothing
    }

-- | The description of a skill group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDescription :: Lens.Lens' SkillGroup (Core.Maybe Types.SkillGroupDescription)
sgDescription = Lens.field @"description"
{-# DEPRECATED sgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of a skill group.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSkillGroupArn :: Lens.Lens' SkillGroup (Core.Maybe Types.Arn)
sgSkillGroupArn = Lens.field @"skillGroupArn"
{-# DEPRECATED sgSkillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead." #-}

-- | The name of a skill group.
--
-- /Note:/ Consider using 'skillGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSkillGroupName :: Lens.Lens' SkillGroup (Core.Maybe Types.SkillGroupName)
sgSkillGroupName = Lens.field @"skillGroupName"
{-# DEPRECATED sgSkillGroupName "Use generic-lens or generic-optics with 'skillGroupName' instead." #-}

instance Core.FromJSON SkillGroup where
  parseJSON =
    Core.withObject "SkillGroup" Core.$
      \x ->
        SkillGroup'
          Core.<$> (x Core..:? "Description")
          Core.<*> (x Core..:? "SkillGroupArn")
          Core.<*> (x Core..:? "SkillGroupName")
