{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillGroupData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillGroupData
  ( SkillGroupData (..),

    -- * Smart constructor
    mkSkillGroupData,

    -- * Lenses
    sgdDescription,
    sgdSkillGroupArn,
    sgdSkillGroupName,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillGroupDescription as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The attributes of a skill group.
--
-- /See:/ 'mkSkillGroupData' smart constructor.
data SkillGroupData = SkillGroupData'
  { -- | The description of a skill group.
    description :: Core.Maybe Types.SkillGroupDescription,
    -- | The skill group ARN of a skill group.
    skillGroupArn :: Core.Maybe Types.Arn,
    -- | The skill group name of a skill group.
    skillGroupName :: Core.Maybe Types.SkillGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SkillGroupData' value with any optional fields omitted.
mkSkillGroupData ::
  SkillGroupData
mkSkillGroupData =
  SkillGroupData'
    { description = Core.Nothing,
      skillGroupArn = Core.Nothing,
      skillGroupName = Core.Nothing
    }

-- | The description of a skill group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgdDescription :: Lens.Lens' SkillGroupData (Core.Maybe Types.SkillGroupDescription)
sgdDescription = Lens.field @"description"
{-# DEPRECATED sgdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The skill group ARN of a skill group.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgdSkillGroupArn :: Lens.Lens' SkillGroupData (Core.Maybe Types.Arn)
sgdSkillGroupArn = Lens.field @"skillGroupArn"
{-# DEPRECATED sgdSkillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead." #-}

-- | The skill group name of a skill group.
--
-- /Note:/ Consider using 'skillGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgdSkillGroupName :: Lens.Lens' SkillGroupData (Core.Maybe Types.SkillGroupName)
sgdSkillGroupName = Lens.field @"skillGroupName"
{-# DEPRECATED sgdSkillGroupName "Use generic-lens or generic-optics with 'skillGroupName' instead." #-}

instance Core.FromJSON SkillGroupData where
  parseJSON =
    Core.withObject "SkillGroupData" Core.$
      \x ->
        SkillGroupData'
          Core.<$> (x Core..:? "Description")
          Core.<*> (x Core..:? "SkillGroupArn")
          Core.<*> (x Core..:? "SkillGroupName")
