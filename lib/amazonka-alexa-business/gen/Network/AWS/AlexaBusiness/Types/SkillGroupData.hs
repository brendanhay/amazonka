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
    sgdSkillGroupARN,
    sgdDescription,
    sgdSkillGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The attributes of a skill group.
--
-- /See:/ 'mkSkillGroupData' smart constructor.
data SkillGroupData = SkillGroupData'
  { -- | The skill group ARN of a skill group.
    skillGroupARN :: Lude.Maybe Lude.Text,
    -- | The description of a skill group.
    description :: Lude.Maybe Lude.Text,
    -- | The skill group name of a skill group.
    skillGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SkillGroupData' with the minimum fields required to make a request.
--
-- * 'skillGroupARN' - The skill group ARN of a skill group.
-- * 'description' - The description of a skill group.
-- * 'skillGroupName' - The skill group name of a skill group.
mkSkillGroupData ::
  SkillGroupData
mkSkillGroupData =
  SkillGroupData'
    { skillGroupARN = Lude.Nothing,
      description = Lude.Nothing,
      skillGroupName = Lude.Nothing
    }

-- | The skill group ARN of a skill group.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgdSkillGroupARN :: Lens.Lens' SkillGroupData (Lude.Maybe Lude.Text)
sgdSkillGroupARN = Lens.lens (skillGroupARN :: SkillGroupData -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: SkillGroupData)
{-# DEPRECATED sgdSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

-- | The description of a skill group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgdDescription :: Lens.Lens' SkillGroupData (Lude.Maybe Lude.Text)
sgdDescription = Lens.lens (description :: SkillGroupData -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SkillGroupData)
{-# DEPRECATED sgdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The skill group name of a skill group.
--
-- /Note:/ Consider using 'skillGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgdSkillGroupName :: Lens.Lens' SkillGroupData (Lude.Maybe Lude.Text)
sgdSkillGroupName = Lens.lens (skillGroupName :: SkillGroupData -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupName = a} :: SkillGroupData)
{-# DEPRECATED sgdSkillGroupName "Use generic-lens or generic-optics with 'skillGroupName' instead." #-}

instance Lude.FromJSON SkillGroupData where
  parseJSON =
    Lude.withObject
      "SkillGroupData"
      ( \x ->
          SkillGroupData'
            Lude.<$> (x Lude..:? "SkillGroupArn")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "SkillGroupName")
      )
