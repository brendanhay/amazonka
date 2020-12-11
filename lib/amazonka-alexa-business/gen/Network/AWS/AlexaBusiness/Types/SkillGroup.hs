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
    sgSkillGroupARN,
    sgDescription,
    sgSkillGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A skill group with attributes.
--
-- /See:/ 'mkSkillGroup' smart constructor.
data SkillGroup = SkillGroup'
  { skillGroupARN ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    skillGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SkillGroup' with the minimum fields required to make a request.
--
-- * 'description' - The description of a skill group.
-- * 'skillGroupARN' - The ARN of a skill group.
-- * 'skillGroupName' - The name of a skill group.
mkSkillGroup ::
  SkillGroup
mkSkillGroup =
  SkillGroup'
    { skillGroupARN = Lude.Nothing,
      description = Lude.Nothing,
      skillGroupName = Lude.Nothing
    }

-- | The ARN of a skill group.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSkillGroupARN :: Lens.Lens' SkillGroup (Lude.Maybe Lude.Text)
sgSkillGroupARN = Lens.lens (skillGroupARN :: SkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: SkillGroup)
{-# DEPRECATED sgSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

-- | The description of a skill group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgDescription :: Lens.Lens' SkillGroup (Lude.Maybe Lude.Text)
sgDescription = Lens.lens (description :: SkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: SkillGroup)
{-# DEPRECATED sgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of a skill group.
--
-- /Note:/ Consider using 'skillGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgSkillGroupName :: Lens.Lens' SkillGroup (Lude.Maybe Lude.Text)
sgSkillGroupName = Lens.lens (skillGroupName :: SkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupName = a} :: SkillGroup)
{-# DEPRECATED sgSkillGroupName "Use generic-lens or generic-optics with 'skillGroupName' instead." #-}

instance Lude.FromJSON SkillGroup where
  parseJSON =
    Lude.withObject
      "SkillGroup"
      ( \x ->
          SkillGroup'
            Lude.<$> (x Lude..:? "SkillGroupArn")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "SkillGroupName")
      )
