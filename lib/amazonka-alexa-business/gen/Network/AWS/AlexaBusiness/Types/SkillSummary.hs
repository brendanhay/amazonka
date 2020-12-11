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
    ssSkillId,
    ssSupportsLinking,
    ssSkillType,
    ssEnablementType,
    ssSkillName,
  )
where

import Network.AWS.AlexaBusiness.Types.EnablementType
import Network.AWS.AlexaBusiness.Types.SkillType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary of skills.
--
-- /See:/ 'mkSkillSummary' smart constructor.
data SkillSummary = SkillSummary'
  { skillId :: Lude.Maybe Lude.Text,
    supportsLinking :: Lude.Maybe Lude.Bool,
    skillType :: Lude.Maybe SkillType,
    enablementType :: Lude.Maybe EnablementType,
    skillName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SkillSummary' with the minimum fields required to make a request.
--
-- * 'enablementType' - Whether the skill is enabled under the user's account, or if it requires linking to be used.
-- * 'skillId' - The ARN of the skill summary.
-- * 'skillName' - The name of the skill.
-- * 'skillType' - Whether the skill is publicly available or is a private skill.
-- * 'supportsLinking' - Linking support for a skill.
mkSkillSummary ::
  SkillSummary
mkSkillSummary =
  SkillSummary'
    { skillId = Lude.Nothing,
      supportsLinking = Lude.Nothing,
      skillType = Lude.Nothing,
      enablementType = Lude.Nothing,
      skillName = Lude.Nothing
    }

-- | The ARN of the skill summary.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSkillId :: Lens.Lens' SkillSummary (Lude.Maybe Lude.Text)
ssSkillId = Lens.lens (skillId :: SkillSummary -> Lude.Maybe Lude.Text) (\s a -> s {skillId = a} :: SkillSummary)
{-# DEPRECATED ssSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

-- | Linking support for a skill.
--
-- /Note:/ Consider using 'supportsLinking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSupportsLinking :: Lens.Lens' SkillSummary (Lude.Maybe Lude.Bool)
ssSupportsLinking = Lens.lens (supportsLinking :: SkillSummary -> Lude.Maybe Lude.Bool) (\s a -> s {supportsLinking = a} :: SkillSummary)
{-# DEPRECATED ssSupportsLinking "Use generic-lens or generic-optics with 'supportsLinking' instead." #-}

-- | Whether the skill is publicly available or is a private skill.
--
-- /Note:/ Consider using 'skillType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSkillType :: Lens.Lens' SkillSummary (Lude.Maybe SkillType)
ssSkillType = Lens.lens (skillType :: SkillSummary -> Lude.Maybe SkillType) (\s a -> s {skillType = a} :: SkillSummary)
{-# DEPRECATED ssSkillType "Use generic-lens or generic-optics with 'skillType' instead." #-}

-- | Whether the skill is enabled under the user's account, or if it requires linking to be used.
--
-- /Note:/ Consider using 'enablementType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssEnablementType :: Lens.Lens' SkillSummary (Lude.Maybe EnablementType)
ssEnablementType = Lens.lens (enablementType :: SkillSummary -> Lude.Maybe EnablementType) (\s a -> s {enablementType = a} :: SkillSummary)
{-# DEPRECATED ssEnablementType "Use generic-lens or generic-optics with 'enablementType' instead." #-}

-- | The name of the skill.
--
-- /Note:/ Consider using 'skillName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSkillName :: Lens.Lens' SkillSummary (Lude.Maybe Lude.Text)
ssSkillName = Lens.lens (skillName :: SkillSummary -> Lude.Maybe Lude.Text) (\s a -> s {skillName = a} :: SkillSummary)
{-# DEPRECATED ssSkillName "Use generic-lens or generic-optics with 'skillName' instead." #-}

instance Lude.FromJSON SkillSummary where
  parseJSON =
    Lude.withObject
      "SkillSummary"
      ( \x ->
          SkillSummary'
            Lude.<$> (x Lude..:? "SkillId")
            Lude.<*> (x Lude..:? "SupportsLinking")
            Lude.<*> (x Lude..:? "SkillType")
            Lude.<*> (x Lude..:? "EnablementType")
            Lude.<*> (x Lude..:? "SkillName")
      )
