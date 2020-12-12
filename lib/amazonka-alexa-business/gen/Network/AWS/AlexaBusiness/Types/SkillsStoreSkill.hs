{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
  ( SkillsStoreSkill (..),

    -- * Smart constructor
    mkSkillsStoreSkill,

    -- * Lenses
    sssSkillId,
    sssSupportsLinking,
    sssSampleUtterances,
    sssShortDescription,
    sssIconURL,
    sssSkillDetails,
    sssSkillName,
  )
where

import Network.AWS.AlexaBusiness.Types.SkillDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The detailed information about an Alexa skill.
--
-- /See:/ 'mkSkillsStoreSkill' smart constructor.
data SkillsStoreSkill = SkillsStoreSkill'
  { skillId ::
      Lude.Maybe Lude.Text,
    supportsLinking :: Lude.Maybe Lude.Bool,
    sampleUtterances :: Lude.Maybe [Lude.Text],
    shortDescription :: Lude.Maybe Lude.Text,
    iconURL :: Lude.Maybe Lude.Text,
    skillDetails :: Lude.Maybe SkillDetails,
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

-- | Creates a value of 'SkillsStoreSkill' with the minimum fields required to make a request.
--
-- * 'iconURL' - The URL where the skill icon resides.
-- * 'sampleUtterances' - Sample utterances that interact with the skill.
-- * 'shortDescription' - Short description about the skill.
-- * 'skillDetails' - Information about the skill.
-- * 'skillId' - The ARN of the skill.
-- * 'skillName' - The name of the skill.
-- * 'supportsLinking' - Linking support for a skill.
mkSkillsStoreSkill ::
  SkillsStoreSkill
mkSkillsStoreSkill =
  SkillsStoreSkill'
    { skillId = Lude.Nothing,
      supportsLinking = Lude.Nothing,
      sampleUtterances = Lude.Nothing,
      shortDescription = Lude.Nothing,
      iconURL = Lude.Nothing,
      skillDetails = Lude.Nothing,
      skillName = Lude.Nothing
    }

-- | The ARN of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSkillId :: Lens.Lens' SkillsStoreSkill (Lude.Maybe Lude.Text)
sssSkillId = Lens.lens (skillId :: SkillsStoreSkill -> Lude.Maybe Lude.Text) (\s a -> s {skillId = a} :: SkillsStoreSkill)
{-# DEPRECATED sssSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

-- | Linking support for a skill.
--
-- /Note:/ Consider using 'supportsLinking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSupportsLinking :: Lens.Lens' SkillsStoreSkill (Lude.Maybe Lude.Bool)
sssSupportsLinking = Lens.lens (supportsLinking :: SkillsStoreSkill -> Lude.Maybe Lude.Bool) (\s a -> s {supportsLinking = a} :: SkillsStoreSkill)
{-# DEPRECATED sssSupportsLinking "Use generic-lens or generic-optics with 'supportsLinking' instead." #-}

-- | Sample utterances that interact with the skill.
--
-- /Note:/ Consider using 'sampleUtterances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSampleUtterances :: Lens.Lens' SkillsStoreSkill (Lude.Maybe [Lude.Text])
sssSampleUtterances = Lens.lens (sampleUtterances :: SkillsStoreSkill -> Lude.Maybe [Lude.Text]) (\s a -> s {sampleUtterances = a} :: SkillsStoreSkill)
{-# DEPRECATED sssSampleUtterances "Use generic-lens or generic-optics with 'sampleUtterances' instead." #-}

-- | Short description about the skill.
--
-- /Note:/ Consider using 'shortDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssShortDescription :: Lens.Lens' SkillsStoreSkill (Lude.Maybe Lude.Text)
sssShortDescription = Lens.lens (shortDescription :: SkillsStoreSkill -> Lude.Maybe Lude.Text) (\s a -> s {shortDescription = a} :: SkillsStoreSkill)
{-# DEPRECATED sssShortDescription "Use generic-lens or generic-optics with 'shortDescription' instead." #-}

-- | The URL where the skill icon resides.
--
-- /Note:/ Consider using 'iconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssIconURL :: Lens.Lens' SkillsStoreSkill (Lude.Maybe Lude.Text)
sssIconURL = Lens.lens (iconURL :: SkillsStoreSkill -> Lude.Maybe Lude.Text) (\s a -> s {iconURL = a} :: SkillsStoreSkill)
{-# DEPRECATED sssIconURL "Use generic-lens or generic-optics with 'iconURL' instead." #-}

-- | Information about the skill.
--
-- /Note:/ Consider using 'skillDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSkillDetails :: Lens.Lens' SkillsStoreSkill (Lude.Maybe SkillDetails)
sssSkillDetails = Lens.lens (skillDetails :: SkillsStoreSkill -> Lude.Maybe SkillDetails) (\s a -> s {skillDetails = a} :: SkillsStoreSkill)
{-# DEPRECATED sssSkillDetails "Use generic-lens or generic-optics with 'skillDetails' instead." #-}

-- | The name of the skill.
--
-- /Note:/ Consider using 'skillName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sssSkillName :: Lens.Lens' SkillsStoreSkill (Lude.Maybe Lude.Text)
sssSkillName = Lens.lens (skillName :: SkillsStoreSkill -> Lude.Maybe Lude.Text) (\s a -> s {skillName = a} :: SkillsStoreSkill)
{-# DEPRECATED sssSkillName "Use generic-lens or generic-optics with 'skillName' instead." #-}

instance Lude.FromJSON SkillsStoreSkill where
  parseJSON =
    Lude.withObject
      "SkillsStoreSkill"
      ( \x ->
          SkillsStoreSkill'
            Lude.<$> (x Lude..:? "SkillId")
            Lude.<*> (x Lude..:? "SupportsLinking")
            Lude.<*> (x Lude..:? "SampleUtterances" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ShortDescription")
            Lude.<*> (x Lude..:? "IconUrl")
            Lude.<*> (x Lude..:? "SkillDetails")
            Lude.<*> (x Lude..:? "SkillName")
      )
