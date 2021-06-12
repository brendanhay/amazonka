{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillsStoreSkill where

import Network.AWS.AlexaBusiness.Types.SkillDetails
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The detailed information about an Alexa skill.
--
-- /See:/ 'newSkillsStoreSkill' smart constructor.
data SkillsStoreSkill = SkillsStoreSkill'
  { -- | The URL where the skill icon resides.
    iconUrl :: Core.Maybe Core.Text,
    -- | The ARN of the skill.
    skillId :: Core.Maybe Core.Text,
    -- | Short description about the skill.
    shortDescription :: Core.Maybe Core.Text,
    -- | Linking support for a skill.
    supportsLinking :: Core.Maybe Core.Bool,
    -- | The name of the skill.
    skillName :: Core.Maybe Core.Text,
    -- | Sample utterances that interact with the skill.
    sampleUtterances :: Core.Maybe [Core.Text],
    -- | Information about the skill.
    skillDetails :: Core.Maybe SkillDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SkillsStoreSkill' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iconUrl', 'skillsStoreSkill_iconUrl' - The URL where the skill icon resides.
--
-- 'skillId', 'skillsStoreSkill_skillId' - The ARN of the skill.
--
-- 'shortDescription', 'skillsStoreSkill_shortDescription' - Short description about the skill.
--
-- 'supportsLinking', 'skillsStoreSkill_supportsLinking' - Linking support for a skill.
--
-- 'skillName', 'skillsStoreSkill_skillName' - The name of the skill.
--
-- 'sampleUtterances', 'skillsStoreSkill_sampleUtterances' - Sample utterances that interact with the skill.
--
-- 'skillDetails', 'skillsStoreSkill_skillDetails' - Information about the skill.
newSkillsStoreSkill ::
  SkillsStoreSkill
newSkillsStoreSkill =
  SkillsStoreSkill'
    { iconUrl = Core.Nothing,
      skillId = Core.Nothing,
      shortDescription = Core.Nothing,
      supportsLinking = Core.Nothing,
      skillName = Core.Nothing,
      sampleUtterances = Core.Nothing,
      skillDetails = Core.Nothing
    }

-- | The URL where the skill icon resides.
skillsStoreSkill_iconUrl :: Lens.Lens' SkillsStoreSkill (Core.Maybe Core.Text)
skillsStoreSkill_iconUrl = Lens.lens (\SkillsStoreSkill' {iconUrl} -> iconUrl) (\s@SkillsStoreSkill' {} a -> s {iconUrl = a} :: SkillsStoreSkill)

-- | The ARN of the skill.
skillsStoreSkill_skillId :: Lens.Lens' SkillsStoreSkill (Core.Maybe Core.Text)
skillsStoreSkill_skillId = Lens.lens (\SkillsStoreSkill' {skillId} -> skillId) (\s@SkillsStoreSkill' {} a -> s {skillId = a} :: SkillsStoreSkill)

-- | Short description about the skill.
skillsStoreSkill_shortDescription :: Lens.Lens' SkillsStoreSkill (Core.Maybe Core.Text)
skillsStoreSkill_shortDescription = Lens.lens (\SkillsStoreSkill' {shortDescription} -> shortDescription) (\s@SkillsStoreSkill' {} a -> s {shortDescription = a} :: SkillsStoreSkill)

-- | Linking support for a skill.
skillsStoreSkill_supportsLinking :: Lens.Lens' SkillsStoreSkill (Core.Maybe Core.Bool)
skillsStoreSkill_supportsLinking = Lens.lens (\SkillsStoreSkill' {supportsLinking} -> supportsLinking) (\s@SkillsStoreSkill' {} a -> s {supportsLinking = a} :: SkillsStoreSkill)

-- | The name of the skill.
skillsStoreSkill_skillName :: Lens.Lens' SkillsStoreSkill (Core.Maybe Core.Text)
skillsStoreSkill_skillName = Lens.lens (\SkillsStoreSkill' {skillName} -> skillName) (\s@SkillsStoreSkill' {} a -> s {skillName = a} :: SkillsStoreSkill)

-- | Sample utterances that interact with the skill.
skillsStoreSkill_sampleUtterances :: Lens.Lens' SkillsStoreSkill (Core.Maybe [Core.Text])
skillsStoreSkill_sampleUtterances = Lens.lens (\SkillsStoreSkill' {sampleUtterances} -> sampleUtterances) (\s@SkillsStoreSkill' {} a -> s {sampleUtterances = a} :: SkillsStoreSkill) Core.. Lens.mapping Lens._Coerce

-- | Information about the skill.
skillsStoreSkill_skillDetails :: Lens.Lens' SkillsStoreSkill (Core.Maybe SkillDetails)
skillsStoreSkill_skillDetails = Lens.lens (\SkillsStoreSkill' {skillDetails} -> skillDetails) (\s@SkillsStoreSkill' {} a -> s {skillDetails = a} :: SkillsStoreSkill)

instance Core.FromJSON SkillsStoreSkill where
  parseJSON =
    Core.withObject
      "SkillsStoreSkill"
      ( \x ->
          SkillsStoreSkill'
            Core.<$> (x Core..:? "IconUrl")
            Core.<*> (x Core..:? "SkillId")
            Core.<*> (x Core..:? "ShortDescription")
            Core.<*> (x Core..:? "SupportsLinking")
            Core.<*> (x Core..:? "SkillName")
            Core.<*> (x Core..:? "SampleUtterances" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SkillDetails")
      )

instance Core.Hashable SkillsStoreSkill

instance Core.NFData SkillsStoreSkill
