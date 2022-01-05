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
-- Module      : Amazonka.AlexaBusiness.Types.SkillsStoreSkill
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.SkillsStoreSkill where

import Amazonka.AlexaBusiness.Types.SkillDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The detailed information about an Alexa skill.
--
-- /See:/ 'newSkillsStoreSkill' smart constructor.
data SkillsStoreSkill = SkillsStoreSkill'
  { -- | The ARN of the skill.
    skillId :: Prelude.Maybe Prelude.Text,
    -- | Linking support for a skill.
    supportsLinking :: Prelude.Maybe Prelude.Bool,
    -- | Sample utterances that interact with the skill.
    sampleUtterances :: Prelude.Maybe [Prelude.Text],
    -- | Short description about the skill.
    shortDescription :: Prelude.Maybe Prelude.Text,
    -- | The URL where the skill icon resides.
    iconUrl :: Prelude.Maybe Prelude.Text,
    -- | Information about the skill.
    skillDetails :: Prelude.Maybe SkillDetails,
    -- | The name of the skill.
    skillName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SkillsStoreSkill' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillId', 'skillsStoreSkill_skillId' - The ARN of the skill.
--
-- 'supportsLinking', 'skillsStoreSkill_supportsLinking' - Linking support for a skill.
--
-- 'sampleUtterances', 'skillsStoreSkill_sampleUtterances' - Sample utterances that interact with the skill.
--
-- 'shortDescription', 'skillsStoreSkill_shortDescription' - Short description about the skill.
--
-- 'iconUrl', 'skillsStoreSkill_iconUrl' - The URL where the skill icon resides.
--
-- 'skillDetails', 'skillsStoreSkill_skillDetails' - Information about the skill.
--
-- 'skillName', 'skillsStoreSkill_skillName' - The name of the skill.
newSkillsStoreSkill ::
  SkillsStoreSkill
newSkillsStoreSkill =
  SkillsStoreSkill'
    { skillId = Prelude.Nothing,
      supportsLinking = Prelude.Nothing,
      sampleUtterances = Prelude.Nothing,
      shortDescription = Prelude.Nothing,
      iconUrl = Prelude.Nothing,
      skillDetails = Prelude.Nothing,
      skillName = Prelude.Nothing
    }

-- | The ARN of the skill.
skillsStoreSkill_skillId :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_skillId = Lens.lens (\SkillsStoreSkill' {skillId} -> skillId) (\s@SkillsStoreSkill' {} a -> s {skillId = a} :: SkillsStoreSkill)

-- | Linking support for a skill.
skillsStoreSkill_supportsLinking :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Bool)
skillsStoreSkill_supportsLinking = Lens.lens (\SkillsStoreSkill' {supportsLinking} -> supportsLinking) (\s@SkillsStoreSkill' {} a -> s {supportsLinking = a} :: SkillsStoreSkill)

-- | Sample utterances that interact with the skill.
skillsStoreSkill_sampleUtterances :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe [Prelude.Text])
skillsStoreSkill_sampleUtterances = Lens.lens (\SkillsStoreSkill' {sampleUtterances} -> sampleUtterances) (\s@SkillsStoreSkill' {} a -> s {sampleUtterances = a} :: SkillsStoreSkill) Prelude.. Lens.mapping Lens.coerced

-- | Short description about the skill.
skillsStoreSkill_shortDescription :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_shortDescription = Lens.lens (\SkillsStoreSkill' {shortDescription} -> shortDescription) (\s@SkillsStoreSkill' {} a -> s {shortDescription = a} :: SkillsStoreSkill)

-- | The URL where the skill icon resides.
skillsStoreSkill_iconUrl :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_iconUrl = Lens.lens (\SkillsStoreSkill' {iconUrl} -> iconUrl) (\s@SkillsStoreSkill' {} a -> s {iconUrl = a} :: SkillsStoreSkill)

-- | Information about the skill.
skillsStoreSkill_skillDetails :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe SkillDetails)
skillsStoreSkill_skillDetails = Lens.lens (\SkillsStoreSkill' {skillDetails} -> skillDetails) (\s@SkillsStoreSkill' {} a -> s {skillDetails = a} :: SkillsStoreSkill)

-- | The name of the skill.
skillsStoreSkill_skillName :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_skillName = Lens.lens (\SkillsStoreSkill' {skillName} -> skillName) (\s@SkillsStoreSkill' {} a -> s {skillName = a} :: SkillsStoreSkill)

instance Core.FromJSON SkillsStoreSkill where
  parseJSON =
    Core.withObject
      "SkillsStoreSkill"
      ( \x ->
          SkillsStoreSkill'
            Prelude.<$> (x Core..:? "SkillId")
            Prelude.<*> (x Core..:? "SupportsLinking")
            Prelude.<*> ( x Core..:? "SampleUtterances"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ShortDescription")
            Prelude.<*> (x Core..:? "IconUrl")
            Prelude.<*> (x Core..:? "SkillDetails")
            Prelude.<*> (x Core..:? "SkillName")
      )

instance Prelude.Hashable SkillsStoreSkill where
  hashWithSalt _salt SkillsStoreSkill' {..} =
    _salt `Prelude.hashWithSalt` skillId
      `Prelude.hashWithSalt` supportsLinking
      `Prelude.hashWithSalt` sampleUtterances
      `Prelude.hashWithSalt` shortDescription
      `Prelude.hashWithSalt` iconUrl
      `Prelude.hashWithSalt` skillDetails
      `Prelude.hashWithSalt` skillName

instance Prelude.NFData SkillsStoreSkill where
  rnf SkillsStoreSkill' {..} =
    Prelude.rnf skillId
      `Prelude.seq` Prelude.rnf supportsLinking
      `Prelude.seq` Prelude.rnf sampleUtterances
      `Prelude.seq` Prelude.rnf shortDescription
      `Prelude.seq` Prelude.rnf iconUrl
      `Prelude.seq` Prelude.rnf skillDetails
      `Prelude.seq` Prelude.rnf skillName
