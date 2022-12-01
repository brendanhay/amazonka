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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.SkillsStoreSkill where

import Amazonka.AlexaBusiness.Types.SkillDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The detailed information about an Alexa skill.
--
-- /See:/ 'newSkillsStoreSkill' smart constructor.
data SkillsStoreSkill = SkillsStoreSkill'
  { -- | Sample utterances that interact with the skill.
    sampleUtterances :: Prelude.Maybe [Prelude.Text],
    -- | Short description about the skill.
    shortDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the skill.
    skillName :: Prelude.Maybe Prelude.Text,
    -- | The URL where the skill icon resides.
    iconUrl :: Prelude.Maybe Prelude.Text,
    -- | Linking support for a skill.
    supportsLinking :: Prelude.Maybe Prelude.Bool,
    -- | Information about the skill.
    skillDetails :: Prelude.Maybe SkillDetails,
    -- | The ARN of the skill.
    skillId :: Prelude.Maybe Prelude.Text
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
-- 'sampleUtterances', 'skillsStoreSkill_sampleUtterances' - Sample utterances that interact with the skill.
--
-- 'shortDescription', 'skillsStoreSkill_shortDescription' - Short description about the skill.
--
-- 'skillName', 'skillsStoreSkill_skillName' - The name of the skill.
--
-- 'iconUrl', 'skillsStoreSkill_iconUrl' - The URL where the skill icon resides.
--
-- 'supportsLinking', 'skillsStoreSkill_supportsLinking' - Linking support for a skill.
--
-- 'skillDetails', 'skillsStoreSkill_skillDetails' - Information about the skill.
--
-- 'skillId', 'skillsStoreSkill_skillId' - The ARN of the skill.
newSkillsStoreSkill ::
  SkillsStoreSkill
newSkillsStoreSkill =
  SkillsStoreSkill'
    { sampleUtterances =
        Prelude.Nothing,
      shortDescription = Prelude.Nothing,
      skillName = Prelude.Nothing,
      iconUrl = Prelude.Nothing,
      supportsLinking = Prelude.Nothing,
      skillDetails = Prelude.Nothing,
      skillId = Prelude.Nothing
    }

-- | Sample utterances that interact with the skill.
skillsStoreSkill_sampleUtterances :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe [Prelude.Text])
skillsStoreSkill_sampleUtterances = Lens.lens (\SkillsStoreSkill' {sampleUtterances} -> sampleUtterances) (\s@SkillsStoreSkill' {} a -> s {sampleUtterances = a} :: SkillsStoreSkill) Prelude.. Lens.mapping Lens.coerced

-- | Short description about the skill.
skillsStoreSkill_shortDescription :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_shortDescription = Lens.lens (\SkillsStoreSkill' {shortDescription} -> shortDescription) (\s@SkillsStoreSkill' {} a -> s {shortDescription = a} :: SkillsStoreSkill)

-- | The name of the skill.
skillsStoreSkill_skillName :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_skillName = Lens.lens (\SkillsStoreSkill' {skillName} -> skillName) (\s@SkillsStoreSkill' {} a -> s {skillName = a} :: SkillsStoreSkill)

-- | The URL where the skill icon resides.
skillsStoreSkill_iconUrl :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_iconUrl = Lens.lens (\SkillsStoreSkill' {iconUrl} -> iconUrl) (\s@SkillsStoreSkill' {} a -> s {iconUrl = a} :: SkillsStoreSkill)

-- | Linking support for a skill.
skillsStoreSkill_supportsLinking :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Bool)
skillsStoreSkill_supportsLinking = Lens.lens (\SkillsStoreSkill' {supportsLinking} -> supportsLinking) (\s@SkillsStoreSkill' {} a -> s {supportsLinking = a} :: SkillsStoreSkill)

-- | Information about the skill.
skillsStoreSkill_skillDetails :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe SkillDetails)
skillsStoreSkill_skillDetails = Lens.lens (\SkillsStoreSkill' {skillDetails} -> skillDetails) (\s@SkillsStoreSkill' {} a -> s {skillDetails = a} :: SkillsStoreSkill)

-- | The ARN of the skill.
skillsStoreSkill_skillId :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_skillId = Lens.lens (\SkillsStoreSkill' {skillId} -> skillId) (\s@SkillsStoreSkill' {} a -> s {skillId = a} :: SkillsStoreSkill)

instance Core.FromJSON SkillsStoreSkill where
  parseJSON =
    Core.withObject
      "SkillsStoreSkill"
      ( \x ->
          SkillsStoreSkill'
            Prelude.<$> ( x Core..:? "SampleUtterances"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ShortDescription")
            Prelude.<*> (x Core..:? "SkillName")
            Prelude.<*> (x Core..:? "IconUrl")
            Prelude.<*> (x Core..:? "SupportsLinking")
            Prelude.<*> (x Core..:? "SkillDetails")
            Prelude.<*> (x Core..:? "SkillId")
      )

instance Prelude.Hashable SkillsStoreSkill where
  hashWithSalt _salt SkillsStoreSkill' {..} =
    _salt `Prelude.hashWithSalt` sampleUtterances
      `Prelude.hashWithSalt` shortDescription
      `Prelude.hashWithSalt` skillName
      `Prelude.hashWithSalt` iconUrl
      `Prelude.hashWithSalt` supportsLinking
      `Prelude.hashWithSalt` skillDetails
      `Prelude.hashWithSalt` skillId

instance Prelude.NFData SkillsStoreSkill where
  rnf SkillsStoreSkill' {..} =
    Prelude.rnf sampleUtterances
      `Prelude.seq` Prelude.rnf shortDescription
      `Prelude.seq` Prelude.rnf skillName
      `Prelude.seq` Prelude.rnf iconUrl
      `Prelude.seq` Prelude.rnf supportsLinking
      `Prelude.seq` Prelude.rnf skillDetails
      `Prelude.seq` Prelude.rnf skillId
