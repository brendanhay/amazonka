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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.SkillsStoreSkill where

import Amazonka.AlexaBusiness.Types.SkillDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The detailed information about an Alexa skill.
--
-- /See:/ 'newSkillsStoreSkill' smart constructor.
data SkillsStoreSkill = SkillsStoreSkill'
  { -- | The URL where the skill icon resides.
    iconUrl :: Prelude.Maybe Prelude.Text,
    -- | Sample utterances that interact with the skill.
    sampleUtterances :: Prelude.Maybe [Prelude.Text],
    -- | Short description about the skill.
    shortDescription :: Prelude.Maybe Prelude.Text,
    -- | Information about the skill.
    skillDetails :: Prelude.Maybe SkillDetails,
    -- | The ARN of the skill.
    skillId :: Prelude.Maybe Prelude.Text,
    -- | The name of the skill.
    skillName :: Prelude.Maybe Prelude.Text,
    -- | Linking support for a skill.
    supportsLinking :: Prelude.Maybe Prelude.Bool
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
-- 'iconUrl', 'skillsStoreSkill_iconUrl' - The URL where the skill icon resides.
--
-- 'sampleUtterances', 'skillsStoreSkill_sampleUtterances' - Sample utterances that interact with the skill.
--
-- 'shortDescription', 'skillsStoreSkill_shortDescription' - Short description about the skill.
--
-- 'skillDetails', 'skillsStoreSkill_skillDetails' - Information about the skill.
--
-- 'skillId', 'skillsStoreSkill_skillId' - The ARN of the skill.
--
-- 'skillName', 'skillsStoreSkill_skillName' - The name of the skill.
--
-- 'supportsLinking', 'skillsStoreSkill_supportsLinking' - Linking support for a skill.
newSkillsStoreSkill ::
  SkillsStoreSkill
newSkillsStoreSkill =
  SkillsStoreSkill'
    { iconUrl = Prelude.Nothing,
      sampleUtterances = Prelude.Nothing,
      shortDescription = Prelude.Nothing,
      skillDetails = Prelude.Nothing,
      skillId = Prelude.Nothing,
      skillName = Prelude.Nothing,
      supportsLinking = Prelude.Nothing
    }

-- | The URL where the skill icon resides.
skillsStoreSkill_iconUrl :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_iconUrl = Lens.lens (\SkillsStoreSkill' {iconUrl} -> iconUrl) (\s@SkillsStoreSkill' {} a -> s {iconUrl = a} :: SkillsStoreSkill)

-- | Sample utterances that interact with the skill.
skillsStoreSkill_sampleUtterances :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe [Prelude.Text])
skillsStoreSkill_sampleUtterances = Lens.lens (\SkillsStoreSkill' {sampleUtterances} -> sampleUtterances) (\s@SkillsStoreSkill' {} a -> s {sampleUtterances = a} :: SkillsStoreSkill) Prelude.. Lens.mapping Lens.coerced

-- | Short description about the skill.
skillsStoreSkill_shortDescription :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_shortDescription = Lens.lens (\SkillsStoreSkill' {shortDescription} -> shortDescription) (\s@SkillsStoreSkill' {} a -> s {shortDescription = a} :: SkillsStoreSkill)

-- | Information about the skill.
skillsStoreSkill_skillDetails :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe SkillDetails)
skillsStoreSkill_skillDetails = Lens.lens (\SkillsStoreSkill' {skillDetails} -> skillDetails) (\s@SkillsStoreSkill' {} a -> s {skillDetails = a} :: SkillsStoreSkill)

-- | The ARN of the skill.
skillsStoreSkill_skillId :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_skillId = Lens.lens (\SkillsStoreSkill' {skillId} -> skillId) (\s@SkillsStoreSkill' {} a -> s {skillId = a} :: SkillsStoreSkill)

-- | The name of the skill.
skillsStoreSkill_skillName :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Text)
skillsStoreSkill_skillName = Lens.lens (\SkillsStoreSkill' {skillName} -> skillName) (\s@SkillsStoreSkill' {} a -> s {skillName = a} :: SkillsStoreSkill)

-- | Linking support for a skill.
skillsStoreSkill_supportsLinking :: Lens.Lens' SkillsStoreSkill (Prelude.Maybe Prelude.Bool)
skillsStoreSkill_supportsLinking = Lens.lens (\SkillsStoreSkill' {supportsLinking} -> supportsLinking) (\s@SkillsStoreSkill' {} a -> s {supportsLinking = a} :: SkillsStoreSkill)

instance Data.FromJSON SkillsStoreSkill where
  parseJSON =
    Data.withObject
      "SkillsStoreSkill"
      ( \x ->
          SkillsStoreSkill'
            Prelude.<$> (x Data..:? "IconUrl")
            Prelude.<*> ( x
                            Data..:? "SampleUtterances"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ShortDescription")
            Prelude.<*> (x Data..:? "SkillDetails")
            Prelude.<*> (x Data..:? "SkillId")
            Prelude.<*> (x Data..:? "SkillName")
            Prelude.<*> (x Data..:? "SupportsLinking")
      )

instance Prelude.Hashable SkillsStoreSkill where
  hashWithSalt _salt SkillsStoreSkill' {..} =
    _salt
      `Prelude.hashWithSalt` iconUrl
      `Prelude.hashWithSalt` sampleUtterances
      `Prelude.hashWithSalt` shortDescription
      `Prelude.hashWithSalt` skillDetails
      `Prelude.hashWithSalt` skillId
      `Prelude.hashWithSalt` skillName
      `Prelude.hashWithSalt` supportsLinking

instance Prelude.NFData SkillsStoreSkill where
  rnf SkillsStoreSkill' {..} =
    Prelude.rnf iconUrl `Prelude.seq`
      Prelude.rnf sampleUtterances `Prelude.seq`
        Prelude.rnf shortDescription `Prelude.seq`
          Prelude.rnf skillDetails `Prelude.seq`
            Prelude.rnf skillId `Prelude.seq`
              Prelude.rnf skillName `Prelude.seq`
                Prelude.rnf supportsLinking
