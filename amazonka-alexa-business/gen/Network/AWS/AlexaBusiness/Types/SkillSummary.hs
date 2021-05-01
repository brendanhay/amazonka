{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.Types.SkillSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillSummary where

import Network.AWS.AlexaBusiness.Types.EnablementType
import Network.AWS.AlexaBusiness.Types.SkillType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary of skills.
--
-- /See:/ 'newSkillSummary' smart constructor.
data SkillSummary = SkillSummary'
  { -- | The ARN of the skill summary.
    skillId :: Prelude.Maybe Prelude.Text,
    -- | Linking support for a skill.
    supportsLinking :: Prelude.Maybe Prelude.Bool,
    -- | Whether the skill is publicly available or is a private skill.
    skillType :: Prelude.Maybe SkillType,
    -- | The name of the skill.
    skillName :: Prelude.Maybe Prelude.Text,
    -- | Whether the skill is enabled under the user\'s account, or if it
    -- requires linking to be used.
    enablementType :: Prelude.Maybe EnablementType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SkillSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillId', 'skillSummary_skillId' - The ARN of the skill summary.
--
-- 'supportsLinking', 'skillSummary_supportsLinking' - Linking support for a skill.
--
-- 'skillType', 'skillSummary_skillType' - Whether the skill is publicly available or is a private skill.
--
-- 'skillName', 'skillSummary_skillName' - The name of the skill.
--
-- 'enablementType', 'skillSummary_enablementType' - Whether the skill is enabled under the user\'s account, or if it
-- requires linking to be used.
newSkillSummary ::
  SkillSummary
newSkillSummary =
  SkillSummary'
    { skillId = Prelude.Nothing,
      supportsLinking = Prelude.Nothing,
      skillType = Prelude.Nothing,
      skillName = Prelude.Nothing,
      enablementType = Prelude.Nothing
    }

-- | The ARN of the skill summary.
skillSummary_skillId :: Lens.Lens' SkillSummary (Prelude.Maybe Prelude.Text)
skillSummary_skillId = Lens.lens (\SkillSummary' {skillId} -> skillId) (\s@SkillSummary' {} a -> s {skillId = a} :: SkillSummary)

-- | Linking support for a skill.
skillSummary_supportsLinking :: Lens.Lens' SkillSummary (Prelude.Maybe Prelude.Bool)
skillSummary_supportsLinking = Lens.lens (\SkillSummary' {supportsLinking} -> supportsLinking) (\s@SkillSummary' {} a -> s {supportsLinking = a} :: SkillSummary)

-- | Whether the skill is publicly available or is a private skill.
skillSummary_skillType :: Lens.Lens' SkillSummary (Prelude.Maybe SkillType)
skillSummary_skillType = Lens.lens (\SkillSummary' {skillType} -> skillType) (\s@SkillSummary' {} a -> s {skillType = a} :: SkillSummary)

-- | The name of the skill.
skillSummary_skillName :: Lens.Lens' SkillSummary (Prelude.Maybe Prelude.Text)
skillSummary_skillName = Lens.lens (\SkillSummary' {skillName} -> skillName) (\s@SkillSummary' {} a -> s {skillName = a} :: SkillSummary)

-- | Whether the skill is enabled under the user\'s account, or if it
-- requires linking to be used.
skillSummary_enablementType :: Lens.Lens' SkillSummary (Prelude.Maybe EnablementType)
skillSummary_enablementType = Lens.lens (\SkillSummary' {enablementType} -> enablementType) (\s@SkillSummary' {} a -> s {enablementType = a} :: SkillSummary)

instance Prelude.FromJSON SkillSummary where
  parseJSON =
    Prelude.withObject
      "SkillSummary"
      ( \x ->
          SkillSummary'
            Prelude.<$> (x Prelude..:? "SkillId")
            Prelude.<*> (x Prelude..:? "SupportsLinking")
            Prelude.<*> (x Prelude..:? "SkillType")
            Prelude.<*> (x Prelude..:? "SkillName")
            Prelude.<*> (x Prelude..:? "EnablementType")
      )

instance Prelude.Hashable SkillSummary

instance Prelude.NFData SkillSummary
