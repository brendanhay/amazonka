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
-- Module      : Amazonka.AlexaBusiness.Types.SkillGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.SkillGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A skill group with attributes.
--
-- /See:/ 'newSkillGroup' smart constructor.
data SkillGroup = SkillGroup'
  { -- | The description of a skill group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a skill group.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a skill group.
    skillGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'skillGroup_description' - The description of a skill group.
--
-- 'skillGroupArn', 'skillGroup_skillGroupArn' - The ARN of a skill group.
--
-- 'skillGroupName', 'skillGroup_skillGroupName' - The name of a skill group.
newSkillGroup ::
  SkillGroup
newSkillGroup =
  SkillGroup'
    { description = Prelude.Nothing,
      skillGroupArn = Prelude.Nothing,
      skillGroupName = Prelude.Nothing
    }

-- | The description of a skill group.
skillGroup_description :: Lens.Lens' SkillGroup (Prelude.Maybe Prelude.Text)
skillGroup_description = Lens.lens (\SkillGroup' {description} -> description) (\s@SkillGroup' {} a -> s {description = a} :: SkillGroup)

-- | The ARN of a skill group.
skillGroup_skillGroupArn :: Lens.Lens' SkillGroup (Prelude.Maybe Prelude.Text)
skillGroup_skillGroupArn = Lens.lens (\SkillGroup' {skillGroupArn} -> skillGroupArn) (\s@SkillGroup' {} a -> s {skillGroupArn = a} :: SkillGroup)

-- | The name of a skill group.
skillGroup_skillGroupName :: Lens.Lens' SkillGroup (Prelude.Maybe Prelude.Text)
skillGroup_skillGroupName = Lens.lens (\SkillGroup' {skillGroupName} -> skillGroupName) (\s@SkillGroup' {} a -> s {skillGroupName = a} :: SkillGroup)

instance Data.FromJSON SkillGroup where
  parseJSON =
    Data.withObject
      "SkillGroup"
      ( \x ->
          SkillGroup'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "SkillGroupArn")
            Prelude.<*> (x Data..:? "SkillGroupName")
      )

instance Prelude.Hashable SkillGroup where
  hashWithSalt _salt SkillGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` skillGroupArn
      `Prelude.hashWithSalt` skillGroupName

instance Prelude.NFData SkillGroup where
  rnf SkillGroup' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf skillGroupArn `Prelude.seq`
        Prelude.rnf skillGroupName
