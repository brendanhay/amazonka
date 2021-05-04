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
-- Module      : Network.AWS.AlexaBusiness.Types.SkillGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A skill group with attributes.
--
-- /See:/ 'newSkillGroup' smart constructor.
data SkillGroup = SkillGroup'
  { -- | The name of a skill group.
    skillGroupName :: Prelude.Maybe Prelude.Text,
    -- | The description of a skill group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a skill group.
    skillGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SkillGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupName', 'skillGroup_skillGroupName' - The name of a skill group.
--
-- 'description', 'skillGroup_description' - The description of a skill group.
--
-- 'skillGroupArn', 'skillGroup_skillGroupArn' - The ARN of a skill group.
newSkillGroup ::
  SkillGroup
newSkillGroup =
  SkillGroup'
    { skillGroupName = Prelude.Nothing,
      description = Prelude.Nothing,
      skillGroupArn = Prelude.Nothing
    }

-- | The name of a skill group.
skillGroup_skillGroupName :: Lens.Lens' SkillGroup (Prelude.Maybe Prelude.Text)
skillGroup_skillGroupName = Lens.lens (\SkillGroup' {skillGroupName} -> skillGroupName) (\s@SkillGroup' {} a -> s {skillGroupName = a} :: SkillGroup)

-- | The description of a skill group.
skillGroup_description :: Lens.Lens' SkillGroup (Prelude.Maybe Prelude.Text)
skillGroup_description = Lens.lens (\SkillGroup' {description} -> description) (\s@SkillGroup' {} a -> s {description = a} :: SkillGroup)

-- | The ARN of a skill group.
skillGroup_skillGroupArn :: Lens.Lens' SkillGroup (Prelude.Maybe Prelude.Text)
skillGroup_skillGroupArn = Lens.lens (\SkillGroup' {skillGroupArn} -> skillGroupArn) (\s@SkillGroup' {} a -> s {skillGroupArn = a} :: SkillGroup)

instance Prelude.FromJSON SkillGroup where
  parseJSON =
    Prelude.withObject
      "SkillGroup"
      ( \x ->
          SkillGroup'
            Prelude.<$> (x Prelude..:? "SkillGroupName")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "SkillGroupArn")
      )

instance Prelude.Hashable SkillGroup

instance Prelude.NFData SkillGroup
