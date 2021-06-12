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
-- Module      : Network.AWS.AlexaBusiness.Types.SkillGroupData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillGroupData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The attributes of a skill group.
--
-- /See:/ 'newSkillGroupData' smart constructor.
data SkillGroupData = SkillGroupData'
  { -- | The skill group name of a skill group.
    skillGroupName :: Core.Maybe Core.Text,
    -- | The description of a skill group.
    description :: Core.Maybe Core.Text,
    -- | The skill group ARN of a skill group.
    skillGroupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SkillGroupData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupName', 'skillGroupData_skillGroupName' - The skill group name of a skill group.
--
-- 'description', 'skillGroupData_description' - The description of a skill group.
--
-- 'skillGroupArn', 'skillGroupData_skillGroupArn' - The skill group ARN of a skill group.
newSkillGroupData ::
  SkillGroupData
newSkillGroupData =
  SkillGroupData'
    { skillGroupName = Core.Nothing,
      description = Core.Nothing,
      skillGroupArn = Core.Nothing
    }

-- | The skill group name of a skill group.
skillGroupData_skillGroupName :: Lens.Lens' SkillGroupData (Core.Maybe Core.Text)
skillGroupData_skillGroupName = Lens.lens (\SkillGroupData' {skillGroupName} -> skillGroupName) (\s@SkillGroupData' {} a -> s {skillGroupName = a} :: SkillGroupData)

-- | The description of a skill group.
skillGroupData_description :: Lens.Lens' SkillGroupData (Core.Maybe Core.Text)
skillGroupData_description = Lens.lens (\SkillGroupData' {description} -> description) (\s@SkillGroupData' {} a -> s {description = a} :: SkillGroupData)

-- | The skill group ARN of a skill group.
skillGroupData_skillGroupArn :: Lens.Lens' SkillGroupData (Core.Maybe Core.Text)
skillGroupData_skillGroupArn = Lens.lens (\SkillGroupData' {skillGroupArn} -> skillGroupArn) (\s@SkillGroupData' {} a -> s {skillGroupArn = a} :: SkillGroupData)

instance Core.FromJSON SkillGroupData where
  parseJSON =
    Core.withObject
      "SkillGroupData"
      ( \x ->
          SkillGroupData'
            Core.<$> (x Core..:? "SkillGroupName")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "SkillGroupArn")
      )

instance Core.Hashable SkillGroupData

instance Core.NFData SkillGroupData
