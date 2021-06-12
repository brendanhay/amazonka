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
-- Module      : Network.AWS.SecretsManager.Types.RotationRulesType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.RotationRulesType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure that defines the rotation configuration for the secret.
--
-- /See:/ 'newRotationRulesType' smart constructor.
data RotationRulesType = RotationRulesType'
  { -- | Specifies the number of days between automatic scheduled rotations of
    -- the secret.
    --
    -- Secrets Manager schedules the next rotation when the previous one is
    -- complete. Secrets Manager schedules the date by adding the rotation
    -- interval (number of days) to the actual date of the last rotation. The
    -- service chooses the hour within that 24-hour date window randomly. The
    -- minute is also chosen somewhat randomly, but weighted towards the top of
    -- the hour and influenced by a variety of factors that help distribute
    -- load.
    automaticallyAfterDays :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RotationRulesType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticallyAfterDays', 'rotationRulesType_automaticallyAfterDays' - Specifies the number of days between automatic scheduled rotations of
-- the secret.
--
-- Secrets Manager schedules the next rotation when the previous one is
-- complete. Secrets Manager schedules the date by adding the rotation
-- interval (number of days) to the actual date of the last rotation. The
-- service chooses the hour within that 24-hour date window randomly. The
-- minute is also chosen somewhat randomly, but weighted towards the top of
-- the hour and influenced by a variety of factors that help distribute
-- load.
newRotationRulesType ::
  RotationRulesType
newRotationRulesType =
  RotationRulesType'
    { automaticallyAfterDays =
        Core.Nothing
    }

-- | Specifies the number of days between automatic scheduled rotations of
-- the secret.
--
-- Secrets Manager schedules the next rotation when the previous one is
-- complete. Secrets Manager schedules the date by adding the rotation
-- interval (number of days) to the actual date of the last rotation. The
-- service chooses the hour within that 24-hour date window randomly. The
-- minute is also chosen somewhat randomly, but weighted towards the top of
-- the hour and influenced by a variety of factors that help distribute
-- load.
rotationRulesType_automaticallyAfterDays :: Lens.Lens' RotationRulesType (Core.Maybe Core.Natural)
rotationRulesType_automaticallyAfterDays = Lens.lens (\RotationRulesType' {automaticallyAfterDays} -> automaticallyAfterDays) (\s@RotationRulesType' {} a -> s {automaticallyAfterDays = a} :: RotationRulesType)

instance Core.FromJSON RotationRulesType where
  parseJSON =
    Core.withObject
      "RotationRulesType"
      ( \x ->
          RotationRulesType'
            Core.<$> (x Core..:? "AutomaticallyAfterDays")
      )

instance Core.Hashable RotationRulesType

instance Core.NFData RotationRulesType

instance Core.ToJSON RotationRulesType where
  toJSON RotationRulesType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AutomaticallyAfterDays" Core..=)
              Core.<$> automaticallyAfterDays
          ]
      )
