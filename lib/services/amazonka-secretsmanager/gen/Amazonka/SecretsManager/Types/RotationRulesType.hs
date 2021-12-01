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
-- Module      : Amazonka.SecretsManager.Types.RotationRulesType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.RotationRulesType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

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
    automaticallyAfterDays :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
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
rotationRulesType_automaticallyAfterDays :: Lens.Lens' RotationRulesType (Prelude.Maybe Prelude.Natural)
rotationRulesType_automaticallyAfterDays = Lens.lens (\RotationRulesType' {automaticallyAfterDays} -> automaticallyAfterDays) (\s@RotationRulesType' {} a -> s {automaticallyAfterDays = a} :: RotationRulesType)

instance Core.FromJSON RotationRulesType where
  parseJSON =
    Core.withObject
      "RotationRulesType"
      ( \x ->
          RotationRulesType'
            Prelude.<$> (x Core..:? "AutomaticallyAfterDays")
      )

instance Prelude.Hashable RotationRulesType where
  hashWithSalt salt' RotationRulesType' {..} =
    salt' `Prelude.hashWithSalt` automaticallyAfterDays

instance Prelude.NFData RotationRulesType where
  rnf RotationRulesType' {..} =
    Prelude.rnf automaticallyAfterDays

instance Core.ToJSON RotationRulesType where
  toJSON RotationRulesType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AutomaticallyAfterDays" Core..=)
              Prelude.<$> automaticallyAfterDays
          ]
      )
