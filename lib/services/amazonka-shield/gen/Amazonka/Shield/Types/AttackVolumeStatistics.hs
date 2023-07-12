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
-- Module      : Amazonka.Shield.Types.AttackVolumeStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.AttackVolumeStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Statistics objects for the various data types in AttackVolume.
--
-- /See:/ 'newAttackVolumeStatistics' smart constructor.
data AttackVolumeStatistics = AttackVolumeStatistics'
  { -- | The maximum attack volume observed for the given unit.
    max :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttackVolumeStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'attackVolumeStatistics_max' - The maximum attack volume observed for the given unit.
newAttackVolumeStatistics ::
  -- | 'max'
  Prelude.Double ->
  AttackVolumeStatistics
newAttackVolumeStatistics pMax_ =
  AttackVolumeStatistics' {max = pMax_}

-- | The maximum attack volume observed for the given unit.
attackVolumeStatistics_max :: Lens.Lens' AttackVolumeStatistics Prelude.Double
attackVolumeStatistics_max = Lens.lens (\AttackVolumeStatistics' {max} -> max) (\s@AttackVolumeStatistics' {} a -> s {max = a} :: AttackVolumeStatistics)

instance Data.FromJSON AttackVolumeStatistics where
  parseJSON =
    Data.withObject
      "AttackVolumeStatistics"
      ( \x ->
          AttackVolumeStatistics'
            Prelude.<$> (x Data..: "Max")
      )

instance Prelude.Hashable AttackVolumeStatistics where
  hashWithSalt _salt AttackVolumeStatistics' {..} =
    _salt `Prelude.hashWithSalt` max

instance Prelude.NFData AttackVolumeStatistics where
  rnf AttackVolumeStatistics' {..} = Prelude.rnf max
