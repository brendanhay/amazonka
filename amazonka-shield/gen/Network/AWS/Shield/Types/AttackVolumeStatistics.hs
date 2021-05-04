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
-- Module      : Network.AWS.Shield.Types.AttackVolumeStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackVolumeStatistics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Statistics objects for the various data types in AttackVolume.
--
-- /See:/ 'newAttackVolumeStatistics' smart constructor.
data AttackVolumeStatistics = AttackVolumeStatistics'
  { -- | The maximum attack volume observed for the given unit.
    max :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AttackVolumeStatistics where
  parseJSON =
    Prelude.withObject
      "AttackVolumeStatistics"
      ( \x ->
          AttackVolumeStatistics'
            Prelude.<$> (x Prelude..: "Max")
      )

instance Prelude.Hashable AttackVolumeStatistics

instance Prelude.NFData AttackVolumeStatistics
