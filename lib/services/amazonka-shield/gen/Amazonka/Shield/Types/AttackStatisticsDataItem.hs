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
-- Module      : Amazonka.Shield.Types.AttackStatisticsDataItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.AttackStatisticsDataItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.AttackVolume

-- | A single attack statistics data record. This is returned by
-- DescribeAttackStatistics along with a time range indicating the time
-- period that the attack statistics apply to.
--
-- /See:/ 'newAttackStatisticsDataItem' smart constructor.
data AttackStatisticsDataItem = AttackStatisticsDataItem'
  { -- | Information about the volume of attacks during the time period. If the
    -- accompanying @AttackCount@ is zero, this setting might be empty.
    attackVolume :: Prelude.Maybe AttackVolume,
    -- | The number of attacks detected during the time period. This is always
    -- present, but might be zero.
    attackCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttackStatisticsDataItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attackVolume', 'attackStatisticsDataItem_attackVolume' - Information about the volume of attacks during the time period. If the
-- accompanying @AttackCount@ is zero, this setting might be empty.
--
-- 'attackCount', 'attackStatisticsDataItem_attackCount' - The number of attacks detected during the time period. This is always
-- present, but might be zero.
newAttackStatisticsDataItem ::
  -- | 'attackCount'
  Prelude.Integer ->
  AttackStatisticsDataItem
newAttackStatisticsDataItem pAttackCount_ =
  AttackStatisticsDataItem'
    { attackVolume =
        Prelude.Nothing,
      attackCount = pAttackCount_
    }

-- | Information about the volume of attacks during the time period. If the
-- accompanying @AttackCount@ is zero, this setting might be empty.
attackStatisticsDataItem_attackVolume :: Lens.Lens' AttackStatisticsDataItem (Prelude.Maybe AttackVolume)
attackStatisticsDataItem_attackVolume = Lens.lens (\AttackStatisticsDataItem' {attackVolume} -> attackVolume) (\s@AttackStatisticsDataItem' {} a -> s {attackVolume = a} :: AttackStatisticsDataItem)

-- | The number of attacks detected during the time period. This is always
-- present, but might be zero.
attackStatisticsDataItem_attackCount :: Lens.Lens' AttackStatisticsDataItem Prelude.Integer
attackStatisticsDataItem_attackCount = Lens.lens (\AttackStatisticsDataItem' {attackCount} -> attackCount) (\s@AttackStatisticsDataItem' {} a -> s {attackCount = a} :: AttackStatisticsDataItem)

instance Data.FromJSON AttackStatisticsDataItem where
  parseJSON =
    Data.withObject
      "AttackStatisticsDataItem"
      ( \x ->
          AttackStatisticsDataItem'
            Prelude.<$> (x Data..:? "AttackVolume")
            Prelude.<*> (x Data..: "AttackCount")
      )

instance Prelude.Hashable AttackStatisticsDataItem where
  hashWithSalt _salt AttackStatisticsDataItem' {..} =
    _salt
      `Prelude.hashWithSalt` attackVolume
      `Prelude.hashWithSalt` attackCount

instance Prelude.NFData AttackStatisticsDataItem where
  rnf AttackStatisticsDataItem' {..} =
    Prelude.rnf attackVolume
      `Prelude.seq` Prelude.rnf attackCount
