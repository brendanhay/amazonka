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
-- Module      : Network.AWS.Shield.Types.AttackStatisticsDataItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackStatisticsDataItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Shield.Types.AttackVolume

-- | A single attack statistics data record. This is returned by
-- DescribeAttackStatistics along with a time range indicating the time
-- period that the attack statistics apply to.
--
-- /See:/ 'newAttackStatisticsDataItem' smart constructor.
data AttackStatisticsDataItem = AttackStatisticsDataItem'
  { -- | Information about the volume of attacks during the time period. If the
    -- accompanying @AttackCount@ is zero, this setting might be empty.
    attackVolume :: Core.Maybe AttackVolume,
    -- | The number of attacks detected during the time period. This is always
    -- present, but might be zero.
    attackCount :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Integer ->
  AttackStatisticsDataItem
newAttackStatisticsDataItem pAttackCount_ =
  AttackStatisticsDataItem'
    { attackVolume =
        Core.Nothing,
      attackCount = pAttackCount_
    }

-- | Information about the volume of attacks during the time period. If the
-- accompanying @AttackCount@ is zero, this setting might be empty.
attackStatisticsDataItem_attackVolume :: Lens.Lens' AttackStatisticsDataItem (Core.Maybe AttackVolume)
attackStatisticsDataItem_attackVolume = Lens.lens (\AttackStatisticsDataItem' {attackVolume} -> attackVolume) (\s@AttackStatisticsDataItem' {} a -> s {attackVolume = a} :: AttackStatisticsDataItem)

-- | The number of attacks detected during the time period. This is always
-- present, but might be zero.
attackStatisticsDataItem_attackCount :: Lens.Lens' AttackStatisticsDataItem Core.Integer
attackStatisticsDataItem_attackCount = Lens.lens (\AttackStatisticsDataItem' {attackCount} -> attackCount) (\s@AttackStatisticsDataItem' {} a -> s {attackCount = a} :: AttackStatisticsDataItem)

instance Core.FromJSON AttackStatisticsDataItem where
  parseJSON =
    Core.withObject
      "AttackStatisticsDataItem"
      ( \x ->
          AttackStatisticsDataItem'
            Core.<$> (x Core..:? "AttackVolume")
            Core.<*> (x Core..: "AttackCount")
      )

instance Core.Hashable AttackStatisticsDataItem

instance Core.NFData AttackStatisticsDataItem
