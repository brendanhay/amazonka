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
-- Module      : Network.AWS.Shield.Types.AttackVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackVolume where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Shield.Types.AttackVolumeStatistics

-- | Information about the volume of attacks during the time period, included
-- in an AttackStatisticsDataItem. If the accompanying @AttackCount@ in the
-- statistics object is zero, this setting might be empty.
--
-- /See:/ 'newAttackVolume' smart constructor.
data AttackVolume = AttackVolume'
  { -- | A statistics object that uses packets per second as the unit. This is
    -- included for network level attacks.
    packetsPerSecond :: Core.Maybe AttackVolumeStatistics,
    -- | A statistics object that uses bits per second as the unit. This is
    -- included for network level attacks.
    bitsPerSecond :: Core.Maybe AttackVolumeStatistics,
    -- | A statistics object that uses requests per second as the unit. This is
    -- included for application level attacks, and is only available for
    -- accounts that are subscribed to Shield Advanced.
    requestsPerSecond :: Core.Maybe AttackVolumeStatistics
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttackVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packetsPerSecond', 'attackVolume_packetsPerSecond' - A statistics object that uses packets per second as the unit. This is
-- included for network level attacks.
--
-- 'bitsPerSecond', 'attackVolume_bitsPerSecond' - A statistics object that uses bits per second as the unit. This is
-- included for network level attacks.
--
-- 'requestsPerSecond', 'attackVolume_requestsPerSecond' - A statistics object that uses requests per second as the unit. This is
-- included for application level attacks, and is only available for
-- accounts that are subscribed to Shield Advanced.
newAttackVolume ::
  AttackVolume
newAttackVolume =
  AttackVolume'
    { packetsPerSecond = Core.Nothing,
      bitsPerSecond = Core.Nothing,
      requestsPerSecond = Core.Nothing
    }

-- | A statistics object that uses packets per second as the unit. This is
-- included for network level attacks.
attackVolume_packetsPerSecond :: Lens.Lens' AttackVolume (Core.Maybe AttackVolumeStatistics)
attackVolume_packetsPerSecond = Lens.lens (\AttackVolume' {packetsPerSecond} -> packetsPerSecond) (\s@AttackVolume' {} a -> s {packetsPerSecond = a} :: AttackVolume)

-- | A statistics object that uses bits per second as the unit. This is
-- included for network level attacks.
attackVolume_bitsPerSecond :: Lens.Lens' AttackVolume (Core.Maybe AttackVolumeStatistics)
attackVolume_bitsPerSecond = Lens.lens (\AttackVolume' {bitsPerSecond} -> bitsPerSecond) (\s@AttackVolume' {} a -> s {bitsPerSecond = a} :: AttackVolume)

-- | A statistics object that uses requests per second as the unit. This is
-- included for application level attacks, and is only available for
-- accounts that are subscribed to Shield Advanced.
attackVolume_requestsPerSecond :: Lens.Lens' AttackVolume (Core.Maybe AttackVolumeStatistics)
attackVolume_requestsPerSecond = Lens.lens (\AttackVolume' {requestsPerSecond} -> requestsPerSecond) (\s@AttackVolume' {} a -> s {requestsPerSecond = a} :: AttackVolume)

instance Core.FromJSON AttackVolume where
  parseJSON =
    Core.withObject
      "AttackVolume"
      ( \x ->
          AttackVolume'
            Core.<$> (x Core..:? "PacketsPerSecond")
            Core.<*> (x Core..:? "BitsPerSecond")
            Core.<*> (x Core..:? "RequestsPerSecond")
      )

instance Core.Hashable AttackVolume

instance Core.NFData AttackVolume
