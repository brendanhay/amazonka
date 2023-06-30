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
-- Module      : Amazonka.Shield.Types.AttackVolume
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.AttackVolume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.AttackVolumeStatistics

-- | Information about the volume of attacks during the time period, included
-- in an AttackStatisticsDataItem. If the accompanying @AttackCount@ in the
-- statistics object is zero, this setting might be empty.
--
-- /See:/ 'newAttackVolume' smart constructor.
data AttackVolume = AttackVolume'
  { -- | A statistics object that uses bits per second as the unit. This is
    -- included for network level attacks.
    bitsPerSecond :: Prelude.Maybe AttackVolumeStatistics,
    -- | A statistics object that uses packets per second as the unit. This is
    -- included for network level attacks.
    packetsPerSecond :: Prelude.Maybe AttackVolumeStatistics,
    -- | A statistics object that uses requests per second as the unit. This is
    -- included for application level attacks, and is only available for
    -- accounts that are subscribed to Shield Advanced.
    requestsPerSecond :: Prelude.Maybe AttackVolumeStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttackVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bitsPerSecond', 'attackVolume_bitsPerSecond' - A statistics object that uses bits per second as the unit. This is
-- included for network level attacks.
--
-- 'packetsPerSecond', 'attackVolume_packetsPerSecond' - A statistics object that uses packets per second as the unit. This is
-- included for network level attacks.
--
-- 'requestsPerSecond', 'attackVolume_requestsPerSecond' - A statistics object that uses requests per second as the unit. This is
-- included for application level attacks, and is only available for
-- accounts that are subscribed to Shield Advanced.
newAttackVolume ::
  AttackVolume
newAttackVolume =
  AttackVolume'
    { bitsPerSecond = Prelude.Nothing,
      packetsPerSecond = Prelude.Nothing,
      requestsPerSecond = Prelude.Nothing
    }

-- | A statistics object that uses bits per second as the unit. This is
-- included for network level attacks.
attackVolume_bitsPerSecond :: Lens.Lens' AttackVolume (Prelude.Maybe AttackVolumeStatistics)
attackVolume_bitsPerSecond = Lens.lens (\AttackVolume' {bitsPerSecond} -> bitsPerSecond) (\s@AttackVolume' {} a -> s {bitsPerSecond = a} :: AttackVolume)

-- | A statistics object that uses packets per second as the unit. This is
-- included for network level attacks.
attackVolume_packetsPerSecond :: Lens.Lens' AttackVolume (Prelude.Maybe AttackVolumeStatistics)
attackVolume_packetsPerSecond = Lens.lens (\AttackVolume' {packetsPerSecond} -> packetsPerSecond) (\s@AttackVolume' {} a -> s {packetsPerSecond = a} :: AttackVolume)

-- | A statistics object that uses requests per second as the unit. This is
-- included for application level attacks, and is only available for
-- accounts that are subscribed to Shield Advanced.
attackVolume_requestsPerSecond :: Lens.Lens' AttackVolume (Prelude.Maybe AttackVolumeStatistics)
attackVolume_requestsPerSecond = Lens.lens (\AttackVolume' {requestsPerSecond} -> requestsPerSecond) (\s@AttackVolume' {} a -> s {requestsPerSecond = a} :: AttackVolume)

instance Data.FromJSON AttackVolume where
  parseJSON =
    Data.withObject
      "AttackVolume"
      ( \x ->
          AttackVolume'
            Prelude.<$> (x Data..:? "BitsPerSecond")
            Prelude.<*> (x Data..:? "PacketsPerSecond")
            Prelude.<*> (x Data..:? "RequestsPerSecond")
      )

instance Prelude.Hashable AttackVolume where
  hashWithSalt _salt AttackVolume' {..} =
    _salt
      `Prelude.hashWithSalt` bitsPerSecond
      `Prelude.hashWithSalt` packetsPerSecond
      `Prelude.hashWithSalt` requestsPerSecond

instance Prelude.NFData AttackVolume where
  rnf AttackVolume' {..} =
    Prelude.rnf bitsPerSecond
      `Prelude.seq` Prelude.rnf packetsPerSecond
      `Prelude.seq` Prelude.rnf requestsPerSecond
