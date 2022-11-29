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
-- Module      : Amazonka.LexV2Models.Types.SlotTypeStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotTypeStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The object that contains the statistical summary of the recommended slot
-- type associated with the bot recommendation.
--
-- /See:/ 'newSlotTypeStatistics' smart constructor.
data SlotTypeStatistics = SlotTypeStatistics'
  { -- | The number of recommended slot types associated with the bot
    -- recommendation.
    discoveredSlotTypeCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotTypeStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoveredSlotTypeCount', 'slotTypeStatistics_discoveredSlotTypeCount' - The number of recommended slot types associated with the bot
-- recommendation.
newSlotTypeStatistics ::
  SlotTypeStatistics
newSlotTypeStatistics =
  SlotTypeStatistics'
    { discoveredSlotTypeCount =
        Prelude.Nothing
    }

-- | The number of recommended slot types associated with the bot
-- recommendation.
slotTypeStatistics_discoveredSlotTypeCount :: Lens.Lens' SlotTypeStatistics (Prelude.Maybe Prelude.Int)
slotTypeStatistics_discoveredSlotTypeCount = Lens.lens (\SlotTypeStatistics' {discoveredSlotTypeCount} -> discoveredSlotTypeCount) (\s@SlotTypeStatistics' {} a -> s {discoveredSlotTypeCount = a} :: SlotTypeStatistics)

instance Core.FromJSON SlotTypeStatistics where
  parseJSON =
    Core.withObject
      "SlotTypeStatistics"
      ( \x ->
          SlotTypeStatistics'
            Prelude.<$> (x Core..:? "discoveredSlotTypeCount")
      )

instance Prelude.Hashable SlotTypeStatistics where
  hashWithSalt _salt SlotTypeStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` discoveredSlotTypeCount

instance Prelude.NFData SlotTypeStatistics where
  rnf SlotTypeStatistics' {..} =
    Prelude.rnf discoveredSlotTypeCount
