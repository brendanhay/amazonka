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
-- Module      : Network.AWS.IoT.Types.AssetPropertyTimestamp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AssetPropertyTimestamp where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An asset property timestamp entry containing the following information.
--
-- /See:/ 'newAssetPropertyTimestamp' smart constructor.
data AssetPropertyTimestamp = AssetPropertyTimestamp'
  { -- | Optional. A string that contains the nanosecond time offset. Accepts
    -- substitution templates.
    offsetInNanos :: Core.Maybe Core.Text,
    -- | A string that contains the time in seconds since epoch. Accepts
    -- substitution templates.
    timeInSeconds :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssetPropertyTimestamp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offsetInNanos', 'assetPropertyTimestamp_offsetInNanos' - Optional. A string that contains the nanosecond time offset. Accepts
-- substitution templates.
--
-- 'timeInSeconds', 'assetPropertyTimestamp_timeInSeconds' - A string that contains the time in seconds since epoch. Accepts
-- substitution templates.
newAssetPropertyTimestamp ::
  -- | 'timeInSeconds'
  Core.Text ->
  AssetPropertyTimestamp
newAssetPropertyTimestamp pTimeInSeconds_ =
  AssetPropertyTimestamp'
    { offsetInNanos =
        Core.Nothing,
      timeInSeconds = pTimeInSeconds_
    }

-- | Optional. A string that contains the nanosecond time offset. Accepts
-- substitution templates.
assetPropertyTimestamp_offsetInNanos :: Lens.Lens' AssetPropertyTimestamp (Core.Maybe Core.Text)
assetPropertyTimestamp_offsetInNanos = Lens.lens (\AssetPropertyTimestamp' {offsetInNanos} -> offsetInNanos) (\s@AssetPropertyTimestamp' {} a -> s {offsetInNanos = a} :: AssetPropertyTimestamp)

-- | A string that contains the time in seconds since epoch. Accepts
-- substitution templates.
assetPropertyTimestamp_timeInSeconds :: Lens.Lens' AssetPropertyTimestamp Core.Text
assetPropertyTimestamp_timeInSeconds = Lens.lens (\AssetPropertyTimestamp' {timeInSeconds} -> timeInSeconds) (\s@AssetPropertyTimestamp' {} a -> s {timeInSeconds = a} :: AssetPropertyTimestamp)

instance Core.FromJSON AssetPropertyTimestamp where
  parseJSON =
    Core.withObject
      "AssetPropertyTimestamp"
      ( \x ->
          AssetPropertyTimestamp'
            Core.<$> (x Core..:? "offsetInNanos")
            Core.<*> (x Core..: "timeInSeconds")
      )

instance Core.Hashable AssetPropertyTimestamp

instance Core.NFData AssetPropertyTimestamp

instance Core.ToJSON AssetPropertyTimestamp where
  toJSON AssetPropertyTimestamp' {..} =
    Core.object
      ( Core.catMaybes
          [ ("offsetInNanos" Core..=) Core.<$> offsetInNanos,
            Core.Just ("timeInSeconds" Core..= timeInSeconds)
          ]
      )
