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
-- Module      : Network.AWS.IoT.Types.AssetPropertyTimestamp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AssetPropertyTimestamp where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An asset property timestamp entry containing the following information.
--
-- /See:/ 'newAssetPropertyTimestamp' smart constructor.
data AssetPropertyTimestamp = AssetPropertyTimestamp'
  { -- | Optional. A string that contains the nanosecond time offset. Accepts
    -- substitution templates.
    offsetInNanos :: Prelude.Maybe Prelude.Text,
    -- | A string that contains the time in seconds since epoch. Accepts
    -- substitution templates.
    timeInSeconds :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AssetPropertyTimestamp
newAssetPropertyTimestamp pTimeInSeconds_ =
  AssetPropertyTimestamp'
    { offsetInNanos =
        Prelude.Nothing,
      timeInSeconds = pTimeInSeconds_
    }

-- | Optional. A string that contains the nanosecond time offset. Accepts
-- substitution templates.
assetPropertyTimestamp_offsetInNanos :: Lens.Lens' AssetPropertyTimestamp (Prelude.Maybe Prelude.Text)
assetPropertyTimestamp_offsetInNanos = Lens.lens (\AssetPropertyTimestamp' {offsetInNanos} -> offsetInNanos) (\s@AssetPropertyTimestamp' {} a -> s {offsetInNanos = a} :: AssetPropertyTimestamp)

-- | A string that contains the time in seconds since epoch. Accepts
-- substitution templates.
assetPropertyTimestamp_timeInSeconds :: Lens.Lens' AssetPropertyTimestamp Prelude.Text
assetPropertyTimestamp_timeInSeconds = Lens.lens (\AssetPropertyTimestamp' {timeInSeconds} -> timeInSeconds) (\s@AssetPropertyTimestamp' {} a -> s {timeInSeconds = a} :: AssetPropertyTimestamp)

instance Prelude.FromJSON AssetPropertyTimestamp where
  parseJSON =
    Prelude.withObject
      "AssetPropertyTimestamp"
      ( \x ->
          AssetPropertyTimestamp'
            Prelude.<$> (x Prelude..:? "offsetInNanos")
            Prelude.<*> (x Prelude..: "timeInSeconds")
      )

instance Prelude.Hashable AssetPropertyTimestamp

instance Prelude.NFData AssetPropertyTimestamp

instance Prelude.ToJSON AssetPropertyTimestamp where
  toJSON AssetPropertyTimestamp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("offsetInNanos" Prelude..=)
              Prelude.<$> offsetInNanos,
            Prelude.Just
              ("timeInSeconds" Prelude..= timeInSeconds)
          ]
      )
