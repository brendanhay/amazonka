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
-- Module      : Amazonka.MediaTailor.Types.AvailSuppression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.AvailSuppression where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types.Mode
import qualified Amazonka.Prelude as Prelude

-- | The configuration for avail suppression, also known as ad suppression.
-- For more information about ad suppression, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/ad-behavior.html Ad Suppression>.
--
-- /See:/ 'newAvailSuppression' smart constructor.
data AvailSuppression = AvailSuppression'
  { -- | A live edge offset time in HH:MM:SS. MediaTailor won\'t fill ad breaks
    -- on or behind this time in the manifest lookback window. If Value is set
    -- to 00:00:00, it is in sync with the live edge, and MediaTailor won\'t
    -- fill any ad breaks on or behind the live edge. If you set a Value time,
    -- MediaTailor won\'t fill any ad breaks on or behind this time in the
    -- manifest lookback window. For example, if you set 00:45:00, then
    -- MediaTailor will fill ad breaks that occur within 45 minutes behind the
    -- live edge, but won\'t fill ad breaks on or behind 45 minutes behind the
    -- live edge.
    value :: Prelude.Maybe Prelude.Text,
    -- | Sets the ad suppression mode. By default, ad suppression is off and all
    -- ad breaks are filled with ads or slate. When Mode is set to
    -- BEHIND_LIVE_EDGE, ad suppression is active and MediaTailor won\'t fill
    -- ad breaks on or behind the ad suppression Value time in the manifest
    -- lookback window.
    mode :: Prelude.Maybe Mode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailSuppression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'availSuppression_value' - A live edge offset time in HH:MM:SS. MediaTailor won\'t fill ad breaks
-- on or behind this time in the manifest lookback window. If Value is set
-- to 00:00:00, it is in sync with the live edge, and MediaTailor won\'t
-- fill any ad breaks on or behind the live edge. If you set a Value time,
-- MediaTailor won\'t fill any ad breaks on or behind this time in the
-- manifest lookback window. For example, if you set 00:45:00, then
-- MediaTailor will fill ad breaks that occur within 45 minutes behind the
-- live edge, but won\'t fill ad breaks on or behind 45 minutes behind the
-- live edge.
--
-- 'mode', 'availSuppression_mode' - Sets the ad suppression mode. By default, ad suppression is off and all
-- ad breaks are filled with ads or slate. When Mode is set to
-- BEHIND_LIVE_EDGE, ad suppression is active and MediaTailor won\'t fill
-- ad breaks on or behind the ad suppression Value time in the manifest
-- lookback window.
newAvailSuppression ::
  AvailSuppression
newAvailSuppression =
  AvailSuppression'
    { value = Prelude.Nothing,
      mode = Prelude.Nothing
    }

-- | A live edge offset time in HH:MM:SS. MediaTailor won\'t fill ad breaks
-- on or behind this time in the manifest lookback window. If Value is set
-- to 00:00:00, it is in sync with the live edge, and MediaTailor won\'t
-- fill any ad breaks on or behind the live edge. If you set a Value time,
-- MediaTailor won\'t fill any ad breaks on or behind this time in the
-- manifest lookback window. For example, if you set 00:45:00, then
-- MediaTailor will fill ad breaks that occur within 45 minutes behind the
-- live edge, but won\'t fill ad breaks on or behind 45 minutes behind the
-- live edge.
availSuppression_value :: Lens.Lens' AvailSuppression (Prelude.Maybe Prelude.Text)
availSuppression_value = Lens.lens (\AvailSuppression' {value} -> value) (\s@AvailSuppression' {} a -> s {value = a} :: AvailSuppression)

-- | Sets the ad suppression mode. By default, ad suppression is off and all
-- ad breaks are filled with ads or slate. When Mode is set to
-- BEHIND_LIVE_EDGE, ad suppression is active and MediaTailor won\'t fill
-- ad breaks on or behind the ad suppression Value time in the manifest
-- lookback window.
availSuppression_mode :: Lens.Lens' AvailSuppression (Prelude.Maybe Mode)
availSuppression_mode = Lens.lens (\AvailSuppression' {mode} -> mode) (\s@AvailSuppression' {} a -> s {mode = a} :: AvailSuppression)

instance Core.FromJSON AvailSuppression where
  parseJSON =
    Core.withObject
      "AvailSuppression"
      ( \x ->
          AvailSuppression'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..:? "Mode")
      )

instance Prelude.Hashable AvailSuppression

instance Prelude.NFData AvailSuppression

instance Core.ToJSON AvailSuppression where
  toJSON AvailSuppression' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Mode" Core..=) Prelude.<$> mode
          ]
      )
