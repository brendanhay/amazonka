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
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TemporalFilterSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
import Network.AWS.MediaLive.Types.TemporalFilterStrength
import qualified Network.AWS.Prelude as Prelude

-- | Temporal Filter Settings
--
-- /See:/ 'newTemporalFilterSettings' smart constructor.
data TemporalFilterSettings = TemporalFilterSettings'
  { -- | If you enable this filter, the results are the following: - If the
    -- source content is noisy (it contains excessive digital artifacts), the
    -- filter cleans up the source. - If the source content is already clean,
    -- the filter tends to decrease the bitrate, especially when the rate
    -- control mode is QVBR.
    postFilterSharpening :: Prelude.Maybe TemporalFilterPostFilterSharpening,
    -- | Choose a filter strength. We recommend a strength of 1 or 2. A higher
    -- strength might take out good information, resulting in an image that is
    -- overly soft.
    strength :: Prelude.Maybe TemporalFilterStrength
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TemporalFilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'postFilterSharpening', 'temporalFilterSettings_postFilterSharpening' - If you enable this filter, the results are the following: - If the
-- source content is noisy (it contains excessive digital artifacts), the
-- filter cleans up the source. - If the source content is already clean,
-- the filter tends to decrease the bitrate, especially when the rate
-- control mode is QVBR.
--
-- 'strength', 'temporalFilterSettings_strength' - Choose a filter strength. We recommend a strength of 1 or 2. A higher
-- strength might take out good information, resulting in an image that is
-- overly soft.
newTemporalFilterSettings ::
  TemporalFilterSettings
newTemporalFilterSettings =
  TemporalFilterSettings'
    { postFilterSharpening =
        Prelude.Nothing,
      strength = Prelude.Nothing
    }

-- | If you enable this filter, the results are the following: - If the
-- source content is noisy (it contains excessive digital artifacts), the
-- filter cleans up the source. - If the source content is already clean,
-- the filter tends to decrease the bitrate, especially when the rate
-- control mode is QVBR.
temporalFilterSettings_postFilterSharpening :: Lens.Lens' TemporalFilterSettings (Prelude.Maybe TemporalFilterPostFilterSharpening)
temporalFilterSettings_postFilterSharpening = Lens.lens (\TemporalFilterSettings' {postFilterSharpening} -> postFilterSharpening) (\s@TemporalFilterSettings' {} a -> s {postFilterSharpening = a} :: TemporalFilterSettings)

-- | Choose a filter strength. We recommend a strength of 1 or 2. A higher
-- strength might take out good information, resulting in an image that is
-- overly soft.
temporalFilterSettings_strength :: Lens.Lens' TemporalFilterSettings (Prelude.Maybe TemporalFilterStrength)
temporalFilterSettings_strength = Lens.lens (\TemporalFilterSettings' {strength} -> strength) (\s@TemporalFilterSettings' {} a -> s {strength = a} :: TemporalFilterSettings)

instance Prelude.FromJSON TemporalFilterSettings where
  parseJSON =
    Prelude.withObject
      "TemporalFilterSettings"
      ( \x ->
          TemporalFilterSettings'
            Prelude.<$> (x Prelude..:? "postFilterSharpening")
            Prelude.<*> (x Prelude..:? "strength")
      )

instance Prelude.Hashable TemporalFilterSettings

instance Prelude.NFData TemporalFilterSettings

instance Prelude.ToJSON TemporalFilterSettings where
  toJSON TemporalFilterSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("postFilterSharpening" Prelude..=)
              Prelude.<$> postFilterSharpening,
            ("strength" Prelude..=) Prelude.<$> strength
          ]
      )
