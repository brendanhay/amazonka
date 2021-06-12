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
-- Module      : Network.AWS.MediaLive.Types.H265FilterSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265FilterSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TemporalFilterSettings

-- | H265 Filter Settings
--
-- /See:/ 'newH265FilterSettings' smart constructor.
data H265FilterSettings = H265FilterSettings'
  { temporalFilterSettings :: Core.Maybe TemporalFilterSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'H265FilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'temporalFilterSettings', 'h265FilterSettings_temporalFilterSettings' - Undocumented member.
newH265FilterSettings ::
  H265FilterSettings
newH265FilterSettings =
  H265FilterSettings'
    { temporalFilterSettings =
        Core.Nothing
    }

-- | Undocumented member.
h265FilterSettings_temporalFilterSettings :: Lens.Lens' H265FilterSettings (Core.Maybe TemporalFilterSettings)
h265FilterSettings_temporalFilterSettings = Lens.lens (\H265FilterSettings' {temporalFilterSettings} -> temporalFilterSettings) (\s@H265FilterSettings' {} a -> s {temporalFilterSettings = a} :: H265FilterSettings)

instance Core.FromJSON H265FilterSettings where
  parseJSON =
    Core.withObject
      "H265FilterSettings"
      ( \x ->
          H265FilterSettings'
            Core.<$> (x Core..:? "temporalFilterSettings")
      )

instance Core.Hashable H265FilterSettings

instance Core.NFData H265FilterSettings

instance Core.ToJSON H265FilterSettings where
  toJSON H265FilterSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("temporalFilterSettings" Core..=)
              Core.<$> temporalFilterSettings
          ]
      )
