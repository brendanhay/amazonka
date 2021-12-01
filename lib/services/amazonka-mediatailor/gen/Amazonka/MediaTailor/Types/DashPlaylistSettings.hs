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
-- Module      : Amazonka.MediaTailor.Types.DashPlaylistSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.DashPlaylistSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Dash manifest configuration parameters.
--
-- /See:/ 'newDashPlaylistSettings' smart constructor.
data DashPlaylistSettings = DashPlaylistSettings'
  { -- | Minimum amount of content (measured in seconds) that a player must keep
    -- available in the buffer. Minimum value: 2 seconds. Maximum value: 60
    -- seconds.
    minBufferTimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | Minimum amount of time (in seconds) that the player should wait before
    -- requesting updates to the manifest. Minimum value: 2 seconds. Maximum
    -- value: 60 seconds.
    minUpdatePeriodSeconds :: Prelude.Maybe Prelude.Int,
    -- | Amount of time (in seconds) that the player should be from the live
    -- point at the end of the manifest. Minimum value: 2 seconds. Maximum
    -- value: 60 seconds.
    suggestedPresentationDelaySeconds :: Prelude.Maybe Prelude.Int,
    -- | The total duration (in seconds) of each manifest. Minimum value: 30
    -- seconds. Maximum value: 3600 seconds.
    manifestWindowSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashPlaylistSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minBufferTimeSeconds', 'dashPlaylistSettings_minBufferTimeSeconds' - Minimum amount of content (measured in seconds) that a player must keep
-- available in the buffer. Minimum value: 2 seconds. Maximum value: 60
-- seconds.
--
-- 'minUpdatePeriodSeconds', 'dashPlaylistSettings_minUpdatePeriodSeconds' - Minimum amount of time (in seconds) that the player should wait before
-- requesting updates to the manifest. Minimum value: 2 seconds. Maximum
-- value: 60 seconds.
--
-- 'suggestedPresentationDelaySeconds', 'dashPlaylistSettings_suggestedPresentationDelaySeconds' - Amount of time (in seconds) that the player should be from the live
-- point at the end of the manifest. Minimum value: 2 seconds. Maximum
-- value: 60 seconds.
--
-- 'manifestWindowSeconds', 'dashPlaylistSettings_manifestWindowSeconds' - The total duration (in seconds) of each manifest. Minimum value: 30
-- seconds. Maximum value: 3600 seconds.
newDashPlaylistSettings ::
  DashPlaylistSettings
newDashPlaylistSettings =
  DashPlaylistSettings'
    { minBufferTimeSeconds =
        Prelude.Nothing,
      minUpdatePeriodSeconds = Prelude.Nothing,
      suggestedPresentationDelaySeconds = Prelude.Nothing,
      manifestWindowSeconds = Prelude.Nothing
    }

-- | Minimum amount of content (measured in seconds) that a player must keep
-- available in the buffer. Minimum value: 2 seconds. Maximum value: 60
-- seconds.
dashPlaylistSettings_minBufferTimeSeconds :: Lens.Lens' DashPlaylistSettings (Prelude.Maybe Prelude.Int)
dashPlaylistSettings_minBufferTimeSeconds = Lens.lens (\DashPlaylistSettings' {minBufferTimeSeconds} -> minBufferTimeSeconds) (\s@DashPlaylistSettings' {} a -> s {minBufferTimeSeconds = a} :: DashPlaylistSettings)

-- | Minimum amount of time (in seconds) that the player should wait before
-- requesting updates to the manifest. Minimum value: 2 seconds. Maximum
-- value: 60 seconds.
dashPlaylistSettings_minUpdatePeriodSeconds :: Lens.Lens' DashPlaylistSettings (Prelude.Maybe Prelude.Int)
dashPlaylistSettings_minUpdatePeriodSeconds = Lens.lens (\DashPlaylistSettings' {minUpdatePeriodSeconds} -> minUpdatePeriodSeconds) (\s@DashPlaylistSettings' {} a -> s {minUpdatePeriodSeconds = a} :: DashPlaylistSettings)

-- | Amount of time (in seconds) that the player should be from the live
-- point at the end of the manifest. Minimum value: 2 seconds. Maximum
-- value: 60 seconds.
dashPlaylistSettings_suggestedPresentationDelaySeconds :: Lens.Lens' DashPlaylistSettings (Prelude.Maybe Prelude.Int)
dashPlaylistSettings_suggestedPresentationDelaySeconds = Lens.lens (\DashPlaylistSettings' {suggestedPresentationDelaySeconds} -> suggestedPresentationDelaySeconds) (\s@DashPlaylistSettings' {} a -> s {suggestedPresentationDelaySeconds = a} :: DashPlaylistSettings)

-- | The total duration (in seconds) of each manifest. Minimum value: 30
-- seconds. Maximum value: 3600 seconds.
dashPlaylistSettings_manifestWindowSeconds :: Lens.Lens' DashPlaylistSettings (Prelude.Maybe Prelude.Int)
dashPlaylistSettings_manifestWindowSeconds = Lens.lens (\DashPlaylistSettings' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@DashPlaylistSettings' {} a -> s {manifestWindowSeconds = a} :: DashPlaylistSettings)

instance Core.FromJSON DashPlaylistSettings where
  parseJSON =
    Core.withObject
      "DashPlaylistSettings"
      ( \x ->
          DashPlaylistSettings'
            Prelude.<$> (x Core..:? "MinBufferTimeSeconds")
            Prelude.<*> (x Core..:? "MinUpdatePeriodSeconds")
            Prelude.<*> (x Core..:? "SuggestedPresentationDelaySeconds")
            Prelude.<*> (x Core..:? "ManifestWindowSeconds")
      )

instance Prelude.Hashable DashPlaylistSettings where
  hashWithSalt salt' DashPlaylistSettings' {..} =
    salt' `Prelude.hashWithSalt` manifestWindowSeconds
      `Prelude.hashWithSalt` suggestedPresentationDelaySeconds
      `Prelude.hashWithSalt` minUpdatePeriodSeconds
      `Prelude.hashWithSalt` minBufferTimeSeconds

instance Prelude.NFData DashPlaylistSettings where
  rnf DashPlaylistSettings' {..} =
    Prelude.rnf minBufferTimeSeconds
      `Prelude.seq` Prelude.rnf manifestWindowSeconds
      `Prelude.seq` Prelude.rnf suggestedPresentationDelaySeconds
      `Prelude.seq` Prelude.rnf minUpdatePeriodSeconds

instance Core.ToJSON DashPlaylistSettings where
  toJSON DashPlaylistSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MinBufferTimeSeconds" Core..=)
              Prelude.<$> minBufferTimeSeconds,
            ("MinUpdatePeriodSeconds" Core..=)
              Prelude.<$> minUpdatePeriodSeconds,
            ("SuggestedPresentationDelaySeconds" Core..=)
              Prelude.<$> suggestedPresentationDelaySeconds,
            ("ManifestWindowSeconds" Core..=)
              Prelude.<$> manifestWindowSeconds
          ]
      )
