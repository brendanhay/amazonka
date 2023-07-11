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
-- Module      : Amazonka.MediaTailor.Types.HlsPlaylistSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.HlsPlaylistSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | HLS playlist configuration parameters.
--
-- /See:/ 'newHlsPlaylistSettings' smart constructor.
data HlsPlaylistSettings = HlsPlaylistSettings'
  { -- | The total duration (in seconds) of each manifest. Minimum value: @30@
    -- seconds. Maximum value: @3600@ seconds.
    manifestWindowSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsPlaylistSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestWindowSeconds', 'hlsPlaylistSettings_manifestWindowSeconds' - The total duration (in seconds) of each manifest. Minimum value: @30@
-- seconds. Maximum value: @3600@ seconds.
newHlsPlaylistSettings ::
  HlsPlaylistSettings
newHlsPlaylistSettings =
  HlsPlaylistSettings'
    { manifestWindowSeconds =
        Prelude.Nothing
    }

-- | The total duration (in seconds) of each manifest. Minimum value: @30@
-- seconds. Maximum value: @3600@ seconds.
hlsPlaylistSettings_manifestWindowSeconds :: Lens.Lens' HlsPlaylistSettings (Prelude.Maybe Prelude.Int)
hlsPlaylistSettings_manifestWindowSeconds = Lens.lens (\HlsPlaylistSettings' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@HlsPlaylistSettings' {} a -> s {manifestWindowSeconds = a} :: HlsPlaylistSettings)

instance Data.FromJSON HlsPlaylistSettings where
  parseJSON =
    Data.withObject
      "HlsPlaylistSettings"
      ( \x ->
          HlsPlaylistSettings'
            Prelude.<$> (x Data..:? "ManifestWindowSeconds")
      )

instance Prelude.Hashable HlsPlaylistSettings where
  hashWithSalt _salt HlsPlaylistSettings' {..} =
    _salt `Prelude.hashWithSalt` manifestWindowSeconds

instance Prelude.NFData HlsPlaylistSettings where
  rnf HlsPlaylistSettings' {..} =
    Prelude.rnf manifestWindowSeconds

instance Data.ToJSON HlsPlaylistSettings where
  toJSON HlsPlaylistSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ManifestWindowSeconds" Data..=)
              Prelude.<$> manifestWindowSeconds
          ]
      )
