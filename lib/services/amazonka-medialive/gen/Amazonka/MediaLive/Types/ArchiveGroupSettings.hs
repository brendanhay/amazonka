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
-- Module      : Amazonka.MediaLive.Types.ArchiveGroupSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ArchiveGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.ArchiveCdnSettings
import Amazonka.MediaLive.Types.OutputLocationRef
import qualified Amazonka.Prelude as Prelude

-- | Archive Group Settings
--
-- /See:/ 'newArchiveGroupSettings' smart constructor.
data ArchiveGroupSettings = ArchiveGroupSettings'
  { -- | Parameters that control interactions with the CDN.
    archiveCdnSettings :: Prelude.Maybe ArchiveCdnSettings,
    -- | Number of seconds to write to archive file before closing and starting a
    -- new one.
    rolloverInterval :: Prelude.Maybe Prelude.Natural,
    -- | A directory and base filename where archive files should be written.
    destination :: OutputLocationRef
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveCdnSettings', 'archiveGroupSettings_archiveCdnSettings' - Parameters that control interactions with the CDN.
--
-- 'rolloverInterval', 'archiveGroupSettings_rolloverInterval' - Number of seconds to write to archive file before closing and starting a
-- new one.
--
-- 'destination', 'archiveGroupSettings_destination' - A directory and base filename where archive files should be written.
newArchiveGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  ArchiveGroupSettings
newArchiveGroupSettings pDestination_ =
  ArchiveGroupSettings'
    { archiveCdnSettings =
        Prelude.Nothing,
      rolloverInterval = Prelude.Nothing,
      destination = pDestination_
    }

-- | Parameters that control interactions with the CDN.
archiveGroupSettings_archiveCdnSettings :: Lens.Lens' ArchiveGroupSettings (Prelude.Maybe ArchiveCdnSettings)
archiveGroupSettings_archiveCdnSettings = Lens.lens (\ArchiveGroupSettings' {archiveCdnSettings} -> archiveCdnSettings) (\s@ArchiveGroupSettings' {} a -> s {archiveCdnSettings = a} :: ArchiveGroupSettings)

-- | Number of seconds to write to archive file before closing and starting a
-- new one.
archiveGroupSettings_rolloverInterval :: Lens.Lens' ArchiveGroupSettings (Prelude.Maybe Prelude.Natural)
archiveGroupSettings_rolloverInterval = Lens.lens (\ArchiveGroupSettings' {rolloverInterval} -> rolloverInterval) (\s@ArchiveGroupSettings' {} a -> s {rolloverInterval = a} :: ArchiveGroupSettings)

-- | A directory and base filename where archive files should be written.
archiveGroupSettings_destination :: Lens.Lens' ArchiveGroupSettings OutputLocationRef
archiveGroupSettings_destination = Lens.lens (\ArchiveGroupSettings' {destination} -> destination) (\s@ArchiveGroupSettings' {} a -> s {destination = a} :: ArchiveGroupSettings)

instance Core.FromJSON ArchiveGroupSettings where
  parseJSON =
    Core.withObject
      "ArchiveGroupSettings"
      ( \x ->
          ArchiveGroupSettings'
            Prelude.<$> (x Core..:? "archiveCdnSettings")
            Prelude.<*> (x Core..:? "rolloverInterval")
            Prelude.<*> (x Core..: "destination")
      )

instance Prelude.Hashable ArchiveGroupSettings where
  hashWithSalt _salt ArchiveGroupSettings' {..} =
    _salt `Prelude.hashWithSalt` archiveCdnSettings
      `Prelude.hashWithSalt` rolloverInterval
      `Prelude.hashWithSalt` destination

instance Prelude.NFData ArchiveGroupSettings where
  rnf ArchiveGroupSettings' {..} =
    Prelude.rnf archiveCdnSettings
      `Prelude.seq` Prelude.rnf rolloverInterval
      `Prelude.seq` Prelude.rnf destination

instance Core.ToJSON ArchiveGroupSettings where
  toJSON ArchiveGroupSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("archiveCdnSettings" Core..=)
              Prelude.<$> archiveCdnSettings,
            ("rolloverInterval" Core..=)
              Prelude.<$> rolloverInterval,
            Prelude.Just ("destination" Core..= destination)
          ]
      )
