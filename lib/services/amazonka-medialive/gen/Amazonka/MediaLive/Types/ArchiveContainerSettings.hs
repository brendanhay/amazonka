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
-- Module      : Amazonka.MediaLive.Types.ArchiveContainerSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ArchiveContainerSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.M2tsSettings
import Amazonka.MediaLive.Types.RawSettings
import qualified Amazonka.Prelude as Prelude

-- | Archive Container Settings
--
-- /See:/ 'newArchiveContainerSettings' smart constructor.
data ArchiveContainerSettings = ArchiveContainerSettings'
  { m2tsSettings :: Prelude.Maybe M2tsSettings,
    rawSettings :: Prelude.Maybe RawSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveContainerSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'm2tsSettings', 'archiveContainerSettings_m2tsSettings' - Undocumented member.
--
-- 'rawSettings', 'archiveContainerSettings_rawSettings' - Undocumented member.
newArchiveContainerSettings ::
  ArchiveContainerSettings
newArchiveContainerSettings =
  ArchiveContainerSettings'
    { m2tsSettings =
        Prelude.Nothing,
      rawSettings = Prelude.Nothing
    }

-- | Undocumented member.
archiveContainerSettings_m2tsSettings :: Lens.Lens' ArchiveContainerSettings (Prelude.Maybe M2tsSettings)
archiveContainerSettings_m2tsSettings = Lens.lens (\ArchiveContainerSettings' {m2tsSettings} -> m2tsSettings) (\s@ArchiveContainerSettings' {} a -> s {m2tsSettings = a} :: ArchiveContainerSettings)

-- | Undocumented member.
archiveContainerSettings_rawSettings :: Lens.Lens' ArchiveContainerSettings (Prelude.Maybe RawSettings)
archiveContainerSettings_rawSettings = Lens.lens (\ArchiveContainerSettings' {rawSettings} -> rawSettings) (\s@ArchiveContainerSettings' {} a -> s {rawSettings = a} :: ArchiveContainerSettings)

instance Data.FromJSON ArchiveContainerSettings where
  parseJSON =
    Data.withObject
      "ArchiveContainerSettings"
      ( \x ->
          ArchiveContainerSettings'
            Prelude.<$> (x Data..:? "m2tsSettings")
            Prelude.<*> (x Data..:? "rawSettings")
      )

instance Prelude.Hashable ArchiveContainerSettings where
  hashWithSalt _salt ArchiveContainerSettings' {..} =
    _salt `Prelude.hashWithSalt` m2tsSettings
      `Prelude.hashWithSalt` rawSettings

instance Prelude.NFData ArchiveContainerSettings where
  rnf ArchiveContainerSettings' {..} =
    Prelude.rnf m2tsSettings
      `Prelude.seq` Prelude.rnf rawSettings

instance Data.ToJSON ArchiveContainerSettings where
  toJSON ArchiveContainerSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("m2tsSettings" Data..=) Prelude.<$> m2tsSettings,
            ("rawSettings" Data..=) Prelude.<$> rawSettings
          ]
      )
