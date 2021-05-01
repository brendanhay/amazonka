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
-- Module      : Network.AWS.MediaLive.Types.ArchiveContainerSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ArchiveContainerSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.M2tsSettings
import Network.AWS.MediaLive.Types.RawSettings
import qualified Network.AWS.Prelude as Prelude

-- | Archive Container Settings
--
-- /See:/ 'newArchiveContainerSettings' smart constructor.
data ArchiveContainerSettings = ArchiveContainerSettings'
  { rawSettings :: Prelude.Maybe RawSettings,
    m2tsSettings :: Prelude.Maybe M2tsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ArchiveContainerSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rawSettings', 'archiveContainerSettings_rawSettings' - Undocumented member.
--
-- 'm2tsSettings', 'archiveContainerSettings_m2tsSettings' - Undocumented member.
newArchiveContainerSettings ::
  ArchiveContainerSettings
newArchiveContainerSettings =
  ArchiveContainerSettings'
    { rawSettings =
        Prelude.Nothing,
      m2tsSettings = Prelude.Nothing
    }

-- | Undocumented member.
archiveContainerSettings_rawSettings :: Lens.Lens' ArchiveContainerSettings (Prelude.Maybe RawSettings)
archiveContainerSettings_rawSettings = Lens.lens (\ArchiveContainerSettings' {rawSettings} -> rawSettings) (\s@ArchiveContainerSettings' {} a -> s {rawSettings = a} :: ArchiveContainerSettings)

-- | Undocumented member.
archiveContainerSettings_m2tsSettings :: Lens.Lens' ArchiveContainerSettings (Prelude.Maybe M2tsSettings)
archiveContainerSettings_m2tsSettings = Lens.lens (\ArchiveContainerSettings' {m2tsSettings} -> m2tsSettings) (\s@ArchiveContainerSettings' {} a -> s {m2tsSettings = a} :: ArchiveContainerSettings)

instance Prelude.FromJSON ArchiveContainerSettings where
  parseJSON =
    Prelude.withObject
      "ArchiveContainerSettings"
      ( \x ->
          ArchiveContainerSettings'
            Prelude.<$> (x Prelude..:? "rawSettings")
            Prelude.<*> (x Prelude..:? "m2tsSettings")
      )

instance Prelude.Hashable ArchiveContainerSettings

instance Prelude.NFData ArchiveContainerSettings

instance Prelude.ToJSON ArchiveContainerSettings where
  toJSON ArchiveContainerSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("rawSettings" Prelude..=) Prelude.<$> rawSettings,
            ("m2tsSettings" Prelude..=)
              Prelude.<$> m2tsSettings
          ]
      )
