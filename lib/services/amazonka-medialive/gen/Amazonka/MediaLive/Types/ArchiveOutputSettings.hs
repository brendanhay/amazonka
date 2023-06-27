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
-- Module      : Amazonka.MediaLive.Types.ArchiveOutputSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ArchiveOutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.ArchiveContainerSettings
import qualified Amazonka.Prelude as Prelude

-- | Archive Output Settings
--
-- /See:/ 'newArchiveOutputSettings' smart constructor.
data ArchiveOutputSettings = ArchiveOutputSettings'
  { -- | Output file extension. If excluded, this will be auto-selected from the
    -- container type.
    extension :: Prelude.Maybe Prelude.Text,
    -- | String concatenated to the end of the destination filename. Required for
    -- multiple outputs of the same type.
    nameModifier :: Prelude.Maybe Prelude.Text,
    -- | Settings specific to the container type of the file.
    containerSettings :: ArchiveContainerSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveOutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extension', 'archiveOutputSettings_extension' - Output file extension. If excluded, this will be auto-selected from the
-- container type.
--
-- 'nameModifier', 'archiveOutputSettings_nameModifier' - String concatenated to the end of the destination filename. Required for
-- multiple outputs of the same type.
--
-- 'containerSettings', 'archiveOutputSettings_containerSettings' - Settings specific to the container type of the file.
newArchiveOutputSettings ::
  -- | 'containerSettings'
  ArchiveContainerSettings ->
  ArchiveOutputSettings
newArchiveOutputSettings pContainerSettings_ =
  ArchiveOutputSettings'
    { extension = Prelude.Nothing,
      nameModifier = Prelude.Nothing,
      containerSettings = pContainerSettings_
    }

-- | Output file extension. If excluded, this will be auto-selected from the
-- container type.
archiveOutputSettings_extension :: Lens.Lens' ArchiveOutputSettings (Prelude.Maybe Prelude.Text)
archiveOutputSettings_extension = Lens.lens (\ArchiveOutputSettings' {extension} -> extension) (\s@ArchiveOutputSettings' {} a -> s {extension = a} :: ArchiveOutputSettings)

-- | String concatenated to the end of the destination filename. Required for
-- multiple outputs of the same type.
archiveOutputSettings_nameModifier :: Lens.Lens' ArchiveOutputSettings (Prelude.Maybe Prelude.Text)
archiveOutputSettings_nameModifier = Lens.lens (\ArchiveOutputSettings' {nameModifier} -> nameModifier) (\s@ArchiveOutputSettings' {} a -> s {nameModifier = a} :: ArchiveOutputSettings)

-- | Settings specific to the container type of the file.
archiveOutputSettings_containerSettings :: Lens.Lens' ArchiveOutputSettings ArchiveContainerSettings
archiveOutputSettings_containerSettings = Lens.lens (\ArchiveOutputSettings' {containerSettings} -> containerSettings) (\s@ArchiveOutputSettings' {} a -> s {containerSettings = a} :: ArchiveOutputSettings)

instance Data.FromJSON ArchiveOutputSettings where
  parseJSON =
    Data.withObject
      "ArchiveOutputSettings"
      ( \x ->
          ArchiveOutputSettings'
            Prelude.<$> (x Data..:? "extension")
            Prelude.<*> (x Data..:? "nameModifier")
            Prelude.<*> (x Data..: "containerSettings")
      )

instance Prelude.Hashable ArchiveOutputSettings where
  hashWithSalt _salt ArchiveOutputSettings' {..} =
    _salt
      `Prelude.hashWithSalt` extension
      `Prelude.hashWithSalt` nameModifier
      `Prelude.hashWithSalt` containerSettings

instance Prelude.NFData ArchiveOutputSettings where
  rnf ArchiveOutputSettings' {..} =
    Prelude.rnf extension
      `Prelude.seq` Prelude.rnf nameModifier
      `Prelude.seq` Prelude.rnf containerSettings

instance Data.ToJSON ArchiveOutputSettings where
  toJSON ArchiveOutputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("extension" Data..=) Prelude.<$> extension,
            ("nameModifier" Data..=) Prelude.<$> nameModifier,
            Prelude.Just
              ("containerSettings" Data..= containerSettings)
          ]
      )
