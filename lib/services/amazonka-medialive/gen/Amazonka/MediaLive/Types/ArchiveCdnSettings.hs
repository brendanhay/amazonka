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
-- Module      : Amazonka.MediaLive.Types.ArchiveCdnSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ArchiveCdnSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.ArchiveS3Settings
import qualified Amazonka.Prelude as Prelude

-- | Archive Cdn Settings
--
-- /See:/ 'newArchiveCdnSettings' smart constructor.
data ArchiveCdnSettings = ArchiveCdnSettings'
  { archiveS3Settings :: Prelude.Maybe ArchiveS3Settings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveCdnSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveS3Settings', 'archiveCdnSettings_archiveS3Settings' - Undocumented member.
newArchiveCdnSettings ::
  ArchiveCdnSettings
newArchiveCdnSettings =
  ArchiveCdnSettings'
    { archiveS3Settings =
        Prelude.Nothing
    }

-- | Undocumented member.
archiveCdnSettings_archiveS3Settings :: Lens.Lens' ArchiveCdnSettings (Prelude.Maybe ArchiveS3Settings)
archiveCdnSettings_archiveS3Settings = Lens.lens (\ArchiveCdnSettings' {archiveS3Settings} -> archiveS3Settings) (\s@ArchiveCdnSettings' {} a -> s {archiveS3Settings = a} :: ArchiveCdnSettings)

instance Core.FromJSON ArchiveCdnSettings where
  parseJSON =
    Core.withObject
      "ArchiveCdnSettings"
      ( \x ->
          ArchiveCdnSettings'
            Prelude.<$> (x Core..:? "archiveS3Settings")
      )

instance Prelude.Hashable ArchiveCdnSettings where
  hashWithSalt _salt ArchiveCdnSettings' {..} =
    _salt `Prelude.hashWithSalt` archiveS3Settings

instance Prelude.NFData ArchiveCdnSettings where
  rnf ArchiveCdnSettings' {..} =
    Prelude.rnf archiveS3Settings

instance Core.ToJSON ArchiveCdnSettings where
  toJSON ArchiveCdnSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("archiveS3Settings" Core..=)
              Prelude.<$> archiveS3Settings
          ]
      )
