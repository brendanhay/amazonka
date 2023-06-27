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
-- Module      : Amazonka.NetworkManager.Types.Bandwidth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Bandwidth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes bandwidth information.
--
-- /See:/ 'newBandwidth' smart constructor.
data Bandwidth = Bandwidth'
  { -- | Download speed in Mbps.
    downloadSpeed :: Prelude.Maybe Prelude.Int,
    -- | Upload speed in Mbps.
    uploadSpeed :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Bandwidth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downloadSpeed', 'bandwidth_downloadSpeed' - Download speed in Mbps.
--
-- 'uploadSpeed', 'bandwidth_uploadSpeed' - Upload speed in Mbps.
newBandwidth ::
  Bandwidth
newBandwidth =
  Bandwidth'
    { downloadSpeed = Prelude.Nothing,
      uploadSpeed = Prelude.Nothing
    }

-- | Download speed in Mbps.
bandwidth_downloadSpeed :: Lens.Lens' Bandwidth (Prelude.Maybe Prelude.Int)
bandwidth_downloadSpeed = Lens.lens (\Bandwidth' {downloadSpeed} -> downloadSpeed) (\s@Bandwidth' {} a -> s {downloadSpeed = a} :: Bandwidth)

-- | Upload speed in Mbps.
bandwidth_uploadSpeed :: Lens.Lens' Bandwidth (Prelude.Maybe Prelude.Int)
bandwidth_uploadSpeed = Lens.lens (\Bandwidth' {uploadSpeed} -> uploadSpeed) (\s@Bandwidth' {} a -> s {uploadSpeed = a} :: Bandwidth)

instance Data.FromJSON Bandwidth where
  parseJSON =
    Data.withObject
      "Bandwidth"
      ( \x ->
          Bandwidth'
            Prelude.<$> (x Data..:? "DownloadSpeed")
            Prelude.<*> (x Data..:? "UploadSpeed")
      )

instance Prelude.Hashable Bandwidth where
  hashWithSalt _salt Bandwidth' {..} =
    _salt
      `Prelude.hashWithSalt` downloadSpeed
      `Prelude.hashWithSalt` uploadSpeed

instance Prelude.NFData Bandwidth where
  rnf Bandwidth' {..} =
    Prelude.rnf downloadSpeed
      `Prelude.seq` Prelude.rnf uploadSpeed

instance Data.ToJSON Bandwidth where
  toJSON Bandwidth' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DownloadSpeed" Data..=) Prelude.<$> downloadSpeed,
            ("UploadSpeed" Data..=) Prelude.<$> uploadSpeed
          ]
      )
