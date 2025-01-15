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
-- Module      : Amazonka.MediaPackage.Types.MssPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.MssPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.MssEncryption
import Amazonka.MediaPackage.Types.StreamSelection
import qualified Amazonka.Prelude as Prelude

-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
--
-- /See:/ 'newMssPackage' smart constructor.
data MssPackage = MssPackage'
  { encryption :: Prelude.Maybe MssEncryption,
    -- | The time window (in seconds) contained in each manifest.
    manifestWindowSeconds :: Prelude.Maybe Prelude.Int,
    -- | The duration (in seconds) of each segment.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    streamSelection :: Prelude.Maybe StreamSelection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MssPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'mssPackage_encryption' - Undocumented member.
--
-- 'manifestWindowSeconds', 'mssPackage_manifestWindowSeconds' - The time window (in seconds) contained in each manifest.
--
-- 'segmentDurationSeconds', 'mssPackage_segmentDurationSeconds' - The duration (in seconds) of each segment.
--
-- 'streamSelection', 'mssPackage_streamSelection' - Undocumented member.
newMssPackage ::
  MssPackage
newMssPackage =
  MssPackage'
    { encryption = Prelude.Nothing,
      manifestWindowSeconds = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      streamSelection = Prelude.Nothing
    }

-- | Undocumented member.
mssPackage_encryption :: Lens.Lens' MssPackage (Prelude.Maybe MssEncryption)
mssPackage_encryption = Lens.lens (\MssPackage' {encryption} -> encryption) (\s@MssPackage' {} a -> s {encryption = a} :: MssPackage)

-- | The time window (in seconds) contained in each manifest.
mssPackage_manifestWindowSeconds :: Lens.Lens' MssPackage (Prelude.Maybe Prelude.Int)
mssPackage_manifestWindowSeconds = Lens.lens (\MssPackage' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@MssPackage' {} a -> s {manifestWindowSeconds = a} :: MssPackage)

-- | The duration (in seconds) of each segment.
mssPackage_segmentDurationSeconds :: Lens.Lens' MssPackage (Prelude.Maybe Prelude.Int)
mssPackage_segmentDurationSeconds = Lens.lens (\MssPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@MssPackage' {} a -> s {segmentDurationSeconds = a} :: MssPackage)

-- | Undocumented member.
mssPackage_streamSelection :: Lens.Lens' MssPackage (Prelude.Maybe StreamSelection)
mssPackage_streamSelection = Lens.lens (\MssPackage' {streamSelection} -> streamSelection) (\s@MssPackage' {} a -> s {streamSelection = a} :: MssPackage)

instance Data.FromJSON MssPackage where
  parseJSON =
    Data.withObject
      "MssPackage"
      ( \x ->
          MssPackage'
            Prelude.<$> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "manifestWindowSeconds")
            Prelude.<*> (x Data..:? "segmentDurationSeconds")
            Prelude.<*> (x Data..:? "streamSelection")
      )

instance Prelude.Hashable MssPackage where
  hashWithSalt _salt MssPackage' {..} =
    _salt
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` manifestWindowSeconds
      `Prelude.hashWithSalt` segmentDurationSeconds
      `Prelude.hashWithSalt` streamSelection

instance Prelude.NFData MssPackage where
  rnf MssPackage' {..} =
    Prelude.rnf encryption `Prelude.seq`
      Prelude.rnf manifestWindowSeconds `Prelude.seq`
        Prelude.rnf segmentDurationSeconds `Prelude.seq`
          Prelude.rnf streamSelection

instance Data.ToJSON MssPackage where
  toJSON MssPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryption" Data..=) Prelude.<$> encryption,
            ("manifestWindowSeconds" Data..=)
              Prelude.<$> manifestWindowSeconds,
            ("segmentDurationSeconds" Data..=)
              Prelude.<$> segmentDurationSeconds,
            ("streamSelection" Data..=)
              Prelude.<$> streamSelection
          ]
      )
