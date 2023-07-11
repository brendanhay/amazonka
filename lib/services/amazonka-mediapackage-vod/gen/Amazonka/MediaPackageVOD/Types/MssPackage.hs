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
-- Module      : Amazonka.MediaPackageVOD.Types.MssPackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.MssPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types.MssEncryption
import Amazonka.MediaPackageVOD.Types.MssManifest
import qualified Amazonka.Prelude as Prelude

-- | A Microsoft Smooth Streaming (MSS) PackagingConfiguration.
--
-- /See:/ 'newMssPackage' smart constructor.
data MssPackage = MssPackage'
  { encryption :: Prelude.Maybe MssEncryption,
    -- | The duration (in seconds) of each segment.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    -- | A list of MSS manifest configurations.
    mssManifests :: [MssManifest]
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
-- 'segmentDurationSeconds', 'mssPackage_segmentDurationSeconds' - The duration (in seconds) of each segment.
--
-- 'mssManifests', 'mssPackage_mssManifests' - A list of MSS manifest configurations.
newMssPackage ::
  MssPackage
newMssPackage =
  MssPackage'
    { encryption = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      mssManifests = Prelude.mempty
    }

-- | Undocumented member.
mssPackage_encryption :: Lens.Lens' MssPackage (Prelude.Maybe MssEncryption)
mssPackage_encryption = Lens.lens (\MssPackage' {encryption} -> encryption) (\s@MssPackage' {} a -> s {encryption = a} :: MssPackage)

-- | The duration (in seconds) of each segment.
mssPackage_segmentDurationSeconds :: Lens.Lens' MssPackage (Prelude.Maybe Prelude.Int)
mssPackage_segmentDurationSeconds = Lens.lens (\MssPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@MssPackage' {} a -> s {segmentDurationSeconds = a} :: MssPackage)

-- | A list of MSS manifest configurations.
mssPackage_mssManifests :: Lens.Lens' MssPackage [MssManifest]
mssPackage_mssManifests = Lens.lens (\MssPackage' {mssManifests} -> mssManifests) (\s@MssPackage' {} a -> s {mssManifests = a} :: MssPackage) Prelude.. Lens.coerced

instance Data.FromJSON MssPackage where
  parseJSON =
    Data.withObject
      "MssPackage"
      ( \x ->
          MssPackage'
            Prelude.<$> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "segmentDurationSeconds")
            Prelude.<*> (x Data..:? "mssManifests" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MssPackage where
  hashWithSalt _salt MssPackage' {..} =
    _salt
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` segmentDurationSeconds
      `Prelude.hashWithSalt` mssManifests

instance Prelude.NFData MssPackage where
  rnf MssPackage' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf mssManifests

instance Data.ToJSON MssPackage where
  toJSON MssPackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryption" Data..=) Prelude.<$> encryption,
            ("segmentDurationSeconds" Data..=)
              Prelude.<$> segmentDurationSeconds,
            Prelude.Just ("mssManifests" Data..= mssManifests)
          ]
      )
