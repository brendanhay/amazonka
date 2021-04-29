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
-- Module      : Network.AWS.MediaPackage.Types.MssPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.MssPackage where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.MssEncryption
import Network.AWS.MediaPackage.Types.StreamSelection
import qualified Network.AWS.Prelude as Prelude

-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
--
-- /See:/ 'newMssPackage' smart constructor.
data MssPackage = MssPackage'
  { streamSelection :: Prelude.Maybe StreamSelection,
    -- | The time window (in seconds) contained in each manifest.
    manifestWindowSeconds :: Prelude.Maybe Prelude.Int,
    encryption :: Prelude.Maybe MssEncryption,
    -- | The duration (in seconds) of each segment.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MssPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamSelection', 'mssPackage_streamSelection' - Undocumented member.
--
-- 'manifestWindowSeconds', 'mssPackage_manifestWindowSeconds' - The time window (in seconds) contained in each manifest.
--
-- 'encryption', 'mssPackage_encryption' - Undocumented member.
--
-- 'segmentDurationSeconds', 'mssPackage_segmentDurationSeconds' - The duration (in seconds) of each segment.
newMssPackage ::
  MssPackage
newMssPackage =
  MssPackage'
    { streamSelection = Prelude.Nothing,
      manifestWindowSeconds = Prelude.Nothing,
      encryption = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing
    }

-- | Undocumented member.
mssPackage_streamSelection :: Lens.Lens' MssPackage (Prelude.Maybe StreamSelection)
mssPackage_streamSelection = Lens.lens (\MssPackage' {streamSelection} -> streamSelection) (\s@MssPackage' {} a -> s {streamSelection = a} :: MssPackage)

-- | The time window (in seconds) contained in each manifest.
mssPackage_manifestWindowSeconds :: Lens.Lens' MssPackage (Prelude.Maybe Prelude.Int)
mssPackage_manifestWindowSeconds = Lens.lens (\MssPackage' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@MssPackage' {} a -> s {manifestWindowSeconds = a} :: MssPackage)

-- | Undocumented member.
mssPackage_encryption :: Lens.Lens' MssPackage (Prelude.Maybe MssEncryption)
mssPackage_encryption = Lens.lens (\MssPackage' {encryption} -> encryption) (\s@MssPackage' {} a -> s {encryption = a} :: MssPackage)

-- | The duration (in seconds) of each segment.
mssPackage_segmentDurationSeconds :: Lens.Lens' MssPackage (Prelude.Maybe Prelude.Int)
mssPackage_segmentDurationSeconds = Lens.lens (\MssPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@MssPackage' {} a -> s {segmentDurationSeconds = a} :: MssPackage)

instance Prelude.FromJSON MssPackage where
  parseJSON =
    Prelude.withObject
      "MssPackage"
      ( \x ->
          MssPackage'
            Prelude.<$> (x Prelude..:? "streamSelection")
            Prelude.<*> (x Prelude..:? "manifestWindowSeconds")
            Prelude.<*> (x Prelude..:? "encryption")
            Prelude.<*> (x Prelude..:? "segmentDurationSeconds")
      )

instance Prelude.Hashable MssPackage

instance Prelude.NFData MssPackage

instance Prelude.ToJSON MssPackage where
  toJSON MssPackage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("streamSelection" Prelude..=)
              Prelude.<$> streamSelection,
            ("manifestWindowSeconds" Prelude..=)
              Prelude.<$> manifestWindowSeconds,
            ("encryption" Prelude..=) Prelude.<$> encryption,
            ("segmentDurationSeconds" Prelude..=)
              Prelude.<$> segmentDurationSeconds
          ]
      )
