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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.MssEncryption
import Network.AWS.MediaPackage.Types.StreamSelection

-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
--
-- /See:/ 'newMssPackage' smart constructor.
data MssPackage = MssPackage'
  { streamSelection :: Core.Maybe StreamSelection,
    -- | The time window (in seconds) contained in each manifest.
    manifestWindowSeconds :: Core.Maybe Core.Int,
    encryption :: Core.Maybe MssEncryption,
    -- | The duration (in seconds) of each segment.
    segmentDurationSeconds :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { streamSelection = Core.Nothing,
      manifestWindowSeconds = Core.Nothing,
      encryption = Core.Nothing,
      segmentDurationSeconds = Core.Nothing
    }

-- | Undocumented member.
mssPackage_streamSelection :: Lens.Lens' MssPackage (Core.Maybe StreamSelection)
mssPackage_streamSelection = Lens.lens (\MssPackage' {streamSelection} -> streamSelection) (\s@MssPackage' {} a -> s {streamSelection = a} :: MssPackage)

-- | The time window (in seconds) contained in each manifest.
mssPackage_manifestWindowSeconds :: Lens.Lens' MssPackage (Core.Maybe Core.Int)
mssPackage_manifestWindowSeconds = Lens.lens (\MssPackage' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@MssPackage' {} a -> s {manifestWindowSeconds = a} :: MssPackage)

-- | Undocumented member.
mssPackage_encryption :: Lens.Lens' MssPackage (Core.Maybe MssEncryption)
mssPackage_encryption = Lens.lens (\MssPackage' {encryption} -> encryption) (\s@MssPackage' {} a -> s {encryption = a} :: MssPackage)

-- | The duration (in seconds) of each segment.
mssPackage_segmentDurationSeconds :: Lens.Lens' MssPackage (Core.Maybe Core.Int)
mssPackage_segmentDurationSeconds = Lens.lens (\MssPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@MssPackage' {} a -> s {segmentDurationSeconds = a} :: MssPackage)

instance Core.FromJSON MssPackage where
  parseJSON =
    Core.withObject
      "MssPackage"
      ( \x ->
          MssPackage'
            Core.<$> (x Core..:? "streamSelection")
            Core.<*> (x Core..:? "manifestWindowSeconds")
            Core.<*> (x Core..:? "encryption")
            Core.<*> (x Core..:? "segmentDurationSeconds")
      )

instance Core.Hashable MssPackage

instance Core.NFData MssPackage

instance Core.ToJSON MssPackage where
  toJSON MssPackage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("streamSelection" Core..=)
              Core.<$> streamSelection,
            ("manifestWindowSeconds" Core..=)
              Core.<$> manifestWindowSeconds,
            ("encryption" Core..=) Core.<$> encryption,
            ("segmentDurationSeconds" Core..=)
              Core.<$> segmentDurationSeconds
          ]
      )
