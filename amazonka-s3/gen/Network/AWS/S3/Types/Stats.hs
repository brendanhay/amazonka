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
-- Module      : Network.AWS.S3.Types.Stats
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Stats where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal

-- | Container for the stats details.
--
-- /See:/ 'newStats' smart constructor.
data Stats = Stats'
  { -- | The total number of object bytes scanned.
    bytesScanned :: Core.Maybe Core.Integer,
    -- | The total number of uncompressed object bytes processed.
    bytesProcessed :: Core.Maybe Core.Integer,
    -- | The total number of bytes of records payload data returned.
    bytesReturned :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Stats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesScanned', 'stats_bytesScanned' - The total number of object bytes scanned.
--
-- 'bytesProcessed', 'stats_bytesProcessed' - The total number of uncompressed object bytes processed.
--
-- 'bytesReturned', 'stats_bytesReturned' - The total number of bytes of records payload data returned.
newStats ::
  Stats
newStats =
  Stats'
    { bytesScanned = Core.Nothing,
      bytesProcessed = Core.Nothing,
      bytesReturned = Core.Nothing
    }

-- | The total number of object bytes scanned.
stats_bytesScanned :: Lens.Lens' Stats (Core.Maybe Core.Integer)
stats_bytesScanned = Lens.lens (\Stats' {bytesScanned} -> bytesScanned) (\s@Stats' {} a -> s {bytesScanned = a} :: Stats)

-- | The total number of uncompressed object bytes processed.
stats_bytesProcessed :: Lens.Lens' Stats (Core.Maybe Core.Integer)
stats_bytesProcessed = Lens.lens (\Stats' {bytesProcessed} -> bytesProcessed) (\s@Stats' {} a -> s {bytesProcessed = a} :: Stats)

-- | The total number of bytes of records payload data returned.
stats_bytesReturned :: Lens.Lens' Stats (Core.Maybe Core.Integer)
stats_bytesReturned = Lens.lens (\Stats' {bytesReturned} -> bytesReturned) (\s@Stats' {} a -> s {bytesReturned = a} :: Stats)

instance Core.FromXML Stats where
  parseXML x =
    Stats'
      Core.<$> (x Core..@? "BytesScanned")
      Core.<*> (x Core..@? "BytesProcessed")
      Core.<*> (x Core..@? "BytesReturned")

instance Core.Hashable Stats

instance Core.NFData Stats
