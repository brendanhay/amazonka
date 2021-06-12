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
-- Module      : Network.AWS.S3.Types.Progress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Progress where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal

-- | This data type contains information about progress of an operation.
--
-- /See:/ 'newProgress' smart constructor.
data Progress = Progress'
  { -- | The current number of object bytes scanned.
    bytesScanned :: Core.Maybe Core.Integer,
    -- | The current number of uncompressed object bytes processed.
    bytesProcessed :: Core.Maybe Core.Integer,
    -- | The current number of bytes of records payload data returned.
    bytesReturned :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Progress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesScanned', 'progress_bytesScanned' - The current number of object bytes scanned.
--
-- 'bytesProcessed', 'progress_bytesProcessed' - The current number of uncompressed object bytes processed.
--
-- 'bytesReturned', 'progress_bytesReturned' - The current number of bytes of records payload data returned.
newProgress ::
  Progress
newProgress =
  Progress'
    { bytesScanned = Core.Nothing,
      bytesProcessed = Core.Nothing,
      bytesReturned = Core.Nothing
    }

-- | The current number of object bytes scanned.
progress_bytesScanned :: Lens.Lens' Progress (Core.Maybe Core.Integer)
progress_bytesScanned = Lens.lens (\Progress' {bytesScanned} -> bytesScanned) (\s@Progress' {} a -> s {bytesScanned = a} :: Progress)

-- | The current number of uncompressed object bytes processed.
progress_bytesProcessed :: Lens.Lens' Progress (Core.Maybe Core.Integer)
progress_bytesProcessed = Lens.lens (\Progress' {bytesProcessed} -> bytesProcessed) (\s@Progress' {} a -> s {bytesProcessed = a} :: Progress)

-- | The current number of bytes of records payload data returned.
progress_bytesReturned :: Lens.Lens' Progress (Core.Maybe Core.Integer)
progress_bytesReturned = Lens.lens (\Progress' {bytesReturned} -> bytesReturned) (\s@Progress' {} a -> s {bytesReturned = a} :: Progress)

instance Core.FromXML Progress where
  parseXML x =
    Progress'
      Core.<$> (x Core..@? "BytesScanned")
      Core.<*> (x Core..@? "BytesProcessed")
      Core.<*> (x Core..@? "BytesReturned")

instance Core.Hashable Progress

instance Core.NFData Progress
