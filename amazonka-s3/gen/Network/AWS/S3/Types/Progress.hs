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
-- Module      : Network.AWS.S3.Types.Progress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Progress where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | This data type contains information about progress of an operation.
--
-- /See:/ 'newProgress' smart constructor.
data Progress = Progress'
  { -- | The current number of object bytes scanned.
    bytesScanned :: Prelude.Maybe Prelude.Integer,
    -- | The current number of uncompressed object bytes processed.
    bytesProcessed :: Prelude.Maybe Prelude.Integer,
    -- | The current number of bytes of records payload data returned.
    bytesReturned :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { bytesScanned = Prelude.Nothing,
      bytesProcessed = Prelude.Nothing,
      bytesReturned = Prelude.Nothing
    }

-- | The current number of object bytes scanned.
progress_bytesScanned :: Lens.Lens' Progress (Prelude.Maybe Prelude.Integer)
progress_bytesScanned = Lens.lens (\Progress' {bytesScanned} -> bytesScanned) (\s@Progress' {} a -> s {bytesScanned = a} :: Progress)

-- | The current number of uncompressed object bytes processed.
progress_bytesProcessed :: Lens.Lens' Progress (Prelude.Maybe Prelude.Integer)
progress_bytesProcessed = Lens.lens (\Progress' {bytesProcessed} -> bytesProcessed) (\s@Progress' {} a -> s {bytesProcessed = a} :: Progress)

-- | The current number of bytes of records payload data returned.
progress_bytesReturned :: Lens.Lens' Progress (Prelude.Maybe Prelude.Integer)
progress_bytesReturned = Lens.lens (\Progress' {bytesReturned} -> bytesReturned) (\s@Progress' {} a -> s {bytesReturned = a} :: Progress)

instance Prelude.FromXML Progress where
  parseXML x =
    Progress'
      Prelude.<$> (x Prelude..@? "BytesScanned")
      Prelude.<*> (x Prelude..@? "BytesProcessed")
      Prelude.<*> (x Prelude..@? "BytesReturned")

instance Prelude.Hashable Progress

instance Prelude.NFData Progress
