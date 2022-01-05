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
-- Module      : Amazonka.S3.Types.Progress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Progress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | This data type contains information about progress of an operation.
--
-- /See:/ 'newProgress' smart constructor.
data Progress = Progress'
  { -- | The current number of bytes of records payload data returned.
    bytesReturned :: Prelude.Maybe Prelude.Integer,
    -- | The current number of object bytes scanned.
    bytesScanned :: Prelude.Maybe Prelude.Integer,
    -- | The current number of uncompressed object bytes processed.
    bytesProcessed :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Progress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesReturned', 'progress_bytesReturned' - The current number of bytes of records payload data returned.
--
-- 'bytesScanned', 'progress_bytesScanned' - The current number of object bytes scanned.
--
-- 'bytesProcessed', 'progress_bytesProcessed' - The current number of uncompressed object bytes processed.
newProgress ::
  Progress
newProgress =
  Progress'
    { bytesReturned = Prelude.Nothing,
      bytesScanned = Prelude.Nothing,
      bytesProcessed = Prelude.Nothing
    }

-- | The current number of bytes of records payload data returned.
progress_bytesReturned :: Lens.Lens' Progress (Prelude.Maybe Prelude.Integer)
progress_bytesReturned = Lens.lens (\Progress' {bytesReturned} -> bytesReturned) (\s@Progress' {} a -> s {bytesReturned = a} :: Progress)

-- | The current number of object bytes scanned.
progress_bytesScanned :: Lens.Lens' Progress (Prelude.Maybe Prelude.Integer)
progress_bytesScanned = Lens.lens (\Progress' {bytesScanned} -> bytesScanned) (\s@Progress' {} a -> s {bytesScanned = a} :: Progress)

-- | The current number of uncompressed object bytes processed.
progress_bytesProcessed :: Lens.Lens' Progress (Prelude.Maybe Prelude.Integer)
progress_bytesProcessed = Lens.lens (\Progress' {bytesProcessed} -> bytesProcessed) (\s@Progress' {} a -> s {bytesProcessed = a} :: Progress)

instance Core.FromXML Progress where
  parseXML x =
    Progress'
      Prelude.<$> (x Core..@? "BytesReturned")
      Prelude.<*> (x Core..@? "BytesScanned")
      Prelude.<*> (x Core..@? "BytesProcessed")

instance Prelude.Hashable Progress where
  hashWithSalt _salt Progress' {..} =
    _salt `Prelude.hashWithSalt` bytesReturned
      `Prelude.hashWithSalt` bytesScanned
      `Prelude.hashWithSalt` bytesProcessed

instance Prelude.NFData Progress where
  rnf Progress' {..} =
    Prelude.rnf bytesReturned
      `Prelude.seq` Prelude.rnf bytesScanned
      `Prelude.seq` Prelude.rnf bytesProcessed
