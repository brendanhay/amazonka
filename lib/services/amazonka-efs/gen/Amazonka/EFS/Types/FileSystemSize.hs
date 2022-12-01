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
-- Module      : Amazonka.EFS.Types.FileSystemSize
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.FileSystemSize where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The latest known metered size (in bytes) of data stored in the file
-- system, in its @Value@ field, and the time at which that size was
-- determined in its @Timestamp@ field. The value doesn\'t represent the
-- size of a consistent snapshot of the file system, but it is eventually
-- consistent when there are no writes to the file system. That is, the
-- value represents the actual size only if the file system is not modified
-- for a period longer than a couple of hours. Otherwise, the value is not
-- necessarily the exact size the file system was at any instant in time.
--
-- /See:/ 'newFileSystemSize' smart constructor.
data FileSystemSize = FileSystemSize'
  { -- | The time at which the size of data, returned in the @Value@ field, was
    -- determined. The value is the integer number of seconds since
    -- 1970-01-01T00:00:00Z.
    timestamp :: Prelude.Maybe Core.POSIX,
    -- | The latest known metered size (in bytes) of data stored in the Standard
    -- storage class.
    valueInStandard :: Prelude.Maybe Prelude.Natural,
    -- | The latest known metered size (in bytes) of data stored in the
    -- Infrequent Access storage class.
    valueInIA :: Prelude.Maybe Prelude.Natural,
    -- | The latest known metered size (in bytes) of data stored in the file
    -- system.
    value :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemSize' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'fileSystemSize_timestamp' - The time at which the size of data, returned in the @Value@ field, was
-- determined. The value is the integer number of seconds since
-- 1970-01-01T00:00:00Z.
--
-- 'valueInStandard', 'fileSystemSize_valueInStandard' - The latest known metered size (in bytes) of data stored in the Standard
-- storage class.
--
-- 'valueInIA', 'fileSystemSize_valueInIA' - The latest known metered size (in bytes) of data stored in the
-- Infrequent Access storage class.
--
-- 'value', 'fileSystemSize_value' - The latest known metered size (in bytes) of data stored in the file
-- system.
newFileSystemSize ::
  -- | 'value'
  Prelude.Natural ->
  FileSystemSize
newFileSystemSize pValue_ =
  FileSystemSize'
    { timestamp = Prelude.Nothing,
      valueInStandard = Prelude.Nothing,
      valueInIA = Prelude.Nothing,
      value = pValue_
    }

-- | The time at which the size of data, returned in the @Value@ field, was
-- determined. The value is the integer number of seconds since
-- 1970-01-01T00:00:00Z.
fileSystemSize_timestamp :: Lens.Lens' FileSystemSize (Prelude.Maybe Prelude.UTCTime)
fileSystemSize_timestamp = Lens.lens (\FileSystemSize' {timestamp} -> timestamp) (\s@FileSystemSize' {} a -> s {timestamp = a} :: FileSystemSize) Prelude.. Lens.mapping Core._Time

-- | The latest known metered size (in bytes) of data stored in the Standard
-- storage class.
fileSystemSize_valueInStandard :: Lens.Lens' FileSystemSize (Prelude.Maybe Prelude.Natural)
fileSystemSize_valueInStandard = Lens.lens (\FileSystemSize' {valueInStandard} -> valueInStandard) (\s@FileSystemSize' {} a -> s {valueInStandard = a} :: FileSystemSize)

-- | The latest known metered size (in bytes) of data stored in the
-- Infrequent Access storage class.
fileSystemSize_valueInIA :: Lens.Lens' FileSystemSize (Prelude.Maybe Prelude.Natural)
fileSystemSize_valueInIA = Lens.lens (\FileSystemSize' {valueInIA} -> valueInIA) (\s@FileSystemSize' {} a -> s {valueInIA = a} :: FileSystemSize)

-- | The latest known metered size (in bytes) of data stored in the file
-- system.
fileSystemSize_value :: Lens.Lens' FileSystemSize Prelude.Natural
fileSystemSize_value = Lens.lens (\FileSystemSize' {value} -> value) (\s@FileSystemSize' {} a -> s {value = a} :: FileSystemSize)

instance Core.FromJSON FileSystemSize where
  parseJSON =
    Core.withObject
      "FileSystemSize"
      ( \x ->
          FileSystemSize'
            Prelude.<$> (x Core..:? "Timestamp")
            Prelude.<*> (x Core..:? "ValueInStandard")
            Prelude.<*> (x Core..:? "ValueInIA")
            Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable FileSystemSize where
  hashWithSalt _salt FileSystemSize' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` valueInStandard
      `Prelude.hashWithSalt` valueInIA
      `Prelude.hashWithSalt` value

instance Prelude.NFData FileSystemSize where
  rnf FileSystemSize' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf valueInStandard
      `Prelude.seq` Prelude.rnf valueInIA
      `Prelude.seq` Prelude.rnf value
