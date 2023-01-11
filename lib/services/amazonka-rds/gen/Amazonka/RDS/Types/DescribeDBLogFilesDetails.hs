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
-- Module      : Amazonka.RDS.Types.DescribeDBLogFilesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DescribeDBLogFilesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This data type is used as a response element to @DescribeDBLogFiles@.
--
-- /See:/ 'newDescribeDBLogFilesDetails' smart constructor.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails'
  { -- | A POSIX timestamp when the last log entry was written.
    lastWritten :: Prelude.Maybe Prelude.Integer,
    -- | The name of the log file for the specified DB instance.
    logFileName :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of the log file for the specified DB instance.
    size :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBLogFilesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastWritten', 'describeDBLogFilesDetails_lastWritten' - A POSIX timestamp when the last log entry was written.
--
-- 'logFileName', 'describeDBLogFilesDetails_logFileName' - The name of the log file for the specified DB instance.
--
-- 'size', 'describeDBLogFilesDetails_size' - The size, in bytes, of the log file for the specified DB instance.
newDescribeDBLogFilesDetails ::
  DescribeDBLogFilesDetails
newDescribeDBLogFilesDetails =
  DescribeDBLogFilesDetails'
    { lastWritten =
        Prelude.Nothing,
      logFileName = Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | A POSIX timestamp when the last log entry was written.
describeDBLogFilesDetails_lastWritten :: Lens.Lens' DescribeDBLogFilesDetails (Prelude.Maybe Prelude.Integer)
describeDBLogFilesDetails_lastWritten = Lens.lens (\DescribeDBLogFilesDetails' {lastWritten} -> lastWritten) (\s@DescribeDBLogFilesDetails' {} a -> s {lastWritten = a} :: DescribeDBLogFilesDetails)

-- | The name of the log file for the specified DB instance.
describeDBLogFilesDetails_logFileName :: Lens.Lens' DescribeDBLogFilesDetails (Prelude.Maybe Prelude.Text)
describeDBLogFilesDetails_logFileName = Lens.lens (\DescribeDBLogFilesDetails' {logFileName} -> logFileName) (\s@DescribeDBLogFilesDetails' {} a -> s {logFileName = a} :: DescribeDBLogFilesDetails)

-- | The size, in bytes, of the log file for the specified DB instance.
describeDBLogFilesDetails_size :: Lens.Lens' DescribeDBLogFilesDetails (Prelude.Maybe Prelude.Integer)
describeDBLogFilesDetails_size = Lens.lens (\DescribeDBLogFilesDetails' {size} -> size) (\s@DescribeDBLogFilesDetails' {} a -> s {size = a} :: DescribeDBLogFilesDetails)

instance Data.FromXML DescribeDBLogFilesDetails where
  parseXML x =
    DescribeDBLogFilesDetails'
      Prelude.<$> (x Data..@? "LastWritten")
      Prelude.<*> (x Data..@? "LogFileName")
      Prelude.<*> (x Data..@? "Size")

instance Prelude.Hashable DescribeDBLogFilesDetails where
  hashWithSalt _salt DescribeDBLogFilesDetails' {..} =
    _salt `Prelude.hashWithSalt` lastWritten
      `Prelude.hashWithSalt` logFileName
      `Prelude.hashWithSalt` size

instance Prelude.NFData DescribeDBLogFilesDetails where
  rnf DescribeDBLogFilesDetails' {..} =
    Prelude.rnf lastWritten
      `Prelude.seq` Prelude.rnf logFileName
      `Prelude.seq` Prelude.rnf size
