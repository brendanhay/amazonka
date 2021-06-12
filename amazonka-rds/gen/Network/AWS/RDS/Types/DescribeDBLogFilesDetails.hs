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
-- Module      : Network.AWS.RDS.Types.DescribeDBLogFilesDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DescribeDBLogFilesDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type is used as a response element to @DescribeDBLogFiles@.
--
-- /See:/ 'newDescribeDBLogFilesDetails' smart constructor.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails'
  { -- | A POSIX timestamp when the last log entry was written.
    lastWritten :: Core.Maybe Core.Integer,
    -- | The name of the log file for the specified DB instance.
    logFileName :: Core.Maybe Core.Text,
    -- | The size, in bytes, of the log file for the specified DB instance.
    size :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      logFileName = Core.Nothing,
      size = Core.Nothing
    }

-- | A POSIX timestamp when the last log entry was written.
describeDBLogFilesDetails_lastWritten :: Lens.Lens' DescribeDBLogFilesDetails (Core.Maybe Core.Integer)
describeDBLogFilesDetails_lastWritten = Lens.lens (\DescribeDBLogFilesDetails' {lastWritten} -> lastWritten) (\s@DescribeDBLogFilesDetails' {} a -> s {lastWritten = a} :: DescribeDBLogFilesDetails)

-- | The name of the log file for the specified DB instance.
describeDBLogFilesDetails_logFileName :: Lens.Lens' DescribeDBLogFilesDetails (Core.Maybe Core.Text)
describeDBLogFilesDetails_logFileName = Lens.lens (\DescribeDBLogFilesDetails' {logFileName} -> logFileName) (\s@DescribeDBLogFilesDetails' {} a -> s {logFileName = a} :: DescribeDBLogFilesDetails)

-- | The size, in bytes, of the log file for the specified DB instance.
describeDBLogFilesDetails_size :: Lens.Lens' DescribeDBLogFilesDetails (Core.Maybe Core.Integer)
describeDBLogFilesDetails_size = Lens.lens (\DescribeDBLogFilesDetails' {size} -> size) (\s@DescribeDBLogFilesDetails' {} a -> s {size = a} :: DescribeDBLogFilesDetails)

instance Core.FromXML DescribeDBLogFilesDetails where
  parseXML x =
    DescribeDBLogFilesDetails'
      Core.<$> (x Core..@? "LastWritten")
      Core.<*> (x Core..@? "LogFileName")
      Core.<*> (x Core..@? "Size")

instance Core.Hashable DescribeDBLogFilesDetails

instance Core.NFData DescribeDBLogFilesDetails
