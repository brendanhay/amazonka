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
-- Module      : Network.AWS.Glue.Types.LastCrawlInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LastCrawlInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.LastCrawlStatus
import qualified Network.AWS.Lens as Lens

-- | Status and error information about the most recent crawl.
--
-- /See:/ 'newLastCrawlInfo' smart constructor.
data LastCrawlInfo = LastCrawlInfo'
  { -- | Status of the last crawl.
    status :: Core.Maybe LastCrawlStatus,
    -- | The prefix for a message about this crawl.
    messagePrefix :: Core.Maybe Core.Text,
    -- | The log group for the last crawl.
    logGroup :: Core.Maybe Core.Text,
    -- | The time at which the crawl started.
    startTime :: Core.Maybe Core.POSIX,
    -- | If an error occurred, the error information about the last crawl.
    errorMessage :: Core.Maybe Core.Text,
    -- | The log stream for the last crawl.
    logStream :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LastCrawlInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'lastCrawlInfo_status' - Status of the last crawl.
--
-- 'messagePrefix', 'lastCrawlInfo_messagePrefix' - The prefix for a message about this crawl.
--
-- 'logGroup', 'lastCrawlInfo_logGroup' - The log group for the last crawl.
--
-- 'startTime', 'lastCrawlInfo_startTime' - The time at which the crawl started.
--
-- 'errorMessage', 'lastCrawlInfo_errorMessage' - If an error occurred, the error information about the last crawl.
--
-- 'logStream', 'lastCrawlInfo_logStream' - The log stream for the last crawl.
newLastCrawlInfo ::
  LastCrawlInfo
newLastCrawlInfo =
  LastCrawlInfo'
    { status = Core.Nothing,
      messagePrefix = Core.Nothing,
      logGroup = Core.Nothing,
      startTime = Core.Nothing,
      errorMessage = Core.Nothing,
      logStream = Core.Nothing
    }

-- | Status of the last crawl.
lastCrawlInfo_status :: Lens.Lens' LastCrawlInfo (Core.Maybe LastCrawlStatus)
lastCrawlInfo_status = Lens.lens (\LastCrawlInfo' {status} -> status) (\s@LastCrawlInfo' {} a -> s {status = a} :: LastCrawlInfo)

-- | The prefix for a message about this crawl.
lastCrawlInfo_messagePrefix :: Lens.Lens' LastCrawlInfo (Core.Maybe Core.Text)
lastCrawlInfo_messagePrefix = Lens.lens (\LastCrawlInfo' {messagePrefix} -> messagePrefix) (\s@LastCrawlInfo' {} a -> s {messagePrefix = a} :: LastCrawlInfo)

-- | The log group for the last crawl.
lastCrawlInfo_logGroup :: Lens.Lens' LastCrawlInfo (Core.Maybe Core.Text)
lastCrawlInfo_logGroup = Lens.lens (\LastCrawlInfo' {logGroup} -> logGroup) (\s@LastCrawlInfo' {} a -> s {logGroup = a} :: LastCrawlInfo)

-- | The time at which the crawl started.
lastCrawlInfo_startTime :: Lens.Lens' LastCrawlInfo (Core.Maybe Core.UTCTime)
lastCrawlInfo_startTime = Lens.lens (\LastCrawlInfo' {startTime} -> startTime) (\s@LastCrawlInfo' {} a -> s {startTime = a} :: LastCrawlInfo) Core.. Lens.mapping Core._Time

-- | If an error occurred, the error information about the last crawl.
lastCrawlInfo_errorMessage :: Lens.Lens' LastCrawlInfo (Core.Maybe Core.Text)
lastCrawlInfo_errorMessage = Lens.lens (\LastCrawlInfo' {errorMessage} -> errorMessage) (\s@LastCrawlInfo' {} a -> s {errorMessage = a} :: LastCrawlInfo)

-- | The log stream for the last crawl.
lastCrawlInfo_logStream :: Lens.Lens' LastCrawlInfo (Core.Maybe Core.Text)
lastCrawlInfo_logStream = Lens.lens (\LastCrawlInfo' {logStream} -> logStream) (\s@LastCrawlInfo' {} a -> s {logStream = a} :: LastCrawlInfo)

instance Core.FromJSON LastCrawlInfo where
  parseJSON =
    Core.withObject
      "LastCrawlInfo"
      ( \x ->
          LastCrawlInfo'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "MessagePrefix")
            Core.<*> (x Core..:? "LogGroup")
            Core.<*> (x Core..:? "StartTime")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "LogStream")
      )

instance Core.Hashable LastCrawlInfo

instance Core.NFData LastCrawlInfo
