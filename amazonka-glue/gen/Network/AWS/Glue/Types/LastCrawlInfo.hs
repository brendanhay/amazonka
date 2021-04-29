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
-- Module      : Network.AWS.Glue.Types.LastCrawlInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LastCrawlInfo where

import Network.AWS.Glue.Types.LastCrawlStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Status and error information about the most recent crawl.
--
-- /See:/ 'newLastCrawlInfo' smart constructor.
data LastCrawlInfo = LastCrawlInfo'
  { -- | Status of the last crawl.
    status :: Prelude.Maybe LastCrawlStatus,
    -- | The prefix for a message about this crawl.
    messagePrefix :: Prelude.Maybe Prelude.Text,
    -- | The log group for the last crawl.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | The time at which the crawl started.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | If an error occurred, the error information about the last crawl.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The log stream for the last crawl.
    logStream :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      messagePrefix = Prelude.Nothing,
      logGroup = Prelude.Nothing,
      startTime = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      logStream = Prelude.Nothing
    }

-- | Status of the last crawl.
lastCrawlInfo_status :: Lens.Lens' LastCrawlInfo (Prelude.Maybe LastCrawlStatus)
lastCrawlInfo_status = Lens.lens (\LastCrawlInfo' {status} -> status) (\s@LastCrawlInfo' {} a -> s {status = a} :: LastCrawlInfo)

-- | The prefix for a message about this crawl.
lastCrawlInfo_messagePrefix :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.Text)
lastCrawlInfo_messagePrefix = Lens.lens (\LastCrawlInfo' {messagePrefix} -> messagePrefix) (\s@LastCrawlInfo' {} a -> s {messagePrefix = a} :: LastCrawlInfo)

-- | The log group for the last crawl.
lastCrawlInfo_logGroup :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.Text)
lastCrawlInfo_logGroup = Lens.lens (\LastCrawlInfo' {logGroup} -> logGroup) (\s@LastCrawlInfo' {} a -> s {logGroup = a} :: LastCrawlInfo)

-- | The time at which the crawl started.
lastCrawlInfo_startTime :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.UTCTime)
lastCrawlInfo_startTime = Lens.lens (\LastCrawlInfo' {startTime} -> startTime) (\s@LastCrawlInfo' {} a -> s {startTime = a} :: LastCrawlInfo) Prelude.. Lens.mapping Prelude._Time

-- | If an error occurred, the error information about the last crawl.
lastCrawlInfo_errorMessage :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.Text)
lastCrawlInfo_errorMessage = Lens.lens (\LastCrawlInfo' {errorMessage} -> errorMessage) (\s@LastCrawlInfo' {} a -> s {errorMessage = a} :: LastCrawlInfo)

-- | The log stream for the last crawl.
lastCrawlInfo_logStream :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.Text)
lastCrawlInfo_logStream = Lens.lens (\LastCrawlInfo' {logStream} -> logStream) (\s@LastCrawlInfo' {} a -> s {logStream = a} :: LastCrawlInfo)

instance Prelude.FromJSON LastCrawlInfo where
  parseJSON =
    Prelude.withObject
      "LastCrawlInfo"
      ( \x ->
          LastCrawlInfo'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "MessagePrefix")
            Prelude.<*> (x Prelude..:? "LogGroup")
            Prelude.<*> (x Prelude..:? "StartTime")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "LogStream")
      )

instance Prelude.Hashable LastCrawlInfo

instance Prelude.NFData LastCrawlInfo
