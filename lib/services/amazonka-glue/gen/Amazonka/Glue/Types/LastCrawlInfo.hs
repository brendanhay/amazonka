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
-- Module      : Amazonka.Glue.Types.LastCrawlInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.LastCrawlInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.LastCrawlStatus
import qualified Amazonka.Prelude as Prelude

-- | Status and error information about the most recent crawl.
--
-- /See:/ 'newLastCrawlInfo' smart constructor.
data LastCrawlInfo = LastCrawlInfo'
  { -- | If an error occurred, the error information about the last crawl.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The log group for the last crawl.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | The log stream for the last crawl.
    logStream :: Prelude.Maybe Prelude.Text,
    -- | The prefix for a message about this crawl.
    messagePrefix :: Prelude.Maybe Prelude.Text,
    -- | The time at which the crawl started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | Status of the last crawl.
    status :: Prelude.Maybe LastCrawlStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LastCrawlInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'lastCrawlInfo_errorMessage' - If an error occurred, the error information about the last crawl.
--
-- 'logGroup', 'lastCrawlInfo_logGroup' - The log group for the last crawl.
--
-- 'logStream', 'lastCrawlInfo_logStream' - The log stream for the last crawl.
--
-- 'messagePrefix', 'lastCrawlInfo_messagePrefix' - The prefix for a message about this crawl.
--
-- 'startTime', 'lastCrawlInfo_startTime' - The time at which the crawl started.
--
-- 'status', 'lastCrawlInfo_status' - Status of the last crawl.
newLastCrawlInfo ::
  LastCrawlInfo
newLastCrawlInfo =
  LastCrawlInfo'
    { errorMessage = Prelude.Nothing,
      logGroup = Prelude.Nothing,
      logStream = Prelude.Nothing,
      messagePrefix = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | If an error occurred, the error information about the last crawl.
lastCrawlInfo_errorMessage :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.Text)
lastCrawlInfo_errorMessage = Lens.lens (\LastCrawlInfo' {errorMessage} -> errorMessage) (\s@LastCrawlInfo' {} a -> s {errorMessage = a} :: LastCrawlInfo)

-- | The log group for the last crawl.
lastCrawlInfo_logGroup :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.Text)
lastCrawlInfo_logGroup = Lens.lens (\LastCrawlInfo' {logGroup} -> logGroup) (\s@LastCrawlInfo' {} a -> s {logGroup = a} :: LastCrawlInfo)

-- | The log stream for the last crawl.
lastCrawlInfo_logStream :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.Text)
lastCrawlInfo_logStream = Lens.lens (\LastCrawlInfo' {logStream} -> logStream) (\s@LastCrawlInfo' {} a -> s {logStream = a} :: LastCrawlInfo)

-- | The prefix for a message about this crawl.
lastCrawlInfo_messagePrefix :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.Text)
lastCrawlInfo_messagePrefix = Lens.lens (\LastCrawlInfo' {messagePrefix} -> messagePrefix) (\s@LastCrawlInfo' {} a -> s {messagePrefix = a} :: LastCrawlInfo)

-- | The time at which the crawl started.
lastCrawlInfo_startTime :: Lens.Lens' LastCrawlInfo (Prelude.Maybe Prelude.UTCTime)
lastCrawlInfo_startTime = Lens.lens (\LastCrawlInfo' {startTime} -> startTime) (\s@LastCrawlInfo' {} a -> s {startTime = a} :: LastCrawlInfo) Prelude.. Lens.mapping Data._Time

-- | Status of the last crawl.
lastCrawlInfo_status :: Lens.Lens' LastCrawlInfo (Prelude.Maybe LastCrawlStatus)
lastCrawlInfo_status = Lens.lens (\LastCrawlInfo' {status} -> status) (\s@LastCrawlInfo' {} a -> s {status = a} :: LastCrawlInfo)

instance Data.FromJSON LastCrawlInfo where
  parseJSON =
    Data.withObject
      "LastCrawlInfo"
      ( \x ->
          LastCrawlInfo'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LogGroup")
            Prelude.<*> (x Data..:? "LogStream")
            Prelude.<*> (x Data..:? "MessagePrefix")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable LastCrawlInfo where
  hashWithSalt _salt LastCrawlInfo' {..} =
    _salt
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` logGroup
      `Prelude.hashWithSalt` logStream
      `Prelude.hashWithSalt` messagePrefix
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData LastCrawlInfo where
  rnf LastCrawlInfo' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf logGroup
      `Prelude.seq` Prelude.rnf logStream
      `Prelude.seq` Prelude.rnf messagePrefix
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
