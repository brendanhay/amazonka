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
-- Module      : Network.AWS.CloudWatchLogs.Types.SearchedLogStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.SearchedLogStream where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the search status of a log stream.
--
-- /See:/ 'newSearchedLogStream' smart constructor.
data SearchedLogStream = SearchedLogStream'
  { -- | The name of the log stream.
    logStreamName :: Core.Maybe Core.Text,
    -- | Indicates whether all the events in this log stream were searched.
    searchedCompletely :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchedLogStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamName', 'searchedLogStream_logStreamName' - The name of the log stream.
--
-- 'searchedCompletely', 'searchedLogStream_searchedCompletely' - Indicates whether all the events in this log stream were searched.
newSearchedLogStream ::
  SearchedLogStream
newSearchedLogStream =
  SearchedLogStream'
    { logStreamName = Core.Nothing,
      searchedCompletely = Core.Nothing
    }

-- | The name of the log stream.
searchedLogStream_logStreamName :: Lens.Lens' SearchedLogStream (Core.Maybe Core.Text)
searchedLogStream_logStreamName = Lens.lens (\SearchedLogStream' {logStreamName} -> logStreamName) (\s@SearchedLogStream' {} a -> s {logStreamName = a} :: SearchedLogStream)

-- | Indicates whether all the events in this log stream were searched.
searchedLogStream_searchedCompletely :: Lens.Lens' SearchedLogStream (Core.Maybe Core.Bool)
searchedLogStream_searchedCompletely = Lens.lens (\SearchedLogStream' {searchedCompletely} -> searchedCompletely) (\s@SearchedLogStream' {} a -> s {searchedCompletely = a} :: SearchedLogStream)

instance Core.FromJSON SearchedLogStream where
  parseJSON =
    Core.withObject
      "SearchedLogStream"
      ( \x ->
          SearchedLogStream'
            Core.<$> (x Core..:? "logStreamName")
            Core.<*> (x Core..:? "searchedCompletely")
      )

instance Core.Hashable SearchedLogStream

instance Core.NFData SearchedLogStream
