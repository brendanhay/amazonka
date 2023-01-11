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
-- Module      : Amazonka.CloudWatchLogs.Types.SearchedLogStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.SearchedLogStream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the search status of a log stream.
--
-- /See:/ 'newSearchedLogStream' smart constructor.
data SearchedLogStream = SearchedLogStream'
  { -- | The name of the log stream.
    logStreamName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether all the events in this log stream were searched.
    searchedCompletely :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { logStreamName = Prelude.Nothing,
      searchedCompletely = Prelude.Nothing
    }

-- | The name of the log stream.
searchedLogStream_logStreamName :: Lens.Lens' SearchedLogStream (Prelude.Maybe Prelude.Text)
searchedLogStream_logStreamName = Lens.lens (\SearchedLogStream' {logStreamName} -> logStreamName) (\s@SearchedLogStream' {} a -> s {logStreamName = a} :: SearchedLogStream)

-- | Indicates whether all the events in this log stream were searched.
searchedLogStream_searchedCompletely :: Lens.Lens' SearchedLogStream (Prelude.Maybe Prelude.Bool)
searchedLogStream_searchedCompletely = Lens.lens (\SearchedLogStream' {searchedCompletely} -> searchedCompletely) (\s@SearchedLogStream' {} a -> s {searchedCompletely = a} :: SearchedLogStream)

instance Data.FromJSON SearchedLogStream where
  parseJSON =
    Data.withObject
      "SearchedLogStream"
      ( \x ->
          SearchedLogStream'
            Prelude.<$> (x Data..:? "logStreamName")
            Prelude.<*> (x Data..:? "searchedCompletely")
      )

instance Prelude.Hashable SearchedLogStream where
  hashWithSalt _salt SearchedLogStream' {..} =
    _salt `Prelude.hashWithSalt` logStreamName
      `Prelude.hashWithSalt` searchedCompletely

instance Prelude.NFData SearchedLogStream where
  rnf SearchedLogStream' {..} =
    Prelude.rnf logStreamName
      `Prelude.seq` Prelude.rnf searchedCompletely
