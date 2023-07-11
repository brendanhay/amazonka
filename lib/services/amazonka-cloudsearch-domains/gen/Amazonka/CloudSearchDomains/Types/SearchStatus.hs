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
-- Module      : Amazonka.CloudSearchDomains.Types.SearchStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types.SearchStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the resource id (@rid@) and the time it took to process the
-- request (@timems@).
--
-- /See:/ 'newSearchStatus' smart constructor.
data SearchStatus = SearchStatus'
  { -- | The encrypted resource ID for the request.
    rid :: Prelude.Maybe Prelude.Text,
    -- | How long it took to process the request, in milliseconds.
    timems :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rid', 'searchStatus_rid' - The encrypted resource ID for the request.
--
-- 'timems', 'searchStatus_timems' - How long it took to process the request, in milliseconds.
newSearchStatus ::
  SearchStatus
newSearchStatus =
  SearchStatus'
    { rid = Prelude.Nothing,
      timems = Prelude.Nothing
    }

-- | The encrypted resource ID for the request.
searchStatus_rid :: Lens.Lens' SearchStatus (Prelude.Maybe Prelude.Text)
searchStatus_rid = Lens.lens (\SearchStatus' {rid} -> rid) (\s@SearchStatus' {} a -> s {rid = a} :: SearchStatus)

-- | How long it took to process the request, in milliseconds.
searchStatus_timems :: Lens.Lens' SearchStatus (Prelude.Maybe Prelude.Integer)
searchStatus_timems = Lens.lens (\SearchStatus' {timems} -> timems) (\s@SearchStatus' {} a -> s {timems = a} :: SearchStatus)

instance Data.FromJSON SearchStatus where
  parseJSON =
    Data.withObject
      "SearchStatus"
      ( \x ->
          SearchStatus'
            Prelude.<$> (x Data..:? "rid")
            Prelude.<*> (x Data..:? "timems")
      )

instance Prelude.Hashable SearchStatus where
  hashWithSalt _salt SearchStatus' {..} =
    _salt
      `Prelude.hashWithSalt` rid
      `Prelude.hashWithSalt` timems

instance Prelude.NFData SearchStatus where
  rnf SearchStatus' {..} =
    Prelude.rnf rid `Prelude.seq` Prelude.rnf timems
