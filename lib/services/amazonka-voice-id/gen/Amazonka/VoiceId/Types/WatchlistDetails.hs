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
-- Module      : Amazonka.VoiceId.Types.WatchlistDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.WatchlistDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the watchlists in a domain.
--
-- /See:/ 'newWatchlistDetails' smart constructor.
data WatchlistDetails = WatchlistDetails'
  { -- | The identifier of the default watchlist.
    defaultWatchlistId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WatchlistDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultWatchlistId', 'watchlistDetails_defaultWatchlistId' - The identifier of the default watchlist.
newWatchlistDetails ::
  -- | 'defaultWatchlistId'
  Prelude.Text ->
  WatchlistDetails
newWatchlistDetails pDefaultWatchlistId_ =
  WatchlistDetails'
    { defaultWatchlistId =
        pDefaultWatchlistId_
    }

-- | The identifier of the default watchlist.
watchlistDetails_defaultWatchlistId :: Lens.Lens' WatchlistDetails Prelude.Text
watchlistDetails_defaultWatchlistId = Lens.lens (\WatchlistDetails' {defaultWatchlistId} -> defaultWatchlistId) (\s@WatchlistDetails' {} a -> s {defaultWatchlistId = a} :: WatchlistDetails)

instance Data.FromJSON WatchlistDetails where
  parseJSON =
    Data.withObject
      "WatchlistDetails"
      ( \x ->
          WatchlistDetails'
            Prelude.<$> (x Data..: "DefaultWatchlistId")
      )

instance Prelude.Hashable WatchlistDetails where
  hashWithSalt _salt WatchlistDetails' {..} =
    _salt `Prelude.hashWithSalt` defaultWatchlistId

instance Prelude.NFData WatchlistDetails where
  rnf WatchlistDetails' {..} =
    Prelude.rnf defaultWatchlistId
