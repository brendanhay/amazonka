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
-- Module      : Amazonka.VoiceId.Types.WatchlistSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.WatchlistSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of information about a watchlist.
--
-- /See:/ 'newWatchlistSummary' smart constructor.
data WatchlistSummary = WatchlistSummary'
  { -- | The timestamp of when the watchlist was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Whether the specified watchlist is the default watchlist of a domain.
    defaultWatchlist :: Prelude.Maybe Prelude.Bool,
    -- | The description of the watchlist.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The identifier of the domain that contains the watchlist.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The name for the watchlist.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The timestamp of when the watchlist was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the watchlist.
    watchlistId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WatchlistSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'watchlistSummary_createdAt' - The timestamp of when the watchlist was created.
--
-- 'defaultWatchlist', 'watchlistSummary_defaultWatchlist' - Whether the specified watchlist is the default watchlist of a domain.
--
-- 'description', 'watchlistSummary_description' - The description of the watchlist.
--
-- 'domainId', 'watchlistSummary_domainId' - The identifier of the domain that contains the watchlist.
--
-- 'name', 'watchlistSummary_name' - The name for the watchlist.
--
-- 'updatedAt', 'watchlistSummary_updatedAt' - The timestamp of when the watchlist was last updated.
--
-- 'watchlistId', 'watchlistSummary_watchlistId' - The identifier of the watchlist.
newWatchlistSummary ::
  WatchlistSummary
newWatchlistSummary =
  WatchlistSummary'
    { createdAt = Prelude.Nothing,
      defaultWatchlist = Prelude.Nothing,
      description = Prelude.Nothing,
      domainId = Prelude.Nothing,
      name = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      watchlistId = Prelude.Nothing
    }

-- | The timestamp of when the watchlist was created.
watchlistSummary_createdAt :: Lens.Lens' WatchlistSummary (Prelude.Maybe Prelude.UTCTime)
watchlistSummary_createdAt = Lens.lens (\WatchlistSummary' {createdAt} -> createdAt) (\s@WatchlistSummary' {} a -> s {createdAt = a} :: WatchlistSummary) Prelude.. Lens.mapping Data._Time

-- | Whether the specified watchlist is the default watchlist of a domain.
watchlistSummary_defaultWatchlist :: Lens.Lens' WatchlistSummary (Prelude.Maybe Prelude.Bool)
watchlistSummary_defaultWatchlist = Lens.lens (\WatchlistSummary' {defaultWatchlist} -> defaultWatchlist) (\s@WatchlistSummary' {} a -> s {defaultWatchlist = a} :: WatchlistSummary)

-- | The description of the watchlist.
watchlistSummary_description :: Lens.Lens' WatchlistSummary (Prelude.Maybe Prelude.Text)
watchlistSummary_description = Lens.lens (\WatchlistSummary' {description} -> description) (\s@WatchlistSummary' {} a -> s {description = a} :: WatchlistSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The identifier of the domain that contains the watchlist.
watchlistSummary_domainId :: Lens.Lens' WatchlistSummary (Prelude.Maybe Prelude.Text)
watchlistSummary_domainId = Lens.lens (\WatchlistSummary' {domainId} -> domainId) (\s@WatchlistSummary' {} a -> s {domainId = a} :: WatchlistSummary)

-- | The name for the watchlist.
watchlistSummary_name :: Lens.Lens' WatchlistSummary (Prelude.Maybe Prelude.Text)
watchlistSummary_name = Lens.lens (\WatchlistSummary' {name} -> name) (\s@WatchlistSummary' {} a -> s {name = a} :: WatchlistSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The timestamp of when the watchlist was last updated.
watchlistSummary_updatedAt :: Lens.Lens' WatchlistSummary (Prelude.Maybe Prelude.UTCTime)
watchlistSummary_updatedAt = Lens.lens (\WatchlistSummary' {updatedAt} -> updatedAt) (\s@WatchlistSummary' {} a -> s {updatedAt = a} :: WatchlistSummary) Prelude.. Lens.mapping Data._Time

-- | The identifier of the watchlist.
watchlistSummary_watchlistId :: Lens.Lens' WatchlistSummary (Prelude.Maybe Prelude.Text)
watchlistSummary_watchlistId = Lens.lens (\WatchlistSummary' {watchlistId} -> watchlistId) (\s@WatchlistSummary' {} a -> s {watchlistId = a} :: WatchlistSummary)

instance Data.FromJSON WatchlistSummary where
  parseJSON =
    Data.withObject
      "WatchlistSummary"
      ( \x ->
          WatchlistSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DefaultWatchlist")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "UpdatedAt")
            Prelude.<*> (x Data..:? "WatchlistId")
      )

instance Prelude.Hashable WatchlistSummary where
  hashWithSalt _salt WatchlistSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` defaultWatchlist
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` watchlistId

instance Prelude.NFData WatchlistSummary where
  rnf WatchlistSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf defaultWatchlist
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf watchlistId
