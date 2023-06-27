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
-- Module      : Amazonka.VoiceId.Types.Watchlist
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.Watchlist where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains all the information about a watchlist.
--
-- /See:/ 'newWatchlist' smart constructor.
data Watchlist = Watchlist'
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
    -- | The timestamp of when the watchlist was updated.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the watchlist.
    watchlistId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Watchlist' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'watchlist_createdAt' - The timestamp of when the watchlist was created.
--
-- 'defaultWatchlist', 'watchlist_defaultWatchlist' - Whether the specified watchlist is the default watchlist of a domain.
--
-- 'description', 'watchlist_description' - The description of the watchlist.
--
-- 'domainId', 'watchlist_domainId' - The identifier of the domain that contains the watchlist.
--
-- 'name', 'watchlist_name' - The name for the watchlist.
--
-- 'updatedAt', 'watchlist_updatedAt' - The timestamp of when the watchlist was updated.
--
-- 'watchlistId', 'watchlist_watchlistId' - The identifier of the watchlist.
newWatchlist ::
  Watchlist
newWatchlist =
  Watchlist'
    { createdAt = Prelude.Nothing,
      defaultWatchlist = Prelude.Nothing,
      description = Prelude.Nothing,
      domainId = Prelude.Nothing,
      name = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      watchlistId = Prelude.Nothing
    }

-- | The timestamp of when the watchlist was created.
watchlist_createdAt :: Lens.Lens' Watchlist (Prelude.Maybe Prelude.UTCTime)
watchlist_createdAt = Lens.lens (\Watchlist' {createdAt} -> createdAt) (\s@Watchlist' {} a -> s {createdAt = a} :: Watchlist) Prelude.. Lens.mapping Data._Time

-- | Whether the specified watchlist is the default watchlist of a domain.
watchlist_defaultWatchlist :: Lens.Lens' Watchlist (Prelude.Maybe Prelude.Bool)
watchlist_defaultWatchlist = Lens.lens (\Watchlist' {defaultWatchlist} -> defaultWatchlist) (\s@Watchlist' {} a -> s {defaultWatchlist = a} :: Watchlist)

-- | The description of the watchlist.
watchlist_description :: Lens.Lens' Watchlist (Prelude.Maybe Prelude.Text)
watchlist_description = Lens.lens (\Watchlist' {description} -> description) (\s@Watchlist' {} a -> s {description = a} :: Watchlist) Prelude.. Lens.mapping Data._Sensitive

-- | The identifier of the domain that contains the watchlist.
watchlist_domainId :: Lens.Lens' Watchlist (Prelude.Maybe Prelude.Text)
watchlist_domainId = Lens.lens (\Watchlist' {domainId} -> domainId) (\s@Watchlist' {} a -> s {domainId = a} :: Watchlist)

-- | The name for the watchlist.
watchlist_name :: Lens.Lens' Watchlist (Prelude.Maybe Prelude.Text)
watchlist_name = Lens.lens (\Watchlist' {name} -> name) (\s@Watchlist' {} a -> s {name = a} :: Watchlist) Prelude.. Lens.mapping Data._Sensitive

-- | The timestamp of when the watchlist was updated.
watchlist_updatedAt :: Lens.Lens' Watchlist (Prelude.Maybe Prelude.UTCTime)
watchlist_updatedAt = Lens.lens (\Watchlist' {updatedAt} -> updatedAt) (\s@Watchlist' {} a -> s {updatedAt = a} :: Watchlist) Prelude.. Lens.mapping Data._Time

-- | The identifier of the watchlist.
watchlist_watchlistId :: Lens.Lens' Watchlist (Prelude.Maybe Prelude.Text)
watchlist_watchlistId = Lens.lens (\Watchlist' {watchlistId} -> watchlistId) (\s@Watchlist' {} a -> s {watchlistId = a} :: Watchlist)

instance Data.FromJSON Watchlist where
  parseJSON =
    Data.withObject
      "Watchlist"
      ( \x ->
          Watchlist'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DefaultWatchlist")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "UpdatedAt")
            Prelude.<*> (x Data..:? "WatchlistId")
      )

instance Prelude.Hashable Watchlist where
  hashWithSalt _salt Watchlist' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` defaultWatchlist
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` watchlistId

instance Prelude.NFData Watchlist where
  rnf Watchlist' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf defaultWatchlist
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf watchlistId
