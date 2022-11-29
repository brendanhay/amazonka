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
-- Module      : Amazonka.CloudSearchDomains.Types.Hits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types.Hits where

import Amazonka.CloudSearchDomains.Types.Hit
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The collection of documents that match the search request.
--
-- /See:/ 'newHits' smart constructor.
data Hits = Hits'
  { -- | The index of the first matching document.
    start :: Prelude.Maybe Prelude.Integer,
    -- | A document that matches the search request.
    hit :: Prelude.Maybe [Hit],
    -- | A cursor that can be used to retrieve the next set of matching documents
    -- when you want to page through a large result set.
    cursor :: Prelude.Maybe Prelude.Text,
    -- | The total number of documents that match the search request.
    found :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Hits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'start', 'hits_start' - The index of the first matching document.
--
-- 'hit', 'hits_hit' - A document that matches the search request.
--
-- 'cursor', 'hits_cursor' - A cursor that can be used to retrieve the next set of matching documents
-- when you want to page through a large result set.
--
-- 'found', 'hits_found' - The total number of documents that match the search request.
newHits ::
  Hits
newHits =
  Hits'
    { start = Prelude.Nothing,
      hit = Prelude.Nothing,
      cursor = Prelude.Nothing,
      found = Prelude.Nothing
    }

-- | The index of the first matching document.
hits_start :: Lens.Lens' Hits (Prelude.Maybe Prelude.Integer)
hits_start = Lens.lens (\Hits' {start} -> start) (\s@Hits' {} a -> s {start = a} :: Hits)

-- | A document that matches the search request.
hits_hit :: Lens.Lens' Hits (Prelude.Maybe [Hit])
hits_hit = Lens.lens (\Hits' {hit} -> hit) (\s@Hits' {} a -> s {hit = a} :: Hits) Prelude.. Lens.mapping Lens.coerced

-- | A cursor that can be used to retrieve the next set of matching documents
-- when you want to page through a large result set.
hits_cursor :: Lens.Lens' Hits (Prelude.Maybe Prelude.Text)
hits_cursor = Lens.lens (\Hits' {cursor} -> cursor) (\s@Hits' {} a -> s {cursor = a} :: Hits)

-- | The total number of documents that match the search request.
hits_found :: Lens.Lens' Hits (Prelude.Maybe Prelude.Integer)
hits_found = Lens.lens (\Hits' {found} -> found) (\s@Hits' {} a -> s {found = a} :: Hits)

instance Core.FromJSON Hits where
  parseJSON =
    Core.withObject
      "Hits"
      ( \x ->
          Hits'
            Prelude.<$> (x Core..:? "start")
            Prelude.<*> (x Core..:? "hit" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "cursor")
            Prelude.<*> (x Core..:? "found")
      )

instance Prelude.Hashable Hits where
  hashWithSalt _salt Hits' {..} =
    _salt `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` hit
      `Prelude.hashWithSalt` cursor
      `Prelude.hashWithSalt` found

instance Prelude.NFData Hits where
  rnf Hits' {..} =
    Prelude.rnf start
      `Prelude.seq` Prelude.rnf hit
      `Prelude.seq` Prelude.rnf cursor
      `Prelude.seq` Prelude.rnf found
