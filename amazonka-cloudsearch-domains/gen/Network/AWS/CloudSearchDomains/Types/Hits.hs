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
-- Module      : Network.AWS.CloudSearchDomains.Types.Hits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Hits where

import Network.AWS.CloudSearchDomains.Types.Hit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The collection of documents that match the search request.
--
-- /See:/ 'newHits' smart constructor.
data Hits = Hits'
  { -- | The total number of documents that match the search request.
    found :: Prelude.Maybe Prelude.Integer,
    -- | A document that matches the search request.
    hit :: Prelude.Maybe [Hit],
    -- | A cursor that can be used to retrieve the next set of matching documents
    -- when you want to page through a large result set.
    cursor :: Prelude.Maybe Prelude.Text,
    -- | The index of the first matching document.
    start :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Hits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'found', 'hits_found' - The total number of documents that match the search request.
--
-- 'hit', 'hits_hit' - A document that matches the search request.
--
-- 'cursor', 'hits_cursor' - A cursor that can be used to retrieve the next set of matching documents
-- when you want to page through a large result set.
--
-- 'start', 'hits_start' - The index of the first matching document.
newHits ::
  Hits
newHits =
  Hits'
    { found = Prelude.Nothing,
      hit = Prelude.Nothing,
      cursor = Prelude.Nothing,
      start = Prelude.Nothing
    }

-- | The total number of documents that match the search request.
hits_found :: Lens.Lens' Hits (Prelude.Maybe Prelude.Integer)
hits_found = Lens.lens (\Hits' {found} -> found) (\s@Hits' {} a -> s {found = a} :: Hits)

-- | A document that matches the search request.
hits_hit :: Lens.Lens' Hits (Prelude.Maybe [Hit])
hits_hit = Lens.lens (\Hits' {hit} -> hit) (\s@Hits' {} a -> s {hit = a} :: Hits) Prelude.. Lens.mapping Prelude._Coerce

-- | A cursor that can be used to retrieve the next set of matching documents
-- when you want to page through a large result set.
hits_cursor :: Lens.Lens' Hits (Prelude.Maybe Prelude.Text)
hits_cursor = Lens.lens (\Hits' {cursor} -> cursor) (\s@Hits' {} a -> s {cursor = a} :: Hits)

-- | The index of the first matching document.
hits_start :: Lens.Lens' Hits (Prelude.Maybe Prelude.Integer)
hits_start = Lens.lens (\Hits' {start} -> start) (\s@Hits' {} a -> s {start = a} :: Hits)

instance Prelude.FromJSON Hits where
  parseJSON =
    Prelude.withObject
      "Hits"
      ( \x ->
          Hits'
            Prelude.<$> (x Prelude..:? "found")
            Prelude.<*> (x Prelude..:? "hit" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "cursor")
            Prelude.<*> (x Prelude..:? "start")
      )

instance Prelude.Hashable Hits

instance Prelude.NFData Hits
