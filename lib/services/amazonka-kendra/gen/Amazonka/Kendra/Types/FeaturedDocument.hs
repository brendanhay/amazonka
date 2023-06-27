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
-- Module      : Amazonka.Kendra.Types.FeaturedDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FeaturedDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A featured document. This document is displayed at the top of the search
-- results page, placed above all other results for certain queries. If
-- there\'s an exact match of a query, then the document is featured in the
-- search results.
--
-- /See:/ 'newFeaturedDocument' smart constructor.
data FeaturedDocument = FeaturedDocument'
  { -- | The identifier of the document to feature in the search results. You can
    -- use the
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_Query.html Query> API
    -- to search for specific documents with their document IDs included in the
    -- result items, or you can use the console.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeaturedDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'featuredDocument_id' - The identifier of the document to feature in the search results. You can
-- use the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Query.html Query> API
-- to search for specific documents with their document IDs included in the
-- result items, or you can use the console.
newFeaturedDocument ::
  FeaturedDocument
newFeaturedDocument =
  FeaturedDocument' {id = Prelude.Nothing}

-- | The identifier of the document to feature in the search results. You can
-- use the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Query.html Query> API
-- to search for specific documents with their document IDs included in the
-- result items, or you can use the console.
featuredDocument_id :: Lens.Lens' FeaturedDocument (Prelude.Maybe Prelude.Text)
featuredDocument_id = Lens.lens (\FeaturedDocument' {id} -> id) (\s@FeaturedDocument' {} a -> s {id = a} :: FeaturedDocument)

instance Data.FromJSON FeaturedDocument where
  parseJSON =
    Data.withObject
      "FeaturedDocument"
      ( \x ->
          FeaturedDocument' Prelude.<$> (x Data..:? "Id")
      )

instance Prelude.Hashable FeaturedDocument where
  hashWithSalt _salt FeaturedDocument' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData FeaturedDocument where
  rnf FeaturedDocument' {..} = Prelude.rnf id

instance Data.ToJSON FeaturedDocument where
  toJSON FeaturedDocument' {..} =
    Data.object
      (Prelude.catMaybes [("Id" Data..=) Prelude.<$> id])
