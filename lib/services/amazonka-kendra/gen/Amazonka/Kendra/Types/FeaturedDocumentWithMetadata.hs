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
-- Module      : Amazonka.Kendra.Types.FeaturedDocumentWithMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FeaturedDocumentWithMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A featured document with its metadata information. This document is
-- displayed at the top of the search results page, placed above all other
-- results for certain queries. If there\'s an exact match of a query, then
-- the document is featured in the search results.
--
-- /See:/ 'newFeaturedDocumentWithMetadata' smart constructor.
data FeaturedDocumentWithMetadata = FeaturedDocumentWithMetadata'
  { -- | The identifier of the featured document with its metadata. You can use
    -- the <https://docs.aws.amazon.com/kendra/latest/dg/API_Query.html Query>
    -- API to search for specific documents with their document IDs included in
    -- the result items, or you can use the console.
    id :: Prelude.Maybe Prelude.Text,
    -- | The main title of the featured document.
    title :: Prelude.Maybe Prelude.Text,
    -- | The source URI location of the featured document.
    uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeaturedDocumentWithMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'featuredDocumentWithMetadata_id' - The identifier of the featured document with its metadata. You can use
-- the <https://docs.aws.amazon.com/kendra/latest/dg/API_Query.html Query>
-- API to search for specific documents with their document IDs included in
-- the result items, or you can use the console.
--
-- 'title', 'featuredDocumentWithMetadata_title' - The main title of the featured document.
--
-- 'uri', 'featuredDocumentWithMetadata_uri' - The source URI location of the featured document.
newFeaturedDocumentWithMetadata ::
  FeaturedDocumentWithMetadata
newFeaturedDocumentWithMetadata =
  FeaturedDocumentWithMetadata'
    { id = Prelude.Nothing,
      title = Prelude.Nothing,
      uri = Prelude.Nothing
    }

-- | The identifier of the featured document with its metadata. You can use
-- the <https://docs.aws.amazon.com/kendra/latest/dg/API_Query.html Query>
-- API to search for specific documents with their document IDs included in
-- the result items, or you can use the console.
featuredDocumentWithMetadata_id :: Lens.Lens' FeaturedDocumentWithMetadata (Prelude.Maybe Prelude.Text)
featuredDocumentWithMetadata_id = Lens.lens (\FeaturedDocumentWithMetadata' {id} -> id) (\s@FeaturedDocumentWithMetadata' {} a -> s {id = a} :: FeaturedDocumentWithMetadata)

-- | The main title of the featured document.
featuredDocumentWithMetadata_title :: Lens.Lens' FeaturedDocumentWithMetadata (Prelude.Maybe Prelude.Text)
featuredDocumentWithMetadata_title = Lens.lens (\FeaturedDocumentWithMetadata' {title} -> title) (\s@FeaturedDocumentWithMetadata' {} a -> s {title = a} :: FeaturedDocumentWithMetadata)

-- | The source URI location of the featured document.
featuredDocumentWithMetadata_uri :: Lens.Lens' FeaturedDocumentWithMetadata (Prelude.Maybe Prelude.Text)
featuredDocumentWithMetadata_uri = Lens.lens (\FeaturedDocumentWithMetadata' {uri} -> uri) (\s@FeaturedDocumentWithMetadata' {} a -> s {uri = a} :: FeaturedDocumentWithMetadata)

instance Data.FromJSON FeaturedDocumentWithMetadata where
  parseJSON =
    Data.withObject
      "FeaturedDocumentWithMetadata"
      ( \x ->
          FeaturedDocumentWithMetadata'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..:? "URI")
      )

instance
  Prelude.Hashable
    FeaturedDocumentWithMetadata
  where
  hashWithSalt _salt FeaturedDocumentWithMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` uri

instance Prelude.NFData FeaturedDocumentWithMetadata where
  rnf FeaturedDocumentWithMetadata' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf uri
