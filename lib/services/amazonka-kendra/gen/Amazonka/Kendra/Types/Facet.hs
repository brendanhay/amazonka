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
-- Module      : Amazonka.Kendra.Types.Facet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.Facet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a document attribute. You can use document attributes
-- as facets.
--
-- For example, the document attribute or facet \"Department\" includes the
-- values \"HR\", \"Engineering\", and \"Accounting\". You can display
-- these values in the search results so that documents can be searched by
-- department.
--
-- You can display up to 10 facet values per facet for a query. If you want
-- to increase this limit, contact
-- <http://aws.amazon.com/contact-us/ Support>.
--
-- /See:/ 'newFacet' smart constructor.
data Facet = Facet'
  { -- | The unique key for the document attribute.
    documentAttributeKey :: Prelude.Maybe Prelude.Text,
    -- | An array of document attributes that are nested facets within a facet.
    --
    -- For example, the document attribute or facet \"Department\" includes a
    -- value called \"Engineering\". In addition, the document attribute or
    -- facet \"SubDepartment\" includes the values \"Frontend\" and \"Backend\"
    -- for documents assigned to \"Engineering\". You can display nested facets
    -- in the search results so that documents can be searched not only by
    -- department but also by a sub department within a department. This helps
    -- your users further narrow their search.
    --
    -- You can only have one nested facet within a facet. If you want to
    -- increase this limit, contact
    -- <http://aws.amazon.com/contact-us/ Support>.
    facets :: Prelude.Maybe [Facet],
    -- | Maximum number of facet values per facet. The default is 10. You can use
    -- this to limit the number of facet values to less than 10. If you want to
    -- increase the default, contact
    -- <http://aws.amazon.com/contact-us/ Support>.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Facet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentAttributeKey', 'facet_documentAttributeKey' - The unique key for the document attribute.
--
-- 'facets', 'facet_facets' - An array of document attributes that are nested facets within a facet.
--
-- For example, the document attribute or facet \"Department\" includes a
-- value called \"Engineering\". In addition, the document attribute or
-- facet \"SubDepartment\" includes the values \"Frontend\" and \"Backend\"
-- for documents assigned to \"Engineering\". You can display nested facets
-- in the search results so that documents can be searched not only by
-- department but also by a sub department within a department. This helps
-- your users further narrow their search.
--
-- You can only have one nested facet within a facet. If you want to
-- increase this limit, contact
-- <http://aws.amazon.com/contact-us/ Support>.
--
-- 'maxResults', 'facet_maxResults' - Maximum number of facet values per facet. The default is 10. You can use
-- this to limit the number of facet values to less than 10. If you want to
-- increase the default, contact
-- <http://aws.amazon.com/contact-us/ Support>.
newFacet ::
  Facet
newFacet =
  Facet'
    { documentAttributeKey = Prelude.Nothing,
      facets = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The unique key for the document attribute.
facet_documentAttributeKey :: Lens.Lens' Facet (Prelude.Maybe Prelude.Text)
facet_documentAttributeKey = Lens.lens (\Facet' {documentAttributeKey} -> documentAttributeKey) (\s@Facet' {} a -> s {documentAttributeKey = a} :: Facet)

-- | An array of document attributes that are nested facets within a facet.
--
-- For example, the document attribute or facet \"Department\" includes a
-- value called \"Engineering\". In addition, the document attribute or
-- facet \"SubDepartment\" includes the values \"Frontend\" and \"Backend\"
-- for documents assigned to \"Engineering\". You can display nested facets
-- in the search results so that documents can be searched not only by
-- department but also by a sub department within a department. This helps
-- your users further narrow their search.
--
-- You can only have one nested facet within a facet. If you want to
-- increase this limit, contact
-- <http://aws.amazon.com/contact-us/ Support>.
facet_facets :: Lens.Lens' Facet (Prelude.Maybe [Facet])
facet_facets = Lens.lens (\Facet' {facets} -> facets) (\s@Facet' {} a -> s {facets = a} :: Facet) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of facet values per facet. The default is 10. You can use
-- this to limit the number of facet values to less than 10. If you want to
-- increase the default, contact
-- <http://aws.amazon.com/contact-us/ Support>.
facet_maxResults :: Lens.Lens' Facet (Prelude.Maybe Prelude.Natural)
facet_maxResults = Lens.lens (\Facet' {maxResults} -> maxResults) (\s@Facet' {} a -> s {maxResults = a} :: Facet)

instance Prelude.Hashable Facet where
  hashWithSalt _salt Facet' {..} =
    _salt
      `Prelude.hashWithSalt` documentAttributeKey
      `Prelude.hashWithSalt` facets
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData Facet where
  rnf Facet' {..} =
    Prelude.rnf documentAttributeKey
      `Prelude.seq` Prelude.rnf facets
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToJSON Facet where
  toJSON Facet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentAttributeKey" Data..=)
              Prelude.<$> documentAttributeKey,
            ("Facets" Data..=) Prelude.<$> facets,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )
