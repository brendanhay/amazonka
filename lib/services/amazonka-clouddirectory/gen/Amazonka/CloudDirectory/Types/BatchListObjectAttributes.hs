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
-- Module      : Amazonka.CloudDirectory.Types.BatchListObjectAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListObjectAttributes where

import Amazonka.CloudDirectory.Types.ObjectReference
import Amazonka.CloudDirectory.Types.SchemaFacet
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a ListObjectAttributes operation.
--
-- /See:/ 'newBatchListObjectAttributes' smart constructor.
data BatchListObjectAttributes = BatchListObjectAttributes'
  { -- | Used to filter the list of object attributes that are associated with a
    -- certain facet.
    facetFilter :: Prelude.Maybe SchemaFacet,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Reference of the object whose attributes need to be listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListObjectAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facetFilter', 'batchListObjectAttributes_facetFilter' - Used to filter the list of object attributes that are associated with a
-- certain facet.
--
-- 'maxResults', 'batchListObjectAttributes_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'nextToken', 'batchListObjectAttributes_nextToken' - The pagination token.
--
-- 'objectReference', 'batchListObjectAttributes_objectReference' - Reference of the object whose attributes need to be listed.
newBatchListObjectAttributes ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectAttributes
newBatchListObjectAttributes pObjectReference_ =
  BatchListObjectAttributes'
    { facetFilter =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      objectReference = pObjectReference_
    }

-- | Used to filter the list of object attributes that are associated with a
-- certain facet.
batchListObjectAttributes_facetFilter :: Lens.Lens' BatchListObjectAttributes (Prelude.Maybe SchemaFacet)
batchListObjectAttributes_facetFilter = Lens.lens (\BatchListObjectAttributes' {facetFilter} -> facetFilter) (\s@BatchListObjectAttributes' {} a -> s {facetFilter = a} :: BatchListObjectAttributes)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
batchListObjectAttributes_maxResults :: Lens.Lens' BatchListObjectAttributes (Prelude.Maybe Prelude.Natural)
batchListObjectAttributes_maxResults = Lens.lens (\BatchListObjectAttributes' {maxResults} -> maxResults) (\s@BatchListObjectAttributes' {} a -> s {maxResults = a} :: BatchListObjectAttributes)

-- | The pagination token.
batchListObjectAttributes_nextToken :: Lens.Lens' BatchListObjectAttributes (Prelude.Maybe Prelude.Text)
batchListObjectAttributes_nextToken = Lens.lens (\BatchListObjectAttributes' {nextToken} -> nextToken) (\s@BatchListObjectAttributes' {} a -> s {nextToken = a} :: BatchListObjectAttributes)

-- | Reference of the object whose attributes need to be listed.
batchListObjectAttributes_objectReference :: Lens.Lens' BatchListObjectAttributes ObjectReference
batchListObjectAttributes_objectReference = Lens.lens (\BatchListObjectAttributes' {objectReference} -> objectReference) (\s@BatchListObjectAttributes' {} a -> s {objectReference = a} :: BatchListObjectAttributes)

instance Prelude.Hashable BatchListObjectAttributes where
  hashWithSalt _salt BatchListObjectAttributes' {..} =
    _salt `Prelude.hashWithSalt` facetFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchListObjectAttributes where
  rnf BatchListObjectAttributes' {..} =
    Prelude.rnf facetFilter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToJSON BatchListObjectAttributes where
  toJSON BatchListObjectAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FacetFilter" Data..=) Prelude.<$> facetFilter,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
