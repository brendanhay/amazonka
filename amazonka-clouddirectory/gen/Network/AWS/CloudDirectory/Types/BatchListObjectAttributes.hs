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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectAttributes where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a ListObjectAttributes operation.
--
-- /See:/ 'newBatchListObjectAttributes' smart constructor.
data BatchListObjectAttributes = BatchListObjectAttributes'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to be retrieved in a single call. This is an
    -- approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | Used to filter the list of object attributes that are associated with a
    -- certain facet.
    facetFilter :: Core.Maybe SchemaFacet,
    -- | Reference of the object whose attributes need to be listed.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchListObjectAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListObjectAttributes_nextToken' - The pagination token.
--
-- 'maxResults', 'batchListObjectAttributes_maxResults' - The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
--
-- 'facetFilter', 'batchListObjectAttributes_facetFilter' - Used to filter the list of object attributes that are associated with a
-- certain facet.
--
-- 'objectReference', 'batchListObjectAttributes_objectReference' - Reference of the object whose attributes need to be listed.
newBatchListObjectAttributes ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectAttributes
newBatchListObjectAttributes pObjectReference_ =
  BatchListObjectAttributes'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      facetFilter = Core.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
batchListObjectAttributes_nextToken :: Lens.Lens' BatchListObjectAttributes (Core.Maybe Core.Text)
batchListObjectAttributes_nextToken = Lens.lens (\BatchListObjectAttributes' {nextToken} -> nextToken) (\s@BatchListObjectAttributes' {} a -> s {nextToken = a} :: BatchListObjectAttributes)

-- | The maximum number of items to be retrieved in a single call. This is an
-- approximate number.
batchListObjectAttributes_maxResults :: Lens.Lens' BatchListObjectAttributes (Core.Maybe Core.Natural)
batchListObjectAttributes_maxResults = Lens.lens (\BatchListObjectAttributes' {maxResults} -> maxResults) (\s@BatchListObjectAttributes' {} a -> s {maxResults = a} :: BatchListObjectAttributes)

-- | Used to filter the list of object attributes that are associated with a
-- certain facet.
batchListObjectAttributes_facetFilter :: Lens.Lens' BatchListObjectAttributes (Core.Maybe SchemaFacet)
batchListObjectAttributes_facetFilter = Lens.lens (\BatchListObjectAttributes' {facetFilter} -> facetFilter) (\s@BatchListObjectAttributes' {} a -> s {facetFilter = a} :: BatchListObjectAttributes)

-- | Reference of the object whose attributes need to be listed.
batchListObjectAttributes_objectReference :: Lens.Lens' BatchListObjectAttributes ObjectReference
batchListObjectAttributes_objectReference = Lens.lens (\BatchListObjectAttributes' {objectReference} -> objectReference) (\s@BatchListObjectAttributes' {} a -> s {objectReference = a} :: BatchListObjectAttributes)

instance Core.Hashable BatchListObjectAttributes

instance Core.NFData BatchListObjectAttributes

instance Core.ToJSON BatchListObjectAttributes where
  toJSON BatchListObjectAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("FacetFilter" Core..=) Core.<$> facetFilter,
            Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
