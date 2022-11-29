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
-- Module      : Amazonka.CloudDirectory.Types.BatchListIncomingTypedLinks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListIncomingTypedLinks where

import Amazonka.CloudDirectory.Types.ObjectReference
import Amazonka.CloudDirectory.Types.TypedLinkAttributeRange
import Amazonka.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns a paginated list of all the incoming TypedLinkSpecifier
-- information for an object inside a BatchRead operation. For more
-- information, see ListIncomingTypedLinks and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListIncomingTypedLinks' smart constructor.
data BatchListIncomingTypedLinks = BatchListIncomingTypedLinks'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Provides range filters for multiple attributes. When providing ranges to
    -- typed link selection, any inexact ranges must be specified at the end.
    -- Any attributes that do not have a range specified are presumed to match
    -- the entire range.
    filterAttributeRanges :: Prelude.Maybe [TypedLinkAttributeRange],
    -- | Filters are interpreted in the order of the attributes on the typed link
    -- facet, not the order in which they are supplied to any API calls.
    filterTypedLink :: Prelude.Maybe TypedLinkSchemaAndFacetName,
    -- | The reference that identifies the object whose attributes will be
    -- listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListIncomingTypedLinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListIncomingTypedLinks_nextToken' - The pagination token.
--
-- 'maxResults', 'batchListIncomingTypedLinks_maxResults' - The maximum number of results to retrieve.
--
-- 'filterAttributeRanges', 'batchListIncomingTypedLinks_filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
--
-- 'filterTypedLink', 'batchListIncomingTypedLinks_filterTypedLink' - Filters are interpreted in the order of the attributes on the typed link
-- facet, not the order in which they are supplied to any API calls.
--
-- 'objectReference', 'batchListIncomingTypedLinks_objectReference' - The reference that identifies the object whose attributes will be
-- listed.
newBatchListIncomingTypedLinks ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListIncomingTypedLinks
newBatchListIncomingTypedLinks pObjectReference_ =
  BatchListIncomingTypedLinks'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filterAttributeRanges = Prelude.Nothing,
      filterTypedLink = Prelude.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
batchListIncomingTypedLinks_nextToken :: Lens.Lens' BatchListIncomingTypedLinks (Prelude.Maybe Prelude.Text)
batchListIncomingTypedLinks_nextToken = Lens.lens (\BatchListIncomingTypedLinks' {nextToken} -> nextToken) (\s@BatchListIncomingTypedLinks' {} a -> s {nextToken = a} :: BatchListIncomingTypedLinks)

-- | The maximum number of results to retrieve.
batchListIncomingTypedLinks_maxResults :: Lens.Lens' BatchListIncomingTypedLinks (Prelude.Maybe Prelude.Natural)
batchListIncomingTypedLinks_maxResults = Lens.lens (\BatchListIncomingTypedLinks' {maxResults} -> maxResults) (\s@BatchListIncomingTypedLinks' {} a -> s {maxResults = a} :: BatchListIncomingTypedLinks)

-- | Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
batchListIncomingTypedLinks_filterAttributeRanges :: Lens.Lens' BatchListIncomingTypedLinks (Prelude.Maybe [TypedLinkAttributeRange])
batchListIncomingTypedLinks_filterAttributeRanges = Lens.lens (\BatchListIncomingTypedLinks' {filterAttributeRanges} -> filterAttributeRanges) (\s@BatchListIncomingTypedLinks' {} a -> s {filterAttributeRanges = a} :: BatchListIncomingTypedLinks) Prelude.. Lens.mapping Lens.coerced

-- | Filters are interpreted in the order of the attributes on the typed link
-- facet, not the order in which they are supplied to any API calls.
batchListIncomingTypedLinks_filterTypedLink :: Lens.Lens' BatchListIncomingTypedLinks (Prelude.Maybe TypedLinkSchemaAndFacetName)
batchListIncomingTypedLinks_filterTypedLink = Lens.lens (\BatchListIncomingTypedLinks' {filterTypedLink} -> filterTypedLink) (\s@BatchListIncomingTypedLinks' {} a -> s {filterTypedLink = a} :: BatchListIncomingTypedLinks)

-- | The reference that identifies the object whose attributes will be
-- listed.
batchListIncomingTypedLinks_objectReference :: Lens.Lens' BatchListIncomingTypedLinks ObjectReference
batchListIncomingTypedLinks_objectReference = Lens.lens (\BatchListIncomingTypedLinks' {objectReference} -> objectReference) (\s@BatchListIncomingTypedLinks' {} a -> s {objectReference = a} :: BatchListIncomingTypedLinks)

instance Prelude.Hashable BatchListIncomingTypedLinks where
  hashWithSalt _salt BatchListIncomingTypedLinks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` filterAttributeRanges
      `Prelude.hashWithSalt` filterTypedLink
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchListIncomingTypedLinks where
  rnf BatchListIncomingTypedLinks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf filterAttributeRanges
      `Prelude.seq` Prelude.rnf filterTypedLink
      `Prelude.seq` Prelude.rnf objectReference

instance Core.ToJSON BatchListIncomingTypedLinks where
  toJSON BatchListIncomingTypedLinks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("FilterAttributeRanges" Core..=)
              Prelude.<$> filterAttributeRanges,
            ("FilterTypedLink" Core..=)
              Prelude.<$> filterTypedLink,
            Prelude.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
