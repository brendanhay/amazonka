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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns a paginated list of all the outgoing TypedLinkSpecifier
-- information for an object inside a BatchRead operation. For more
-- information, see ListOutgoingTypedLinks and BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListOutgoingTypedLinks' smart constructor.
data BatchListOutgoingTypedLinks = BatchListOutgoingTypedLinks'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filters are interpreted in the order of the attributes defined on the
    -- typed link facet, not the order they are supplied to any API calls.
    filterTypedLink :: Prelude.Maybe TypedLinkSchemaAndFacetName,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Provides range filters for multiple attributes. When providing ranges to
    -- typed link selection, any inexact ranges must be specified at the end.
    -- Any attributes that do not have a range specified are presumed to match
    -- the entire range.
    filterAttributeRanges :: Prelude.Maybe [TypedLinkAttributeRange],
    -- | The reference that identifies the object whose attributes will be
    -- listed.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListOutgoingTypedLinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListOutgoingTypedLinks_nextToken' - The pagination token.
--
-- 'filterTypedLink', 'batchListOutgoingTypedLinks_filterTypedLink' - Filters are interpreted in the order of the attributes defined on the
-- typed link facet, not the order they are supplied to any API calls.
--
-- 'maxResults', 'batchListOutgoingTypedLinks_maxResults' - The maximum number of results to retrieve.
--
-- 'filterAttributeRanges', 'batchListOutgoingTypedLinks_filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
--
-- 'objectReference', 'batchListOutgoingTypedLinks_objectReference' - The reference that identifies the object whose attributes will be
-- listed.
newBatchListOutgoingTypedLinks ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListOutgoingTypedLinks
newBatchListOutgoingTypedLinks pObjectReference_ =
  BatchListOutgoingTypedLinks'
    { nextToken =
        Prelude.Nothing,
      filterTypedLink = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filterAttributeRanges = Prelude.Nothing,
      objectReference = pObjectReference_
    }

-- | The pagination token.
batchListOutgoingTypedLinks_nextToken :: Lens.Lens' BatchListOutgoingTypedLinks (Prelude.Maybe Prelude.Text)
batchListOutgoingTypedLinks_nextToken = Lens.lens (\BatchListOutgoingTypedLinks' {nextToken} -> nextToken) (\s@BatchListOutgoingTypedLinks' {} a -> s {nextToken = a} :: BatchListOutgoingTypedLinks)

-- | Filters are interpreted in the order of the attributes defined on the
-- typed link facet, not the order they are supplied to any API calls.
batchListOutgoingTypedLinks_filterTypedLink :: Lens.Lens' BatchListOutgoingTypedLinks (Prelude.Maybe TypedLinkSchemaAndFacetName)
batchListOutgoingTypedLinks_filterTypedLink = Lens.lens (\BatchListOutgoingTypedLinks' {filterTypedLink} -> filterTypedLink) (\s@BatchListOutgoingTypedLinks' {} a -> s {filterTypedLink = a} :: BatchListOutgoingTypedLinks)

-- | The maximum number of results to retrieve.
batchListOutgoingTypedLinks_maxResults :: Lens.Lens' BatchListOutgoingTypedLinks (Prelude.Maybe Prelude.Natural)
batchListOutgoingTypedLinks_maxResults = Lens.lens (\BatchListOutgoingTypedLinks' {maxResults} -> maxResults) (\s@BatchListOutgoingTypedLinks' {} a -> s {maxResults = a} :: BatchListOutgoingTypedLinks)

-- | Provides range filters for multiple attributes. When providing ranges to
-- typed link selection, any inexact ranges must be specified at the end.
-- Any attributes that do not have a range specified are presumed to match
-- the entire range.
batchListOutgoingTypedLinks_filterAttributeRanges :: Lens.Lens' BatchListOutgoingTypedLinks (Prelude.Maybe [TypedLinkAttributeRange])
batchListOutgoingTypedLinks_filterAttributeRanges = Lens.lens (\BatchListOutgoingTypedLinks' {filterAttributeRanges} -> filterAttributeRanges) (\s@BatchListOutgoingTypedLinks' {} a -> s {filterAttributeRanges = a} :: BatchListOutgoingTypedLinks) Prelude.. Lens.mapping Lens._Coerce

-- | The reference that identifies the object whose attributes will be
-- listed.
batchListOutgoingTypedLinks_objectReference :: Lens.Lens' BatchListOutgoingTypedLinks ObjectReference
batchListOutgoingTypedLinks_objectReference = Lens.lens (\BatchListOutgoingTypedLinks' {objectReference} -> objectReference) (\s@BatchListOutgoingTypedLinks' {} a -> s {objectReference = a} :: BatchListOutgoingTypedLinks)

instance Prelude.Hashable BatchListOutgoingTypedLinks

instance Prelude.NFData BatchListOutgoingTypedLinks

instance Core.ToJSON BatchListOutgoingTypedLinks where
  toJSON BatchListOutgoingTypedLinks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("FilterTypedLink" Core..=)
              Prelude.<$> filterTypedLink,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("FilterAttributeRanges" Core..=)
              Prelude.<$> filterAttributeRanges,
            Prelude.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
