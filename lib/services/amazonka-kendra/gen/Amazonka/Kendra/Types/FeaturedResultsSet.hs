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
-- Module      : Amazonka.Kendra.Types.FeaturedResultsSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FeaturedResultsSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.FeaturedDocument
import Amazonka.Kendra.Types.FeaturedResultsSetStatus
import qualified Amazonka.Prelude as Prelude

-- | A set of featured results that are displayed at the top of your search
-- results. Featured results are placed above all other results for certain
-- queries. If there\'s an exact match of a query, then one or more
-- specific documents are featured in the search results.
--
-- /See:/ 'newFeaturedResultsSet' smart constructor.
data FeaturedResultsSet = FeaturedResultsSet'
  { -- | The Unix timestamp when the set of featured results was created.
    creationTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The description for the set of featured results.
    description :: Prelude.Maybe Prelude.Text,
    -- | The list of document IDs for the documents you want to feature at the
    -- top of the search results page. You can use the
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_Query.html Query> API
    -- to search for specific documents with their document IDs included in the
    -- result items, or you can use the console.
    --
    -- You can add up to four featured documents. You can request to increase
    -- this limit by contacting <http://aws.amazon.com/contact-us/ Support>.
    --
    -- Specific queries are mapped to specific documents for featuring in the
    -- results. If a query contains an exact match, then one or more specific
    -- documents are featured in the results. The exact match applies to the
    -- full query. For example, if you only specify \'Kendra\', queries such as
    -- \'How does kendra semantically rank results?\' will not render the
    -- featured results. Featured results are designed for specific queries,
    -- rather than queries that are too broad in scope.
    featuredDocuments :: Prelude.Maybe [FeaturedDocument],
    -- | The identifier of the set of featured results.
    featuredResultsSetId :: Prelude.Maybe Prelude.Text,
    -- | The name for the set of featured results.
    featuredResultsSetName :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp when the set of featured results was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The list of queries for featuring results.
    --
    -- Specific queries are mapped to specific documents for featuring in the
    -- results. If a query contains an exact match, then one or more specific
    -- documents are featured in the results. The exact match applies to the
    -- full query. For example, if you only specify \'Kendra\', queries such as
    -- \'How does kendra semantically rank results?\' will not render the
    -- featured results. Featured results are designed for specific queries,
    -- rather than queries that are too broad in scope.
    queryTexts :: Prelude.Maybe [Prelude.Text],
    -- | The current status of the set of featured results. When the value is
    -- @ACTIVE@, featured results are ready for use. You can still configure
    -- your settings before setting the status to @ACTIVE@. You can set the
    -- status to @ACTIVE@ or @INACTIVE@ using the
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
    -- API. The queries you specify for featured results must be unique per
    -- featured results set for each index, whether the status is @ACTIVE@ or
    -- @INACTIVE@.
    status :: Prelude.Maybe FeaturedResultsSetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeaturedResultsSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'featuredResultsSet_creationTimestamp' - The Unix timestamp when the set of featured results was created.
--
-- 'description', 'featuredResultsSet_description' - The description for the set of featured results.
--
-- 'featuredDocuments', 'featuredResultsSet_featuredDocuments' - The list of document IDs for the documents you want to feature at the
-- top of the search results page. You can use the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Query.html Query> API
-- to search for specific documents with their document IDs included in the
-- result items, or you can use the console.
--
-- You can add up to four featured documents. You can request to increase
-- this limit by contacting <http://aws.amazon.com/contact-us/ Support>.
--
-- Specific queries are mapped to specific documents for featuring in the
-- results. If a query contains an exact match, then one or more specific
-- documents are featured in the results. The exact match applies to the
-- full query. For example, if you only specify \'Kendra\', queries such as
-- \'How does kendra semantically rank results?\' will not render the
-- featured results. Featured results are designed for specific queries,
-- rather than queries that are too broad in scope.
--
-- 'featuredResultsSetId', 'featuredResultsSet_featuredResultsSetId' - The identifier of the set of featured results.
--
-- 'featuredResultsSetName', 'featuredResultsSet_featuredResultsSetName' - The name for the set of featured results.
--
-- 'lastUpdatedTimestamp', 'featuredResultsSet_lastUpdatedTimestamp' - The Unix timestamp when the set of featured results was last updated.
--
-- 'queryTexts', 'featuredResultsSet_queryTexts' - The list of queries for featuring results.
--
-- Specific queries are mapped to specific documents for featuring in the
-- results. If a query contains an exact match, then one or more specific
-- documents are featured in the results. The exact match applies to the
-- full query. For example, if you only specify \'Kendra\', queries such as
-- \'How does kendra semantically rank results?\' will not render the
-- featured results. Featured results are designed for specific queries,
-- rather than queries that are too broad in scope.
--
-- 'status', 'featuredResultsSet_status' - The current status of the set of featured results. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. You can set the
-- status to @ACTIVE@ or @INACTIVE@ using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
-- API. The queries you specify for featured results must be unique per
-- featured results set for each index, whether the status is @ACTIVE@ or
-- @INACTIVE@.
newFeaturedResultsSet ::
  FeaturedResultsSet
newFeaturedResultsSet =
  FeaturedResultsSet'
    { creationTimestamp =
        Prelude.Nothing,
      description = Prelude.Nothing,
      featuredDocuments = Prelude.Nothing,
      featuredResultsSetId = Prelude.Nothing,
      featuredResultsSetName = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      queryTexts = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Unix timestamp when the set of featured results was created.
featuredResultsSet_creationTimestamp :: Lens.Lens' FeaturedResultsSet (Prelude.Maybe Prelude.Integer)
featuredResultsSet_creationTimestamp = Lens.lens (\FeaturedResultsSet' {creationTimestamp} -> creationTimestamp) (\s@FeaturedResultsSet' {} a -> s {creationTimestamp = a} :: FeaturedResultsSet)

-- | The description for the set of featured results.
featuredResultsSet_description :: Lens.Lens' FeaturedResultsSet (Prelude.Maybe Prelude.Text)
featuredResultsSet_description = Lens.lens (\FeaturedResultsSet' {description} -> description) (\s@FeaturedResultsSet' {} a -> s {description = a} :: FeaturedResultsSet)

-- | The list of document IDs for the documents you want to feature at the
-- top of the search results page. You can use the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_Query.html Query> API
-- to search for specific documents with their document IDs included in the
-- result items, or you can use the console.
--
-- You can add up to four featured documents. You can request to increase
-- this limit by contacting <http://aws.amazon.com/contact-us/ Support>.
--
-- Specific queries are mapped to specific documents for featuring in the
-- results. If a query contains an exact match, then one or more specific
-- documents are featured in the results. The exact match applies to the
-- full query. For example, if you only specify \'Kendra\', queries such as
-- \'How does kendra semantically rank results?\' will not render the
-- featured results. Featured results are designed for specific queries,
-- rather than queries that are too broad in scope.
featuredResultsSet_featuredDocuments :: Lens.Lens' FeaturedResultsSet (Prelude.Maybe [FeaturedDocument])
featuredResultsSet_featuredDocuments = Lens.lens (\FeaturedResultsSet' {featuredDocuments} -> featuredDocuments) (\s@FeaturedResultsSet' {} a -> s {featuredDocuments = a} :: FeaturedResultsSet) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the set of featured results.
featuredResultsSet_featuredResultsSetId :: Lens.Lens' FeaturedResultsSet (Prelude.Maybe Prelude.Text)
featuredResultsSet_featuredResultsSetId = Lens.lens (\FeaturedResultsSet' {featuredResultsSetId} -> featuredResultsSetId) (\s@FeaturedResultsSet' {} a -> s {featuredResultsSetId = a} :: FeaturedResultsSet)

-- | The name for the set of featured results.
featuredResultsSet_featuredResultsSetName :: Lens.Lens' FeaturedResultsSet (Prelude.Maybe Prelude.Text)
featuredResultsSet_featuredResultsSetName = Lens.lens (\FeaturedResultsSet' {featuredResultsSetName} -> featuredResultsSetName) (\s@FeaturedResultsSet' {} a -> s {featuredResultsSetName = a} :: FeaturedResultsSet)

-- | The Unix timestamp when the set of featured results was last updated.
featuredResultsSet_lastUpdatedTimestamp :: Lens.Lens' FeaturedResultsSet (Prelude.Maybe Prelude.Integer)
featuredResultsSet_lastUpdatedTimestamp = Lens.lens (\FeaturedResultsSet' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@FeaturedResultsSet' {} a -> s {lastUpdatedTimestamp = a} :: FeaturedResultsSet)

-- | The list of queries for featuring results.
--
-- Specific queries are mapped to specific documents for featuring in the
-- results. If a query contains an exact match, then one or more specific
-- documents are featured in the results. The exact match applies to the
-- full query. For example, if you only specify \'Kendra\', queries such as
-- \'How does kendra semantically rank results?\' will not render the
-- featured results. Featured results are designed for specific queries,
-- rather than queries that are too broad in scope.
featuredResultsSet_queryTexts :: Lens.Lens' FeaturedResultsSet (Prelude.Maybe [Prelude.Text])
featuredResultsSet_queryTexts = Lens.lens (\FeaturedResultsSet' {queryTexts} -> queryTexts) (\s@FeaturedResultsSet' {} a -> s {queryTexts = a} :: FeaturedResultsSet) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the set of featured results. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. You can set the
-- status to @ACTIVE@ or @INACTIVE@ using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
-- API. The queries you specify for featured results must be unique per
-- featured results set for each index, whether the status is @ACTIVE@ or
-- @INACTIVE@.
featuredResultsSet_status :: Lens.Lens' FeaturedResultsSet (Prelude.Maybe FeaturedResultsSetStatus)
featuredResultsSet_status = Lens.lens (\FeaturedResultsSet' {status} -> status) (\s@FeaturedResultsSet' {} a -> s {status = a} :: FeaturedResultsSet)

instance Data.FromJSON FeaturedResultsSet where
  parseJSON =
    Data.withObject
      "FeaturedResultsSet"
      ( \x ->
          FeaturedResultsSet'
            Prelude.<$> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> ( x
                            Data..:? "FeaturedDocuments"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FeaturedResultsSetId")
            Prelude.<*> (x Data..:? "FeaturedResultsSetName")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "QueryTexts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable FeaturedResultsSet where
  hashWithSalt _salt FeaturedResultsSet' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` featuredDocuments
      `Prelude.hashWithSalt` featuredResultsSetId
      `Prelude.hashWithSalt` featuredResultsSetName
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` queryTexts
      `Prelude.hashWithSalt` status

instance Prelude.NFData FeaturedResultsSet where
  rnf FeaturedResultsSet' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf featuredDocuments
      `Prelude.seq` Prelude.rnf featuredResultsSetId
      `Prelude.seq` Prelude.rnf featuredResultsSetName
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf queryTexts
      `Prelude.seq` Prelude.rnf status
