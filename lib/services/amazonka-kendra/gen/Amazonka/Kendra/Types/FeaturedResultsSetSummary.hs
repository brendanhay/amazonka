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
-- Module      : Amazonka.Kendra.Types.FeaturedResultsSetSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FeaturedResultsSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.FeaturedResultsSetStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information for a set of featured results. Featured results are
-- placed above all other results for certain queries. If there\'s an exact
-- match of a query, then one or more specific documents are featured in
-- the search results.
--
-- /See:/ 'newFeaturedResultsSetSummary' smart constructor.
data FeaturedResultsSetSummary = FeaturedResultsSetSummary'
  { -- | The Unix timestamp when the set of featured results was created.
    creationTimestamp :: Prelude.Maybe Prelude.Integer,
    -- | The identifier of the set of featured results.
    featuredResultsSetId :: Prelude.Maybe Prelude.Text,
    -- | The name for the set of featured results.
    featuredResultsSetName :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp when the set of featured results was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Integer,
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
-- Create a value of 'FeaturedResultsSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'featuredResultsSetSummary_creationTimestamp' - The Unix timestamp when the set of featured results was created.
--
-- 'featuredResultsSetId', 'featuredResultsSetSummary_featuredResultsSetId' - The identifier of the set of featured results.
--
-- 'featuredResultsSetName', 'featuredResultsSetSummary_featuredResultsSetName' - The name for the set of featured results.
--
-- 'lastUpdatedTimestamp', 'featuredResultsSetSummary_lastUpdatedTimestamp' - The Unix timestamp when the set of featured results was last updated.
--
-- 'status', 'featuredResultsSetSummary_status' - The current status of the set of featured results. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. You can set the
-- status to @ACTIVE@ or @INACTIVE@ using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
-- API. The queries you specify for featured results must be unique per
-- featured results set for each index, whether the status is @ACTIVE@ or
-- @INACTIVE@.
newFeaturedResultsSetSummary ::
  FeaturedResultsSetSummary
newFeaturedResultsSetSummary =
  FeaturedResultsSetSummary'
    { creationTimestamp =
        Prelude.Nothing,
      featuredResultsSetId = Prelude.Nothing,
      featuredResultsSetName = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Unix timestamp when the set of featured results was created.
featuredResultsSetSummary_creationTimestamp :: Lens.Lens' FeaturedResultsSetSummary (Prelude.Maybe Prelude.Integer)
featuredResultsSetSummary_creationTimestamp = Lens.lens (\FeaturedResultsSetSummary' {creationTimestamp} -> creationTimestamp) (\s@FeaturedResultsSetSummary' {} a -> s {creationTimestamp = a} :: FeaturedResultsSetSummary)

-- | The identifier of the set of featured results.
featuredResultsSetSummary_featuredResultsSetId :: Lens.Lens' FeaturedResultsSetSummary (Prelude.Maybe Prelude.Text)
featuredResultsSetSummary_featuredResultsSetId = Lens.lens (\FeaturedResultsSetSummary' {featuredResultsSetId} -> featuredResultsSetId) (\s@FeaturedResultsSetSummary' {} a -> s {featuredResultsSetId = a} :: FeaturedResultsSetSummary)

-- | The name for the set of featured results.
featuredResultsSetSummary_featuredResultsSetName :: Lens.Lens' FeaturedResultsSetSummary (Prelude.Maybe Prelude.Text)
featuredResultsSetSummary_featuredResultsSetName = Lens.lens (\FeaturedResultsSetSummary' {featuredResultsSetName} -> featuredResultsSetName) (\s@FeaturedResultsSetSummary' {} a -> s {featuredResultsSetName = a} :: FeaturedResultsSetSummary)

-- | The Unix timestamp when the set of featured results was last updated.
featuredResultsSetSummary_lastUpdatedTimestamp :: Lens.Lens' FeaturedResultsSetSummary (Prelude.Maybe Prelude.Integer)
featuredResultsSetSummary_lastUpdatedTimestamp = Lens.lens (\FeaturedResultsSetSummary' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@FeaturedResultsSetSummary' {} a -> s {lastUpdatedTimestamp = a} :: FeaturedResultsSetSummary)

-- | The current status of the set of featured results. When the value is
-- @ACTIVE@, featured results are ready for use. You can still configure
-- your settings before setting the status to @ACTIVE@. You can set the
-- status to @ACTIVE@ or @INACTIVE@ using the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_UpdateFeaturedResultsSet.html UpdateFeaturedResultsSet>
-- API. The queries you specify for featured results must be unique per
-- featured results set for each index, whether the status is @ACTIVE@ or
-- @INACTIVE@.
featuredResultsSetSummary_status :: Lens.Lens' FeaturedResultsSetSummary (Prelude.Maybe FeaturedResultsSetStatus)
featuredResultsSetSummary_status = Lens.lens (\FeaturedResultsSetSummary' {status} -> status) (\s@FeaturedResultsSetSummary' {} a -> s {status = a} :: FeaturedResultsSetSummary)

instance Data.FromJSON FeaturedResultsSetSummary where
  parseJSON =
    Data.withObject
      "FeaturedResultsSetSummary"
      ( \x ->
          FeaturedResultsSetSummary'
            Prelude.<$> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "FeaturedResultsSetId")
            Prelude.<*> (x Data..:? "FeaturedResultsSetName")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable FeaturedResultsSetSummary where
  hashWithSalt _salt FeaturedResultsSetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` featuredResultsSetId
      `Prelude.hashWithSalt` featuredResultsSetName
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` status

instance Prelude.NFData FeaturedResultsSetSummary where
  rnf FeaturedResultsSetSummary' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf featuredResultsSetId
      `Prelude.seq` Prelude.rnf featuredResultsSetName
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf status
