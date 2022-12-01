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
-- Module      : Amazonka.ResourceExplorer2.Types.ResourceCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.ResourceCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the number of results that match the query. At this
-- time, Amazon Web Services Resource Explorer doesn\'t count more than
-- 1,000 matches for any query. This structure provides information about
-- whether the query exceeded this limit.
--
-- This field is included in every page when you paginate the results.
--
-- /See:/ 'newResourceCount' smart constructor.
data ResourceCount = ResourceCount'
  { -- | Indicates whether the @TotalResources@ value represents an exhaustive
    -- count of search results.
    --
    -- -   If @True@, it indicates that the search was exhaustive. Every
    --     resource that matches the query was counted.
    --
    -- -   If @False@, then the search reached the limit of 1,000 matching
    --     results, and stopped counting.
    complete :: Prelude.Maybe Prelude.Bool,
    -- | The number of resources that match the search query. This value can\'t
    -- exceed 1,000. If there are more than 1,000 resources that match the
    -- query, then only 1,000 are counted and the @Complete@ field is set to
    -- false. We recommend that you refine your query to return a smaller
    -- number of results.
    totalResources :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complete', 'resourceCount_complete' - Indicates whether the @TotalResources@ value represents an exhaustive
-- count of search results.
--
-- -   If @True@, it indicates that the search was exhaustive. Every
--     resource that matches the query was counted.
--
-- -   If @False@, then the search reached the limit of 1,000 matching
--     results, and stopped counting.
--
-- 'totalResources', 'resourceCount_totalResources' - The number of resources that match the search query. This value can\'t
-- exceed 1,000. If there are more than 1,000 resources that match the
-- query, then only 1,000 are counted and the @Complete@ field is set to
-- false. We recommend that you refine your query to return a smaller
-- number of results.
newResourceCount ::
  ResourceCount
newResourceCount =
  ResourceCount'
    { complete = Prelude.Nothing,
      totalResources = Prelude.Nothing
    }

-- | Indicates whether the @TotalResources@ value represents an exhaustive
-- count of search results.
--
-- -   If @True@, it indicates that the search was exhaustive. Every
--     resource that matches the query was counted.
--
-- -   If @False@, then the search reached the limit of 1,000 matching
--     results, and stopped counting.
resourceCount_complete :: Lens.Lens' ResourceCount (Prelude.Maybe Prelude.Bool)
resourceCount_complete = Lens.lens (\ResourceCount' {complete} -> complete) (\s@ResourceCount' {} a -> s {complete = a} :: ResourceCount)

-- | The number of resources that match the search query. This value can\'t
-- exceed 1,000. If there are more than 1,000 resources that match the
-- query, then only 1,000 are counted and the @Complete@ field is set to
-- false. We recommend that you refine your query to return a smaller
-- number of results.
resourceCount_totalResources :: Lens.Lens' ResourceCount (Prelude.Maybe Prelude.Integer)
resourceCount_totalResources = Lens.lens (\ResourceCount' {totalResources} -> totalResources) (\s@ResourceCount' {} a -> s {totalResources = a} :: ResourceCount)

instance Core.FromJSON ResourceCount where
  parseJSON =
    Core.withObject
      "ResourceCount"
      ( \x ->
          ResourceCount'
            Prelude.<$> (x Core..:? "Complete")
            Prelude.<*> (x Core..:? "TotalResources")
      )

instance Prelude.Hashable ResourceCount where
  hashWithSalt _salt ResourceCount' {..} =
    _salt `Prelude.hashWithSalt` complete
      `Prelude.hashWithSalt` totalResources

instance Prelude.NFData ResourceCount where
  rnf ResourceCount' {..} =
    Prelude.rnf complete
      `Prelude.seq` Prelude.rnf totalResources
