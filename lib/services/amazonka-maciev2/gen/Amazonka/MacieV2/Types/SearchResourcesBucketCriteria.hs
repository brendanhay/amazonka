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
-- Module      : Amazonka.MacieV2.Types.SearchResourcesBucketCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SearchResourcesBucketCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SearchResourcesCriteriaBlock
import qualified Amazonka.Prelude as Prelude

-- | Specifies property- and tag-based conditions that define filter criteria
-- for including or excluding S3 buckets from the query results. Exclude
-- conditions take precedence over include conditions.
--
-- /See:/ 'newSearchResourcesBucketCriteria' smart constructor.
data SearchResourcesBucketCriteria = SearchResourcesBucketCriteria'
  { -- | The property- and tag-based conditions that determine which buckets to
    -- exclude from the results.
    excludes :: Prelude.Maybe SearchResourcesCriteriaBlock,
    -- | The property- and tag-based conditions that determine which buckets to
    -- include in the results.
    includes :: Prelude.Maybe SearchResourcesCriteriaBlock
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesBucketCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludes', 'searchResourcesBucketCriteria_excludes' - The property- and tag-based conditions that determine which buckets to
-- exclude from the results.
--
-- 'includes', 'searchResourcesBucketCriteria_includes' - The property- and tag-based conditions that determine which buckets to
-- include in the results.
newSearchResourcesBucketCriteria ::
  SearchResourcesBucketCriteria
newSearchResourcesBucketCriteria =
  SearchResourcesBucketCriteria'
    { excludes =
        Prelude.Nothing,
      includes = Prelude.Nothing
    }

-- | The property- and tag-based conditions that determine which buckets to
-- exclude from the results.
searchResourcesBucketCriteria_excludes :: Lens.Lens' SearchResourcesBucketCriteria (Prelude.Maybe SearchResourcesCriteriaBlock)
searchResourcesBucketCriteria_excludes = Lens.lens (\SearchResourcesBucketCriteria' {excludes} -> excludes) (\s@SearchResourcesBucketCriteria' {} a -> s {excludes = a} :: SearchResourcesBucketCriteria)

-- | The property- and tag-based conditions that determine which buckets to
-- include in the results.
searchResourcesBucketCriteria_includes :: Lens.Lens' SearchResourcesBucketCriteria (Prelude.Maybe SearchResourcesCriteriaBlock)
searchResourcesBucketCriteria_includes = Lens.lens (\SearchResourcesBucketCriteria' {includes} -> includes) (\s@SearchResourcesBucketCriteria' {} a -> s {includes = a} :: SearchResourcesBucketCriteria)

instance
  Prelude.Hashable
    SearchResourcesBucketCriteria
  where
  hashWithSalt _salt SearchResourcesBucketCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` excludes
      `Prelude.hashWithSalt` includes

instance Prelude.NFData SearchResourcesBucketCriteria where
  rnf SearchResourcesBucketCriteria' {..} =
    Prelude.rnf excludes
      `Prelude.seq` Prelude.rnf includes

instance Data.ToJSON SearchResourcesBucketCriteria where
  toJSON SearchResourcesBucketCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("excludes" Data..=) Prelude.<$> excludes,
            ("includes" Data..=) Prelude.<$> includes
          ]
      )
