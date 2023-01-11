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
-- Module      : Amazonka.MacieV2.Types.SearchResourcesCriteriaBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SearchResourcesCriteriaBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SearchResourcesCriteria
import qualified Amazonka.Prelude as Prelude

-- | Specifies property- and tag-based conditions that define filter criteria
-- for including or excluding Amazon Web Services resources from the query
-- results.
--
-- /See:/ 'newSearchResourcesCriteriaBlock' smart constructor.
data SearchResourcesCriteriaBlock = SearchResourcesCriteriaBlock'
  { -- | An array of objects, one for each property- or tag-based condition that
    -- includes or excludes resources from the query results. If you specify
    -- more than one condition, Amazon Macie uses AND logic to join the
    -- conditions.
    and :: Prelude.Maybe [SearchResourcesCriteria]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesCriteriaBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'and', 'searchResourcesCriteriaBlock_and' - An array of objects, one for each property- or tag-based condition that
-- includes or excludes resources from the query results. If you specify
-- more than one condition, Amazon Macie uses AND logic to join the
-- conditions.
newSearchResourcesCriteriaBlock ::
  SearchResourcesCriteriaBlock
newSearchResourcesCriteriaBlock =
  SearchResourcesCriteriaBlock'
    { and =
        Prelude.Nothing
    }

-- | An array of objects, one for each property- or tag-based condition that
-- includes or excludes resources from the query results. If you specify
-- more than one condition, Amazon Macie uses AND logic to join the
-- conditions.
searchResourcesCriteriaBlock_and :: Lens.Lens' SearchResourcesCriteriaBlock (Prelude.Maybe [SearchResourcesCriteria])
searchResourcesCriteriaBlock_and = Lens.lens (\SearchResourcesCriteriaBlock' {and} -> and) (\s@SearchResourcesCriteriaBlock' {} a -> s {and = a} :: SearchResourcesCriteriaBlock) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    SearchResourcesCriteriaBlock
  where
  hashWithSalt _salt SearchResourcesCriteriaBlock' {..} =
    _salt `Prelude.hashWithSalt` and

instance Prelude.NFData SearchResourcesCriteriaBlock where
  rnf SearchResourcesCriteriaBlock' {..} =
    Prelude.rnf and

instance Data.ToJSON SearchResourcesCriteriaBlock where
  toJSON SearchResourcesCriteriaBlock' {..} =
    Data.object
      (Prelude.catMaybes [("and" Data..=) Prelude.<$> and])
