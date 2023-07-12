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
-- Module      : Amazonka.Inspector2.Types.LambdaFunctionAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.LambdaFunctionAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.LambdaFunctionSortBy
import Amazonka.Inspector2.Types.MapFilter
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | The details that define a findings aggregation based on AWS Lambda
-- functions.
--
-- /See:/ 'newLambdaFunctionAggregation' smart constructor.
data LambdaFunctionAggregation = LambdaFunctionAggregation'
  { -- | The AWS Lambda function names to include in the aggregation results.
    functionNames :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The tags to include in the aggregation results.
    functionTags :: Prelude.Maybe (Prelude.NonEmpty MapFilter),
    -- | The resource IDs to include in the aggregation results.
    resourceIds :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | Returns findings aggregated by AWS Lambda function runtime environments.
    runtimes :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The finding severity to use for sorting the results.
    sortBy :: Prelude.Maybe LambdaFunctionSortBy,
    -- | The order to use for sorting the results.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionNames', 'lambdaFunctionAggregation_functionNames' - The AWS Lambda function names to include in the aggregation results.
--
-- 'functionTags', 'lambdaFunctionAggregation_functionTags' - The tags to include in the aggregation results.
--
-- 'resourceIds', 'lambdaFunctionAggregation_resourceIds' - The resource IDs to include in the aggregation results.
--
-- 'runtimes', 'lambdaFunctionAggregation_runtimes' - Returns findings aggregated by AWS Lambda function runtime environments.
--
-- 'sortBy', 'lambdaFunctionAggregation_sortBy' - The finding severity to use for sorting the results.
--
-- 'sortOrder', 'lambdaFunctionAggregation_sortOrder' - The order to use for sorting the results.
newLambdaFunctionAggregation ::
  LambdaFunctionAggregation
newLambdaFunctionAggregation =
  LambdaFunctionAggregation'
    { functionNames =
        Prelude.Nothing,
      functionTags = Prelude.Nothing,
      resourceIds = Prelude.Nothing,
      runtimes = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The AWS Lambda function names to include in the aggregation results.
lambdaFunctionAggregation_functionNames :: Lens.Lens' LambdaFunctionAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
lambdaFunctionAggregation_functionNames = Lens.lens (\LambdaFunctionAggregation' {functionNames} -> functionNames) (\s@LambdaFunctionAggregation' {} a -> s {functionNames = a} :: LambdaFunctionAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The tags to include in the aggregation results.
lambdaFunctionAggregation_functionTags :: Lens.Lens' LambdaFunctionAggregation (Prelude.Maybe (Prelude.NonEmpty MapFilter))
lambdaFunctionAggregation_functionTags = Lens.lens (\LambdaFunctionAggregation' {functionTags} -> functionTags) (\s@LambdaFunctionAggregation' {} a -> s {functionTags = a} :: LambdaFunctionAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The resource IDs to include in the aggregation results.
lambdaFunctionAggregation_resourceIds :: Lens.Lens' LambdaFunctionAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
lambdaFunctionAggregation_resourceIds = Lens.lens (\LambdaFunctionAggregation' {resourceIds} -> resourceIds) (\s@LambdaFunctionAggregation' {} a -> s {resourceIds = a} :: LambdaFunctionAggregation) Prelude.. Lens.mapping Lens.coerced

-- | Returns findings aggregated by AWS Lambda function runtime environments.
lambdaFunctionAggregation_runtimes :: Lens.Lens' LambdaFunctionAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
lambdaFunctionAggregation_runtimes = Lens.lens (\LambdaFunctionAggregation' {runtimes} -> runtimes) (\s@LambdaFunctionAggregation' {} a -> s {runtimes = a} :: LambdaFunctionAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The finding severity to use for sorting the results.
lambdaFunctionAggregation_sortBy :: Lens.Lens' LambdaFunctionAggregation (Prelude.Maybe LambdaFunctionSortBy)
lambdaFunctionAggregation_sortBy = Lens.lens (\LambdaFunctionAggregation' {sortBy} -> sortBy) (\s@LambdaFunctionAggregation' {} a -> s {sortBy = a} :: LambdaFunctionAggregation)

-- | The order to use for sorting the results.
lambdaFunctionAggregation_sortOrder :: Lens.Lens' LambdaFunctionAggregation (Prelude.Maybe SortOrder)
lambdaFunctionAggregation_sortOrder = Lens.lens (\LambdaFunctionAggregation' {sortOrder} -> sortOrder) (\s@LambdaFunctionAggregation' {} a -> s {sortOrder = a} :: LambdaFunctionAggregation)

instance Prelude.Hashable LambdaFunctionAggregation where
  hashWithSalt _salt LambdaFunctionAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` functionNames
      `Prelude.hashWithSalt` functionTags
      `Prelude.hashWithSalt` resourceIds
      `Prelude.hashWithSalt` runtimes
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData LambdaFunctionAggregation where
  rnf LambdaFunctionAggregation' {..} =
    Prelude.rnf functionNames
      `Prelude.seq` Prelude.rnf functionTags
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf runtimes
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON LambdaFunctionAggregation where
  toJSON LambdaFunctionAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("functionNames" Data..=) Prelude.<$> functionNames,
            ("functionTags" Data..=) Prelude.<$> functionTags,
            ("resourceIds" Data..=) Prelude.<$> resourceIds,
            ("runtimes" Data..=) Prelude.<$> runtimes,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
