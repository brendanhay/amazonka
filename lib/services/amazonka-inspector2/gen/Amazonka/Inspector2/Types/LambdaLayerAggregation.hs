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
-- Module      : Amazonka.Inspector2.Types.LambdaLayerAggregation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.LambdaLayerAggregation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.LambdaLayerSortBy
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.StringFilter
import qualified Amazonka.Prelude as Prelude

-- | The details that define a findings aggregation based on an AWS Lambda
-- function\'s layers.
--
-- /See:/ 'newLambdaLayerAggregation' smart constructor.
data LambdaLayerAggregation = LambdaLayerAggregation'
  { -- | The names of the AWS Lambda functions associated with the layers.
    functionNames :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The Amazon Resource Name (ARN) of the AWS Lambda function layer.
    layerArns :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The resource IDs for the AWS Lambda function layers.
    resourceIds :: Prelude.Maybe (Prelude.NonEmpty StringFilter),
    -- | The finding severity to use for sorting the results.
    sortBy :: Prelude.Maybe LambdaLayerSortBy,
    -- | The order to use for sorting the results.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaLayerAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionNames', 'lambdaLayerAggregation_functionNames' - The names of the AWS Lambda functions associated with the layers.
--
-- 'layerArns', 'lambdaLayerAggregation_layerArns' - The Amazon Resource Name (ARN) of the AWS Lambda function layer.
--
-- 'resourceIds', 'lambdaLayerAggregation_resourceIds' - The resource IDs for the AWS Lambda function layers.
--
-- 'sortBy', 'lambdaLayerAggregation_sortBy' - The finding severity to use for sorting the results.
--
-- 'sortOrder', 'lambdaLayerAggregation_sortOrder' - The order to use for sorting the results.
newLambdaLayerAggregation ::
  LambdaLayerAggregation
newLambdaLayerAggregation =
  LambdaLayerAggregation'
    { functionNames =
        Prelude.Nothing,
      layerArns = Prelude.Nothing,
      resourceIds = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | The names of the AWS Lambda functions associated with the layers.
lambdaLayerAggregation_functionNames :: Lens.Lens' LambdaLayerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
lambdaLayerAggregation_functionNames = Lens.lens (\LambdaLayerAggregation' {functionNames} -> functionNames) (\s@LambdaLayerAggregation' {} a -> s {functionNames = a} :: LambdaLayerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the AWS Lambda function layer.
lambdaLayerAggregation_layerArns :: Lens.Lens' LambdaLayerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
lambdaLayerAggregation_layerArns = Lens.lens (\LambdaLayerAggregation' {layerArns} -> layerArns) (\s@LambdaLayerAggregation' {} a -> s {layerArns = a} :: LambdaLayerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The resource IDs for the AWS Lambda function layers.
lambdaLayerAggregation_resourceIds :: Lens.Lens' LambdaLayerAggregation (Prelude.Maybe (Prelude.NonEmpty StringFilter))
lambdaLayerAggregation_resourceIds = Lens.lens (\LambdaLayerAggregation' {resourceIds} -> resourceIds) (\s@LambdaLayerAggregation' {} a -> s {resourceIds = a} :: LambdaLayerAggregation) Prelude.. Lens.mapping Lens.coerced

-- | The finding severity to use for sorting the results.
lambdaLayerAggregation_sortBy :: Lens.Lens' LambdaLayerAggregation (Prelude.Maybe LambdaLayerSortBy)
lambdaLayerAggregation_sortBy = Lens.lens (\LambdaLayerAggregation' {sortBy} -> sortBy) (\s@LambdaLayerAggregation' {} a -> s {sortBy = a} :: LambdaLayerAggregation)

-- | The order to use for sorting the results.
lambdaLayerAggregation_sortOrder :: Lens.Lens' LambdaLayerAggregation (Prelude.Maybe SortOrder)
lambdaLayerAggregation_sortOrder = Lens.lens (\LambdaLayerAggregation' {sortOrder} -> sortOrder) (\s@LambdaLayerAggregation' {} a -> s {sortOrder = a} :: LambdaLayerAggregation)

instance Prelude.Hashable LambdaLayerAggregation where
  hashWithSalt _salt LambdaLayerAggregation' {..} =
    _salt `Prelude.hashWithSalt` functionNames
      `Prelude.hashWithSalt` layerArns
      `Prelude.hashWithSalt` resourceIds
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData LambdaLayerAggregation where
  rnf LambdaLayerAggregation' {..} =
    Prelude.rnf functionNames
      `Prelude.seq` Prelude.rnf layerArns
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToJSON LambdaLayerAggregation where
  toJSON LambdaLayerAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("functionNames" Data..=) Prelude.<$> functionNames,
            ("layerArns" Data..=) Prelude.<$> layerArns,
            ("resourceIds" Data..=) Prelude.<$> resourceIds,
            ("sortBy" Data..=) Prelude.<$> sortBy,
            ("sortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )
