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
-- Module      : Network.AWS.SSM.Types.OpsAggregator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsAggregator where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.OpsFilter

-- | One or more aggregators for viewing counts of OpsData using different
-- dimensions such as @Source@, @CreatedTime@, or @Source and CreatedTime@,
-- to name a few.
--
-- /See:/ 'newOpsAggregator' smart constructor.
data OpsAggregator = OpsAggregator'
  { -- | The data type name to use for viewing counts of OpsData.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | A nested aggregator for viewing counts of OpsData.
    aggregators :: Prelude.Maybe (Prelude.NonEmpty OpsAggregator),
    -- | The aggregator value.
    values :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The aggregator filters.
    filters :: Prelude.Maybe (Prelude.NonEmpty OpsFilter),
    -- | The name of an OpsData attribute on which to limit the count of OpsData.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | Either a @Range@ or @Count@ aggregator for limiting an OpsData summary.
    aggregatorType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpsAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'opsAggregator_typeName' - The data type name to use for viewing counts of OpsData.
--
-- 'aggregators', 'opsAggregator_aggregators' - A nested aggregator for viewing counts of OpsData.
--
-- 'values', 'opsAggregator_values' - The aggregator value.
--
-- 'filters', 'opsAggregator_filters' - The aggregator filters.
--
-- 'attributeName', 'opsAggregator_attributeName' - The name of an OpsData attribute on which to limit the count of OpsData.
--
-- 'aggregatorType', 'opsAggregator_aggregatorType' - Either a @Range@ or @Count@ aggregator for limiting an OpsData summary.
newOpsAggregator ::
  OpsAggregator
newOpsAggregator =
  OpsAggregator'
    { typeName = Prelude.Nothing,
      aggregators = Prelude.Nothing,
      values = Prelude.Nothing,
      filters = Prelude.Nothing,
      attributeName = Prelude.Nothing,
      aggregatorType = Prelude.Nothing
    }

-- | The data type name to use for viewing counts of OpsData.
opsAggregator_typeName :: Lens.Lens' OpsAggregator (Prelude.Maybe Prelude.Text)
opsAggregator_typeName = Lens.lens (\OpsAggregator' {typeName} -> typeName) (\s@OpsAggregator' {} a -> s {typeName = a} :: OpsAggregator)

-- | A nested aggregator for viewing counts of OpsData.
opsAggregator_aggregators :: Lens.Lens' OpsAggregator (Prelude.Maybe (Prelude.NonEmpty OpsAggregator))
opsAggregator_aggregators = Lens.lens (\OpsAggregator' {aggregators} -> aggregators) (\s@OpsAggregator' {} a -> s {aggregators = a} :: OpsAggregator) Prelude.. Lens.mapping Lens.coerced

-- | The aggregator value.
opsAggregator_values :: Lens.Lens' OpsAggregator (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
opsAggregator_values = Lens.lens (\OpsAggregator' {values} -> values) (\s@OpsAggregator' {} a -> s {values = a} :: OpsAggregator) Prelude.. Lens.mapping Lens.coerced

-- | The aggregator filters.
opsAggregator_filters :: Lens.Lens' OpsAggregator (Prelude.Maybe (Prelude.NonEmpty OpsFilter))
opsAggregator_filters = Lens.lens (\OpsAggregator' {filters} -> filters) (\s@OpsAggregator' {} a -> s {filters = a} :: OpsAggregator) Prelude.. Lens.mapping Lens.coerced

-- | The name of an OpsData attribute on which to limit the count of OpsData.
opsAggregator_attributeName :: Lens.Lens' OpsAggregator (Prelude.Maybe Prelude.Text)
opsAggregator_attributeName = Lens.lens (\OpsAggregator' {attributeName} -> attributeName) (\s@OpsAggregator' {} a -> s {attributeName = a} :: OpsAggregator)

-- | Either a @Range@ or @Count@ aggregator for limiting an OpsData summary.
opsAggregator_aggregatorType :: Lens.Lens' OpsAggregator (Prelude.Maybe Prelude.Text)
opsAggregator_aggregatorType = Lens.lens (\OpsAggregator' {aggregatorType} -> aggregatorType) (\s@OpsAggregator' {} a -> s {aggregatorType = a} :: OpsAggregator)

instance Prelude.Hashable OpsAggregator

instance Prelude.NFData OpsAggregator

instance Core.ToJSON OpsAggregator where
  toJSON OpsAggregator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TypeName" Core..=) Prelude.<$> typeName,
            ("Aggregators" Core..=) Prelude.<$> aggregators,
            ("Values" Core..=) Prelude.<$> values,
            ("Filters" Core..=) Prelude.<$> filters,
            ("AttributeName" Core..=) Prelude.<$> attributeName,
            ("AggregatorType" Core..=)
              Prelude.<$> aggregatorType
          ]
      )
