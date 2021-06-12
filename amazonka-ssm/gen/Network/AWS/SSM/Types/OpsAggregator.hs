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
import Network.AWS.SSM.Types.OpsFilter

-- | One or more aggregators for viewing counts of OpsItems using different
-- dimensions such as @Source@, @CreatedTime@, or @Source and CreatedTime@,
-- to name a few.
--
-- /See:/ 'newOpsAggregator' smart constructor.
data OpsAggregator = OpsAggregator'
  { -- | The data type name to use for viewing counts of OpsItems.
    typeName :: Core.Maybe Core.Text,
    -- | The name of an OpsItem attribute on which to limit the count of
    -- OpsItems.
    attributeName :: Core.Maybe Core.Text,
    -- | The aggregator value.
    values :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Either a Range or Count aggregator for limiting an OpsItem summary.
    aggregatorType :: Core.Maybe Core.Text,
    -- | The aggregator filters.
    filters :: Core.Maybe (Core.NonEmpty OpsFilter),
    -- | A nested aggregator for viewing counts of OpsItems.
    aggregators :: Core.Maybe (Core.NonEmpty OpsAggregator)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpsAggregator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'opsAggregator_typeName' - The data type name to use for viewing counts of OpsItems.
--
-- 'attributeName', 'opsAggregator_attributeName' - The name of an OpsItem attribute on which to limit the count of
-- OpsItems.
--
-- 'values', 'opsAggregator_values' - The aggregator value.
--
-- 'aggregatorType', 'opsAggregator_aggregatorType' - Either a Range or Count aggregator for limiting an OpsItem summary.
--
-- 'filters', 'opsAggregator_filters' - The aggregator filters.
--
-- 'aggregators', 'opsAggregator_aggregators' - A nested aggregator for viewing counts of OpsItems.
newOpsAggregator ::
  OpsAggregator
newOpsAggregator =
  OpsAggregator'
    { typeName = Core.Nothing,
      attributeName = Core.Nothing,
      values = Core.Nothing,
      aggregatorType = Core.Nothing,
      filters = Core.Nothing,
      aggregators = Core.Nothing
    }

-- | The data type name to use for viewing counts of OpsItems.
opsAggregator_typeName :: Lens.Lens' OpsAggregator (Core.Maybe Core.Text)
opsAggregator_typeName = Lens.lens (\OpsAggregator' {typeName} -> typeName) (\s@OpsAggregator' {} a -> s {typeName = a} :: OpsAggregator)

-- | The name of an OpsItem attribute on which to limit the count of
-- OpsItems.
opsAggregator_attributeName :: Lens.Lens' OpsAggregator (Core.Maybe Core.Text)
opsAggregator_attributeName = Lens.lens (\OpsAggregator' {attributeName} -> attributeName) (\s@OpsAggregator' {} a -> s {attributeName = a} :: OpsAggregator)

-- | The aggregator value.
opsAggregator_values :: Lens.Lens' OpsAggregator (Core.Maybe (Core.HashMap Core.Text Core.Text))
opsAggregator_values = Lens.lens (\OpsAggregator' {values} -> values) (\s@OpsAggregator' {} a -> s {values = a} :: OpsAggregator) Core.. Lens.mapping Lens._Coerce

-- | Either a Range or Count aggregator for limiting an OpsItem summary.
opsAggregator_aggregatorType :: Lens.Lens' OpsAggregator (Core.Maybe Core.Text)
opsAggregator_aggregatorType = Lens.lens (\OpsAggregator' {aggregatorType} -> aggregatorType) (\s@OpsAggregator' {} a -> s {aggregatorType = a} :: OpsAggregator)

-- | The aggregator filters.
opsAggregator_filters :: Lens.Lens' OpsAggregator (Core.Maybe (Core.NonEmpty OpsFilter))
opsAggregator_filters = Lens.lens (\OpsAggregator' {filters} -> filters) (\s@OpsAggregator' {} a -> s {filters = a} :: OpsAggregator) Core.. Lens.mapping Lens._Coerce

-- | A nested aggregator for viewing counts of OpsItems.
opsAggregator_aggregators :: Lens.Lens' OpsAggregator (Core.Maybe (Core.NonEmpty OpsAggregator))
opsAggregator_aggregators = Lens.lens (\OpsAggregator' {aggregators} -> aggregators) (\s@OpsAggregator' {} a -> s {aggregators = a} :: OpsAggregator) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable OpsAggregator

instance Core.NFData OpsAggregator

instance Core.ToJSON OpsAggregator where
  toJSON OpsAggregator' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TypeName" Core..=) Core.<$> typeName,
            ("AttributeName" Core..=) Core.<$> attributeName,
            ("Values" Core..=) Core.<$> values,
            ("AggregatorType" Core..=) Core.<$> aggregatorType,
            ("Filters" Core..=) Core.<$> filters,
            ("Aggregators" Core..=) Core.<$> aggregators
          ]
      )
