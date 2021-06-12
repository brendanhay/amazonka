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
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName
import Network.AWS.Redshift.Types.OperatorType

-- | A set of elements to filter the returned node configurations.
--
-- /See:/ 'newNodeConfigurationOptionsFilter' smart constructor.
data NodeConfigurationOptionsFilter = NodeConfigurationOptionsFilter'
  { -- | List of values. Compare Name using Operator to Values. If filter Name is
    -- NumberOfNodes, then values can range from 0 to 200. If filter Name is
    -- EstimatedDiskUtilizationPercent, then values can range from 0 to 100.
    -- For example, filter NumberOfNodes (name) GT (operator) 3 (values).
    values :: Core.Maybe [Core.Text],
    -- | The filter operator. If filter Name is NodeType only the \'in\' operator
    -- is supported. Provide one value to evaluate for \'eq\', \'lt\', \'le\',
    -- \'gt\', and \'ge\'. Provide two values to evaluate for \'between\'.
    -- Provide a list of values for \'in\'.
    operator :: Core.Maybe OperatorType,
    -- | The name of the element to filter.
    name :: Core.Maybe NodeConfigurationOptionsFilterName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeConfigurationOptionsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'nodeConfigurationOptionsFilter_values' - List of values. Compare Name using Operator to Values. If filter Name is
-- NumberOfNodes, then values can range from 0 to 200. If filter Name is
-- EstimatedDiskUtilizationPercent, then values can range from 0 to 100.
-- For example, filter NumberOfNodes (name) GT (operator) 3 (values).
--
-- 'operator', 'nodeConfigurationOptionsFilter_operator' - The filter operator. If filter Name is NodeType only the \'in\' operator
-- is supported. Provide one value to evaluate for \'eq\', \'lt\', \'le\',
-- \'gt\', and \'ge\'. Provide two values to evaluate for \'between\'.
-- Provide a list of values for \'in\'.
--
-- 'name', 'nodeConfigurationOptionsFilter_name' - The name of the element to filter.
newNodeConfigurationOptionsFilter ::
  NodeConfigurationOptionsFilter
newNodeConfigurationOptionsFilter =
  NodeConfigurationOptionsFilter'
    { values =
        Core.Nothing,
      operator = Core.Nothing,
      name = Core.Nothing
    }

-- | List of values. Compare Name using Operator to Values. If filter Name is
-- NumberOfNodes, then values can range from 0 to 200. If filter Name is
-- EstimatedDiskUtilizationPercent, then values can range from 0 to 100.
-- For example, filter NumberOfNodes (name) GT (operator) 3 (values).
nodeConfigurationOptionsFilter_values :: Lens.Lens' NodeConfigurationOptionsFilter (Core.Maybe [Core.Text])
nodeConfigurationOptionsFilter_values = Lens.lens (\NodeConfigurationOptionsFilter' {values} -> values) (\s@NodeConfigurationOptionsFilter' {} a -> s {values = a} :: NodeConfigurationOptionsFilter) Core.. Lens.mapping Lens._Coerce

-- | The filter operator. If filter Name is NodeType only the \'in\' operator
-- is supported. Provide one value to evaluate for \'eq\', \'lt\', \'le\',
-- \'gt\', and \'ge\'. Provide two values to evaluate for \'between\'.
-- Provide a list of values for \'in\'.
nodeConfigurationOptionsFilter_operator :: Lens.Lens' NodeConfigurationOptionsFilter (Core.Maybe OperatorType)
nodeConfigurationOptionsFilter_operator = Lens.lens (\NodeConfigurationOptionsFilter' {operator} -> operator) (\s@NodeConfigurationOptionsFilter' {} a -> s {operator = a} :: NodeConfigurationOptionsFilter)

-- | The name of the element to filter.
nodeConfigurationOptionsFilter_name :: Lens.Lens' NodeConfigurationOptionsFilter (Core.Maybe NodeConfigurationOptionsFilterName)
nodeConfigurationOptionsFilter_name = Lens.lens (\NodeConfigurationOptionsFilter' {name} -> name) (\s@NodeConfigurationOptionsFilter' {} a -> s {name = a} :: NodeConfigurationOptionsFilter)

instance Core.Hashable NodeConfigurationOptionsFilter

instance Core.NFData NodeConfigurationOptionsFilter

instance Core.ToQuery NodeConfigurationOptionsFilter where
  toQuery NodeConfigurationOptionsFilter' {..} =
    Core.mconcat
      [ "Value"
          Core.=: Core.toQuery
            (Core.toQueryList "item" Core.<$> values),
        "Operator" Core.=: operator,
        "Name" Core.=: name
      ]
