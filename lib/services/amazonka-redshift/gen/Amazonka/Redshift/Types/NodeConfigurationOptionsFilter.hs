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
-- Module      : Amazonka.Redshift.Types.NodeConfigurationOptionsFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.NodeConfigurationOptionsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.NodeConfigurationOptionsFilterName
import Amazonka.Redshift.Types.OperatorType

-- | A set of elements to filter the returned node configurations.
--
-- /See:/ 'newNodeConfigurationOptionsFilter' smart constructor.
data NodeConfigurationOptionsFilter = NodeConfigurationOptionsFilter'
  { -- | List of values. Compare Name using Operator to Values. If filter Name is
    -- NumberOfNodes, then values can range from 0 to 200. If filter Name is
    -- EstimatedDiskUtilizationPercent, then values can range from 0 to 100.
    -- For example, filter NumberOfNodes (name) GT (operator) 3 (values).
    values :: Prelude.Maybe [Prelude.Text],
    -- | The filter operator. If filter Name is NodeType only the \'in\' operator
    -- is supported. Provide one value to evaluate for \'eq\', \'lt\', \'le\',
    -- \'gt\', and \'ge\'. Provide two values to evaluate for \'between\'.
    -- Provide a list of values for \'in\'.
    operator :: Prelude.Maybe OperatorType,
    -- | The name of the element to filter.
    name :: Prelude.Maybe NodeConfigurationOptionsFilterName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      operator = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | List of values. Compare Name using Operator to Values. If filter Name is
-- NumberOfNodes, then values can range from 0 to 200. If filter Name is
-- EstimatedDiskUtilizationPercent, then values can range from 0 to 100.
-- For example, filter NumberOfNodes (name) GT (operator) 3 (values).
nodeConfigurationOptionsFilter_values :: Lens.Lens' NodeConfigurationOptionsFilter (Prelude.Maybe [Prelude.Text])
nodeConfigurationOptionsFilter_values = Lens.lens (\NodeConfigurationOptionsFilter' {values} -> values) (\s@NodeConfigurationOptionsFilter' {} a -> s {values = a} :: NodeConfigurationOptionsFilter) Prelude.. Lens.mapping Lens.coerced

-- | The filter operator. If filter Name is NodeType only the \'in\' operator
-- is supported. Provide one value to evaluate for \'eq\', \'lt\', \'le\',
-- \'gt\', and \'ge\'. Provide two values to evaluate for \'between\'.
-- Provide a list of values for \'in\'.
nodeConfigurationOptionsFilter_operator :: Lens.Lens' NodeConfigurationOptionsFilter (Prelude.Maybe OperatorType)
nodeConfigurationOptionsFilter_operator = Lens.lens (\NodeConfigurationOptionsFilter' {operator} -> operator) (\s@NodeConfigurationOptionsFilter' {} a -> s {operator = a} :: NodeConfigurationOptionsFilter)

-- | The name of the element to filter.
nodeConfigurationOptionsFilter_name :: Lens.Lens' NodeConfigurationOptionsFilter (Prelude.Maybe NodeConfigurationOptionsFilterName)
nodeConfigurationOptionsFilter_name = Lens.lens (\NodeConfigurationOptionsFilter' {name} -> name) (\s@NodeConfigurationOptionsFilter' {} a -> s {name = a} :: NodeConfigurationOptionsFilter)

instance
  Prelude.Hashable
    NodeConfigurationOptionsFilter
  where
  hashWithSalt
    salt'
    NodeConfigurationOptionsFilter' {..} =
      salt' `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` operator
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    NodeConfigurationOptionsFilter
  where
  rnf NodeConfigurationOptionsFilter' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator

instance Core.ToQuery NodeConfigurationOptionsFilter where
  toQuery NodeConfigurationOptionsFilter' {..} =
    Prelude.mconcat
      [ "Value"
          Core.=: Core.toQuery
            (Core.toQueryList "item" Prelude.<$> values),
        "Operator" Core.=: operator,
        "Name" Core.=: name
      ]
