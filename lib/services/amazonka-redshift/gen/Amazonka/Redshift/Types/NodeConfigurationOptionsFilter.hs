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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.NodeConfigurationOptionsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.NodeConfigurationOptionsFilterName
import Amazonka.Redshift.Types.OperatorType

-- | A set of elements to filter the returned node configurations.
--
-- /See:/ 'newNodeConfigurationOptionsFilter' smart constructor.
data NodeConfigurationOptionsFilter = NodeConfigurationOptionsFilter'
  { -- | The name of the element to filter.
    name :: Prelude.Maybe NodeConfigurationOptionsFilterName,
    -- | The filter operator. If filter Name is NodeType only the \'in\' operator
    -- is supported. Provide one value to evaluate for \'eq\', \'lt\', \'le\',
    -- \'gt\', and \'ge\'. Provide two values to evaluate for \'between\'.
    -- Provide a list of values for \'in\'.
    operator :: Prelude.Maybe OperatorType,
    -- | List of values. Compare Name using Operator to Values. If filter Name is
    -- NumberOfNodes, then values can range from 0 to 200. If filter Name is
    -- EstimatedDiskUtilizationPercent, then values can range from 0 to 100.
    -- For example, filter NumberOfNodes (name) GT (operator) 3 (values).
    values :: Prelude.Maybe [Prelude.Text]
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
-- 'name', 'nodeConfigurationOptionsFilter_name' - The name of the element to filter.
--
-- 'operator', 'nodeConfigurationOptionsFilter_operator' - The filter operator. If filter Name is NodeType only the \'in\' operator
-- is supported. Provide one value to evaluate for \'eq\', \'lt\', \'le\',
-- \'gt\', and \'ge\'. Provide two values to evaluate for \'between\'.
-- Provide a list of values for \'in\'.
--
-- 'values', 'nodeConfigurationOptionsFilter_values' - List of values. Compare Name using Operator to Values. If filter Name is
-- NumberOfNodes, then values can range from 0 to 200. If filter Name is
-- EstimatedDiskUtilizationPercent, then values can range from 0 to 100.
-- For example, filter NumberOfNodes (name) GT (operator) 3 (values).
newNodeConfigurationOptionsFilter ::
  NodeConfigurationOptionsFilter
newNodeConfigurationOptionsFilter =
  NodeConfigurationOptionsFilter'
    { name =
        Prelude.Nothing,
      operator = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the element to filter.
nodeConfigurationOptionsFilter_name :: Lens.Lens' NodeConfigurationOptionsFilter (Prelude.Maybe NodeConfigurationOptionsFilterName)
nodeConfigurationOptionsFilter_name = Lens.lens (\NodeConfigurationOptionsFilter' {name} -> name) (\s@NodeConfigurationOptionsFilter' {} a -> s {name = a} :: NodeConfigurationOptionsFilter)

-- | The filter operator. If filter Name is NodeType only the \'in\' operator
-- is supported. Provide one value to evaluate for \'eq\', \'lt\', \'le\',
-- \'gt\', and \'ge\'. Provide two values to evaluate for \'between\'.
-- Provide a list of values for \'in\'.
nodeConfigurationOptionsFilter_operator :: Lens.Lens' NodeConfigurationOptionsFilter (Prelude.Maybe OperatorType)
nodeConfigurationOptionsFilter_operator = Lens.lens (\NodeConfigurationOptionsFilter' {operator} -> operator) (\s@NodeConfigurationOptionsFilter' {} a -> s {operator = a} :: NodeConfigurationOptionsFilter)

-- | List of values. Compare Name using Operator to Values. If filter Name is
-- NumberOfNodes, then values can range from 0 to 200. If filter Name is
-- EstimatedDiskUtilizationPercent, then values can range from 0 to 100.
-- For example, filter NumberOfNodes (name) GT (operator) 3 (values).
nodeConfigurationOptionsFilter_values :: Lens.Lens' NodeConfigurationOptionsFilter (Prelude.Maybe [Prelude.Text])
nodeConfigurationOptionsFilter_values = Lens.lens (\NodeConfigurationOptionsFilter' {values} -> values) (\s@NodeConfigurationOptionsFilter' {} a -> s {values = a} :: NodeConfigurationOptionsFilter) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    NodeConfigurationOptionsFilter
  where
  hashWithSalt
    _salt
    NodeConfigurationOptionsFilter' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` operator
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    NodeConfigurationOptionsFilter
  where
  rnf NodeConfigurationOptionsFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf values

instance Data.ToQuery NodeConfigurationOptionsFilter where
  toQuery NodeConfigurationOptionsFilter' {..} =
    Prelude.mconcat
      [ "Name" Data.=: name,
        "Operator" Data.=: operator,
        "Value"
          Data.=: Data.toQuery
            (Data.toQueryList "item" Prelude.<$> values)
      ]
