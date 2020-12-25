{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter
  ( NodeConfigurationOptionsFilter (..),

    -- * Smart constructor
    mkNodeConfigurationOptionsFilter,

    -- * Lenses
    ncofName,
    ncofOperator,
    ncofValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName as Types
import qualified Network.AWS.Redshift.Types.OperatorType as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | A set of elements to filter the returned node configurations.
--
-- /See:/ 'mkNodeConfigurationOptionsFilter' smart constructor.
data NodeConfigurationOptionsFilter = NodeConfigurationOptionsFilter'
  { -- | The name of the element to filter.
    name :: Core.Maybe Types.NodeConfigurationOptionsFilterName,
    -- | The filter operator. If filter Name is NodeType only the 'in' operator is supported. Provide one value to evaluate for 'eq', 'lt', 'le', 'gt', and 'ge'. Provide two values to evaluate for 'between'. Provide a list of values for 'in'.
    operator :: Core.Maybe Types.OperatorType,
    -- | List of values. Compare Name using Operator to Values. If filter Name is NumberOfNodes, then values can range from 0 to 200. If filter Name is EstimatedDiskUtilizationPercent, then values can range from 0 to 100. For example, filter NumberOfNodes (name) GT (operator) 3 (values).
    values :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeConfigurationOptionsFilter' value with any optional fields omitted.
mkNodeConfigurationOptionsFilter ::
  NodeConfigurationOptionsFilter
mkNodeConfigurationOptionsFilter =
  NodeConfigurationOptionsFilter'
    { name = Core.Nothing,
      operator = Core.Nothing,
      values = Core.Nothing
    }

-- | The name of the element to filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncofName :: Lens.Lens' NodeConfigurationOptionsFilter (Core.Maybe Types.NodeConfigurationOptionsFilterName)
ncofName = Lens.field @"name"
{-# DEPRECATED ncofName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The filter operator. If filter Name is NodeType only the 'in' operator is supported. Provide one value to evaluate for 'eq', 'lt', 'le', 'gt', and 'ge'. Provide two values to evaluate for 'between'. Provide a list of values for 'in'.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncofOperator :: Lens.Lens' NodeConfigurationOptionsFilter (Core.Maybe Types.OperatorType)
ncofOperator = Lens.field @"operator"
{-# DEPRECATED ncofOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

-- | List of values. Compare Name using Operator to Values. If filter Name is NumberOfNodes, then values can range from 0 to 200. If filter Name is EstimatedDiskUtilizationPercent, then values can range from 0 to 100. For example, filter NumberOfNodes (name) GT (operator) 3 (values).
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncofValues :: Lens.Lens' NodeConfigurationOptionsFilter (Core.Maybe [Types.String])
ncofValues = Lens.field @"values"
{-# DEPRECATED ncofValues "Use generic-lens or generic-optics with 'values' instead." #-}
