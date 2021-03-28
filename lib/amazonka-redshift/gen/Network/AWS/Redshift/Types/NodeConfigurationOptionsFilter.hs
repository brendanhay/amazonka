{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.NodeConfigurationOptionsFilter
  ( NodeConfigurationOptionsFilter (..)
  -- * Smart constructor
  , mkNodeConfigurationOptionsFilter
  -- * Lenses
  , ncofName
  , ncofOperator
  , ncofValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.NodeConfigurationOptionsFilterName as Types
import qualified Network.AWS.Redshift.Types.OperatorType as Types

-- | A set of elements to filter the returned node configurations.
--
-- /See:/ 'mkNodeConfigurationOptionsFilter' smart constructor.
data NodeConfigurationOptionsFilter = NodeConfigurationOptionsFilter'
  { name :: Core.Maybe Types.NodeConfigurationOptionsFilterName
    -- ^ The name of the element to filter.
  , operator :: Core.Maybe Types.OperatorType
    -- ^ The filter operator. If filter Name is NodeType only the 'in' operator is supported. Provide one value to evaluate for 'eq', 'lt', 'le', 'gt', and 'ge'. Provide two values to evaluate for 'between'. Provide a list of values for 'in'.
  , values :: Core.Maybe [Core.Text]
    -- ^ List of values. Compare Name using Operator to Values. If filter Name is NumberOfNodes, then values can range from 0 to 200. If filter Name is EstimatedDiskUtilizationPercent, then values can range from 0 to 100. For example, filter NumberOfNodes (name) GT (operator) 3 (values).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeConfigurationOptionsFilter' value with any optional fields omitted.
mkNodeConfigurationOptionsFilter
    :: NodeConfigurationOptionsFilter
mkNodeConfigurationOptionsFilter
  = NodeConfigurationOptionsFilter'{name = Core.Nothing,
                                    operator = Core.Nothing, values = Core.Nothing}

-- | The name of the element to filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncofName :: Lens.Lens' NodeConfigurationOptionsFilter (Core.Maybe Types.NodeConfigurationOptionsFilterName)
ncofName = Lens.field @"name"
{-# INLINEABLE ncofName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The filter operator. If filter Name is NodeType only the 'in' operator is supported. Provide one value to evaluate for 'eq', 'lt', 'le', 'gt', and 'ge'. Provide two values to evaluate for 'between'. Provide a list of values for 'in'.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncofOperator :: Lens.Lens' NodeConfigurationOptionsFilter (Core.Maybe Types.OperatorType)
ncofOperator = Lens.field @"operator"
{-# INLINEABLE ncofOperator #-}
{-# DEPRECATED operator "Use generic-lens or generic-optics with 'operator' instead"  #-}

-- | List of values. Compare Name using Operator to Values. If filter Name is NumberOfNodes, then values can range from 0 to 200. If filter Name is EstimatedDiskUtilizationPercent, then values can range from 0 to 100. For example, filter NumberOfNodes (name) GT (operator) 3 (values).
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncofValues :: Lens.Lens' NodeConfigurationOptionsFilter (Core.Maybe [Core.Text])
ncofValues = Lens.field @"values"
{-# INLINEABLE ncofValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery NodeConfigurationOptionsFilter where
        toQuery NodeConfigurationOptionsFilter{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Name") name Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Operator") operator
              Core.<>
              Core.toQueryPair "Value"
                (Core.maybe Core.mempty (Core.toQueryList "item") values)
