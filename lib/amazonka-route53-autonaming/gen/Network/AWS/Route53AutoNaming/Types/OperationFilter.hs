{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.OperationFilter
  ( OperationFilter (..)
  -- * Smart constructor
  , mkOperationFilter
  -- * Lenses
  , ofName
  , ofValues
  , ofCondition
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.FilterCondition as Types
import qualified Network.AWS.Route53AutoNaming.Types.FilterValue as Types
import qualified Network.AWS.Route53AutoNaming.Types.OperationFilterName as Types

-- | A complex type that lets you select the operations that you want to list.
--
-- /See:/ 'mkOperationFilter' smart constructor.
data OperationFilter = OperationFilter'
  { name :: Types.OperationFilterName
    -- ^ Specify the operations that you want to get:
--
--
--     * __NAMESPACE_ID__ : Gets operations related to specified namespaces.
--
--
--     * __SERVICE_ID__ : Gets operations related to specified services.
--
--
--     * __STATUS__ : Gets operations based on the status of the operations: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .
--
--
--     * __TYPE__ : Gets specified types of operation.
--
--
--     * __UPDATE_DATE__ : Gets operations that changed status during a specified date/time range. 
--
--
  , values :: [Types.FilterValue]
    -- ^ Specify values that are applicable to the value that you specify for @Name@ : 
--
--
--     * __NAMESPACE_ID__ : Specify one namespace ID.
--
--
--     * __SERVICE_ID__ : Specify one service ID.
--
--
--     * __STATUS__ : Specify one or more statuses: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .
--
--
--     * __TYPE__ : Specify one or more of the following types: @CREATE_NAMESPACE@ , @DELETE_NAMESPACE@ , @UPDATE_SERVICE@ , @REGISTER_INSTANCE@ , or @DEREGISTER_INSTANCE@ .
--
--
--     * __UPDATE_DATE__ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value.
--
--
  , condition :: Core.Maybe Types.FilterCondition
    -- ^ The operator that you want to use to determine whether an operation matches the specified value. Valid values for condition include:
--
--
--     * @EQ@ : When you specify @EQ@ for the condition, you can specify only one value. @EQ@ is supported for @NAMESPACE_ID@ , @SERVICE_ID@ , @STATUS@ , and @TYPE@ . @EQ@ is the default condition and can be omitted.
--
--
--     * @IN@ : When you specify @IN@ for the condition, you can specify a list of one or more values. @IN@ is supported for @STATUS@ and @TYPE@ . An operation must match one of the specified values to be returned in the response.
--
--
--     * @BETWEEN@ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value. @BETWEEN@ is supported for @UPDATE_DATE@ . 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OperationFilter' value with any optional fields omitted.
mkOperationFilter
    :: Types.OperationFilterName -- ^ 'name'
    -> OperationFilter
mkOperationFilter name
  = OperationFilter'{name, values = Core.mempty,
                     condition = Core.Nothing}

-- | Specify the operations that you want to get:
--
--
--     * __NAMESPACE_ID__ : Gets operations related to specified namespaces.
--
--
--     * __SERVICE_ID__ : Gets operations related to specified services.
--
--
--     * __STATUS__ : Gets operations based on the status of the operations: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .
--
--
--     * __TYPE__ : Gets specified types of operation.
--
--
--     * __UPDATE_DATE__ : Gets operations that changed status during a specified date/time range. 
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofName :: Lens.Lens' OperationFilter Types.OperationFilterName
ofName = Lens.field @"name"
{-# INLINEABLE ofName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specify values that are applicable to the value that you specify for @Name@ : 
--
--
--     * __NAMESPACE_ID__ : Specify one namespace ID.
--
--
--     * __SERVICE_ID__ : Specify one service ID.
--
--
--     * __STATUS__ : Specify one or more statuses: @SUBMITTED@ , @PENDING@ , @SUCCEED@ , or @FAIL@ .
--
--
--     * __TYPE__ : Specify one or more of the following types: @CREATE_NAMESPACE@ , @DELETE_NAMESPACE@ , @UPDATE_SERVICE@ , @REGISTER_INSTANCE@ , or @DEREGISTER_INSTANCE@ .
--
--
--     * __UPDATE_DATE__ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value.
--
--
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofValues :: Lens.Lens' OperationFilter [Types.FilterValue]
ofValues = Lens.field @"values"
{-# INLINEABLE ofValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

-- | The operator that you want to use to determine whether an operation matches the specified value. Valid values for condition include:
--
--
--     * @EQ@ : When you specify @EQ@ for the condition, you can specify only one value. @EQ@ is supported for @NAMESPACE_ID@ , @SERVICE_ID@ , @STATUS@ , and @TYPE@ . @EQ@ is the default condition and can be omitted.
--
--
--     * @IN@ : When you specify @IN@ for the condition, you can specify a list of one or more values. @IN@ is supported for @STATUS@ and @TYPE@ . An operation must match one of the specified values to be returned in the response.
--
--
--     * @BETWEEN@ : Specify a start date and an end date in Unix date/time format and Coordinated Universal Time (UTC). The start date must be the first value. @BETWEEN@ is supported for @UPDATE_DATE@ . 
--
--
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofCondition :: Lens.Lens' OperationFilter (Core.Maybe Types.FilterCondition)
ofCondition = Lens.field @"condition"
{-# INLINEABLE ofCondition #-}
{-# DEPRECATED condition "Use generic-lens or generic-optics with 'condition' instead"  #-}

instance Core.FromJSON OperationFilter where
        toJSON OperationFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Values" Core..= values),
                  ("Condition" Core..=) Core.<$> condition])
