{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationFilter
  ( OperationFilter (..),

    -- * Smart constructor
    mkOperationFilter,

    -- * Lenses
    ofCondition,
    ofName,
    ofValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.FilterCondition
import Network.AWS.Route53AutoNaming.Types.OperationFilterName

-- | A complex type that lets you select the operations that you want to list.
--
-- /See:/ 'mkOperationFilter' smart constructor.
data OperationFilter = OperationFilter'
  { condition ::
      Lude.Maybe FilterCondition,
    name :: OperationFilterName,
    values :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OperationFilter' with the minimum fields required to make a request.
--
-- * 'condition' - The operator that you want to use to determine whether an operation matches the specified value. Valid values for condition include:
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
-- * 'name' - Specify the operations that you want to get:
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
-- * 'values' - Specify values that are applicable to the value that you specify for @Name@ :
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
mkOperationFilter ::
  -- | 'name'
  OperationFilterName ->
  OperationFilter
mkOperationFilter pName_ =
  OperationFilter'
    { condition = Lude.Nothing,
      name = pName_,
      values = Lude.mempty
    }

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
ofCondition :: Lens.Lens' OperationFilter (Lude.Maybe FilterCondition)
ofCondition = Lens.lens (condition :: OperationFilter -> Lude.Maybe FilterCondition) (\s a -> s {condition = a} :: OperationFilter)
{-# DEPRECATED ofCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

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
ofName :: Lens.Lens' OperationFilter OperationFilterName
ofName = Lens.lens (name :: OperationFilter -> OperationFilterName) (\s a -> s {name = a} :: OperationFilter)
{-# DEPRECATED ofName "Use generic-lens or generic-optics with 'name' instead." #-}

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
ofValues :: Lens.Lens' OperationFilter [Lude.Text]
ofValues = Lens.lens (values :: OperationFilter -> [Lude.Text]) (\s a -> s {values = a} :: OperationFilter)
{-# DEPRECATED ofValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.ToJSON OperationFilter where
  toJSON OperationFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Condition" Lude..=) Lude.<$> condition,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Values" Lude..= values)
          ]
      )
