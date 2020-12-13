{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemFilter
  ( OpsItemFilter (..),

    -- * Smart constructor
    mkOpsItemFilter,

    -- * Lenses
    oifValues,
    oifOperator,
    oifKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.OpsItemFilterKey
import Network.AWS.SSM.Types.OpsItemFilterOperator

-- | Describes an OpsItem filter.
--
-- /See:/ 'mkOpsItemFilter' smart constructor.
data OpsItemFilter = OpsItemFilter'
  { -- | The filter value.
    values :: [Lude.Text],
    -- | The operator used by the filter call.
    operator :: OpsItemFilterOperator,
    -- | The name of the filter.
    key :: OpsItemFilterKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsItemFilter' with the minimum fields required to make a request.
--
-- * 'values' - The filter value.
-- * 'operator' - The operator used by the filter call.
-- * 'key' - The name of the filter.
mkOpsItemFilter ::
  -- | 'operator'
  OpsItemFilterOperator ->
  -- | 'key'
  OpsItemFilterKey ->
  OpsItemFilter
mkOpsItemFilter pOperator_ pKey_ =
  OpsItemFilter'
    { values = Lude.mempty,
      operator = pOperator_,
      key = pKey_
    }

-- | The filter value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oifValues :: Lens.Lens' OpsItemFilter [Lude.Text]
oifValues = Lens.lens (values :: OpsItemFilter -> [Lude.Text]) (\s a -> s {values = a} :: OpsItemFilter)
{-# DEPRECATED oifValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The operator used by the filter call.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oifOperator :: Lens.Lens' OpsItemFilter OpsItemFilterOperator
oifOperator = Lens.lens (operator :: OpsItemFilter -> OpsItemFilterOperator) (\s a -> s {operator = a} :: OpsItemFilter)
{-# DEPRECATED oifOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oifKey :: Lens.Lens' OpsItemFilter OpsItemFilterKey
oifKey = Lens.lens (key :: OpsItemFilter -> OpsItemFilterKey) (\s a -> s {key = a} :: OpsItemFilter)
{-# DEPRECATED oifKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON OpsItemFilter where
  toJSON OpsItemFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Values" Lude..= values),
            Lude.Just ("Operator" Lude..= operator),
            Lude.Just ("Key" Lude..= key)
          ]
      )
