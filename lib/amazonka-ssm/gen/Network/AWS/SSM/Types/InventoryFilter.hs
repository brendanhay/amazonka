{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryFilter
  ( InventoryFilter (..),

    -- * Smart constructor
    mkInventoryFilter,

    -- * Lenses
    ifType,
    ifKey,
    ifValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InventoryQueryOperatorType

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /See:/ 'mkInventoryFilter' smart constructor.
data InventoryFilter = InventoryFilter'
  { type' ::
      Lude.Maybe InventoryQueryOperatorType,
    key :: Lude.Text,
    values :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryFilter' with the minimum fields required to make a request.
--
-- * 'key' - The name of the filter key.
-- * 'type'' - The type of filter.
-- * 'values' - Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
mkInventoryFilter ::
  -- | 'key'
  Lude.Text ->
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  InventoryFilter
mkInventoryFilter pKey_ pValues_ =
  InventoryFilter'
    { type' = Lude.Nothing,
      key = pKey_,
      values = pValues_
    }

-- | The type of filter.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifType :: Lens.Lens' InventoryFilter (Lude.Maybe InventoryQueryOperatorType)
ifType = Lens.lens (type' :: InventoryFilter -> Lude.Maybe InventoryQueryOperatorType) (\s a -> s {type' = a} :: InventoryFilter)
{-# DEPRECATED ifType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name of the filter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifKey :: Lens.Lens' InventoryFilter Lude.Text
ifKey = Lens.lens (key :: InventoryFilter -> Lude.Text) (\s a -> s {key = a} :: InventoryFilter)
{-# DEPRECATED ifKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Inventory filter values. Example: inventory filter where instance IDs are specified as values Key=AWS:InstanceInformation.InstanceId,Values= i-a12b3c4d5e6g, i-1a2b3c4d5e6,Type=Equal
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifValues :: Lens.Lens' InventoryFilter (Lude.NonEmpty Lude.Text)
ifValues = Lens.lens (values :: InventoryFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: InventoryFilter)
{-# DEPRECATED ifValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.ToJSON InventoryFilter where
  toJSON InventoryFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Type" Lude..=) Lude.<$> type',
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("Values" Lude..= values)
          ]
      )
