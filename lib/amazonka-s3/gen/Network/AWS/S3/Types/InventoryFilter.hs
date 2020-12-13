{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventoryFilter
  ( InventoryFilter (..),

    -- * Smart constructor
    mkInventoryFilter,

    -- * Lenses
    ifPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
-- /See:/ 'mkInventoryFilter' smart constructor.
newtype InventoryFilter = InventoryFilter'
  { -- | The prefix that an object must have to be included in the inventory results.
    prefix :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InventoryFilter' with the minimum fields required to make a request.
--
-- * 'prefix' - The prefix that an object must have to be included in the inventory results.
mkInventoryFilter ::
  -- | 'prefix'
  Lude.Text ->
  InventoryFilter
mkInventoryFilter pPrefix_ = InventoryFilter' {prefix = pPrefix_}

-- | The prefix that an object must have to be included in the inventory results.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPrefix :: Lens.Lens' InventoryFilter Lude.Text
ifPrefix = Lens.lens (prefix :: InventoryFilter -> Lude.Text) (\s a -> s {prefix = a} :: InventoryFilter)
{-# DEPRECATED ifPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

instance Lude.FromXML InventoryFilter where
  parseXML x = InventoryFilter' Lude.<$> (x Lude..@ "Prefix")

instance Lude.ToXML InventoryFilter where
  toXML InventoryFilter' {..} = Lude.mconcat ["Prefix" Lude.@= prefix]
