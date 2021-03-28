{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.InventoryFilter
  ( InventoryFilter (..)
  -- * Smart constructor
  , mkInventoryFilter
  -- * Lenses
  , ifPrefix
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Prefix as Types

-- | Specifies an inventory filter. The inventory only includes objects that meet the filter's criteria.
--
-- /See:/ 'mkInventoryFilter' smart constructor.
newtype InventoryFilter = InventoryFilter'
  { prefix :: Types.Prefix
    -- ^ The prefix that an object must have to be included in the inventory results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InventoryFilter' value with any optional fields omitted.
mkInventoryFilter
    :: Types.Prefix -- ^ 'prefix'
    -> InventoryFilter
mkInventoryFilter prefix = InventoryFilter'{prefix}

-- | The prefix that an object must have to be included in the inventory results.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPrefix :: Lens.Lens' InventoryFilter Types.Prefix
ifPrefix = Lens.field @"prefix"
{-# INLINEABLE ifPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

instance Core.ToXML InventoryFilter where
        toXML InventoryFilter{..} = Core.toXMLElement "Prefix" prefix

instance Core.FromXML InventoryFilter where
        parseXML x = InventoryFilter' Core.<$> (x Core..@ "Prefix")
