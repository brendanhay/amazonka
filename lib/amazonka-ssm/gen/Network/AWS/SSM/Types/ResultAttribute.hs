{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResultAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResultAttribute
  ( ResultAttribute (..)
  -- * Smart constructor
  , mkResultAttribute
  -- * Lenses
  , raTypeName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.InventoryItemTypeName as Types

-- | The inventory item result attribute.
--
-- /See:/ 'mkResultAttribute' smart constructor.
newtype ResultAttribute = ResultAttribute'
  { typeName :: Types.InventoryItemTypeName
    -- ^ Name of the inventory item type. Valid value: AWS:InstanceInformation. Default Value: AWS:InstanceInformation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResultAttribute' value with any optional fields omitted.
mkResultAttribute
    :: Types.InventoryItemTypeName -- ^ 'typeName'
    -> ResultAttribute
mkResultAttribute typeName = ResultAttribute'{typeName}

-- | Name of the inventory item type. Valid value: AWS:InstanceInformation. Default Value: AWS:InstanceInformation.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raTypeName :: Lens.Lens' ResultAttribute Types.InventoryItemTypeName
raTypeName = Lens.field @"typeName"
{-# INLINEABLE raTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

instance Core.FromJSON ResultAttribute where
        toJSON ResultAttribute{..}
          = Core.object
              (Core.catMaybes [Core.Just ("TypeName" Core..= typeName)])
