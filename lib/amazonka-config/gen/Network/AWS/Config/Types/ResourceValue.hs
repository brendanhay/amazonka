{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ResourceValue
  ( ResourceValue (..)
  -- * Smart constructor
  , mkResourceValue
  -- * Lenses
  , rvValue
  ) where

import qualified Network.AWS.Config.Types.ResourceValueType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The dynamic value of the resource.
--
-- /See:/ 'mkResourceValue' smart constructor.
newtype ResourceValue = ResourceValue'
  { value :: Types.ResourceValueType
    -- ^ The value is a resource ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceValue' value with any optional fields omitted.
mkResourceValue
    :: Types.ResourceValueType -- ^ 'value'
    -> ResourceValue
mkResourceValue value = ResourceValue'{value}

-- | The value is a resource ID.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvValue :: Lens.Lens' ResourceValue Types.ResourceValueType
rvValue = Lens.field @"value"
{-# INLINEABLE rvValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON ResourceValue where
        toJSON ResourceValue{..}
          = Core.object (Core.catMaybes [Core.Just ("Value" Core..= value)])

instance Core.FromJSON ResourceValue where
        parseJSON
          = Core.withObject "ResourceValue" Core.$
              \ x -> ResourceValue' Core.<$> (x Core..: "Value")
