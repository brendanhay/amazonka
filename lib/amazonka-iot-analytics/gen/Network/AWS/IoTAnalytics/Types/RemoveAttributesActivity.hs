{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
  ( RemoveAttributesActivity (..)
  -- * Smart constructor
  , mkRemoveAttributesActivity
  -- * Lenses
  , raaName
  , raaAttributes
  , raaNext
  ) where

import qualified Network.AWS.IoTAnalytics.Types.ActivityName as Types
import qualified Network.AWS.IoTAnalytics.Types.AttributeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An activity that removes attributes from a message.
--
-- /See:/ 'mkRemoveAttributesActivity' smart constructor.
data RemoveAttributesActivity = RemoveAttributesActivity'
  { name :: Types.ActivityName
    -- ^ The name of the @removeAttributes@ activity.
  , attributes :: Core.NonEmpty Types.AttributeName
    -- ^ A list of 1-50 attributes to remove from the message.
  , next :: Core.Maybe Types.ActivityName
    -- ^ The next activity in the pipeline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAttributesActivity' value with any optional fields omitted.
mkRemoveAttributesActivity
    :: Types.ActivityName -- ^ 'name'
    -> Core.NonEmpty Types.AttributeName -- ^ 'attributes'
    -> RemoveAttributesActivity
mkRemoveAttributesActivity name attributes
  = RemoveAttributesActivity'{name, attributes, next = Core.Nothing}

-- | The name of the @removeAttributes@ activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raaName :: Lens.Lens' RemoveAttributesActivity Types.ActivityName
raaName = Lens.field @"name"
{-# INLINEABLE raaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of 1-50 attributes to remove from the message.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raaAttributes :: Lens.Lens' RemoveAttributesActivity (Core.NonEmpty Types.AttributeName)
raaAttributes = Lens.field @"attributes"
{-# INLINEABLE raaAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raaNext :: Lens.Lens' RemoveAttributesActivity (Core.Maybe Types.ActivityName)
raaNext = Lens.field @"next"
{-# INLINEABLE raaNext #-}
{-# DEPRECATED next "Use generic-lens or generic-optics with 'next' instead"  #-}

instance Core.FromJSON RemoveAttributesActivity where
        toJSON RemoveAttributesActivity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("attributes" Core..= attributes),
                  ("next" Core..=) Core.<$> next])

instance Core.FromJSON RemoveAttributesActivity where
        parseJSON
          = Core.withObject "RemoveAttributesActivity" Core.$
              \ x ->
                RemoveAttributesActivity' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "attributes" Core.<*>
                    x Core..:? "next"
