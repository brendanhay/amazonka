{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
  ( SelectAttributesActivity (..)
  -- * Smart constructor
  , mkSelectAttributesActivity
  -- * Lenses
  , saaName
  , saaAttributes
  , saaNext
  ) where

import qualified Network.AWS.IoTAnalytics.Types.ActivityName as Types
import qualified Network.AWS.IoTAnalytics.Types.AttributeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Creates a new message using only the specified attributes from the original message.
--
-- /See:/ 'mkSelectAttributesActivity' smart constructor.
data SelectAttributesActivity = SelectAttributesActivity'
  { name :: Types.ActivityName
    -- ^ The name of the @selectAttributes@ activity.
  , attributes :: Core.NonEmpty Types.AttributeName
    -- ^ A list of the attributes to select from the message.
  , next :: Core.Maybe Types.ActivityName
    -- ^ The next activity in the pipeline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelectAttributesActivity' value with any optional fields omitted.
mkSelectAttributesActivity
    :: Types.ActivityName -- ^ 'name'
    -> Core.NonEmpty Types.AttributeName -- ^ 'attributes'
    -> SelectAttributesActivity
mkSelectAttributesActivity name attributes
  = SelectAttributesActivity'{name, attributes, next = Core.Nothing}

-- | The name of the @selectAttributes@ activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaName :: Lens.Lens' SelectAttributesActivity Types.ActivityName
saaName = Lens.field @"name"
{-# INLINEABLE saaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of the attributes to select from the message.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaAttributes :: Lens.Lens' SelectAttributesActivity (Core.NonEmpty Types.AttributeName)
saaAttributes = Lens.field @"attributes"
{-# INLINEABLE saaAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaNext :: Lens.Lens' SelectAttributesActivity (Core.Maybe Types.ActivityName)
saaNext = Lens.field @"next"
{-# INLINEABLE saaNext #-}
{-# DEPRECATED next "Use generic-lens or generic-optics with 'next' instead"  #-}

instance Core.FromJSON SelectAttributesActivity where
        toJSON SelectAttributesActivity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("attributes" Core..= attributes),
                  ("next" Core..=) Core.<$> next])

instance Core.FromJSON SelectAttributesActivity where
        parseJSON
          = Core.withObject "SelectAttributesActivity" Core.$
              \ x ->
                SelectAttributesActivity' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "attributes" Core.<*>
                    x Core..:? "next"
