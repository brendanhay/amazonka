{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.UnsuccessfulItem
  ( UnsuccessfulItem (..)
  -- * Smart constructor
  , mkUnsuccessfulItem
  -- * Lenses
  , uiError
  , uiResourceId
  ) where

import qualified Network.AWS.EC2.Types.UnsuccessfulItemError as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about items that were not successfully processed in a batch call.
--
-- /See:/ 'mkUnsuccessfulItem' smart constructor.
data UnsuccessfulItem = UnsuccessfulItem'
  { error :: Core.Maybe Types.UnsuccessfulItemError
    -- ^ Information about the error.
  , resourceId :: Core.Maybe Core.Text
    -- ^ The ID of the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnsuccessfulItem' value with any optional fields omitted.
mkUnsuccessfulItem
    :: UnsuccessfulItem
mkUnsuccessfulItem
  = UnsuccessfulItem'{error = Core.Nothing,
                      resourceId = Core.Nothing}

-- | Information about the error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiError :: Lens.Lens' UnsuccessfulItem (Core.Maybe Types.UnsuccessfulItemError)
uiError = Lens.field @"error"
{-# INLINEABLE uiError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiResourceId :: Lens.Lens' UnsuccessfulItem (Core.Maybe Core.Text)
uiResourceId = Lens.field @"resourceId"
{-# INLINEABLE uiResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

instance Core.FromXML UnsuccessfulItem where
        parseXML x
          = UnsuccessfulItem' Core.<$>
              (x Core..@? "error") Core.<*> x Core..@? "resourceId"
