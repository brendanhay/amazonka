{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.UpdateAttributesRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.UpdateAttributesRequest
  ( UpdateAttributesRequest (..)
  -- * Smart constructor
  , mkUpdateAttributesRequest
  -- * Lenses
  , uarBlacklist
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies one or more attributes to remove from all the endpoints that are associated with an application.
--
-- /See:/ 'mkUpdateAttributesRequest' smart constructor.
newtype UpdateAttributesRequest = UpdateAttributesRequest'
  { blacklist :: Core.Maybe [Core.Text]
    -- ^ An array of the attributes to remove from all the endpoints that are associated with the application. The array can specify the complete, exact name of each attribute to remove or it can specify a glob pattern that an attribute name must match in order for the attribute to be removed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAttributesRequest' value with any optional fields omitted.
mkUpdateAttributesRequest
    :: UpdateAttributesRequest
mkUpdateAttributesRequest
  = UpdateAttributesRequest'{blacklist = Core.Nothing}

-- | An array of the attributes to remove from all the endpoints that are associated with the application. The array can specify the complete, exact name of each attribute to remove or it can specify a glob pattern that an attribute name must match in order for the attribute to be removed.
--
-- /Note:/ Consider using 'blacklist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarBlacklist :: Lens.Lens' UpdateAttributesRequest (Core.Maybe [Core.Text])
uarBlacklist = Lens.field @"blacklist"
{-# INLINEABLE uarBlacklist #-}
{-# DEPRECATED blacklist "Use generic-lens or generic-optics with 'blacklist' instead"  #-}

instance Core.FromJSON UpdateAttributesRequest where
        toJSON UpdateAttributesRequest{..}
          = Core.object
              (Core.catMaybes [("Blacklist" Core..=) Core.<$> blacklist])
