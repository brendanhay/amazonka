{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EndpointsResponse
  ( EndpointsResponse (..)
  -- * Smart constructor
  , mkEndpointsResponse
  -- * Lenses
  , erItem
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EndpointResponse as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about all the endpoints that are associated with a user ID.
--
-- /See:/ 'mkEndpointsResponse' smart constructor.
newtype EndpointsResponse = EndpointsResponse'
  { item :: [Types.EndpointResponse]
    -- ^ An array of responses, one for each endpoint that's associated with the user ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointsResponse' value with any optional fields omitted.
mkEndpointsResponse
    :: EndpointsResponse
mkEndpointsResponse = EndpointsResponse'{item = Core.mempty}

-- | An array of responses, one for each endpoint that's associated with the user ID.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erItem :: Lens.Lens' EndpointsResponse [Types.EndpointResponse]
erItem = Lens.field @"item"
{-# INLINEABLE erItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

instance Core.FromJSON EndpointsResponse where
        parseJSON
          = Core.withObject "EndpointsResponse" Core.$
              \ x ->
                EndpointsResponse' Core.<$>
                  (x Core..:? "Item" Core..!= Core.mempty)
