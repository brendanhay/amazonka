{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointBatchRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EndpointBatchRequest
  ( EndpointBatchRequest (..)
  -- * Smart constructor
  , mkEndpointBatchRequest
  -- * Lenses
  , ebrItem
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EndpointBatchItem as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies a batch of endpoints to create or update and the settings and attributes to set or change for each endpoint.
--
-- /See:/ 'mkEndpointBatchRequest' smart constructor.
newtype EndpointBatchRequest = EndpointBatchRequest'
  { item :: [Types.EndpointBatchItem]
    -- ^ An array that defines the endpoints to create or update and, for each endpoint, the property values to set or change. An array can contain a maximum of 100 items.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointBatchRequest' value with any optional fields omitted.
mkEndpointBatchRequest
    :: EndpointBatchRequest
mkEndpointBatchRequest = EndpointBatchRequest'{item = Core.mempty}

-- | An array that defines the endpoints to create or update and, for each endpoint, the property values to set or change. An array can contain a maximum of 100 items.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebrItem :: Lens.Lens' EndpointBatchRequest [Types.EndpointBatchItem]
ebrItem = Lens.field @"item"
{-# INLINEABLE ebrItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

instance Core.FromJSON EndpointBatchRequest where
        toJSON EndpointBatchRequest{..}
          = Core.object (Core.catMaybes [Core.Just ("Item" Core..= item)])
