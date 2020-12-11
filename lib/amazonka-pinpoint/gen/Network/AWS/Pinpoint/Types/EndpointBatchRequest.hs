-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointBatchRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointBatchRequest
  ( EndpointBatchRequest (..),

    -- * Smart constructor
    mkEndpointBatchRequest,

    -- * Lenses
    ebrItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointBatchItem
import qualified Network.AWS.Prelude as Lude

-- | Specifies a batch of endpoints to create or update and the settings and attributes to set or change for each endpoint.
--
-- /See:/ 'mkEndpointBatchRequest' smart constructor.
newtype EndpointBatchRequest = EndpointBatchRequest'
  { item ::
      [EndpointBatchItem]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointBatchRequest' with the minimum fields required to make a request.
--
-- * 'item' - An array that defines the endpoints to create or update and, for each endpoint, the property values to set or change. An array can contain a maximum of 100 items.
mkEndpointBatchRequest ::
  EndpointBatchRequest
mkEndpointBatchRequest = EndpointBatchRequest' {item = Lude.mempty}

-- | An array that defines the endpoints to create or update and, for each endpoint, the property values to set or change. An array can contain a maximum of 100 items.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebrItem :: Lens.Lens' EndpointBatchRequest [EndpointBatchItem]
ebrItem = Lens.lens (item :: EndpointBatchRequest -> [EndpointBatchItem]) (\s a -> s {item = a} :: EndpointBatchRequest)
{-# DEPRECATED ebrItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.ToJSON EndpointBatchRequest where
  toJSON EndpointBatchRequest' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Item" Lude..= item)])
