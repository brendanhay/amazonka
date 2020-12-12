{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointsResponse
  ( EndpointsResponse (..),

    -- * Smart constructor
    mkEndpointsResponse,

    -- * Lenses
    eItem,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointResponse
import qualified Network.AWS.Prelude as Lude

-- | Provides information about all the endpoints that are associated with a user ID.
--
-- /See:/ 'mkEndpointsResponse' smart constructor.
newtype EndpointsResponse = EndpointsResponse'
  { item ::
      [EndpointResponse]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointsResponse' with the minimum fields required to make a request.
--
-- * 'item' - An array of responses, one for each endpoint that's associated with the user ID.
mkEndpointsResponse ::
  EndpointsResponse
mkEndpointsResponse = EndpointsResponse' {item = Lude.mempty}

-- | An array of responses, one for each endpoint that's associated with the user ID.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eItem :: Lens.Lens' EndpointsResponse [EndpointResponse]
eItem = Lens.lens (item :: EndpointsResponse -> [EndpointResponse]) (\s a -> s {item = a} :: EndpointsResponse)
{-# DEPRECATED eItem "Use generic-lens or generic-optics with 'item' instead." #-}

instance Lude.FromJSON EndpointsResponse where
  parseJSON =
    Lude.withObject
      "EndpointsResponse"
      ( \x ->
          EndpointsResponse'
            Lude.<$> (x Lude..:? "Item" Lude..!= Lude.mempty)
      )
