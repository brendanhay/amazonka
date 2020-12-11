-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointItemResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointItemResponse
  ( EndpointItemResponse (..),

    -- * Smart constructor
    mkEndpointItemResponse,

    -- * Lenses
    eiMessage,
    eiStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the status code and message that result from processing data for an endpoint.
--
-- /See:/ 'mkEndpointItemResponse' smart constructor.
data EndpointItemResponse = EndpointItemResponse'
  { message ::
      Lude.Maybe Lude.Text,
    statusCode :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointItemResponse' with the minimum fields required to make a request.
--
-- * 'message' - The custom message that's returned in the response as a result of processing the endpoint data.
-- * 'statusCode' - The status code that's returned in the response as a result of processing the endpoint data.
mkEndpointItemResponse ::
  EndpointItemResponse
mkEndpointItemResponse =
  EndpointItemResponse'
    { message = Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | The custom message that's returned in the response as a result of processing the endpoint data.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiMessage :: Lens.Lens' EndpointItemResponse (Lude.Maybe Lude.Text)
eiMessage = Lens.lens (message :: EndpointItemResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: EndpointItemResponse)
{-# DEPRECATED eiMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The status code that's returned in the response as a result of processing the endpoint data.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiStatusCode :: Lens.Lens' EndpointItemResponse (Lude.Maybe Lude.Int)
eiStatusCode = Lens.lens (statusCode :: EndpointItemResponse -> Lude.Maybe Lude.Int) (\s a -> s {statusCode = a} :: EndpointItemResponse)
{-# DEPRECATED eiStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON EndpointItemResponse where
  parseJSON =
    Lude.withObject
      "EndpointItemResponse"
      ( \x ->
          EndpointItemResponse'
            Lude.<$> (x Lude..:? "Message") Lude.<*> (x Lude..:? "StatusCode")
      )
