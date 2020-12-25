{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    eirMessage,
    eirStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the status code and message that result from processing data for an endpoint.
--
-- /See:/ 'mkEndpointItemResponse' smart constructor.
data EndpointItemResponse = EndpointItemResponse'
  { -- | The custom message that's returned in the response as a result of processing the endpoint data.
    message :: Core.Maybe Core.Text,
    -- | The status code that's returned in the response as a result of processing the endpoint data.
    statusCode :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointItemResponse' value with any optional fields omitted.
mkEndpointItemResponse ::
  EndpointItemResponse
mkEndpointItemResponse =
  EndpointItemResponse'
    { message = Core.Nothing,
      statusCode = Core.Nothing
    }

-- | The custom message that's returned in the response as a result of processing the endpoint data.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirMessage :: Lens.Lens' EndpointItemResponse (Core.Maybe Core.Text)
eirMessage = Lens.field @"message"
{-# DEPRECATED eirMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The status code that's returned in the response as a result of processing the endpoint data.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eirStatusCode :: Lens.Lens' EndpointItemResponse (Core.Maybe Core.Int)
eirStatusCode = Lens.field @"statusCode"
{-# DEPRECATED eirStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Core.FromJSON EndpointItemResponse where
  parseJSON =
    Core.withObject "EndpointItemResponse" Core.$
      \x ->
        EndpointItemResponse'
          Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "StatusCode")
