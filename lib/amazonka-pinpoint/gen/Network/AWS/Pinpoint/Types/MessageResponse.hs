{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.MessageResponse
  ( MessageResponse (..)
  -- * Smart constructor
  , mkMessageResponse
  -- * Lenses
  , mrApplicationId
  , mrEndpointResult
  , mrRequestId
  , mrResult
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EndpointMessageResult as Types
import qualified Network.AWS.Pinpoint.Types.MessageResult as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about the results of a request to send a message to an endpoint address.
--
-- /See:/ 'mkMessageResponse' smart constructor.
data MessageResponse = MessageResponse'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application that was used to send the message.
  , endpointResult :: Core.Maybe (Core.HashMap Core.Text Types.EndpointMessageResult)
    -- ^ A map that contains a multipart response for each address that the message was sent to. In the map, the endpoint ID is the key and the result is the value.
  , requestId :: Core.Maybe Core.Text
    -- ^ The identifier for the original request that the message was delivered for.
  , result :: Core.Maybe (Core.HashMap Core.Text Types.MessageResult)
    -- ^ A map that contains a multipart response for each address (email address, phone number, or push notification token) that the message was sent to. In the map, the address is the key and the result is the value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageResponse' value with any optional fields omitted.
mkMessageResponse
    :: Core.Text -- ^ 'applicationId'
    -> MessageResponse
mkMessageResponse applicationId
  = MessageResponse'{applicationId, endpointResult = Core.Nothing,
                     requestId = Core.Nothing, result = Core.Nothing}

-- | The unique identifier for the application that was used to send the message.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrApplicationId :: Lens.Lens' MessageResponse Core.Text
mrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE mrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | A map that contains a multipart response for each address that the message was sent to. In the map, the endpoint ID is the key and the result is the value.
--
-- /Note:/ Consider using 'endpointResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrEndpointResult :: Lens.Lens' MessageResponse (Core.Maybe (Core.HashMap Core.Text Types.EndpointMessageResult))
mrEndpointResult = Lens.field @"endpointResult"
{-# INLINEABLE mrEndpointResult #-}
{-# DEPRECATED endpointResult "Use generic-lens or generic-optics with 'endpointResult' instead"  #-}

-- | The identifier for the original request that the message was delivered for.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrRequestId :: Lens.Lens' MessageResponse (Core.Maybe Core.Text)
mrRequestId = Lens.field @"requestId"
{-# INLINEABLE mrRequestId #-}
{-# DEPRECATED requestId "Use generic-lens or generic-optics with 'requestId' instead"  #-}

-- | A map that contains a multipart response for each address (email address, phone number, or push notification token) that the message was sent to. In the map, the address is the key and the result is the value.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrResult :: Lens.Lens' MessageResponse (Core.Maybe (Core.HashMap Core.Text Types.MessageResult))
mrResult = Lens.field @"result"
{-# INLINEABLE mrResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

instance Core.FromJSON MessageResponse where
        parseJSON
          = Core.withObject "MessageResponse" Core.$
              \ x ->
                MessageResponse' Core.<$>
                  (x Core..: "ApplicationId") Core.<*> x Core..:? "EndpointResult"
                    Core.<*> x Core..:? "RequestId"
                    Core.<*> x Core..:? "Result"
