{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SendUsersMessageResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SendUsersMessageResponse
  ( SendUsersMessageResponse (..)
  -- * Smart constructor
  , mkSendUsersMessageResponse
  -- * Lenses
  , sumrApplicationId
  , sumrRequestId
  , sumrResult
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EndpointMessageResult as Types
import qualified Network.AWS.Prelude as Core

-- | Provides information about which users and endpoints a message was sent to.
--
-- /See:/ 'mkSendUsersMessageResponse' smart constructor.
data SendUsersMessageResponse = SendUsersMessageResponse'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application that was used to send the message.
  , requestId :: Core.Maybe Core.Text
    -- ^ The unique identifier that was assigned to the message request.
  , result :: Core.Maybe (Core.HashMap Core.Text (Core.HashMap Core.Text Types.EndpointMessageResult))
    -- ^ An object that indicates which endpoints the message was sent to, for each user. The object lists user IDs and, for each user ID, provides the endpoint IDs that the message was sent to. For each endpoint ID, it provides an EndpointMessageResult object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SendUsersMessageResponse' value with any optional fields omitted.
mkSendUsersMessageResponse
    :: Core.Text -- ^ 'applicationId'
    -> SendUsersMessageResponse
mkSendUsersMessageResponse applicationId
  = SendUsersMessageResponse'{applicationId,
                              requestId = Core.Nothing, result = Core.Nothing}

-- | The unique identifier for the application that was used to send the message.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrApplicationId :: Lens.Lens' SendUsersMessageResponse Core.Text
sumrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE sumrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The unique identifier that was assigned to the message request.
--
-- /Note:/ Consider using 'requestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrRequestId :: Lens.Lens' SendUsersMessageResponse (Core.Maybe Core.Text)
sumrRequestId = Lens.field @"requestId"
{-# INLINEABLE sumrRequestId #-}
{-# DEPRECATED requestId "Use generic-lens or generic-optics with 'requestId' instead"  #-}

-- | An object that indicates which endpoints the message was sent to, for each user. The object lists user IDs and, for each user ID, provides the endpoint IDs that the message was sent to. For each endpoint ID, it provides an EndpointMessageResult object.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumrResult :: Lens.Lens' SendUsersMessageResponse (Core.Maybe (Core.HashMap Core.Text (Core.HashMap Core.Text Types.EndpointMessageResult)))
sumrResult = Lens.field @"result"
{-# INLINEABLE sumrResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

instance Core.FromJSON SendUsersMessageResponse where
        parseJSON
          = Core.withObject "SendUsersMessageResponse" Core.$
              \ x ->
                SendUsersMessageResponse' Core.<$>
                  (x Core..: "ApplicationId") Core.<*> x Core..:? "RequestId"
                    Core.<*> x Core..:? "Result"
