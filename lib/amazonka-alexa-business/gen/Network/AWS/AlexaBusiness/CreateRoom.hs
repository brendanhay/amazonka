{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a room with the specified details.
module Network.AWS.AlexaBusiness.CreateRoom
    (
    -- * Creating a request
      CreateRoom (..)
    , mkCreateRoom
    -- ** Request lenses
    , crRoomName
    , crClientRequestToken
    , crDescription
    , crProfileArn
    , crProviderCalendarId
    , crTags

    -- * Destructuring the response
    , CreateRoomResponse (..)
    , mkCreateRoomResponse
    -- ** Response lenses
    , crrrsRoomArn
    , crrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRoom' smart constructor.
data CreateRoom = CreateRoom'
  { roomName :: Types.RoomName
    -- ^ The name for the room.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique, user-specified identifier for this request that ensures idempotency. 
  , description :: Core.Maybe Types.Description
    -- ^ The description for the room.
  , profileArn :: Core.Maybe Types.Arn
    -- ^ The profile ARN for the room. This is required.
  , providerCalendarId :: Core.Maybe Types.ProviderCalendarId
    -- ^ The calendar ARN for the room.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the room.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoom' value with any optional fields omitted.
mkCreateRoom
    :: Types.RoomName -- ^ 'roomName'
    -> CreateRoom
mkCreateRoom roomName
  = CreateRoom'{roomName, clientRequestToken = Core.Nothing,
                description = Core.Nothing, profileArn = Core.Nothing,
                providerCalendarId = Core.Nothing, tags = Core.Nothing}

-- | The name for the room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRoomName :: Lens.Lens' CreateRoom Types.RoomName
crRoomName = Lens.field @"roomName"
{-# INLINEABLE crRoomName #-}
{-# DEPRECATED roomName "Use generic-lens or generic-optics with 'roomName' instead"  #-}

-- | A unique, user-specified identifier for this request that ensures idempotency. 
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crClientRequestToken :: Lens.Lens' CreateRoom (Core.Maybe Types.ClientRequestToken)
crClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE crClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | The description for the room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' CreateRoom (Core.Maybe Types.Description)
crDescription = Lens.field @"description"
{-# INLINEABLE crDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The profile ARN for the room. This is required.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crProfileArn :: Lens.Lens' CreateRoom (Core.Maybe Types.Arn)
crProfileArn = Lens.field @"profileArn"
{-# INLINEABLE crProfileArn #-}
{-# DEPRECATED profileArn "Use generic-lens or generic-optics with 'profileArn' instead"  #-}

-- | The calendar ARN for the room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crProviderCalendarId :: Lens.Lens' CreateRoom (Core.Maybe Types.ProviderCalendarId)
crProviderCalendarId = Lens.field @"providerCalendarId"
{-# INLINEABLE crProviderCalendarId #-}
{-# DEPRECATED providerCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead"  #-}

-- | The tags for the room.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRoom (Core.Maybe [Types.Tag])
crTags = Lens.field @"tags"
{-# INLINEABLE crTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateRoom where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRoom where
        toHeaders CreateRoom{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.CreateRoom") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRoom where
        toJSON CreateRoom{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RoomName" Core..= roomName),
                  ("ClientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("Description" Core..=) Core.<$> description,
                  ("ProfileArn" Core..=) Core.<$> profileArn,
                  ("ProviderCalendarId" Core..=) Core.<$> providerCalendarId,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateRoom where
        type Rs CreateRoom = CreateRoomResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRoomResponse' Core.<$>
                   (x Core..:? "RoomArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRoomResponse' smart constructor.
data CreateRoomResponse = CreateRoomResponse'
  { roomArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the newly created room in the response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRoomResponse' value with any optional fields omitted.
mkCreateRoomResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateRoomResponse
mkCreateRoomResponse responseStatus
  = CreateRoomResponse'{roomArn = Core.Nothing, responseStatus}

-- | The ARN of the newly created room in the response.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsRoomArn :: Lens.Lens' CreateRoomResponse (Core.Maybe Types.Arn)
crrrsRoomArn = Lens.field @"roomArn"
{-# INLINEABLE crrrsRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateRoomResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
