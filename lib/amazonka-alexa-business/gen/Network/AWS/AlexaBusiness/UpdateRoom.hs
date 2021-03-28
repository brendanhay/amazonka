{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room details by room ARN.
module Network.AWS.AlexaBusiness.UpdateRoom
    (
    -- * Creating a request
      UpdateRoom (..)
    , mkUpdateRoom
    -- ** Request lenses
    , urDescription
    , urProfileArn
    , urProviderCalendarId
    , urRoomArn
    , urRoomName

    -- * Destructuring the response
    , UpdateRoomResponse (..)
    , mkUpdateRoomResponse
    -- ** Response lenses
    , urrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRoom' smart constructor.
data UpdateRoom = UpdateRoom'
  { description :: Core.Maybe Types.Description
    -- ^ The updated description for the room.
  , profileArn :: Core.Maybe Types.ProfileArn
    -- ^ The updated profile ARN for the room.
  , providerCalendarId :: Core.Maybe Types.ProviderCalendarId
    -- ^ The updated provider calendar ARN for the room.
  , roomArn :: Core.Maybe Types.RoomArn
    -- ^ The ARN of the room to update. 
  , roomName :: Core.Maybe Types.RoomName
    -- ^ The updated name for the room.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoom' value with any optional fields omitted.
mkUpdateRoom
    :: UpdateRoom
mkUpdateRoom
  = UpdateRoom'{description = Core.Nothing,
                profileArn = Core.Nothing, providerCalendarId = Core.Nothing,
                roomArn = Core.Nothing, roomName = Core.Nothing}

-- | The updated description for the room.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDescription :: Lens.Lens' UpdateRoom (Core.Maybe Types.Description)
urDescription = Lens.field @"description"
{-# INLINEABLE urDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The updated profile ARN for the room.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urProfileArn :: Lens.Lens' UpdateRoom (Core.Maybe Types.ProfileArn)
urProfileArn = Lens.field @"profileArn"
{-# INLINEABLE urProfileArn #-}
{-# DEPRECATED profileArn "Use generic-lens or generic-optics with 'profileArn' instead"  #-}

-- | The updated provider calendar ARN for the room.
--
-- /Note:/ Consider using 'providerCalendarId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urProviderCalendarId :: Lens.Lens' UpdateRoom (Core.Maybe Types.ProviderCalendarId)
urProviderCalendarId = Lens.field @"providerCalendarId"
{-# INLINEABLE urProviderCalendarId #-}
{-# DEPRECATED providerCalendarId "Use generic-lens or generic-optics with 'providerCalendarId' instead"  #-}

-- | The ARN of the room to update. 
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRoomArn :: Lens.Lens' UpdateRoom (Core.Maybe Types.RoomArn)
urRoomArn = Lens.field @"roomArn"
{-# INLINEABLE urRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

-- | The updated name for the room.
--
-- /Note:/ Consider using 'roomName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRoomName :: Lens.Lens' UpdateRoom (Core.Maybe Types.RoomName)
urRoomName = Lens.field @"roomName"
{-# INLINEABLE urRoomName #-}
{-# DEPRECATED roomName "Use generic-lens or generic-optics with 'roomName' instead"  #-}

instance Core.ToQuery UpdateRoom where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRoom where
        toHeaders UpdateRoom{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.UpdateRoom") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRoom where
        toJSON UpdateRoom{..}
          = Core.object
              (Core.catMaybes
                 [("Description" Core..=) Core.<$> description,
                  ("ProfileArn" Core..=) Core.<$> profileArn,
                  ("ProviderCalendarId" Core..=) Core.<$> providerCalendarId,
                  ("RoomArn" Core..=) Core.<$> roomArn,
                  ("RoomName" Core..=) Core.<$> roomName])

instance Core.AWSRequest UpdateRoom where
        type Rs UpdateRoom = UpdateRoomResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateRoomResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRoomResponse' smart constructor.
newtype UpdateRoomResponse = UpdateRoomResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRoomResponse' value with any optional fields omitted.
mkUpdateRoomResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateRoomResponse
mkUpdateRoomResponse responseStatus
  = UpdateRoomResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateRoomResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
