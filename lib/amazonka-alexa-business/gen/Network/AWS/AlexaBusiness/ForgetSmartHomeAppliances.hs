{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets smart home appliances associated to a room.
module Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances
    (
    -- * Creating a request
      ForgetSmartHomeAppliances (..)
    , mkForgetSmartHomeAppliances
    -- ** Request lenses
    , fshaRoomArn

    -- * Destructuring the response
    , ForgetSmartHomeAppliancesResponse (..)
    , mkForgetSmartHomeAppliancesResponse
    -- ** Response lenses
    , fsharrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkForgetSmartHomeAppliances' smart constructor.
newtype ForgetSmartHomeAppliances = ForgetSmartHomeAppliances'
  { roomArn :: Types.RoomArn
    -- ^ The room that the appliances are associated with.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ForgetSmartHomeAppliances' value with any optional fields omitted.
mkForgetSmartHomeAppliances
    :: Types.RoomArn -- ^ 'roomArn'
    -> ForgetSmartHomeAppliances
mkForgetSmartHomeAppliances roomArn
  = ForgetSmartHomeAppliances'{roomArn}

-- | The room that the appliances are associated with.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fshaRoomArn :: Lens.Lens' ForgetSmartHomeAppliances Types.RoomArn
fshaRoomArn = Lens.field @"roomArn"
{-# INLINEABLE fshaRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

instance Core.ToQuery ForgetSmartHomeAppliances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ForgetSmartHomeAppliances where
        toHeaders ForgetSmartHomeAppliances{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.ForgetSmartHomeAppliances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ForgetSmartHomeAppliances where
        toJSON ForgetSmartHomeAppliances{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RoomArn" Core..= roomArn)])

instance Core.AWSRequest ForgetSmartHomeAppliances where
        type Rs ForgetSmartHomeAppliances =
             ForgetSmartHomeAppliancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ForgetSmartHomeAppliancesResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkForgetSmartHomeAppliancesResponse' smart constructor.
newtype ForgetSmartHomeAppliancesResponse = ForgetSmartHomeAppliancesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ForgetSmartHomeAppliancesResponse' value with any optional fields omitted.
mkForgetSmartHomeAppliancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ForgetSmartHomeAppliancesResponse
mkForgetSmartHomeAppliancesResponse responseStatus
  = ForgetSmartHomeAppliancesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsharrsResponseStatus :: Lens.Lens' ForgetSmartHomeAppliancesResponse Core.Int
fsharrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE fsharrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
