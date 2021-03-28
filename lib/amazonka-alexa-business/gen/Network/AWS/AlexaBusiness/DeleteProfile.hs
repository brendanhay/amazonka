{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a room profile by the profile ARN.
module Network.AWS.AlexaBusiness.DeleteProfile
    (
    -- * Creating a request
      DeleteProfile (..)
    , mkDeleteProfile
    -- ** Request lenses
    , dpProfileArn

    -- * Destructuring the response
    , DeleteProfileResponse (..)
    , mkDeleteProfileResponse
    -- ** Response lenses
    , dprrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteProfile' smart constructor.
newtype DeleteProfile = DeleteProfile'
  { profileArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the room profile to delete. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProfile' value with any optional fields omitted.
mkDeleteProfile
    :: DeleteProfile
mkDeleteProfile = DeleteProfile'{profileArn = Core.Nothing}

-- | The ARN of the room profile to delete. Required.
--
-- /Note:/ Consider using 'profileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProfileArn :: Lens.Lens' DeleteProfile (Core.Maybe Types.Arn)
dpProfileArn = Lens.field @"profileArn"
{-# INLINEABLE dpProfileArn #-}
{-# DEPRECATED profileArn "Use generic-lens or generic-optics with 'profileArn' instead"  #-}

instance Core.ToQuery DeleteProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteProfile where
        toHeaders DeleteProfile{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteProfile where
        toJSON DeleteProfile{..}
          = Core.object
              (Core.catMaybes [("ProfileArn" Core..=) Core.<$> profileArn])

instance Core.AWSRequest DeleteProfile where
        type Rs DeleteProfile = DeleteProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteProfileResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteProfileResponse' smart constructor.
newtype DeleteProfileResponse = DeleteProfileResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteProfileResponse' value with any optional fields omitted.
mkDeleteProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteProfileResponse
mkDeleteProfileResponse responseStatus
  = DeleteProfileResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DeleteProfileResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
