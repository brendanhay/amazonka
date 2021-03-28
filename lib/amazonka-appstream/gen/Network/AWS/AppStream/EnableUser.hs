{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.EnableUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a user in the user pool. After being enabled, users can sign in to AppStream 2.0 and open applications from the stacks to which they are assigned.
module Network.AWS.AppStream.EnableUser
    (
    -- * Creating a request
      EnableUser (..)
    , mkEnableUser
    -- ** Request lenses
    , euUserName
    , euAuthenticationType

    -- * Destructuring the response
    , EnableUserResponse (..)
    , mkEnableUserResponse
    -- ** Response lenses
    , eurrsResponseStatus
    ) where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableUser' smart constructor.
data EnableUser = EnableUser'
  { userName :: Types.Username
    -- ^ The email address of the user.
  , authenticationType :: Types.AuthenticationType
    -- ^ The authentication type for the user. You must specify USERPOOL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableUser' value with any optional fields omitted.
mkEnableUser
    :: Types.Username -- ^ 'userName'
    -> Types.AuthenticationType -- ^ 'authenticationType'
    -> EnableUser
mkEnableUser userName authenticationType
  = EnableUser'{userName, authenticationType}

-- | The email address of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euUserName :: Lens.Lens' EnableUser Types.Username
euUserName = Lens.field @"userName"
{-# INLINEABLE euUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The authentication type for the user. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euAuthenticationType :: Lens.Lens' EnableUser Types.AuthenticationType
euAuthenticationType = Lens.field @"authenticationType"
{-# INLINEABLE euAuthenticationType #-}
{-# DEPRECATED authenticationType "Use generic-lens or generic-optics with 'authenticationType' instead"  #-}

instance Core.ToQuery EnableUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders EnableUser where
        toHeaders EnableUser{..}
          = Core.pure ("X-Amz-Target", "PhotonAdminProxyService.EnableUser")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON EnableUser where
        toJSON EnableUser{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserName" Core..= userName),
                  Core.Just ("AuthenticationType" Core..= authenticationType)])

instance Core.AWSRequest EnableUser where
        type Rs EnableUser = EnableUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 EnableUserResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableUserResponse' smart constructor.
newtype EnableUserResponse = EnableUserResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableUserResponse' value with any optional fields omitted.
mkEnableUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EnableUserResponse
mkEnableUserResponse responseStatus
  = EnableUserResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eurrsResponseStatus :: Lens.Lens' EnableUserResponse Core.Int
eurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE eurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
