{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetCurrentUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of the current user for whom the authentication token was generated. This is not a valid action for SigV4 (administrative API) clients.
--
-- This action requires an authentication token. To get an authentication token, register an application with Amazon WorkDocs. For more information, see <https://docs.aws.amazon.com/workdocs/latest/developerguide/wd-auth-user.html Authentication and Access Control for User Applications> in the /Amazon WorkDocs Developer Guide/ .
module Network.AWS.WorkDocs.GetCurrentUser
    (
    -- * Creating a request
      GetCurrentUser (..)
    , mkGetCurrentUser
    -- ** Request lenses
    , gcuAuthenticationToken

    -- * Destructuring the response
    , GetCurrentUserResponse (..)
    , mkGetCurrentUserResponse
    -- ** Response lenses
    , gcurrsUser
    , gcurrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetCurrentUser' smart constructor.
newtype GetCurrentUser = GetCurrentUser'
  { authenticationToken :: Types.AuthenticationToken
    -- ^ Amazon WorkDocs authentication token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCurrentUser' value with any optional fields omitted.
mkGetCurrentUser
    :: Types.AuthenticationToken -- ^ 'authenticationToken'
    -> GetCurrentUser
mkGetCurrentUser authenticationToken
  = GetCurrentUser'{authenticationToken}

-- | Amazon WorkDocs authentication token.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcuAuthenticationToken :: Lens.Lens' GetCurrentUser Types.AuthenticationToken
gcuAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE gcuAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

instance Core.ToQuery GetCurrentUser where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCurrentUser where
        toHeaders GetCurrentUser{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetCurrentUser where
        type Rs GetCurrentUser = GetCurrentUserResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/api/v1/me",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCurrentUserResponse' Core.<$>
                   (x Core..:? "User") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCurrentUserResponse' smart constructor.
data GetCurrentUserResponse = GetCurrentUserResponse'
  { user :: Core.Maybe Types.User
    -- ^ Metadata of the user.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetCurrentUserResponse' value with any optional fields omitted.
mkGetCurrentUserResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCurrentUserResponse
mkGetCurrentUserResponse responseStatus
  = GetCurrentUserResponse'{user = Core.Nothing, responseStatus}

-- | Metadata of the user.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcurrsUser :: Lens.Lens' GetCurrentUserResponse (Core.Maybe Types.User)
gcurrsUser = Lens.field @"user"
{-# INLINEABLE gcurrsUser #-}
{-# DEPRECATED user "Use generic-lens or generic-optics with 'user' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcurrsResponseStatus :: Lens.Lens' GetCurrentUserResponse Core.Int
gcurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
