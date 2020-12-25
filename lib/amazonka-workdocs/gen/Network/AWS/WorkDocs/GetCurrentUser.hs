{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetCurrentUser (..),
    mkGetCurrentUser,

    -- ** Request lenses
    gcuAuthenticationToken,

    -- * Destructuring the response
    GetCurrentUserResponse (..),
    mkGetCurrentUserResponse,

    -- ** Response lenses
    gcurrsUser,
    gcurrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetCurrentUser' smart constructor.
newtype GetCurrentUser = GetCurrentUser'
  { -- | Amazon WorkDocs authentication token.
    authenticationToken :: Types.AuthenticationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCurrentUser' value with any optional fields omitted.
mkGetCurrentUser ::
  -- | 'authenticationToken'
  Types.AuthenticationToken ->
  GetCurrentUser
mkGetCurrentUser authenticationToken =
  GetCurrentUser' {authenticationToken}

-- | Amazon WorkDocs authentication token.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcuAuthenticationToken :: Lens.Lens' GetCurrentUser Types.AuthenticationToken
gcuAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED gcuAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Core.AWSRequest GetCurrentUser where
  type Rs GetCurrentUser = GetCurrentUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/api/v1/me",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCurrentUserResponse'
            Core.<$> (x Core..:? "User") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCurrentUserResponse' smart constructor.
data GetCurrentUserResponse = GetCurrentUserResponse'
  { -- | Metadata of the user.
    user :: Core.Maybe Types.User,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetCurrentUserResponse' value with any optional fields omitted.
mkGetCurrentUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCurrentUserResponse
mkGetCurrentUserResponse responseStatus =
  GetCurrentUserResponse' {user = Core.Nothing, responseStatus}

-- | Metadata of the user.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcurrsUser :: Lens.Lens' GetCurrentUserResponse (Core.Maybe Types.User)
gcurrsUser = Lens.field @"user"
{-# DEPRECATED gcurrsUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcurrsResponseStatus :: Lens.Lens' GetCurrentUserResponse Core.Int
gcurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
