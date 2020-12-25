{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.ActivateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the specified user. Only active users can access Amazon WorkDocs.
module Network.AWS.WorkDocs.ActivateUser
  ( -- * Creating a request
    ActivateUser (..),
    mkActivateUser,

    -- ** Request lenses
    auUserId,
    auAuthenticationToken,

    -- * Destructuring the response
    ActivateUserResponse (..),
    mkActivateUserResponse,

    -- ** Response lenses
    aurrsUser,
    aurrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkActivateUser' smart constructor.
data ActivateUser = ActivateUser'
  { -- | The ID of the user.
    userId :: Types.IdType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivateUser' value with any optional fields omitted.
mkActivateUser ::
  -- | 'userId'
  Types.IdType ->
  ActivateUser
mkActivateUser userId =
  ActivateUser' {userId, authenticationToken = Core.Nothing}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auUserId :: Lens.Lens' ActivateUser Types.IdType
auUserId = Lens.field @"userId"
{-# DEPRECATED auUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auAuthenticationToken :: Lens.Lens' ActivateUser (Core.Maybe Types.AuthenticationHeaderType)
auAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED auAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Core.FromJSON ActivateUser where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest ActivateUser where
  type Rs ActivateUser = ActivateUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/users/" Core.<> (Core.toText userId)
                Core.<> ("/activation")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ActivateUserResponse'
            Core.<$> (x Core..:? "User") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkActivateUserResponse' smart constructor.
data ActivateUserResponse = ActivateUserResponse'
  { -- | The user information.
    user :: Core.Maybe Types.User,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ActivateUserResponse' value with any optional fields omitted.
mkActivateUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ActivateUserResponse
mkActivateUserResponse responseStatus =
  ActivateUserResponse' {user = Core.Nothing, responseStatus}

-- | The user information.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aurrsUser :: Lens.Lens' ActivateUserResponse (Core.Maybe Types.User)
aurrsUser = Lens.field @"user"
{-# DEPRECATED aurrsUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aurrsResponseStatus :: Lens.Lens' ActivateUserResponse Core.Int
aurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
