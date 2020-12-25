{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    EnableUser (..),
    mkEnableUser,

    -- ** Request lenses
    euUserName,
    euAuthenticationType,

    -- * Destructuring the response
    EnableUserResponse (..),
    mkEnableUserResponse,

    -- ** Response lenses
    eurrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableUser' smart constructor.
data EnableUser = EnableUser'
  { -- | The email address of the user.
    userName :: Types.Username,
    -- | The authentication type for the user. You must specify USERPOOL.
    authenticationType :: Types.AuthenticationType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableUser' value with any optional fields omitted.
mkEnableUser ::
  -- | 'userName'
  Types.Username ->
  -- | 'authenticationType'
  Types.AuthenticationType ->
  EnableUser
mkEnableUser userName authenticationType =
  EnableUser' {userName, authenticationType}

-- | The email address of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euUserName :: Lens.Lens' EnableUser Types.Username
euUserName = Lens.field @"userName"
{-# DEPRECATED euUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The authentication type for the user. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
euAuthenticationType :: Lens.Lens' EnableUser Types.AuthenticationType
euAuthenticationType = Lens.field @"authenticationType"
{-# DEPRECATED euAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Core.FromJSON EnableUser where
  toJSON EnableUser {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserName" Core..= userName),
            Core.Just ("AuthenticationType" Core..= authenticationType)
          ]
      )

instance Core.AWSRequest EnableUser where
  type Rs EnableUser = EnableUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "PhotonAdminProxyService.EnableUser")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableUserResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableUserResponse' smart constructor.
newtype EnableUserResponse = EnableUserResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableUserResponse' value with any optional fields omitted.
mkEnableUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableUserResponse
mkEnableUserResponse responseStatus =
  EnableUserResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eurrsResponseStatus :: Lens.Lens' EnableUserResponse Core.Int
eurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
