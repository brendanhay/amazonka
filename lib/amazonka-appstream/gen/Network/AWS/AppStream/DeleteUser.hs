{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user from the user pool.
module Network.AWS.AppStream.DeleteUser
  ( -- * Creating a request
    DeleteUser (..),
    mkDeleteUser,

    -- ** Request lenses
    dufUserName,
    dufAuthenticationType,

    -- * Destructuring the response
    DeleteUserResponse (..),
    mkDeleteUserResponse,

    -- ** Response lenses
    durgrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The email address of the user.
    userName :: Types.Username,
    -- | The authentication type for the user. You must specify USERPOOL.
    authenticationType :: Types.AuthenticationType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUser' value with any optional fields omitted.
mkDeleteUser ::
  -- | 'userName'
  Types.Username ->
  -- | 'authenticationType'
  Types.AuthenticationType ->
  DeleteUser
mkDeleteUser userName authenticationType =
  DeleteUser' {userName, authenticationType}

-- | The email address of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufUserName :: Lens.Lens' DeleteUser Types.Username
dufUserName = Lens.field @"userName"
{-# DEPRECATED dufUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The authentication type for the user. You must specify USERPOOL.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dufAuthenticationType :: Lens.Lens' DeleteUser Types.AuthenticationType
dufAuthenticationType = Lens.field @"authenticationType"
{-# DEPRECATED dufAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Core.FromJSON DeleteUser where
  toJSON DeleteUser {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserName" Core..= userName),
            Core.Just ("AuthenticationType" Core..= authenticationType)
          ]
      )

instance Core.AWSRequest DeleteUser where
  type Rs DeleteUser = DeleteUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "PhotonAdminProxyService.DeleteUser")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUserResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteUserResponse' smart constructor.
newtype DeleteUserResponse = DeleteUserResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserResponse' value with any optional fields omitted.
mkDeleteUserResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteUserResponse
mkDeleteUserResponse responseStatus =
  DeleteUserResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
durgrsResponseStatus :: Lens.Lens' DeleteUserResponse Core.Int
durgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED durgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
