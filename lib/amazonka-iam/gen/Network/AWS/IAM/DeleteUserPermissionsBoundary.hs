{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteUserPermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary for the specified IAM user.
--
-- /Important:/ Deleting the permissions boundary for a user might increase its permissions by allowing the user to perform all the actions granted in its permissions policies.
module Network.AWS.IAM.DeleteUserPermissionsBoundary
  ( -- * Creating a request
    DeleteUserPermissionsBoundary (..),
    mkDeleteUserPermissionsBoundary,

    -- ** Request lenses
    dupbUserName,

    -- * Destructuring the response
    DeleteUserPermissionsBoundaryResponse (..),
    mkDeleteUserPermissionsBoundaryResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteUserPermissionsBoundary' smart constructor.
newtype DeleteUserPermissionsBoundary = DeleteUserPermissionsBoundary'
  { -- | The name (friendly name, not ARN) of the IAM user from which you want to remove the permissions boundary.
    userName :: Types.UserNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPermissionsBoundary' value with any optional fields omitted.
mkDeleteUserPermissionsBoundary ::
  -- | 'userName'
  Types.UserNameType ->
  DeleteUserPermissionsBoundary
mkDeleteUserPermissionsBoundary userName =
  DeleteUserPermissionsBoundary' {userName}

-- | The name (friendly name, not ARN) of the IAM user from which you want to remove the permissions boundary.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupbUserName :: Lens.Lens' DeleteUserPermissionsBoundary Types.UserNameType
dupbUserName = Lens.field @"userName"
{-# DEPRECATED dupbUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.AWSRequest DeleteUserPermissionsBoundary where
  type
    Rs DeleteUserPermissionsBoundary =
      DeleteUserPermissionsBoundaryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteUserPermissionsBoundary")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "UserName" userName)
            )
      }
  response =
    Response.receiveNull DeleteUserPermissionsBoundaryResponse'

-- | /See:/ 'mkDeleteUserPermissionsBoundaryResponse' smart constructor.
data DeleteUserPermissionsBoundaryResponse = DeleteUserPermissionsBoundaryResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPermissionsBoundaryResponse' value with any optional fields omitted.
mkDeleteUserPermissionsBoundaryResponse ::
  DeleteUserPermissionsBoundaryResponse
mkDeleteUserPermissionsBoundaryResponse =
  DeleteUserPermissionsBoundaryResponse'
