{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeactivateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified user, which revokes the user's access to Amazon WorkDocs.
module Network.AWS.WorkDocs.DeactivateUser
  ( -- * Creating a request
    DeactivateUser (..),
    mkDeactivateUser,

    -- ** Request lenses
    dUserId,
    dAuthenticationToken,

    -- * Destructuring the response
    DeactivateUserResponse (..),
    mkDeactivateUserResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeactivateUser' smart constructor.
data DeactivateUser = DeactivateUser'
  { -- | The ID of the user.
    userId :: Types.IdType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeactivateUser' value with any optional fields omitted.
mkDeactivateUser ::
  -- | 'userId'
  Types.IdType ->
  DeactivateUser
mkDeactivateUser userId =
  DeactivateUser' {userId, authenticationToken = Core.Nothing}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserId :: Lens.Lens' DeactivateUser Types.IdType
dUserId = Lens.field @"userId"
{-# DEPRECATED dUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAuthenticationToken :: Lens.Lens' DeactivateUser (Core.Maybe Types.AuthenticationHeaderType)
dAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED dAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Core.AWSRequest DeactivateUser where
  type Rs DeactivateUser = DeactivateUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/users/" Core.<> (Core.toText userId)
                Core.<> ("/activation")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response = Response.receiveNull DeactivateUserResponse'

-- | /See:/ 'mkDeactivateUserResponse' smart constructor.
data DeactivateUserResponse = DeactivateUserResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeactivateUserResponse' value with any optional fields omitted.
mkDeactivateUserResponse ::
  DeactivateUserResponse
mkDeactivateUserResponse = DeactivateUserResponse'
