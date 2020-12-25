{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteUserPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Cognito user pool.
module Network.AWS.CognitoIdentityProvider.DeleteUserPool
  ( -- * Creating a request
    DeleteUserPool (..),
    mkDeleteUserPool,

    -- ** Request lenses
    dupUserPoolId,

    -- * Destructuring the response
    DeleteUserPoolResponse (..),
    mkDeleteUserPoolResponse,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete a user pool.
--
-- /See:/ 'mkDeleteUserPool' smart constructor.
newtype DeleteUserPool = DeleteUserPool'
  { -- | The user pool ID for the user pool you want to delete.
    userPoolId :: Types.UserPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPool' value with any optional fields omitted.
mkDeleteUserPool ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  DeleteUserPool
mkDeleteUserPool userPoolId = DeleteUserPool' {userPoolId}

-- | The user pool ID for the user pool you want to delete.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUserPoolId :: Lens.Lens' DeleteUserPool Types.UserPoolId
dupUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED dupUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Core.FromJSON DeleteUserPool where
  toJSON DeleteUserPool {..} =
    Core.object
      (Core.catMaybes [Core.Just ("UserPoolId" Core..= userPoolId)])

instance Core.AWSRequest DeleteUserPool where
  type Rs DeleteUserPool = DeleteUserPoolResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.DeleteUserPool"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteUserPoolResponse'

-- | /See:/ 'mkDeleteUserPoolResponse' smart constructor.
data DeleteUserPoolResponse = DeleteUserPoolResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteUserPoolResponse' value with any optional fields omitted.
mkDeleteUserPoolResponse ::
  DeleteUserPoolResponse
mkDeleteUserPoolResponse = DeleteUserPoolResponse'
