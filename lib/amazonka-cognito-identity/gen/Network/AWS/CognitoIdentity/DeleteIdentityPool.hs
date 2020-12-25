{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DeleteIdentityPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity pool. Once a pool is deleted, users will not be able to authenticate with the pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DeleteIdentityPool
  ( -- * Creating a request
    DeleteIdentityPool (..),
    mkDeleteIdentityPool,

    -- ** Request lenses
    dIdentityPoolId,

    -- * Destructuring the response
    DeleteIdentityPoolResponse (..),
    mkDeleteIdentityPoolResponse,
  )
where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the DeleteIdentityPool action.
--
-- /See:/ 'mkDeleteIdentityPool' smart constructor.
newtype DeleteIdentityPool = DeleteIdentityPool'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Types.IdentityPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityPool' value with any optional fields omitted.
mkDeleteIdentityPool ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  DeleteIdentityPool
mkDeleteIdentityPool identityPoolId =
  DeleteIdentityPool' {identityPoolId}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIdentityPoolId :: Lens.Lens' DeleteIdentityPool Types.IdentityPoolId
dIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED dIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Core.FromJSON DeleteIdentityPool where
  toJSON DeleteIdentityPool {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("IdentityPoolId" Core..= identityPoolId)]
      )

instance Core.AWSRequest DeleteIdentityPool where
  type Rs DeleteIdentityPool = DeleteIdentityPoolResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityService.DeleteIdentityPool")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteIdentityPoolResponse'

-- | /See:/ 'mkDeleteIdentityPoolResponse' smart constructor.
data DeleteIdentityPoolResponse = DeleteIdentityPoolResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityPoolResponse' value with any optional fields omitted.
mkDeleteIdentityPoolResponse ::
  DeleteIdentityPoolResponse
mkDeleteIdentityPoolResponse = DeleteIdentityPoolResponse'
