{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DeleteIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes identities from an identity pool. You can specify a list of 1-60 identities that you want to delete.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DeleteIdentities
  ( -- * Creating a request
    DeleteIdentities (..),
    mkDeleteIdentities,

    -- ** Request lenses
    diIdentityIdsToDelete,

    -- * Destructuring the response
    DeleteIdentitiesResponse (..),
    mkDeleteIdentitiesResponse,

    -- ** Response lenses
    dirrsUnprocessedIdentityIds,
    dirrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @DeleteIdentities@ action.
--
-- /See:/ 'mkDeleteIdentities' smart constructor.
newtype DeleteIdentities = DeleteIdentities'
  { -- | A list of 1-60 identities that you want to delete.
    identityIdsToDelete :: Core.NonEmpty Types.IdentityId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentities' value with any optional fields omitted.
mkDeleteIdentities ::
  -- | 'identityIdsToDelete'
  Core.NonEmpty Types.IdentityId ->
  DeleteIdentities
mkDeleteIdentities identityIdsToDelete =
  DeleteIdentities' {identityIdsToDelete}

-- | A list of 1-60 identities that you want to delete.
--
-- /Note:/ Consider using 'identityIdsToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIdentityIdsToDelete :: Lens.Lens' DeleteIdentities (Core.NonEmpty Types.IdentityId)
diIdentityIdsToDelete = Lens.field @"identityIdsToDelete"
{-# DEPRECATED diIdentityIdsToDelete "Use generic-lens or generic-optics with 'identityIdsToDelete' instead." #-}

instance Core.FromJSON DeleteIdentities where
  toJSON DeleteIdentities {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("IdentityIdsToDelete" Core..= identityIdsToDelete)]
      )

instance Core.AWSRequest DeleteIdentities where
  type Rs DeleteIdentities = DeleteIdentitiesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSCognitoIdentityService.DeleteIdentities")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteIdentitiesResponse'
            Core.<$> (x Core..:? "UnprocessedIdentityIds")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returned in response to a successful @DeleteIdentities@ operation.
--
-- /See:/ 'mkDeleteIdentitiesResponse' smart constructor.
data DeleteIdentitiesResponse = DeleteIdentitiesResponse'
  { -- | An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.
    unprocessedIdentityIds :: Core.Maybe [Types.UnprocessedIdentityId],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentitiesResponse' value with any optional fields omitted.
mkDeleteIdentitiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteIdentitiesResponse
mkDeleteIdentitiesResponse responseStatus =
  DeleteIdentitiesResponse'
    { unprocessedIdentityIds = Core.Nothing,
      responseStatus
    }

-- | An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.
--
-- /Note:/ Consider using 'unprocessedIdentityIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsUnprocessedIdentityIds :: Lens.Lens' DeleteIdentitiesResponse (Core.Maybe [Types.UnprocessedIdentityId])
dirrsUnprocessedIdentityIds = Lens.field @"unprocessedIdentityIds"
{-# DEPRECATED dirrsUnprocessedIdentityIds "Use generic-lens or generic-optics with 'unprocessedIdentityIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DeleteIdentitiesResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
