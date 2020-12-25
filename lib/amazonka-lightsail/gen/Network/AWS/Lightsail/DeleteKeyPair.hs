{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific SSH key pair.
--
-- The @delete key pair@ operation supports tag-based access control via resource tags applied to the resource identified by @key pair name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteKeyPair
  ( -- * Creating a request
    DeleteKeyPair (..),
    mkDeleteKeyPair,

    -- ** Request lenses
    dkpKeyPairName,

    -- * Destructuring the response
    DeleteKeyPairResponse (..),
    mkDeleteKeyPairResponse,

    -- ** Response lenses
    dkprrsOperation,
    dkprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteKeyPair' smart constructor.
newtype DeleteKeyPair = DeleteKeyPair'
  { -- | The name of the key pair to delete.
    keyPairName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteKeyPair' value with any optional fields omitted.
mkDeleteKeyPair ::
  -- | 'keyPairName'
  Types.ResourceName ->
  DeleteKeyPair
mkDeleteKeyPair keyPairName = DeleteKeyPair' {keyPairName}

-- | The name of the key pair to delete.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpKeyPairName :: Lens.Lens' DeleteKeyPair Types.ResourceName
dkpKeyPairName = Lens.field @"keyPairName"
{-# DEPRECATED dkpKeyPairName "Use generic-lens or generic-optics with 'keyPairName' instead." #-}

instance Core.FromJSON DeleteKeyPair where
  toJSON DeleteKeyPair {..} =
    Core.object
      (Core.catMaybes [Core.Just ("keyPairName" Core..= keyPairName)])

instance Core.AWSRequest DeleteKeyPair where
  type Rs DeleteKeyPair = DeleteKeyPairResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteKeyPair")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteKeyPairResponse'
            Core.<$> (x Core..:? "operation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse = DeleteKeyPairResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Core.Maybe Types.Operation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteKeyPairResponse' value with any optional fields omitted.
mkDeleteKeyPairResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteKeyPairResponse
mkDeleteKeyPairResponse responseStatus =
  DeleteKeyPairResponse' {operation = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprrsOperation :: Lens.Lens' DeleteKeyPairResponse (Core.Maybe Types.Operation)
dkprrsOperation = Lens.field @"operation"
{-# DEPRECATED dkprrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprrsResponseStatus :: Lens.Lens' DeleteKeyPairResponse Core.Int
dkprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dkprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
