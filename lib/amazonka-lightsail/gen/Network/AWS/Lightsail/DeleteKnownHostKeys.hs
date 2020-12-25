{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteKnownHostKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the known host key or certificate used by the Amazon Lightsail browser-based SSH or RDP clients to authenticate an instance. This operation enables the Lightsail browser-based SSH or RDP clients to connect to the instance after a host key mismatch.
--
-- /Important:/ Perform this operation only if you were expecting the host key or certificate mismatch or if you are familiar with the new host key or certificate on the instance. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-troubleshooting-browser-based-ssh-rdp-client-connection Troubleshooting connection issues when using the Amazon Lightsail browser-based SSH or RDP client> .
module Network.AWS.Lightsail.DeleteKnownHostKeys
  ( -- * Creating a request
    DeleteKnownHostKeys (..),
    mkDeleteKnownHostKeys,

    -- ** Request lenses
    dkhkInstanceName,

    -- * Destructuring the response
    DeleteKnownHostKeysResponse (..),
    mkDeleteKnownHostKeysResponse,

    -- ** Response lenses
    dkhkrrsOperations,
    dkhkrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteKnownHostKeys' smart constructor.
newtype DeleteKnownHostKeys = DeleteKnownHostKeys'
  { -- | The name of the instance for which you want to reset the host key or certificate.
    instanceName :: Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteKnownHostKeys' value with any optional fields omitted.
mkDeleteKnownHostKeys ::
  -- | 'instanceName'
  Types.ResourceName ->
  DeleteKnownHostKeys
mkDeleteKnownHostKeys instanceName =
  DeleteKnownHostKeys' {instanceName}

-- | The name of the instance for which you want to reset the host key or certificate.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkhkInstanceName :: Lens.Lens' DeleteKnownHostKeys Types.ResourceName
dkhkInstanceName = Lens.field @"instanceName"
{-# DEPRECATED dkhkInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Core.FromJSON DeleteKnownHostKeys where
  toJSON DeleteKnownHostKeys {..} =
    Core.object
      (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest DeleteKnownHostKeys where
  type Rs DeleteKnownHostKeys = DeleteKnownHostKeysResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Lightsail_20161128.DeleteKnownHostKeys")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteKnownHostKeysResponse'
            Core.<$> (x Core..:? "operations") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteKnownHostKeysResponse' smart constructor.
data DeleteKnownHostKeysResponse = DeleteKnownHostKeysResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Core.Maybe [Types.Operation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteKnownHostKeysResponse' value with any optional fields omitted.
mkDeleteKnownHostKeysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteKnownHostKeysResponse
mkDeleteKnownHostKeysResponse responseStatus =
  DeleteKnownHostKeysResponse'
    { operations = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkhkrrsOperations :: Lens.Lens' DeleteKnownHostKeysResponse (Core.Maybe [Types.Operation])
dkhkrrsOperations = Lens.field @"operations"
{-# DEPRECATED dkhkrrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkhkrrsResponseStatus :: Lens.Lens' DeleteKnownHostKeysResponse Core.Int
dkhkrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dkhkrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
