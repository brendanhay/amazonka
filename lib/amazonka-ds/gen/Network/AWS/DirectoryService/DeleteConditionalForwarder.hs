{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteConditionalForwarder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conditional forwarder that has been set up for your AWS directory.
module Network.AWS.DirectoryService.DeleteConditionalForwarder
  ( -- * Creating a request
    DeleteConditionalForwarder (..),
    mkDeleteConditionalForwarder,

    -- ** Request lenses
    dcffDirectoryId,
    dcffRemoteDomainName,

    -- * Destructuring the response
    DeleteConditionalForwarderResponse (..),
    mkDeleteConditionalForwarderResponse,

    -- ** Response lenses
    dcfrfrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes a conditional forwarder.
--
-- /See:/ 'mkDeleteConditionalForwarder' smart constructor.
data DeleteConditionalForwarder = DeleteConditionalForwarder'
  { -- | The directory ID for which you are deleting the conditional forwarder.
    directoryId :: Types.DirectoryId,
    -- | The fully qualified domain name (FQDN) of the remote domain with which you are deleting the conditional forwarder.
    remoteDomainName :: Types.RemoteDomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConditionalForwarder' value with any optional fields omitted.
mkDeleteConditionalForwarder ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'remoteDomainName'
  Types.RemoteDomainName ->
  DeleteConditionalForwarder
mkDeleteConditionalForwarder directoryId remoteDomainName =
  DeleteConditionalForwarder' {directoryId, remoteDomainName}

-- | The directory ID for which you are deleting the conditional forwarder.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcffDirectoryId :: Lens.Lens' DeleteConditionalForwarder Types.DirectoryId
dcffDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED dcffDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The fully qualified domain name (FQDN) of the remote domain with which you are deleting the conditional forwarder.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcffRemoteDomainName :: Lens.Lens' DeleteConditionalForwarder Types.RemoteDomainName
dcffRemoteDomainName = Lens.field @"remoteDomainName"
{-# DEPRECATED dcffRemoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead." #-}

instance Core.FromJSON DeleteConditionalForwarder where
  toJSON DeleteConditionalForwarder {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("RemoteDomainName" Core..= remoteDomainName)
          ]
      )

instance Core.AWSRequest DeleteConditionalForwarder where
  type
    Rs DeleteConditionalForwarder =
      DeleteConditionalForwarderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "DirectoryService_20150416.DeleteConditionalForwarder"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConditionalForwarderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The result of a DeleteConditionalForwarder request.
--
-- /See:/ 'mkDeleteConditionalForwarderResponse' smart constructor.
newtype DeleteConditionalForwarderResponse = DeleteConditionalForwarderResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConditionalForwarderResponse' value with any optional fields omitted.
mkDeleteConditionalForwarderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteConditionalForwarderResponse
mkDeleteConditionalForwarderResponse responseStatus =
  DeleteConditionalForwarderResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrfrsResponseStatus :: Lens.Lens' DeleteConditionalForwarderResponse Core.Int
dcfrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcfrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
