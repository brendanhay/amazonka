{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDomainEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific domain entry.
--
-- The @delete domain entry@ operation supports tag-based access control via resource tags applied to the resource identified by @domain name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteDomainEntry
  ( -- * Creating a request
    DeleteDomainEntry (..),
    mkDeleteDomainEntry,

    -- ** Request lenses
    ddeDomainName,
    ddeDomainEntry,

    -- * Destructuring the response
    DeleteDomainEntryResponse (..),
    mkDeleteDomainEntryResponse,

    -- ** Response lenses
    dderrsOperation,
    dderrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDomainEntry' smart constructor.
data DeleteDomainEntry = DeleteDomainEntry'
  { -- | The name of the domain entry to delete.
    domainName :: Types.DomainName,
    -- | An array of key-value pairs containing information about your domain entries.
    domainEntry :: Types.DomainEntry
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainEntry' value with any optional fields omitted.
mkDeleteDomainEntry ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'domainEntry'
  Types.DomainEntry ->
  DeleteDomainEntry
mkDeleteDomainEntry domainName domainEntry =
  DeleteDomainEntry' {domainName, domainEntry}

-- | The name of the domain entry to delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeDomainName :: Lens.Lens' DeleteDomainEntry Types.DomainName
ddeDomainName = Lens.field @"domainName"
{-# DEPRECATED ddeDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | An array of key-value pairs containing information about your domain entries.
--
-- /Note:/ Consider using 'domainEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeDomainEntry :: Lens.Lens' DeleteDomainEntry Types.DomainEntry
ddeDomainEntry = Lens.field @"domainEntry"
{-# DEPRECATED ddeDomainEntry "Use generic-lens or generic-optics with 'domainEntry' instead." #-}

instance Core.FromJSON DeleteDomainEntry where
  toJSON DeleteDomainEntry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domainName" Core..= domainName),
            Core.Just ("domainEntry" Core..= domainEntry)
          ]
      )

instance Core.AWSRequest DeleteDomainEntry where
  type Rs DeleteDomainEntry = DeleteDomainEntryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteDomainEntry")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDomainEntryResponse'
            Core.<$> (x Core..:? "operation") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDomainEntryResponse' smart constructor.
data DeleteDomainEntryResponse = DeleteDomainEntryResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Core.Maybe Types.Operation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteDomainEntryResponse' value with any optional fields omitted.
mkDeleteDomainEntryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDomainEntryResponse
mkDeleteDomainEntryResponse responseStatus =
  DeleteDomainEntryResponse'
    { operation = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dderrsOperation :: Lens.Lens' DeleteDomainEntryResponse (Core.Maybe Types.Operation)
dderrsOperation = Lens.field @"operation"
{-# DEPRECATED dderrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dderrsResponseStatus :: Lens.Lens' DeleteDomainEntryResponse Core.Int
dderrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dderrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
