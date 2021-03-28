{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteDomainEntry (..)
    , mkDeleteDomainEntry
    -- ** Request lenses
    , ddeDomainName
    , ddeDomainEntry

    -- * Destructuring the response
    , DeleteDomainEntryResponse (..)
    , mkDeleteDomainEntryResponse
    -- ** Response lenses
    , dderrsOperation
    , dderrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDomainEntry' smart constructor.
data DeleteDomainEntry = DeleteDomainEntry'
  { domainName :: Types.DomainName
    -- ^ The name of the domain entry to delete.
  , domainEntry :: Types.DomainEntry
    -- ^ An array of key-value pairs containing information about your domain entries.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainEntry' value with any optional fields omitted.
mkDeleteDomainEntry
    :: Types.DomainName -- ^ 'domainName'
    -> Types.DomainEntry -- ^ 'domainEntry'
    -> DeleteDomainEntry
mkDeleteDomainEntry domainName domainEntry
  = DeleteDomainEntry'{domainName, domainEntry}

-- | The name of the domain entry to delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeDomainName :: Lens.Lens' DeleteDomainEntry Types.DomainName
ddeDomainName = Lens.field @"domainName"
{-# INLINEABLE ddeDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | An array of key-value pairs containing information about your domain entries.
--
-- /Note:/ Consider using 'domainEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeDomainEntry :: Lens.Lens' DeleteDomainEntry Types.DomainEntry
ddeDomainEntry = Lens.field @"domainEntry"
{-# INLINEABLE ddeDomainEntry #-}
{-# DEPRECATED domainEntry "Use generic-lens or generic-optics with 'domainEntry' instead"  #-}

instance Core.ToQuery DeleteDomainEntry where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDomainEntry where
        toHeaders DeleteDomainEntry{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DeleteDomainEntry")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDomainEntry where
        toJSON DeleteDomainEntry{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domainName" Core..= domainName),
                  Core.Just ("domainEntry" Core..= domainEntry)])

instance Core.AWSRequest DeleteDomainEntry where
        type Rs DeleteDomainEntry = DeleteDomainEntryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDomainEntryResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDomainEntryResponse' smart constructor.
data DeleteDomainEntryResponse = DeleteDomainEntryResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDomainEntryResponse' value with any optional fields omitted.
mkDeleteDomainEntryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDomainEntryResponse
mkDeleteDomainEntryResponse responseStatus
  = DeleteDomainEntryResponse'{operation = Core.Nothing,
                               responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dderrsOperation :: Lens.Lens' DeleteDomainEntryResponse (Core.Maybe Types.Operation)
dderrsOperation = Lens.field @"operation"
{-# INLINEABLE dderrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dderrsResponseStatus :: Lens.Lens' DeleteDomainEntryResponse Core.Int
dderrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dderrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
