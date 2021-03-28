{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteConditionalForwarder (..)
    , mkDeleteConditionalForwarder
    -- ** Request lenses
    , dcffDirectoryId
    , dcffRemoteDomainName

    -- * Destructuring the response
    , DeleteConditionalForwarderResponse (..)
    , mkDeleteConditionalForwarderResponse
    -- ** Response lenses
    , dcfrfrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes a conditional forwarder.
--
-- /See:/ 'mkDeleteConditionalForwarder' smart constructor.
data DeleteConditionalForwarder = DeleteConditionalForwarder'
  { directoryId :: Types.DirectoryId
    -- ^ The directory ID for which you are deleting the conditional forwarder.
  , remoteDomainName :: Types.RemoteDomainName
    -- ^ The fully qualified domain name (FQDN) of the remote domain with which you are deleting the conditional forwarder.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConditionalForwarder' value with any optional fields omitted.
mkDeleteConditionalForwarder
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.RemoteDomainName -- ^ 'remoteDomainName'
    -> DeleteConditionalForwarder
mkDeleteConditionalForwarder directoryId remoteDomainName
  = DeleteConditionalForwarder'{directoryId, remoteDomainName}

-- | The directory ID for which you are deleting the conditional forwarder.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcffDirectoryId :: Lens.Lens' DeleteConditionalForwarder Types.DirectoryId
dcffDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dcffDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The fully qualified domain name (FQDN) of the remote domain with which you are deleting the conditional forwarder.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcffRemoteDomainName :: Lens.Lens' DeleteConditionalForwarder Types.RemoteDomainName
dcffRemoteDomainName = Lens.field @"remoteDomainName"
{-# INLINEABLE dcffRemoteDomainName #-}
{-# DEPRECATED remoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead"  #-}

instance Core.ToQuery DeleteConditionalForwarder where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteConditionalForwarder where
        toHeaders DeleteConditionalForwarder{..}
          = Core.pure
              ("X-Amz-Target",
               "DirectoryService_20150416.DeleteConditionalForwarder")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteConditionalForwarder where
        toJSON DeleteConditionalForwarder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("RemoteDomainName" Core..= remoteDomainName)])

instance Core.AWSRequest DeleteConditionalForwarder where
        type Rs DeleteConditionalForwarder =
             DeleteConditionalForwarderResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteConditionalForwarderResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The result of a DeleteConditionalForwarder request.
--
-- /See:/ 'mkDeleteConditionalForwarderResponse' smart constructor.
newtype DeleteConditionalForwarderResponse = DeleteConditionalForwarderResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteConditionalForwarderResponse' value with any optional fields omitted.
mkDeleteConditionalForwarderResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteConditionalForwarderResponse
mkDeleteConditionalForwarderResponse responseStatus
  = DeleteConditionalForwarderResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrfrsResponseStatus :: Lens.Lens' DeleteConditionalForwarderResponse Core.Int
dcfrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcfrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
