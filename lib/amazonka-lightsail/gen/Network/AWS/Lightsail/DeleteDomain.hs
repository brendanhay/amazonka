{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain recordset and all of its domain records.
--
-- The @delete domain@ operation supports tag-based access control via resource tags applied to the resource identified by @domain name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteDomain
    (
    -- * Creating a request
      DeleteDomain (..)
    , mkDeleteDomain
    -- ** Request lenses
    , ddDomainName

    -- * Destructuring the response
    , DeleteDomainResponse (..)
    , mkDeleteDomainResponse
    -- ** Response lenses
    , ddrgrsOperation
    , ddrgrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDomain' smart constructor.
newtype DeleteDomain = DeleteDomain'
  { domainName :: Types.DomainName
    -- ^ The specific domain name to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomain' value with any optional fields omitted.
mkDeleteDomain
    :: Types.DomainName -- ^ 'domainName'
    -> DeleteDomain
mkDeleteDomain domainName = DeleteDomain'{domainName}

-- | The specific domain name to delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainName :: Lens.Lens' DeleteDomain Types.DomainName
ddDomainName = Lens.field @"domainName"
{-# INLINEABLE ddDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

instance Core.ToQuery DeleteDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDomain where
        toHeaders DeleteDomain{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.DeleteDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteDomain where
        toJSON DeleteDomain{..}
          = Core.object
              (Core.catMaybes [Core.Just ("domainName" Core..= domainName)])

instance Core.AWSRequest DeleteDomain where
        type Rs DeleteDomain = DeleteDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDomainResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDomainResponse' value with any optional fields omitted.
mkDeleteDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDomainResponse
mkDeleteDomainResponse responseStatus
  = DeleteDomainResponse'{operation = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrgrsOperation :: Lens.Lens' DeleteDomainResponse (Core.Maybe Types.Operation)
ddrgrsOperation = Lens.field @"operation"
{-# INLINEABLE ddrgrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrgrsResponseStatus :: Lens.Lens' DeleteDomainResponse Core.Int
ddrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
