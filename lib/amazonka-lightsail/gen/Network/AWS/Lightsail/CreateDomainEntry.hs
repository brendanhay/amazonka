{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDomainEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one of the following domain name system (DNS) records in a domain DNS zone: Address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT).
--
-- The @create domain entry@ operation supports tag-based access control via resource tags applied to the resource identified by @domain name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDomainEntry
    (
    -- * Creating a request
      CreateDomainEntry (..)
    , mkCreateDomainEntry
    -- ** Request lenses
    , cdeDomainName
    , cdeDomainEntry

    -- * Destructuring the response
    , CreateDomainEntryResponse (..)
    , mkCreateDomainEntryResponse
    -- ** Response lenses
    , cderrsOperation
    , cderrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDomainEntry' smart constructor.
data CreateDomainEntry = CreateDomainEntry'
  { domainName :: Types.DomainName
    -- ^ The domain name (e.g., @example.com@ ) for which you want to create the domain entry.
  , domainEntry :: Types.DomainEntry
    -- ^ An array of key-value pairs containing information about the domain entry request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomainEntry' value with any optional fields omitted.
mkCreateDomainEntry
    :: Types.DomainName -- ^ 'domainName'
    -> Types.DomainEntry -- ^ 'domainEntry'
    -> CreateDomainEntry
mkCreateDomainEntry domainName domainEntry
  = CreateDomainEntry'{domainName, domainEntry}

-- | The domain name (e.g., @example.com@ ) for which you want to create the domain entry.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeDomainName :: Lens.Lens' CreateDomainEntry Types.DomainName
cdeDomainName = Lens.field @"domainName"
{-# INLINEABLE cdeDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | An array of key-value pairs containing information about the domain entry request.
--
-- /Note:/ Consider using 'domainEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeDomainEntry :: Lens.Lens' CreateDomainEntry Types.DomainEntry
cdeDomainEntry = Lens.field @"domainEntry"
{-# INLINEABLE cdeDomainEntry #-}
{-# DEPRECATED domainEntry "Use generic-lens or generic-optics with 'domainEntry' instead"  #-}

instance Core.ToQuery CreateDomainEntry where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDomainEntry where
        toHeaders CreateDomainEntry{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateDomainEntry")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDomainEntry where
        toJSON CreateDomainEntry{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domainName" Core..= domainName),
                  Core.Just ("domainEntry" Core..= domainEntry)])

instance Core.AWSRequest CreateDomainEntry where
        type Rs CreateDomainEntry = CreateDomainEntryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDomainEntryResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDomainEntryResponse' smart constructor.
data CreateDomainEntryResponse = CreateDomainEntryResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDomainEntryResponse' value with any optional fields omitted.
mkCreateDomainEntryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDomainEntryResponse
mkCreateDomainEntryResponse responseStatus
  = CreateDomainEntryResponse'{operation = Core.Nothing,
                               responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsOperation :: Lens.Lens' CreateDomainEntryResponse (Core.Maybe Types.Operation)
cderrsOperation = Lens.field @"operation"
{-# INLINEABLE cderrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cderrsResponseStatus :: Lens.Lens' CreateDomainEntryResponse Core.Int
cderrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cderrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
