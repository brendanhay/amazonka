{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateDomainEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a domain recordset after it is created.
--
-- The @update domain entry@ operation supports tag-based access control via resource tags applied to the resource identified by @domain name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.UpdateDomainEntry
    (
    -- * Creating a request
      UpdateDomainEntry (..)
    , mkUpdateDomainEntry
    -- ** Request lenses
    , udeDomainName
    , udeDomainEntry

    -- * Destructuring the response
    , UpdateDomainEntryResponse (..)
    , mkUpdateDomainEntryResponse
    -- ** Response lenses
    , uderrsOperations
    , uderrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDomainEntry' smart constructor.
data UpdateDomainEntry = UpdateDomainEntry'
  { domainName :: Types.DomainName
    -- ^ The name of the domain recordset to update.
  , domainEntry :: Types.DomainEntry
    -- ^ An array of key-value pairs containing information about the domain entry.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainEntry' value with any optional fields omitted.
mkUpdateDomainEntry
    :: Types.DomainName -- ^ 'domainName'
    -> Types.DomainEntry -- ^ 'domainEntry'
    -> UpdateDomainEntry
mkUpdateDomainEntry domainName domainEntry
  = UpdateDomainEntry'{domainName, domainEntry}

-- | The name of the domain recordset to update.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeDomainName :: Lens.Lens' UpdateDomainEntry Types.DomainName
udeDomainName = Lens.field @"domainName"
{-# INLINEABLE udeDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | An array of key-value pairs containing information about the domain entry.
--
-- /Note:/ Consider using 'domainEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeDomainEntry :: Lens.Lens' UpdateDomainEntry Types.DomainEntry
udeDomainEntry = Lens.field @"domainEntry"
{-# INLINEABLE udeDomainEntry #-}
{-# DEPRECATED domainEntry "Use generic-lens or generic-optics with 'domainEntry' instead"  #-}

instance Core.ToQuery UpdateDomainEntry where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDomainEntry where
        toHeaders UpdateDomainEntry{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.UpdateDomainEntry")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDomainEntry where
        toJSON UpdateDomainEntry{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domainName" Core..= domainName),
                  Core.Just ("domainEntry" Core..= domainEntry)])

instance Core.AWSRequest UpdateDomainEntry where
        type Rs UpdateDomainEntry = UpdateDomainEntryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateDomainEntryResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDomainEntryResponse' smart constructor.
data UpdateDomainEntryResponse = UpdateDomainEntryResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateDomainEntryResponse' value with any optional fields omitted.
mkUpdateDomainEntryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateDomainEntryResponse
mkUpdateDomainEntryResponse responseStatus
  = UpdateDomainEntryResponse'{operations = Core.Nothing,
                               responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uderrsOperations :: Lens.Lens' UpdateDomainEntryResponse (Core.Maybe [Types.Operation])
uderrsOperations = Lens.field @"operations"
{-# INLINEABLE uderrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uderrsResponseStatus :: Lens.Lens' UpdateDomainEntryResponse Core.Int
uderrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uderrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
