{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain resource for the specified domain (e.g., example.com).
--
-- The @create domain@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDomain
    (
    -- * Creating a request
      CreateDomain (..)
    , mkCreateDomain
    -- ** Request lenses
    , cdfDomainName
    , cdfTags

    -- * Destructuring the response
    , CreateDomainResponse (..)
    , mkCreateDomainResponse
    -- ** Response lenses
    , crsOperation
    , crsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { domainName :: Types.DomainName
    -- ^ The domain name to manage (e.g., @example.com@ ).
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomain' value with any optional fields omitted.
mkCreateDomain
    :: Types.DomainName -- ^ 'domainName'
    -> CreateDomain
mkCreateDomain domainName
  = CreateDomain'{domainName, tags = Core.Nothing}

-- | The domain name to manage (e.g., @example.com@ ).
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfDomainName :: Lens.Lens' CreateDomain Types.DomainName
cdfDomainName = Lens.field @"domainName"
{-# INLINEABLE cdfDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfTags :: Lens.Lens' CreateDomain (Core.Maybe [Types.Tag])
cdfTags = Lens.field @"tags"
{-# INLINEABLE cdfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDomain where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDomain where
        toHeaders CreateDomain{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.CreateDomain")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDomain where
        toJSON CreateDomain{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domainName" Core..= domainName),
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateDomain where
        type Rs CreateDomain = CreateDomainResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDomainResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDomainResponse' value with any optional fields omitted.
mkCreateDomainResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDomainResponse
mkCreateDomainResponse responseStatus
  = CreateDomainResponse'{operation = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsOperation :: Lens.Lens' CreateDomainResponse (Core.Maybe Types.Operation)
crsOperation = Lens.field @"operation"
{-# INLINEABLE crsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateDomainResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
