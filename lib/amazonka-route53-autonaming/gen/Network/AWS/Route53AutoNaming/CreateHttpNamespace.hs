{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.CreateHttpNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HTTP namespace. Service instances that you register using an HTTP namespace can be discovered using a @DiscoverInstances@ request but can't be discovered using DNS. 
--
-- For the current quota on the number of namespaces that you can create using the same AWS account, see <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map quotas> in the /AWS Cloud Map Developer Guide/ .
module Network.AWS.Route53AutoNaming.CreateHttpNamespace
    (
    -- * Creating a request
      CreateHttpNamespace (..)
    , mkCreateHttpNamespace
    -- ** Request lenses
    , chnName
    , chnCreatorRequestId
    , chnDescription
    , chnTags

    -- * Destructuring the response
    , CreateHttpNamespaceResponse (..)
    , mkCreateHttpNamespaceResponse
    -- ** Response lenses
    , chnrrsOperationId
    , chnrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkCreateHttpNamespace' smart constructor.
data CreateHttpNamespace = CreateHttpNamespace'
  { name :: Types.NamespaceName
    -- ^ The name that you want to assign to this namespace.
  , creatorRequestId :: Core.Maybe Types.ResourceId
    -- ^ A unique string that identifies the request and that allows failed @CreateHttpNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
  , description :: Core.Maybe Types.ResourceDescription
    -- ^ A description for the namespace.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHttpNamespace' value with any optional fields omitted.
mkCreateHttpNamespace
    :: Types.NamespaceName -- ^ 'name'
    -> CreateHttpNamespace
mkCreateHttpNamespace name
  = CreateHttpNamespace'{name, creatorRequestId = Core.Nothing,
                         description = Core.Nothing, tags = Core.Nothing}

-- | The name that you want to assign to this namespace.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chnName :: Lens.Lens' CreateHttpNamespace Types.NamespaceName
chnName = Lens.field @"name"
{-# INLINEABLE chnName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique string that identifies the request and that allows failed @CreateHttpNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chnCreatorRequestId :: Lens.Lens' CreateHttpNamespace (Core.Maybe Types.ResourceId)
chnCreatorRequestId = Lens.field @"creatorRequestId"
{-# INLINEABLE chnCreatorRequestId #-}
{-# DEPRECATED creatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead"  #-}

-- | A description for the namespace.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chnDescription :: Lens.Lens' CreateHttpNamespace (Core.Maybe Types.ResourceDescription)
chnDescription = Lens.field @"description"
{-# INLINEABLE chnDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chnTags :: Lens.Lens' CreateHttpNamespace (Core.Maybe [Types.Tag])
chnTags = Lens.field @"tags"
{-# INLINEABLE chnTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateHttpNamespace where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateHttpNamespace where
        toHeaders CreateHttpNamespace{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.CreateHttpNamespace")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateHttpNamespace where
        toJSON CreateHttpNamespace{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("CreatorRequestId" Core..=) Core.<$> creatorRequestId,
                  ("Description" Core..=) Core.<$> description,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateHttpNamespace where
        type Rs CreateHttpNamespace = CreateHttpNamespaceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateHttpNamespaceResponse' Core.<$>
                   (x Core..:? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateHttpNamespaceResponse' smart constructor.
data CreateHttpNamespaceResponse = CreateHttpNamespaceResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateHttpNamespaceResponse' value with any optional fields omitted.
mkCreateHttpNamespaceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateHttpNamespaceResponse
mkCreateHttpNamespaceResponse responseStatus
  = CreateHttpNamespaceResponse'{operationId = Core.Nothing,
                                 responseStatus}

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chnrrsOperationId :: Lens.Lens' CreateHttpNamespaceResponse (Core.Maybe Types.OperationId)
chnrrsOperationId = Lens.field @"operationId"
{-# INLINEABLE chnrrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chnrrsResponseStatus :: Lens.Lens' CreateHttpNamespaceResponse Core.Int
chnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE chnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
