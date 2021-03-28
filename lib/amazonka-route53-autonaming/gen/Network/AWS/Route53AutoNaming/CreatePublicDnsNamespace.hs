{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.CreatePublicDnsNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a public namespace based on DNS, which will be visible on the internet. The namespace defines your service naming scheme. For example, if you name your namespace @example.com@ and name your service @backend@ , the resulting DNS name for the service will be @backend.example.com@ . For the current quota on the number of namespaces that you can create using the same AWS account, see <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits> in the /AWS Cloud Map Developer Guide/ .
module Network.AWS.Route53AutoNaming.CreatePublicDnsNamespace
    (
    -- * Creating a request
      CreatePublicDnsNamespace (..)
    , mkCreatePublicDnsNamespace
    -- ** Request lenses
    , cpdnName
    , cpdnCreatorRequestId
    , cpdnDescription
    , cpdnTags

    -- * Destructuring the response
    , CreatePublicDnsNamespaceResponse (..)
    , mkCreatePublicDnsNamespaceResponse
    -- ** Response lenses
    , cpdnrrsOperationId
    , cpdnrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkCreatePublicDnsNamespace' smart constructor.
data CreatePublicDnsNamespace = CreatePublicDnsNamespace'
  { name :: Types.NamespaceName
    -- ^ The name that you want to assign to this namespace.
  , creatorRequestId :: Core.Maybe Types.ResourceId
    -- ^ A unique string that identifies the request and that allows failed @CreatePublicDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
  , description :: Core.Maybe Types.ResourceDescription
    -- ^ A description for the namespace.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePublicDnsNamespace' value with any optional fields omitted.
mkCreatePublicDnsNamespace
    :: Types.NamespaceName -- ^ 'name'
    -> CreatePublicDnsNamespace
mkCreatePublicDnsNamespace name
  = CreatePublicDnsNamespace'{name, creatorRequestId = Core.Nothing,
                              description = Core.Nothing, tags = Core.Nothing}

-- | The name that you want to assign to this namespace.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnName :: Lens.Lens' CreatePublicDnsNamespace Types.NamespaceName
cpdnName = Lens.field @"name"
{-# INLINEABLE cpdnName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique string that identifies the request and that allows failed @CreatePublicDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnCreatorRequestId :: Lens.Lens' CreatePublicDnsNamespace (Core.Maybe Types.ResourceId)
cpdnCreatorRequestId = Lens.field @"creatorRequestId"
{-# INLINEABLE cpdnCreatorRequestId #-}
{-# DEPRECATED creatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead"  #-}

-- | A description for the namespace.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnDescription :: Lens.Lens' CreatePublicDnsNamespace (Core.Maybe Types.ResourceDescription)
cpdnDescription = Lens.field @"description"
{-# INLINEABLE cpdnDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnTags :: Lens.Lens' CreatePublicDnsNamespace (Core.Maybe [Types.Tag])
cpdnTags = Lens.field @"tags"
{-# INLINEABLE cpdnTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreatePublicDnsNamespace where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePublicDnsNamespace where
        toHeaders CreatePublicDnsNamespace{..}
          = Core.pure
              ("X-Amz-Target",
               "Route53AutoNaming_v20170314.CreatePublicDnsNamespace")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePublicDnsNamespace where
        toJSON CreatePublicDnsNamespace{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("CreatorRequestId" Core..=) Core.<$> creatorRequestId,
                  ("Description" Core..=) Core.<$> description,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreatePublicDnsNamespace where
        type Rs CreatePublicDnsNamespace = CreatePublicDnsNamespaceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePublicDnsNamespaceResponse' Core.<$>
                   (x Core..:? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePublicDnsNamespaceResponse' smart constructor.
data CreatePublicDnsNamespaceResponse = CreatePublicDnsNamespaceResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePublicDnsNamespaceResponse' value with any optional fields omitted.
mkCreatePublicDnsNamespaceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePublicDnsNamespaceResponse
mkCreatePublicDnsNamespaceResponse responseStatus
  = CreatePublicDnsNamespaceResponse'{operationId = Core.Nothing,
                                      responseStatus}

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnrrsOperationId :: Lens.Lens' CreatePublicDnsNamespaceResponse (Core.Maybe Types.OperationId)
cpdnrrsOperationId = Lens.field @"operationId"
{-# INLINEABLE cpdnrrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnrrsResponseStatus :: Lens.Lens' CreatePublicDnsNamespaceResponse Core.Int
cpdnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpdnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
