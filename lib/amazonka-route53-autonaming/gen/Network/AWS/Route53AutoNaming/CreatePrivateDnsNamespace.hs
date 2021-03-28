{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.CreatePrivateDnsNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a private namespace based on DNS, which will be visible only inside a specified Amazon VPC. The namespace defines your service naming scheme. For example, if you name your namespace @example.com@ and name your service @backend@ , the resulting DNS name for the service will be @backend.example.com@ . For the current quota on the number of namespaces that you can create using the same AWS account, see <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits> in the /AWS Cloud Map Developer Guide/ .
module Network.AWS.Route53AutoNaming.CreatePrivateDnsNamespace
    (
    -- * Creating a request
      CreatePrivateDnsNamespace (..)
    , mkCreatePrivateDnsNamespace
    -- ** Request lenses
    , cName
    , cVpc
    , cCreatorRequestId
    , cDescription
    , cTags

    -- * Destructuring the response
    , CreatePrivateDnsNamespaceResponse (..)
    , mkCreatePrivateDnsNamespaceResponse
    -- ** Response lenses
    , crsOperationId
    , crsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkCreatePrivateDnsNamespace' smart constructor.
data CreatePrivateDnsNamespace = CreatePrivateDnsNamespace'
  { name :: Types.NamespaceName
    -- ^ The name that you want to assign to this namespace. When you create a private DNS namespace, AWS Cloud Map automatically creates an Amazon Route 53 private hosted zone that has the same name as the namespace.
  , vpc :: Types.ResourceId
    -- ^ The ID of the Amazon VPC that you want to associate the namespace with.
  , creatorRequestId :: Core.Maybe Types.ResourceId
    -- ^ A unique string that identifies the request and that allows failed @CreatePrivateDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
  , description :: Core.Maybe Types.ResourceDescription
    -- ^ A description for the namespace.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePrivateDnsNamespace' value with any optional fields omitted.
mkCreatePrivateDnsNamespace
    :: Types.NamespaceName -- ^ 'name'
    -> Types.ResourceId -- ^ 'vpc'
    -> CreatePrivateDnsNamespace
mkCreatePrivateDnsNamespace name vpc
  = CreatePrivateDnsNamespace'{name, vpc,
                               creatorRequestId = Core.Nothing, description = Core.Nothing,
                               tags = Core.Nothing}

-- | The name that you want to assign to this namespace. When you create a private DNS namespace, AWS Cloud Map automatically creates an Amazon Route 53 private hosted zone that has the same name as the namespace.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CreatePrivateDnsNamespace Types.NamespaceName
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the Amazon VPC that you want to associate the namespace with.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVpc :: Lens.Lens' CreatePrivateDnsNamespace Types.ResourceId
cVpc = Lens.field @"vpc"
{-# INLINEABLE cVpc #-}
{-# DEPRECATED vpc "Use generic-lens or generic-optics with 'vpc' instead"  #-}

-- | A unique string that identifies the request and that allows failed @CreatePrivateDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreatorRequestId :: Lens.Lens' CreatePrivateDnsNamespace (Core.Maybe Types.ResourceId)
cCreatorRequestId = Lens.field @"creatorRequestId"
{-# INLINEABLE cCreatorRequestId #-}
{-# DEPRECATED creatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead"  #-}

-- | A description for the namespace.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreatePrivateDnsNamespace (Core.Maybe Types.ResourceDescription)
cDescription = Lens.field @"description"
{-# INLINEABLE cDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreatePrivateDnsNamespace (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreatePrivateDnsNamespace where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePrivateDnsNamespace where
        toHeaders CreatePrivateDnsNamespace{..}
          = Core.pure
              ("X-Amz-Target",
               "Route53AutoNaming_v20170314.CreatePrivateDnsNamespace")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePrivateDnsNamespace where
        toJSON CreatePrivateDnsNamespace{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name), Core.Just ("Vpc" Core..= vpc),
                  ("CreatorRequestId" Core..=) Core.<$> creatorRequestId,
                  ("Description" Core..=) Core.<$> description,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreatePrivateDnsNamespace where
        type Rs CreatePrivateDnsNamespace =
             CreatePrivateDnsNamespaceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePrivateDnsNamespaceResponse' Core.<$>
                   (x Core..:? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePrivateDnsNamespaceResponse' smart constructor.
data CreatePrivateDnsNamespaceResponse = CreatePrivateDnsNamespaceResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePrivateDnsNamespaceResponse' value with any optional fields omitted.
mkCreatePrivateDnsNamespaceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePrivateDnsNamespaceResponse
mkCreatePrivateDnsNamespaceResponse responseStatus
  = CreatePrivateDnsNamespaceResponse'{operationId = Core.Nothing,
                                       responseStatus}

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsOperationId :: Lens.Lens' CreatePrivateDnsNamespaceResponse (Core.Maybe Types.OperationId)
crsOperationId = Lens.field @"operationId"
{-# INLINEABLE crsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreatePrivateDnsNamespaceResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
