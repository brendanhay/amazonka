{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteResolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a @Resolver@ object.
module Network.AWS.AppSync.DeleteResolver
    (
    -- * Creating a request
      DeleteResolver (..)
    , mkDeleteResolver
    -- ** Request lenses
    , drApiId
    , drTypeName
    , drFieldName

    -- * Destructuring the response
    , DeleteResolverResponse (..)
    , mkDeleteResolverResponse
    -- ** Response lenses
    , drrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteResolver' smart constructor.
data DeleteResolver = DeleteResolver'
  { apiId :: Core.Text
    -- ^ The API ID.
  , typeName :: Types.ResourceName
    -- ^ The name of the resolver type.
  , fieldName :: Types.ResourceName
    -- ^ The resolver field name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResolver' value with any optional fields omitted.
mkDeleteResolver
    :: Core.Text -- ^ 'apiId'
    -> Types.ResourceName -- ^ 'typeName'
    -> Types.ResourceName -- ^ 'fieldName'
    -> DeleteResolver
mkDeleteResolver apiId typeName fieldName
  = DeleteResolver'{apiId, typeName, fieldName}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drApiId :: Lens.Lens' DeleteResolver Core.Text
drApiId = Lens.field @"apiId"
{-# INLINEABLE drApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The name of the resolver type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drTypeName :: Lens.Lens' DeleteResolver Types.ResourceName
drTypeName = Lens.field @"typeName"
{-# INLINEABLE drTypeName #-}
{-# DEPRECATED typeName "Use generic-lens or generic-optics with 'typeName' instead"  #-}

-- | The resolver field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drFieldName :: Lens.Lens' DeleteResolver Types.ResourceName
drFieldName = Lens.field @"fieldName"
{-# INLINEABLE drFieldName #-}
{-# DEPRECATED fieldName "Use generic-lens or generic-optics with 'fieldName' instead"  #-}

instance Core.ToQuery DeleteResolver where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteResolver where
        toHeaders DeleteResolver{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteResolver where
        type Rs DeleteResolver = DeleteResolverResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/types/" Core.<>
                             Core.toText typeName
                             Core.<> "/resolvers/"
                             Core.<> Core.toText fieldName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteResolverResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteResolverResponse' smart constructor.
newtype DeleteResolverResponse = DeleteResolverResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResolverResponse' value with any optional fields omitted.
mkDeleteResolverResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteResolverResponse
mkDeleteResolverResponse responseStatus
  = DeleteResolverResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DeleteResolverResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
