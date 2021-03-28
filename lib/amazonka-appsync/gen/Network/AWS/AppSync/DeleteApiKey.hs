{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteApiKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an API key.
module Network.AWS.AppSync.DeleteApiKey
    (
    -- * Creating a request
      DeleteApiKey (..)
    , mkDeleteApiKey
    -- ** Request lenses
    , dakApiId
    , dakId

    -- * Destructuring the response
    , DeleteApiKeyResponse (..)
    , mkDeleteApiKeyResponse
    -- ** Response lenses
    , dakrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApiKey' smart constructor.
data DeleteApiKey = DeleteApiKey'
  { apiId :: Core.Text
    -- ^ The API ID.
  , id :: Core.Text
    -- ^ The ID for the API key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApiKey' value with any optional fields omitted.
mkDeleteApiKey
    :: Core.Text -- ^ 'apiId'
    -> Core.Text -- ^ 'id'
    -> DeleteApiKey
mkDeleteApiKey apiId id = DeleteApiKey'{apiId, id}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakApiId :: Lens.Lens' DeleteApiKey Core.Text
dakApiId = Lens.field @"apiId"
{-# INLINEABLE dakApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The ID for the API key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakId :: Lens.Lens' DeleteApiKey Core.Text
dakId = Lens.field @"id"
{-# INLINEABLE dakId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteApiKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApiKey where
        toHeaders DeleteApiKey{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteApiKey where
        type Rs DeleteApiKey = DeleteApiKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/apikeys/" Core.<>
                             Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteApiKeyResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApiKeyResponse' smart constructor.
newtype DeleteApiKeyResponse = DeleteApiKeyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApiKeyResponse' value with any optional fields omitted.
mkDeleteApiKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteApiKeyResponse
mkDeleteApiKeyResponse responseStatus
  = DeleteApiKeyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakrrsResponseStatus :: Lens.Lens' DeleteApiKeyResponse Core.Int
dakrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dakrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
