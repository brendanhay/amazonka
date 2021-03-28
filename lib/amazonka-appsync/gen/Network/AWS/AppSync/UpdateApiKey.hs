{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateApiKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an API key. The key can be updated while it is not deleted.
module Network.AWS.AppSync.UpdateApiKey
    (
    -- * Creating a request
      UpdateApiKey (..)
    , mkUpdateApiKey
    -- ** Request lenses
    , uakApiId
    , uakId
    , uakDescription
    , uakExpires

    -- * Destructuring the response
    , UpdateApiKeyResponse (..)
    , mkUpdateApiKeyResponse
    -- ** Response lenses
    , uakrrsApiKey
    , uakrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApiKey' smart constructor.
data UpdateApiKey = UpdateApiKey'
  { apiId :: Core.Text
    -- ^ The ID for the GraphQL API.
  , id :: Core.Text
    -- ^ The API key ID.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the purpose of the API key.
  , expires :: Core.Maybe Core.Integer
    -- ^ The time from update time after which the API key expires. The date is represented as seconds since the epoch. For more information, see .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApiKey' value with any optional fields omitted.
mkUpdateApiKey
    :: Core.Text -- ^ 'apiId'
    -> Core.Text -- ^ 'id'
    -> UpdateApiKey
mkUpdateApiKey apiId id
  = UpdateApiKey'{apiId, id, description = Core.Nothing,
                  expires = Core.Nothing}

-- | The ID for the GraphQL API.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakApiId :: Lens.Lens' UpdateApiKey Core.Text
uakApiId = Lens.field @"apiId"
{-# INLINEABLE uakApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The API key ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakId :: Lens.Lens' UpdateApiKey Core.Text
uakId = Lens.field @"id"
{-# INLINEABLE uakId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A description of the purpose of the API key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakDescription :: Lens.Lens' UpdateApiKey (Core.Maybe Core.Text)
uakDescription = Lens.field @"description"
{-# INLINEABLE uakDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The time from update time after which the API key expires. The date is represented as seconds since the epoch. For more information, see .
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakExpires :: Lens.Lens' UpdateApiKey (Core.Maybe Core.Integer)
uakExpires = Lens.field @"expires"
{-# INLINEABLE uakExpires #-}
{-# DEPRECATED expires "Use generic-lens or generic-optics with 'expires' instead"  #-}

instance Core.ToQuery UpdateApiKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApiKey where
        toHeaders UpdateApiKey{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApiKey where
        toJSON UpdateApiKey{..}
          = Core.object
              (Core.catMaybes
                 [("description" Core..=) Core.<$> description,
                  ("expires" Core..=) Core.<$> expires])

instance Core.AWSRequest UpdateApiKey where
        type Rs UpdateApiKey = UpdateApiKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/apikeys/" Core.<>
                             Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateApiKeyResponse' Core.<$>
                   (x Core..:? "apiKey") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApiKeyResponse' smart constructor.
data UpdateApiKeyResponse = UpdateApiKeyResponse'
  { apiKey :: Core.Maybe Types.ApiKey
    -- ^ The API key.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApiKeyResponse' value with any optional fields omitted.
mkUpdateApiKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateApiKeyResponse
mkUpdateApiKeyResponse responseStatus
  = UpdateApiKeyResponse'{apiKey = Core.Nothing, responseStatus}

-- | The API key.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakrrsApiKey :: Lens.Lens' UpdateApiKeyResponse (Core.Maybe Types.ApiKey)
uakrrsApiKey = Lens.field @"apiKey"
{-# INLINEABLE uakrrsApiKey #-}
{-# DEPRECATED apiKey "Use generic-lens or generic-optics with 'apiKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uakrrsResponseStatus :: Lens.Lens' UpdateApiKeyResponse Core.Int
uakrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uakrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
