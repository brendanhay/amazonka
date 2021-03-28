{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateApiKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a unique key that you can distribute to clients who are executing your API.
module Network.AWS.AppSync.CreateApiKey
    (
    -- * Creating a request
      CreateApiKey (..)
    , mkCreateApiKey
    -- ** Request lenses
    , cakApiId
    , cakDescription
    , cakExpires

    -- * Destructuring the response
    , CreateApiKeyResponse (..)
    , mkCreateApiKeyResponse
    -- ** Response lenses
    , cakrrsApiKey
    , cakrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateApiKey' smart constructor.
data CreateApiKey = CreateApiKey'
  { apiId :: Core.Text
    -- ^ The ID for your GraphQL API.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the purpose of the API key.
  , expires :: Core.Maybe Core.Integer
    -- ^ The time from creation time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour. The default value for this parameter is 7 days from creation time. For more information, see .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApiKey' value with any optional fields omitted.
mkCreateApiKey
    :: Core.Text -- ^ 'apiId'
    -> CreateApiKey
mkCreateApiKey apiId
  = CreateApiKey'{apiId, description = Core.Nothing,
                  expires = Core.Nothing}

-- | The ID for your GraphQL API.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakApiId :: Lens.Lens' CreateApiKey Core.Text
cakApiId = Lens.field @"apiId"
{-# INLINEABLE cakApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | A description of the purpose of the API key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakDescription :: Lens.Lens' CreateApiKey (Core.Maybe Core.Text)
cakDescription = Lens.field @"description"
{-# INLINEABLE cakDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The time from creation time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour. The default value for this parameter is 7 days from creation time. For more information, see .
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakExpires :: Lens.Lens' CreateApiKey (Core.Maybe Core.Integer)
cakExpires = Lens.field @"expires"
{-# INLINEABLE cakExpires #-}
{-# DEPRECATED expires "Use generic-lens or generic-optics with 'expires' instead"  #-}

instance Core.ToQuery CreateApiKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApiKey where
        toHeaders CreateApiKey{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateApiKey where
        toJSON CreateApiKey{..}
          = Core.object
              (Core.catMaybes
                 [("description" Core..=) Core.<$> description,
                  ("expires" Core..=) Core.<$> expires])

instance Core.AWSRequest CreateApiKey where
        type Rs CreateApiKey = CreateApiKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/apikeys",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateApiKeyResponse' Core.<$>
                   (x Core..:? "apiKey") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateApiKeyResponse' smart constructor.
data CreateApiKeyResponse = CreateApiKeyResponse'
  { apiKey :: Core.Maybe Types.ApiKey
    -- ^ The API key.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApiKeyResponse' value with any optional fields omitted.
mkCreateApiKeyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateApiKeyResponse
mkCreateApiKeyResponse responseStatus
  = CreateApiKeyResponse'{apiKey = Core.Nothing, responseStatus}

-- | The API key.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakrrsApiKey :: Lens.Lens' CreateApiKeyResponse (Core.Maybe Types.ApiKey)
cakrrsApiKey = Lens.field @"apiKey"
{-# INLINEABLE cakrrsApiKey #-}
{-# DEPRECATED apiKey "Use generic-lens or generic-optics with 'apiKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakrrsResponseStatus :: Lens.Lens' CreateApiKeyResponse Core.Int
cakrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cakrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
