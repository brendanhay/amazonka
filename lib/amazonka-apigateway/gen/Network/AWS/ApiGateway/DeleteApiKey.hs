{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteApiKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'ApiKey' resource.
module Network.AWS.ApiGateway.DeleteApiKey
    (
    -- * Creating a request
      DeleteApiKey (..)
    , mkDeleteApiKey
    -- ** Request lenses
    , dakApiKey

    -- * Destructuring the response
    , DeleteApiKeyResponse (..)
    , mkDeleteApiKeyResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete the 'ApiKey' resource.
--
-- /See:/ 'mkDeleteApiKey' smart constructor.
newtype DeleteApiKey = DeleteApiKey'
  { apiKey :: Core.Text
    -- ^ [Required] The identifier of the 'ApiKey' resource to be deleted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApiKey' value with any optional fields omitted.
mkDeleteApiKey
    :: Core.Text -- ^ 'apiKey'
    -> DeleteApiKey
mkDeleteApiKey apiKey = DeleteApiKey'{apiKey}

-- | [Required] The identifier of the 'ApiKey' resource to be deleted.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dakApiKey :: Lens.Lens' DeleteApiKey Core.Text
dakApiKey = Lens.field @"apiKey"
{-# INLINEABLE dakApiKey #-}
{-# DEPRECATED apiKey "Use generic-lens or generic-optics with 'apiKey' instead"  #-}

instance Core.ToQuery DeleteApiKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApiKey where
        toHeaders DeleteApiKey{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteApiKey where
        type Rs DeleteApiKey = DeleteApiKeyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/apikeys/" Core.<> Core.toText apiKey,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteApiKeyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteApiKeyResponse' smart constructor.
data DeleteApiKeyResponse = DeleteApiKeyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApiKeyResponse' value with any optional fields omitted.
mkDeleteApiKeyResponse
    :: DeleteApiKeyResponse
mkDeleteApiKeyResponse = DeleteApiKeyResponse'
