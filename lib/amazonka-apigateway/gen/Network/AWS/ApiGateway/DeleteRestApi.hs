{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteRestApi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified API.
module Network.AWS.ApiGateway.DeleteRestApi
    (
    -- * Creating a request
      DeleteRestApi (..)
    , mkDeleteRestApi
    -- ** Request lenses
    , draRestApiId

    -- * Destructuring the response
    , DeleteRestApiResponse (..)
    , mkDeleteRestApiResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete the specified API from your collection.
--
-- /See:/ 'mkDeleteRestApi' smart constructor.
newtype DeleteRestApi = DeleteRestApi'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRestApi' value with any optional fields omitted.
mkDeleteRestApi
    :: Core.Text -- ^ 'restApiId'
    -> DeleteRestApi
mkDeleteRestApi restApiId = DeleteRestApi'{restApiId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draRestApiId :: Lens.Lens' DeleteRestApi Core.Text
draRestApiId = Lens.field @"restApiId"
{-# INLINEABLE draRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

instance Core.ToQuery DeleteRestApi where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRestApi where
        toHeaders DeleteRestApi{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteRestApi where
        type Rs DeleteRestApi = DeleteRestApiResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/restapis/" Core.<> Core.toText restApiId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteRestApiResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRestApiResponse' smart constructor.
data DeleteRestApiResponse = DeleteRestApiResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRestApiResponse' value with any optional fields omitted.
mkDeleteRestApiResponse
    :: DeleteRestApiResponse
mkDeleteRestApiResponse = DeleteRestApiResponse'
