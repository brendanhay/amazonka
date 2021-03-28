{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteDocumentationPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.DeleteDocumentationPart
    (
    -- * Creating a request
      DeleteDocumentationPart (..)
    , mkDeleteDocumentationPart
    -- ** Request lenses
    , ddpRestApiId
    , ddpDocumentationPartId

    -- * Destructuring the response
    , DeleteDocumentationPartResponse (..)
    , mkDeleteDocumentationPartResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes an existing documentation part of an API.
--
-- /See:/ 'mkDeleteDocumentationPart' smart constructor.
data DeleteDocumentationPart = DeleteDocumentationPart'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , documentationPartId :: Core.Text
    -- ^ [Required] The identifier of the to-be-deleted documentation part.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocumentationPart' value with any optional fields omitted.
mkDeleteDocumentationPart
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'documentationPartId'
    -> DeleteDocumentationPart
mkDeleteDocumentationPart restApiId documentationPartId
  = DeleteDocumentationPart'{restApiId, documentationPartId}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpRestApiId :: Lens.Lens' DeleteDocumentationPart Core.Text
ddpRestApiId = Lens.field @"restApiId"
{-# INLINEABLE ddpRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the to-be-deleted documentation part.
--
-- /Note:/ Consider using 'documentationPartId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpDocumentationPartId :: Lens.Lens' DeleteDocumentationPart Core.Text
ddpDocumentationPartId = Lens.field @"documentationPartId"
{-# INLINEABLE ddpDocumentationPartId #-}
{-# DEPRECATED documentationPartId "Use generic-lens or generic-optics with 'documentationPartId' instead"  #-}

instance Core.ToQuery DeleteDocumentationPart where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDocumentationPart where
        toHeaders DeleteDocumentationPart{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteDocumentationPart where
        type Rs DeleteDocumentationPart = DeleteDocumentationPartResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/parts/"
                             Core.<> Core.toText documentationPartId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeleteDocumentationPartResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteDocumentationPartResponse' smart constructor.
data DeleteDocumentationPartResponse = DeleteDocumentationPartResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocumentationPartResponse' value with any optional fields omitted.
mkDeleteDocumentationPartResponse
    :: DeleteDocumentationPartResponse
mkDeleteDocumentationPartResponse
  = DeleteDocumentationPartResponse'
