{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateDocumentationPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.UpdateDocumentationPart
    (
    -- * Creating a request
      UpdateDocumentationPart (..)
    , mkUpdateDocumentationPart
    -- ** Request lenses
    , udpRestApiId
    , udpDocumentationPartId
    , udpPatchOperations

     -- * Destructuring the response
    , Types.DocumentationPart (..)
    , Types.mkDocumentationPart
    -- ** Response lenses
    , Types.dpId
    , Types.dpLocation
    , Types.dpProperties
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates an existing documentation part of a given API.
--
-- /See:/ 'mkUpdateDocumentationPart' smart constructor.
data UpdateDocumentationPart = UpdateDocumentationPart'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , documentationPartId :: Core.Text
    -- ^ [Required] The identifier of the to-be-updated documentation part.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocumentationPart' value with any optional fields omitted.
mkUpdateDocumentationPart
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'documentationPartId'
    -> UpdateDocumentationPart
mkUpdateDocumentationPart restApiId documentationPartId
  = UpdateDocumentationPart'{restApiId, documentationPartId,
                             patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpRestApiId :: Lens.Lens' UpdateDocumentationPart Core.Text
udpRestApiId = Lens.field @"restApiId"
{-# INLINEABLE udpRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The identifier of the to-be-updated documentation part.
--
-- /Note:/ Consider using 'documentationPartId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpDocumentationPartId :: Lens.Lens' UpdateDocumentationPart Core.Text
udpDocumentationPartId = Lens.field @"documentationPartId"
{-# INLINEABLE udpDocumentationPartId #-}
{-# DEPRECATED documentationPartId "Use generic-lens or generic-optics with 'documentationPartId' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udpPatchOperations :: Lens.Lens' UpdateDocumentationPart (Core.Maybe [Types.PatchOperation])
udpPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE udpPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateDocumentationPart where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDocumentationPart where
        toHeaders UpdateDocumentationPart{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateDocumentationPart where
        toJSON UpdateDocumentationPart{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateDocumentationPart where
        type Rs UpdateDocumentationPart = Types.DocumentationPart
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/parts/"
                             Core.<> Core.toText documentationPartId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
