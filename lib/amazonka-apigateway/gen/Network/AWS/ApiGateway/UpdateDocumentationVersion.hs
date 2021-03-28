{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateDocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.UpdateDocumentationVersion
    (
    -- * Creating a request
      UpdateDocumentationVersion (..)
    , mkUpdateDocumentationVersion
    -- ** Request lenses
    , udvRestApiId
    , udvDocumentationVersion
    , udvPatchOperations

     -- * Destructuring the response
    , Types.DocumentationVersion (..)
    , Types.mkDocumentationVersion
    -- ** Response lenses
    , Types.dvCreatedDate
    , Types.dvDescription
    , Types.dvVersion
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates an existing documentation version of an API.
--
-- /See:/ 'mkUpdateDocumentationVersion' smart constructor.
data UpdateDocumentationVersion = UpdateDocumentationVersion'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' ..
  , documentationVersion :: Core.Text
    -- ^ [Required] The version identifier of the to-be-updated documentation version.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocumentationVersion' value with any optional fields omitted.
mkUpdateDocumentationVersion
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'documentationVersion'
    -> UpdateDocumentationVersion
mkUpdateDocumentationVersion restApiId documentationVersion
  = UpdateDocumentationVersion'{restApiId, documentationVersion,
                                patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' ..
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvRestApiId :: Lens.Lens' UpdateDocumentationVersion Core.Text
udvRestApiId = Lens.field @"restApiId"
{-# INLINEABLE udvRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The version identifier of the to-be-updated documentation version.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvDocumentationVersion :: Lens.Lens' UpdateDocumentationVersion Core.Text
udvDocumentationVersion = Lens.field @"documentationVersion"
{-# INLINEABLE udvDocumentationVersion #-}
{-# DEPRECATED documentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvPatchOperations :: Lens.Lens' UpdateDocumentationVersion (Core.Maybe [Types.PatchOperation])
udvPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE udvPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateDocumentationVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDocumentationVersion where
        toHeaders UpdateDocumentationVersion{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateDocumentationVersion where
        toJSON UpdateDocumentationVersion{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateDocumentationVersion where
        type Rs UpdateDocumentationVersion = Types.DocumentationVersion
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/versions/"
                             Core.<> Core.toText documentationVersion,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
