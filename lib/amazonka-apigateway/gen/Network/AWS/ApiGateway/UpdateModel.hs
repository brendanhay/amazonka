{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.UpdateModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a model.
module Network.AWS.ApiGateway.UpdateModel
    (
    -- * Creating a request
      UpdateModel (..)
    , mkUpdateModel
    -- ** Request lenses
    , umRestApiId
    , umModelName
    , umPatchOperations

     -- * Destructuring the response
    , Types.Model (..)
    , Types.mkModel
    -- ** Response lenses
    , Types.mContentType
    , Types.mDescription
    , Types.mId
    , Types.mName
    , Types.mSchema
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to update an existing model in an existing 'RestApi' resource.
--
-- /See:/ 'mkUpdateModel' smart constructor.
data UpdateModel = UpdateModel'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , modelName :: Core.Text
    -- ^ [Required] The name of the model to update.
  , patchOperations :: Core.Maybe [Types.PatchOperation]
    -- ^ A list of update operations to be applied to the specified resource and in the order specified in this list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateModel' value with any optional fields omitted.
mkUpdateModel
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'modelName'
    -> UpdateModel
mkUpdateModel restApiId modelName
  = UpdateModel'{restApiId, modelName,
                 patchOperations = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umRestApiId :: Lens.Lens' UpdateModel Core.Text
umRestApiId = Lens.field @"restApiId"
{-# INLINEABLE umRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the model to update.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umModelName :: Lens.Lens' UpdateModel Core.Text
umModelName = Lens.field @"modelName"
{-# INLINEABLE umModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- /Note:/ Consider using 'patchOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umPatchOperations :: Lens.Lens' UpdateModel (Core.Maybe [Types.PatchOperation])
umPatchOperations = Lens.field @"patchOperations"
{-# INLINEABLE umPatchOperations #-}
{-# DEPRECATED patchOperations "Use generic-lens or generic-optics with 'patchOperations' instead"  #-}

instance Core.ToQuery UpdateModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateModel where
        toHeaders UpdateModel{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON UpdateModel where
        toJSON UpdateModel{..}
          = Core.object
              (Core.catMaybes
                 [("patchOperations" Core..=) Core.<$> patchOperations])

instance Core.AWSRequest UpdateModel where
        type Rs UpdateModel = Types.Model
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/models/"
                             Core.<> Core.toText modelName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
