{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model.
module Network.AWS.ApiGateway.DeleteModel
    (
    -- * Creating a request
      DeleteModel (..)
    , mkDeleteModel
    -- ** Request lenses
    , dmRestApiId
    , dmModelName

    -- * Destructuring the response
    , DeleteModelResponse (..)
    , mkDeleteModelResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an existing model in an existing 'RestApi' resource.
--
-- /See:/ 'mkDeleteModel' smart constructor.
data DeleteModel = DeleteModel'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , modelName :: Core.Text
    -- ^ [Required] The name of the model to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteModel' value with any optional fields omitted.
mkDeleteModel
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'modelName'
    -> DeleteModel
mkDeleteModel restApiId modelName
  = DeleteModel'{restApiId, modelName}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmRestApiId :: Lens.Lens' DeleteModel Core.Text
dmRestApiId = Lens.field @"restApiId"
{-# INLINEABLE dmRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the model to delete.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmModelName :: Lens.Lens' DeleteModel Core.Text
dmModelName = Lens.field @"modelName"
{-# INLINEABLE dmModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

instance Core.ToQuery DeleteModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteModel where
        toHeaders DeleteModel{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteModel where
        type Rs DeleteModel = DeleteModelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/models/"
                             Core.<> Core.toText modelName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteModelResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteModelResponse' smart constructor.
data DeleteModelResponse = DeleteModelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteModelResponse' value with any optional fields omitted.
mkDeleteModelResponse
    :: DeleteModelResponse
mkDeleteModelResponse = DeleteModelResponse'
