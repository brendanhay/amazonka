{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.DeleteStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'Stage' resource.
module Network.AWS.ApiGateway.DeleteStage
    (
    -- * Creating a request
      DeleteStage (..)
    , mkDeleteStage
    -- ** Request lenses
    , dsRestApiId
    , dsStageName

    -- * Destructuring the response
    , DeleteStageResponse (..)
    , mkDeleteStageResponse
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to delete a 'Stage' resource.
--
-- /See:/ 'mkDeleteStage' smart constructor.
data DeleteStage = DeleteStage'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , stageName :: Core.Text
    -- ^ [Required] The name of the 'Stage' resource to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStage' value with any optional fields omitted.
mkDeleteStage
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'stageName'
    -> DeleteStage
mkDeleteStage restApiId stageName
  = DeleteStage'{restApiId, stageName}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsRestApiId :: Lens.Lens' DeleteStage Core.Text
dsRestApiId = Lens.field @"restApiId"
{-# INLINEABLE dsRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the 'Stage' resource to delete.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStageName :: Lens.Lens' DeleteStage Core.Text
dsStageName = Lens.field @"stageName"
{-# INLINEABLE dsStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

instance Core.ToQuery DeleteStage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteStage where
        toHeaders DeleteStage{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest DeleteStage where
        type Rs DeleteStage = DeleteStageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/stages/"
                             Core.<> Core.toText stageName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteStageResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteStageResponse' smart constructor.
data DeleteStageResponse = DeleteStageResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStageResponse' value with any optional fields omitted.
mkDeleteStageResponse
    :: DeleteStageResponse
mkDeleteStageResponse = DeleteStageResponse'
