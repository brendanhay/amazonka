{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetStages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more 'Stage' resources.
module Network.AWS.ApiGateway.GetStages
    (
    -- * Creating a request
      GetStages (..)
    , mkGetStages
    -- ** Request lenses
    , gsRestApiId
    , gsDeploymentId

    -- * Destructuring the response
    , GetStagesResponse (..)
    , mkGetStagesResponse
    -- ** Response lenses
    , gsrrsItem
    , gsrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about one or more 'Stage' resources.
--
-- /See:/ 'mkGetStages' smart constructor.
data GetStages = GetStages'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , deploymentId :: Core.Maybe Core.Text
    -- ^ The stages' deployment identifiers.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetStages' value with any optional fields omitted.
mkGetStages
    :: Core.Text -- ^ 'restApiId'
    -> GetStages
mkGetStages restApiId
  = GetStages'{restApiId, deploymentId = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsRestApiId :: Lens.Lens' GetStages Core.Text
gsRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gsRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The stages' deployment identifiers.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsDeploymentId :: Lens.Lens' GetStages (Core.Maybe Core.Text)
gsDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE gsDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

instance Core.ToQuery GetStages where
        toQuery GetStages{..}
          = Core.maybe Core.mempty (Core.toQueryPair "deploymentId")
              deploymentId

instance Core.ToHeaders GetStages where
        toHeaders GetStages{..} = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetStages where
        type Rs GetStages = GetStagesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/stages",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetStagesResponse' Core.<$>
                   (x Core..:? "item") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A list of 'Stage' resources that are associated with the 'ApiKey' resource.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/stages.html Deploying API in Stages> 
--
-- /See:/ 'mkGetStagesResponse' smart constructor.
data GetStagesResponse = GetStagesResponse'
  { item :: Core.Maybe [Types.Stage]
    -- ^ The current page of elements from this collection.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetStagesResponse' value with any optional fields omitted.
mkGetStagesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetStagesResponse
mkGetStagesResponse responseStatus
  = GetStagesResponse'{item = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'item' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsItem :: Lens.Lens' GetStagesResponse (Core.Maybe [Types.Stage])
gsrrsItem = Lens.field @"item"
{-# INLINEABLE gsrrsItem #-}
{-# DEPRECATED item "Use generic-lens or generic-optics with 'item' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetStagesResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
