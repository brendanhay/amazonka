{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a 'Deployments' collection.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetDeployments
    (
    -- * Creating a request
      GetDeployments (..)
    , mkGetDeployments
    -- ** Request lenses
    , gdRestApiId
    , gdLimit
    , gdPosition

    -- * Destructuring the response
    , GetDeploymentsResponse (..)
    , mkGetDeploymentsResponse
    -- ** Response lenses
    , gdrrsItems
    , gdrrsPosition
    , gdrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests API Gateway to get information about a 'Deployments' collection.
--
-- /See:/ 'mkGetDeployments' smart constructor.
data GetDeployments = GetDeployments'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDeployments' value with any optional fields omitted.
mkGetDeployments
    :: Core.Text -- ^ 'restApiId'
    -> GetDeployments
mkGetDeployments restApiId
  = GetDeployments'{restApiId, limit = Core.Nothing,
                    position = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdRestApiId :: Lens.Lens' GetDeployments Core.Text
gdRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gdRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdLimit :: Lens.Lens' GetDeployments (Core.Maybe Core.Int)
gdLimit = Lens.field @"limit"
{-# INLINEABLE gdLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdPosition :: Lens.Lens' GetDeployments (Core.Maybe Core.Text)
gdPosition = Lens.field @"position"
{-# INLINEABLE gdPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetDeployments where
        toQuery GetDeployments{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetDeployments where
        toHeaders GetDeployments{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetDeployments where
        type Rs GetDeployments = GetDeploymentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/deployments",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDeploymentsResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetDeployments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Represents a collection resource that contains zero or more references to your existing deployments, and links that guide you on how to interact with your collection. The collection offers a paginated view of the contained deployments.
--
-- To create a new deployment of a 'RestApi' , make a @POST@ request against this resource. To view, update, or delete an existing deployment, make a @GET@ , @PATCH@ , or @DELETE@ request, respectively, on a specified 'Deployment' resource.<https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-deploy-api.html Deploying an API> , <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-deployment.html AWS CLI> , <https://aws.amazon.com/tools/ AWS SDKs> 
--
-- /See:/ 'mkGetDeploymentsResponse' smart constructor.
data GetDeploymentsResponse = GetDeploymentsResponse'
  { items :: Core.Maybe [Types.Deployment]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDeploymentsResponse' value with any optional fields omitted.
mkGetDeploymentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDeploymentsResponse
mkGetDeploymentsResponse responseStatus
  = GetDeploymentsResponse'{items = Core.Nothing,
                            position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsItems :: Lens.Lens' GetDeploymentsResponse (Core.Maybe [Types.Deployment])
gdrrsItems = Lens.field @"items"
{-# INLINEABLE gdrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsPosition :: Lens.Lens' GetDeploymentsResponse (Core.Maybe Core.Text)
gdrrsPosition = Lens.field @"position"
{-# INLINEABLE gdrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDeploymentsResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
