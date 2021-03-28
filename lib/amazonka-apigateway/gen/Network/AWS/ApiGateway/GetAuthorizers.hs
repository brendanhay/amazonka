{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetAuthorizers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Authorizers' resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizers.html AWS CLI> 
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetAuthorizers
    (
    -- * Creating a request
      GetAuthorizers (..)
    , mkGetAuthorizers
    -- ** Request lenses
    , gaRestApiId
    , gaLimit
    , gaPosition

    -- * Destructuring the response
    , GetAuthorizersResponse (..)
    , mkGetAuthorizersResponse
    -- ** Response lenses
    , garrsItems
    , garrsPosition
    , garrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe an existing 'Authorizers' resource.
--
-- /See:/ 'mkGetAuthorizers' smart constructor.
data GetAuthorizers = GetAuthorizers'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAuthorizers' value with any optional fields omitted.
mkGetAuthorizers
    :: Core.Text -- ^ 'restApiId'
    -> GetAuthorizers
mkGetAuthorizers restApiId
  = GetAuthorizers'{restApiId, limit = Core.Nothing,
                    position = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaRestApiId :: Lens.Lens' GetAuthorizers Core.Text
gaRestApiId = Lens.field @"restApiId"
{-# INLINEABLE gaRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaLimit :: Lens.Lens' GetAuthorizers (Core.Maybe Core.Int)
gaLimit = Lens.field @"limit"
{-# INLINEABLE gaLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaPosition :: Lens.Lens' GetAuthorizers (Core.Maybe Core.Text)
gaPosition = Lens.field @"position"
{-# INLINEABLE gaPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetAuthorizers where
        toQuery GetAuthorizers{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetAuthorizers where
        toHeaders GetAuthorizers{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetAuthorizers where
        type Rs GetAuthorizers = GetAuthorizersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/authorizers",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAuthorizersResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetAuthorizers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Represents a collection of 'Authorizer' resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer> <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer> 
--
-- /See:/ 'mkGetAuthorizersResponse' smart constructor.
data GetAuthorizersResponse = GetAuthorizersResponse'
  { items :: Core.Maybe [Types.Authorizer]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAuthorizersResponse' value with any optional fields omitted.
mkGetAuthorizersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAuthorizersResponse
mkGetAuthorizersResponse responseStatus
  = GetAuthorizersResponse'{items = Core.Nothing,
                            position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsItems :: Lens.Lens' GetAuthorizersResponse (Core.Maybe [Types.Authorizer])
garrsItems = Lens.field @"items"
{-# INLINEABLE garrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsPosition :: Lens.Lens' GetAuthorizersResponse (Core.Maybe Core.Text)
garrsPosition = Lens.field @"position"
{-# INLINEABLE garrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAuthorizersResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
