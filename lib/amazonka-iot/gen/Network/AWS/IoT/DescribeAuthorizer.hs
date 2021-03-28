{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an authorizer.
module Network.AWS.IoT.DescribeAuthorizer
    (
    -- * Creating a request
      DescribeAuthorizer (..)
    , mkDescribeAuthorizer
    -- ** Request lenses
    , daAuthorizerName

    -- * Destructuring the response
    , DescribeAuthorizerResponse (..)
    , mkDescribeAuthorizerResponse
    -- ** Response lenses
    , darrsAuthorizerDescription
    , darrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAuthorizer' smart constructor.
newtype DescribeAuthorizer = DescribeAuthorizer'
  { authorizerName :: Types.AuthorizerName
    -- ^ The name of the authorizer to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAuthorizer' value with any optional fields omitted.
mkDescribeAuthorizer
    :: Types.AuthorizerName -- ^ 'authorizerName'
    -> DescribeAuthorizer
mkDescribeAuthorizer authorizerName
  = DescribeAuthorizer'{authorizerName}

-- | The name of the authorizer to describe.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAuthorizerName :: Lens.Lens' DescribeAuthorizer Types.AuthorizerName
daAuthorizerName = Lens.field @"authorizerName"
{-# INLINEABLE daAuthorizerName #-}
{-# DEPRECATED authorizerName "Use generic-lens or generic-optics with 'authorizerName' instead"  #-}

instance Core.ToQuery DescribeAuthorizer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAuthorizer where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAuthorizer where
        type Rs DescribeAuthorizer = DescribeAuthorizerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/authorizer/" Core.<> Core.toText authorizerName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAuthorizerResponse' Core.<$>
                   (x Core..:? "authorizerDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAuthorizerResponse' smart constructor.
data DescribeAuthorizerResponse = DescribeAuthorizerResponse'
  { authorizerDescription :: Core.Maybe Types.AuthorizerDescription
    -- ^ The authorizer description.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAuthorizerResponse' value with any optional fields omitted.
mkDescribeAuthorizerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAuthorizerResponse
mkDescribeAuthorizerResponse responseStatus
  = DescribeAuthorizerResponse'{authorizerDescription = Core.Nothing,
                                responseStatus}

-- | The authorizer description.
--
-- /Note:/ Consider using 'authorizerDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAuthorizerDescription :: Lens.Lens' DescribeAuthorizerResponse (Core.Maybe Types.AuthorizerDescription)
darrsAuthorizerDescription = Lens.field @"authorizerDescription"
{-# INLINEABLE darrsAuthorizerDescription #-}
{-# DEPRECATED authorizerDescription "Use generic-lens or generic-optics with 'authorizerDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAuthorizerResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
