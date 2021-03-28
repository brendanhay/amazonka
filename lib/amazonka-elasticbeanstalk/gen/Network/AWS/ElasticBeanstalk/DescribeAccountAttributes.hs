{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns attributes related to AWS Elastic Beanstalk that are associated with the calling AWS account.
--
-- The result currently has one set of attributes—resource quotas.
module Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
    (
    -- * Creating a request
      DescribeAccountAttributes (..)
    , mkDescribeAccountAttributes

    -- * Destructuring the response
    , DescribeAccountAttributesResponse (..)
    , mkDescribeAccountAttributesResponse
    -- ** Response lenses
    , daarrsResourceQuotas
    , daarrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributes' value with any optional fields omitted.
mkDescribeAccountAttributes
    :: DescribeAccountAttributes
mkDescribeAccountAttributes = DescribeAccountAttributes'

instance Core.ToQuery DescribeAccountAttributes where
        toQuery DescribeAccountAttributes{..}
          = Core.toQueryPair "Action"
              ("DescribeAccountAttributes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)

instance Core.ToHeaders DescribeAccountAttributes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAccountAttributes where
        type Rs DescribeAccountAttributes =
             DescribeAccountAttributesResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeAccountAttributesResult"
              (\ s h x ->
                 DescribeAccountAttributesResponse' Core.<$>
                   (x Core..@? "ResourceQuotas") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { resourceQuotas :: Core.Maybe Types.ResourceQuotas
    -- ^ The Elastic Beanstalk resource quotas associated with the calling AWS account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAccountAttributesResponse' value with any optional fields omitted.
mkDescribeAccountAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse responseStatus
  = DescribeAccountAttributesResponse'{resourceQuotas = Core.Nothing,
                                       responseStatus}

-- | The Elastic Beanstalk resource quotas associated with the calling AWS account.
--
-- /Note:/ Consider using 'resourceQuotas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsResourceQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Core.Maybe Types.ResourceQuotas)
daarrsResourceQuotas = Lens.field @"resourceQuotas"
{-# INLINEABLE daarrsResourceQuotas #-}
{-# DEPRECATED resourceQuotas "Use generic-lens or generic-optics with 'resourceQuotas' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarrsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Core.Int
daarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
