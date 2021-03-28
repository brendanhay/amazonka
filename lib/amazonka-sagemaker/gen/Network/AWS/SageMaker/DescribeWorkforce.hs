{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists private workforce information, including workforce name, Amazon Resource Name (ARN), and, if applicable, allowed IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ). Allowable IP address ranges are the IP addresses that workers can use to access tasks. 
--
-- /Important:/ This operation applies only to private workforces.
module Network.AWS.SageMaker.DescribeWorkforce
    (
    -- * Creating a request
      DescribeWorkforce (..)
    , mkDescribeWorkforce
    -- ** Request lenses
    , dWorkforceName

    -- * Destructuring the response
    , DescribeWorkforceResponse (..)
    , mkDescribeWorkforceResponse
    -- ** Response lenses
    , dwrrsWorkforce
    , dwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeWorkforce' smart constructor.
newtype DescribeWorkforce = DescribeWorkforce'
  { workforceName :: Types.WorkforceName
    -- ^ The name of the private workforce whose access you want to restrict. @WorkforceName@ is automatically set to @default@ when a workforce is created and cannot be modified. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkforce' value with any optional fields omitted.
mkDescribeWorkforce
    :: Types.WorkforceName -- ^ 'workforceName'
    -> DescribeWorkforce
mkDescribeWorkforce workforceName
  = DescribeWorkforce'{workforceName}

-- | The name of the private workforce whose access you want to restrict. @WorkforceName@ is automatically set to @default@ when a workforce is created and cannot be modified. 
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dWorkforceName :: Lens.Lens' DescribeWorkforce Types.WorkforceName
dWorkforceName = Lens.field @"workforceName"
{-# INLINEABLE dWorkforceName #-}
{-# DEPRECATED workforceName "Use generic-lens or generic-optics with 'workforceName' instead"  #-}

instance Core.ToQuery DescribeWorkforce where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeWorkforce where
        toHeaders DescribeWorkforce{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeWorkforce") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeWorkforce where
        toJSON DescribeWorkforce{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkforceName" Core..= workforceName)])

instance Core.AWSRequest DescribeWorkforce where
        type Rs DescribeWorkforce = DescribeWorkforceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeWorkforceResponse' Core.<$>
                   (x Core..: "Workforce") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeWorkforceResponse' smart constructor.
data DescribeWorkforceResponse = DescribeWorkforceResponse'
  { workforce :: Types.Workforce
    -- ^ A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeWorkforceResponse' value with any optional fields omitted.
mkDescribeWorkforceResponse
    :: Types.Workforce -- ^ 'workforce'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeWorkforceResponse
mkDescribeWorkforceResponse workforce responseStatus
  = DescribeWorkforceResponse'{workforce, responseStatus}

-- | A single private workforce, which is automatically created when you create your first private work team. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
--
-- /Note:/ Consider using 'workforce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsWorkforce :: Lens.Lens' DescribeWorkforceResponse Types.Workforce
dwrrsWorkforce = Lens.field @"workforce"
{-# INLINEABLE dwrrsWorkforce #-}
{-# DEPRECATED workforce "Use generic-lens or generic-optics with 'workforce' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrrsResponseStatus :: Lens.Lens' DescribeWorkforceResponse Core.Int
dwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
