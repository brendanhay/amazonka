{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.DescribeEnvironments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about AWS Cloud9 development environments.
module Network.AWS.Cloud9.DescribeEnvironments
    (
    -- * Creating a request
      DescribeEnvironments (..)
    , mkDescribeEnvironments
    -- ** Request lenses
    , deEnvironmentIds

    -- * Destructuring the response
    , DescribeEnvironmentsResponse (..)
    , mkDescribeEnvironmentsResponse
    -- ** Response lenses
    , drsEnvironments
    , drsResponseStatus
    ) where

import qualified Network.AWS.Cloud9.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEnvironments' smart constructor.
newtype DescribeEnvironments = DescribeEnvironments'
  { environmentIds :: Core.NonEmpty Types.EnvironmentId
    -- ^ The IDs of individual environments to get information about.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironments' value with any optional fields omitted.
mkDescribeEnvironments
    :: Core.NonEmpty Types.EnvironmentId -- ^ 'environmentIds'
    -> DescribeEnvironments
mkDescribeEnvironments environmentIds
  = DescribeEnvironments'{environmentIds}

-- | The IDs of individual environments to get information about.
--
-- /Note:/ Consider using 'environmentIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEnvironmentIds :: Lens.Lens' DescribeEnvironments (Core.NonEmpty Types.EnvironmentId)
deEnvironmentIds = Lens.field @"environmentIds"
{-# INLINEABLE deEnvironmentIds #-}
{-# DEPRECATED environmentIds "Use generic-lens or generic-optics with 'environmentIds' instead"  #-}

instance Core.ToQuery DescribeEnvironments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeEnvironments where
        toHeaders DescribeEnvironments{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCloud9WorkspaceManagementService.DescribeEnvironments")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeEnvironments where
        toJSON DescribeEnvironments{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("environmentIds" Core..= environmentIds)])

instance Core.AWSRequest DescribeEnvironments where
        type Rs DescribeEnvironments = DescribeEnvironmentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeEnvironmentsResponse' Core.<$>
                   (x Core..:? "environments") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeEnvironmentsResponse' smart constructor.
data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse'
  { environments :: Core.Maybe [Types.Environment]
    -- ^ Information about the environments that are returned.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEnvironmentsResponse' value with any optional fields omitted.
mkDescribeEnvironmentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeEnvironmentsResponse
mkDescribeEnvironmentsResponse responseStatus
  = DescribeEnvironmentsResponse'{environments = Core.Nothing,
                                  responseStatus}

-- | Information about the environments that are returned.
--
-- /Note:/ Consider using 'environments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsEnvironments :: Lens.Lens' DescribeEnvironmentsResponse (Core.Maybe [Types.Environment])
drsEnvironments = Lens.field @"environments"
{-# INLINEABLE drsEnvironments #-}
{-# DEPRECATED environments "Use generic-lens or generic-optics with 'environments' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeEnvironmentsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
