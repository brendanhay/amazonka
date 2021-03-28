{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScalingProcessTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scaling process types for use with the 'ResumeProcesses' and 'SuspendProcesses' APIs.
module Network.AWS.AutoScaling.DescribeScalingProcessTypes
    (
    -- * Creating a request
      DescribeScalingProcessTypes (..)
    , mkDescribeScalingProcessTypes

    -- * Destructuring the response
    , DescribeScalingProcessTypesResponse (..)
    , mkDescribeScalingProcessTypesResponse
    -- ** Response lenses
    , dsptrrsProcesses
    , dsptrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeScalingProcessTypes' smart constructor.
data DescribeScalingProcessTypes = DescribeScalingProcessTypes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingProcessTypes' value with any optional fields omitted.
mkDescribeScalingProcessTypes
    :: DescribeScalingProcessTypes
mkDescribeScalingProcessTypes = DescribeScalingProcessTypes'

instance Core.ToQuery DescribeScalingProcessTypes where
        toQuery DescribeScalingProcessTypes{..}
          = Core.toQueryPair "Action"
              ("DescribeScalingProcessTypes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)

instance Core.ToHeaders DescribeScalingProcessTypes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeScalingProcessTypes where
        type Rs DescribeScalingProcessTypes =
             DescribeScalingProcessTypesResponse
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
          = Response.receiveXMLWrapper "DescribeScalingProcessTypesResult"
              (\ s h x ->
                 DescribeScalingProcessTypesResponse' Core.<$>
                   (x Core..@? "Processes" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeScalingProcessTypesResponse' smart constructor.
data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse'
  { processes :: Core.Maybe [Types.ProcessType]
    -- ^ The names of the process types.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingProcessTypesResponse' value with any optional fields omitted.
mkDescribeScalingProcessTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeScalingProcessTypesResponse
mkDescribeScalingProcessTypesResponse responseStatus
  = DescribeScalingProcessTypesResponse'{processes = Core.Nothing,
                                         responseStatus}

-- | The names of the process types.
--
-- /Note:/ Consider using 'processes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsptrrsProcesses :: Lens.Lens' DescribeScalingProcessTypesResponse (Core.Maybe [Types.ProcessType])
dsptrrsProcesses = Lens.field @"processes"
{-# INLINEABLE dsptrrsProcesses #-}
{-# DEPRECATED processes "Use generic-lens or generic-optics with 'processes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsptrrsResponseStatus :: Lens.Lens' DescribeScalingProcessTypesResponse Core.Int
dsptrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsptrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
