{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific work team. You can see information such as the create date, the last updated date, membership information, and the work team's Amazon Resource Name (ARN).
module Network.AWS.SageMaker.DescribeWorkteam
    (
    -- * Creating a request
      DescribeWorkteam (..)
    , mkDescribeWorkteam
    -- ** Request lenses
    , dWorkteamName

    -- * Destructuring the response
    , DescribeWorkteamResponse (..)
    , mkDescribeWorkteamResponse
    -- ** Response lenses
    , dwrgrsWorkteam
    , dwrgrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeWorkteam' smart constructor.
newtype DescribeWorkteam = DescribeWorkteam'
  { workteamName :: Types.WorkteamName
    -- ^ The name of the work team to return a description of.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkteam' value with any optional fields omitted.
mkDescribeWorkteam
    :: Types.WorkteamName -- ^ 'workteamName'
    -> DescribeWorkteam
mkDescribeWorkteam workteamName = DescribeWorkteam'{workteamName}

-- | The name of the work team to return a description of.
--
-- /Note:/ Consider using 'workteamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dWorkteamName :: Lens.Lens' DescribeWorkteam Types.WorkteamName
dWorkteamName = Lens.field @"workteamName"
{-# INLINEABLE dWorkteamName #-}
{-# DEPRECATED workteamName "Use generic-lens or generic-optics with 'workteamName' instead"  #-}

instance Core.ToQuery DescribeWorkteam where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeWorkteam where
        toHeaders DescribeWorkteam{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeWorkteam") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeWorkteam where
        toJSON DescribeWorkteam{..}
          = Core.object
              (Core.catMaybes [Core.Just ("WorkteamName" Core..= workteamName)])

instance Core.AWSRequest DescribeWorkteam where
        type Rs DescribeWorkteam = DescribeWorkteamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeWorkteamResponse' Core.<$>
                   (x Core..: "Workteam") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeWorkteamResponse' smart constructor.
data DescribeWorkteamResponse = DescribeWorkteamResponse'
  { workteam :: Types.Workteam
    -- ^ A @Workteam@ instance that contains information about the work team. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeWorkteamResponse' value with any optional fields omitted.
mkDescribeWorkteamResponse
    :: Types.Workteam -- ^ 'workteam'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeWorkteamResponse
mkDescribeWorkteamResponse workteam responseStatus
  = DescribeWorkteamResponse'{workteam, responseStatus}

-- | A @Workteam@ instance that contains information about the work team. 
--
-- /Note:/ Consider using 'workteam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrgrsWorkteam :: Lens.Lens' DescribeWorkteamResponse Types.Workteam
dwrgrsWorkteam = Lens.field @"workteam"
{-# INLINEABLE dwrgrsWorkteam #-}
{-# DEPRECATED workteam "Use generic-lens or generic-optics with 'workteam' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwrgrsResponseStatus :: Lens.Lens' DescribeWorkteamResponse Core.Int
dwrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
