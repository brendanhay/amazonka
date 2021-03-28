{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of apps.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeApps
    (
    -- * Creating a request
      DescribeApps (..)
    , mkDescribeApps
    -- ** Request lenses
    , daAppIds
    , daStackId

    -- * Destructuring the response
    , DescribeAppsResponse (..)
    , mkDescribeAppsResponse
    -- ** Response lenses
    , darrsApps
    , darrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeApps' smart constructor.
data DescribeApps = DescribeApps'
  { appIds :: Core.Maybe [Core.Text]
    -- ^ An array of app IDs for the apps to be described. If you use this parameter, @DescribeApps@ returns a description of the specified apps. Otherwise, it returns a description of every app.
  , stackId :: Core.Maybe Core.Text
    -- ^ The app stack ID. If you use this parameter, @DescribeApps@ returns a description of the apps in the specified stack.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeApps' value with any optional fields omitted.
mkDescribeApps
    :: DescribeApps
mkDescribeApps
  = DescribeApps'{appIds = Core.Nothing, stackId = Core.Nothing}

-- | An array of app IDs for the apps to be described. If you use this parameter, @DescribeApps@ returns a description of the specified apps. Otherwise, it returns a description of every app.
--
-- /Note:/ Consider using 'appIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppIds :: Lens.Lens' DescribeApps (Core.Maybe [Core.Text])
daAppIds = Lens.field @"appIds"
{-# INLINEABLE daAppIds #-}
{-# DEPRECATED appIds "Use generic-lens or generic-optics with 'appIds' instead"  #-}

-- | The app stack ID. If you use this parameter, @DescribeApps@ returns a description of the apps in the specified stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daStackId :: Lens.Lens' DescribeApps (Core.Maybe Core.Text)
daStackId = Lens.field @"stackId"
{-# INLINEABLE daStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery DescribeApps where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeApps where
        toHeaders DescribeApps{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeApps")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeApps where
        toJSON DescribeApps{..}
          = Core.object
              (Core.catMaybes
                 [("AppIds" Core..=) Core.<$> appIds,
                  ("StackId" Core..=) Core.<$> stackId])

instance Core.AWSRequest DescribeApps where
        type Rs DescribeApps = DescribeAppsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAppsResponse' Core.<$>
                   (x Core..:? "Apps") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeApps@ request.
--
-- /See:/ 'mkDescribeAppsResponse' smart constructor.
data DescribeAppsResponse = DescribeAppsResponse'
  { apps :: Core.Maybe [Types.App]
    -- ^ An array of @App@ objects that describe the specified apps. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAppsResponse' value with any optional fields omitted.
mkDescribeAppsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAppsResponse
mkDescribeAppsResponse responseStatus
  = DescribeAppsResponse'{apps = Core.Nothing, responseStatus}

-- | An array of @App@ objects that describe the specified apps. 
--
-- /Note:/ Consider using 'apps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsApps :: Lens.Lens' DescribeAppsResponse (Core.Maybe [Types.App])
darrsApps = Lens.field @"apps"
{-# INLINEABLE darrsApps #-}
{-# DEPRECATED apps "Use generic-lens or generic-optics with 'apps' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAppsResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
