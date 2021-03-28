{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeCommands
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the results of specified commands.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeCommands
    (
    -- * Creating a request
      DescribeCommands (..)
    , mkDescribeCommands
    -- ** Request lenses
    , dcCommandIds
    , dcDeploymentId
    , dcInstanceId

    -- * Destructuring the response
    , DescribeCommandsResponse (..)
    , mkDescribeCommandsResponse
    -- ** Response lenses
    , dcrrsCommands
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCommands' smart constructor.
data DescribeCommands = DescribeCommands'
  { commandIds :: Core.Maybe [Core.Text]
    -- ^ An array of command IDs. If you include this parameter, @DescribeCommands@ returns a description of the specified commands. Otherwise, it returns a description of every command.
  , deploymentId :: Core.Maybe Core.Text
    -- ^ The deployment ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified deployment.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The instance ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCommands' value with any optional fields omitted.
mkDescribeCommands
    :: DescribeCommands
mkDescribeCommands
  = DescribeCommands'{commandIds = Core.Nothing,
                      deploymentId = Core.Nothing, instanceId = Core.Nothing}

-- | An array of command IDs. If you include this parameter, @DescribeCommands@ returns a description of the specified commands. Otherwise, it returns a description of every command.
--
-- /Note:/ Consider using 'commandIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCommandIds :: Lens.Lens' DescribeCommands (Core.Maybe [Core.Text])
dcCommandIds = Lens.field @"commandIds"
{-# INLINEABLE dcCommandIds #-}
{-# DEPRECATED commandIds "Use generic-lens or generic-optics with 'commandIds' instead"  #-}

-- | The deployment ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified deployment.
--
-- /Note:/ Consider using 'deploymentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDeploymentId :: Lens.Lens' DescribeCommands (Core.Maybe Core.Text)
dcDeploymentId = Lens.field @"deploymentId"
{-# INLINEABLE dcDeploymentId #-}
{-# DEPRECATED deploymentId "Use generic-lens or generic-optics with 'deploymentId' instead"  #-}

-- | The instance ID. If you include this parameter, @DescribeCommands@ returns a description of the commands associated with the specified instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcInstanceId :: Lens.Lens' DescribeCommands (Core.Maybe Core.Text)
dcInstanceId = Lens.field @"instanceId"
{-# INLINEABLE dcInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery DescribeCommands where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCommands where
        toHeaders DescribeCommands{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeCommands")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCommands where
        toJSON DescribeCommands{..}
          = Core.object
              (Core.catMaybes
                 [("CommandIds" Core..=) Core.<$> commandIds,
                  ("DeploymentId" Core..=) Core.<$> deploymentId,
                  ("InstanceId" Core..=) Core.<$> instanceId])

instance Core.AWSRequest DescribeCommands where
        type Rs DescribeCommands = DescribeCommandsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCommandsResponse' Core.<$>
                   (x Core..:? "Commands") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeCommands@ request.
--
-- /See:/ 'mkDescribeCommandsResponse' smart constructor.
data DescribeCommandsResponse = DescribeCommandsResponse'
  { commands :: Core.Maybe [Types.Command]
    -- ^ An array of @Command@ objects that describe each of the specified commands.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCommandsResponse' value with any optional fields omitted.
mkDescribeCommandsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCommandsResponse
mkDescribeCommandsResponse responseStatus
  = DescribeCommandsResponse'{commands = Core.Nothing,
                              responseStatus}

-- | An array of @Command@ objects that describe each of the specified commands.
--
-- /Note:/ Consider using 'commands' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCommands :: Lens.Lens' DescribeCommandsResponse (Core.Maybe [Types.Command])
dcrrsCommands = Lens.field @"commands"
{-# INLINEABLE dcrrsCommands #-}
{-# DEPRECATED commands "Use generic-lens or generic-optics with 'commands' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeCommandsResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
