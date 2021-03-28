{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeStackProvisioningParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a stack's provisioning parameters.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeStackProvisioningParameters
    (
    -- * Creating a request
      DescribeStackProvisioningParameters (..)
    , mkDescribeStackProvisioningParameters
    -- ** Request lenses
    , dsppStackId

    -- * Destructuring the response
    , DescribeStackProvisioningParametersResponse (..)
    , mkDescribeStackProvisioningParametersResponse
    -- ** Response lenses
    , dspprrsAgentInstallerUrl
    , dspprrsParameters
    , dspprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStackProvisioningParameters' smart constructor.
newtype DescribeStackProvisioningParameters = DescribeStackProvisioningParameters'
  { stackId :: Core.Text
    -- ^ The stack ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackProvisioningParameters' value with any optional fields omitted.
mkDescribeStackProvisioningParameters
    :: Core.Text -- ^ 'stackId'
    -> DescribeStackProvisioningParameters
mkDescribeStackProvisioningParameters stackId
  = DescribeStackProvisioningParameters'{stackId}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsppStackId :: Lens.Lens' DescribeStackProvisioningParameters Core.Text
dsppStackId = Lens.field @"stackId"
{-# INLINEABLE dsppStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery DescribeStackProvisioningParameters where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeStackProvisioningParameters where
        toHeaders DescribeStackProvisioningParameters{..}
          = Core.pure
              ("X-Amz-Target",
               "OpsWorks_20130218.DescribeStackProvisioningParameters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeStackProvisioningParameters where
        toJSON DescribeStackProvisioningParameters{..}
          = Core.object
              (Core.catMaybes [Core.Just ("StackId" Core..= stackId)])

instance Core.AWSRequest DescribeStackProvisioningParameters where
        type Rs DescribeStackProvisioningParameters =
             DescribeStackProvisioningParametersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeStackProvisioningParametersResponse' Core.<$>
                   (x Core..:? "AgentInstallerUrl") Core.<*> x Core..:? "Parameters"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeStackProvisioningParameters@ request.
--
-- /See:/ 'mkDescribeStackProvisioningParametersResponse' smart constructor.
data DescribeStackProvisioningParametersResponse = DescribeStackProvisioningParametersResponse'
  { agentInstallerUrl :: Core.Maybe Core.Text
    -- ^ The AWS OpsWorks Stacks agent installer's URL.
  , parameters :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ An embedded object that contains the provisioning parameters.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackProvisioningParametersResponse' value with any optional fields omitted.
mkDescribeStackProvisioningParametersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeStackProvisioningParametersResponse
mkDescribeStackProvisioningParametersResponse responseStatus
  = DescribeStackProvisioningParametersResponse'{agentInstallerUrl =
                                                   Core.Nothing,
                                                 parameters = Core.Nothing, responseStatus}

-- | The AWS OpsWorks Stacks agent installer's URL.
--
-- /Note:/ Consider using 'agentInstallerUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspprrsAgentInstallerUrl :: Lens.Lens' DescribeStackProvisioningParametersResponse (Core.Maybe Core.Text)
dspprrsAgentInstallerUrl = Lens.field @"agentInstallerUrl"
{-# INLINEABLE dspprrsAgentInstallerUrl #-}
{-# DEPRECATED agentInstallerUrl "Use generic-lens or generic-optics with 'agentInstallerUrl' instead"  #-}

-- | An embedded object that contains the provisioning parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspprrsParameters :: Lens.Lens' DescribeStackProvisioningParametersResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
dspprrsParameters = Lens.field @"parameters"
{-# INLINEABLE dspprrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspprrsResponseStatus :: Lens.Lens' DescribeStackProvisioningParametersResponse Core.Int
dspprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dspprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
