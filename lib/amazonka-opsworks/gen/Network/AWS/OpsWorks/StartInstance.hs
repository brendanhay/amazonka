{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.StartInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specified instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.StartInstance
    (
    -- * Creating a request
      StartInstance (..)
    , mkStartInstance
    -- ** Request lenses
    , sInstanceId

    -- * Destructuring the response
    , StartInstanceResponse (..)
    , mkStartInstanceResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartInstance' smart constructor.
newtype StartInstance = StartInstance'
  { instanceId :: Core.Text
    -- ^ The instance ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartInstance' value with any optional fields omitted.
mkStartInstance
    :: Core.Text -- ^ 'instanceId'
    -> StartInstance
mkStartInstance instanceId = StartInstance'{instanceId}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstanceId :: Lens.Lens' StartInstance Core.Text
sInstanceId = Lens.field @"instanceId"
{-# INLINEABLE sInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery StartInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartInstance where
        toHeaders StartInstance{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.StartInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartInstance where
        toJSON StartInstance{..}
          = Core.object
              (Core.catMaybes [Core.Just ("InstanceId" Core..= instanceId)])

instance Core.AWSRequest StartInstance where
        type Rs StartInstance = StartInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull StartInstanceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartInstanceResponse' smart constructor.
data StartInstanceResponse = StartInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartInstanceResponse' value with any optional fields omitted.
mkStartInstanceResponse
    :: StartInstanceResponse
mkStartInstanceResponse = StartInstanceResponse'
