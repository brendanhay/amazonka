{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RebootInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a specified instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html Starting, Stopping, and Rebooting Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RebootInstance
    (
    -- * Creating a request
      RebootInstance (..)
    , mkRebootInstance
    -- ** Request lenses
    , riInstanceId

    -- * Destructuring the response
    , RebootInstanceResponse (..)
    , mkRebootInstanceResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRebootInstance' smart constructor.
newtype RebootInstance = RebootInstance'
  { instanceId :: Core.Text
    -- ^ The instance ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebootInstance' value with any optional fields omitted.
mkRebootInstance
    :: Core.Text -- ^ 'instanceId'
    -> RebootInstance
mkRebootInstance instanceId = RebootInstance'{instanceId}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceId :: Lens.Lens' RebootInstance Core.Text
riInstanceId = Lens.field @"instanceId"
{-# INLINEABLE riInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery RebootInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RebootInstance where
        toHeaders RebootInstance{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.RebootInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RebootInstance where
        toJSON RebootInstance{..}
          = Core.object
              (Core.catMaybes [Core.Just ("InstanceId" Core..= instanceId)])

instance Core.AWSRequest RebootInstance where
        type Rs RebootInstance = RebootInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull RebootInstanceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRebootInstanceResponse' smart constructor.
data RebootInstanceResponse = RebootInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebootInstanceResponse' value with any optional fields omitted.
mkRebootInstanceResponse
    :: RebootInstanceResponse
mkRebootInstanceResponse = RebootInstanceResponse'
