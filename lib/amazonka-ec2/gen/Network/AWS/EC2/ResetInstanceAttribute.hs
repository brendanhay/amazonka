{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ResetInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets an attribute of an instance to its default value. To reset the @kernel@ or @ramdisk@ , the instance must be in a stopped state. To reset the @sourceDestCheck@ , the instance can be either running or stopped.
--
-- The @sourceDestCheck@ attribute controls whether source/destination checking is enabled. The default value is @true@ , which means checking is enabled. This value must be @false@ for a NAT instance to perform NAT. For more information, see <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.ResetInstanceAttribute
  ( -- * Creating a request
    ResetInstanceAttribute (..),
    mkResetInstanceAttribute,

    -- ** Request lenses
    riaAttribute,
    riaInstanceId,
    riaDryRun,

    -- * Destructuring the response
    ResetInstanceAttributeResponse (..),
    mkResetInstanceAttributeResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetInstanceAttribute' smart constructor.
data ResetInstanceAttribute = ResetInstanceAttribute'
  { -- | The attribute to reset.
    --
    -- /Important:/ You can only reset the following attributes: @kernel@ | @ramdisk@ | @sourceDestCheck@ . To change an instance attribute, use 'ModifyInstanceAttribute' .
    attribute :: Types.InstanceAttributeName,
    -- | The ID of the instance.
    instanceId :: Types.InstanceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetInstanceAttribute' value with any optional fields omitted.
mkResetInstanceAttribute ::
  -- | 'attribute'
  Types.InstanceAttributeName ->
  -- | 'instanceId'
  Types.InstanceId ->
  ResetInstanceAttribute
mkResetInstanceAttribute attribute instanceId =
  ResetInstanceAttribute'
    { attribute,
      instanceId,
      dryRun = Core.Nothing
    }

-- | The attribute to reset.
--
-- /Important:/ You can only reset the following attributes: @kernel@ | @ramdisk@ | @sourceDestCheck@ . To change an instance attribute, use 'ModifyInstanceAttribute' .
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaAttribute :: Lens.Lens' ResetInstanceAttribute Types.InstanceAttributeName
riaAttribute = Lens.field @"attribute"
{-# DEPRECATED riaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaInstanceId :: Lens.Lens' ResetInstanceAttribute Types.InstanceId
riaInstanceId = Lens.field @"instanceId"
{-# DEPRECATED riaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaDryRun :: Lens.Lens' ResetInstanceAttribute (Core.Maybe Core.Bool)
riaDryRun = Lens.field @"dryRun"
{-# DEPRECATED riaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest ResetInstanceAttribute where
  type Rs ResetInstanceAttribute = ResetInstanceAttributeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ResetInstanceAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Attribute" attribute)
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull ResetInstanceAttributeResponse'

-- | /See:/ 'mkResetInstanceAttributeResponse' smart constructor.
data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetInstanceAttributeResponse' value with any optional fields omitted.
mkResetInstanceAttributeResponse ::
  ResetInstanceAttributeResponse
mkResetInstanceAttributeResponse = ResetInstanceAttributeResponse'
