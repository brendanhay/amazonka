{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ResetInstanceAttribute (..)
    , mkResetInstanceAttribute
    -- ** Request lenses
    , riaAttribute
    , riaInstanceId
    , riaDryRun

    -- * Destructuring the response
    , ResetInstanceAttributeResponse (..)
    , mkResetInstanceAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetInstanceAttribute' smart constructor.
data ResetInstanceAttribute = ResetInstanceAttribute'
  { attribute :: Types.InstanceAttributeName
    -- ^ The attribute to reset.
--
-- /Important:/ You can only reset the following attributes: @kernel@ | @ramdisk@ | @sourceDestCheck@ . To change an instance attribute, use 'ModifyInstanceAttribute' .
  , instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetInstanceAttribute' value with any optional fields omitted.
mkResetInstanceAttribute
    :: Types.InstanceAttributeName -- ^ 'attribute'
    -> Types.InstanceId -- ^ 'instanceId'
    -> ResetInstanceAttribute
mkResetInstanceAttribute attribute instanceId
  = ResetInstanceAttribute'{attribute, instanceId,
                            dryRun = Core.Nothing}

-- | The attribute to reset.
--
-- /Important:/ You can only reset the following attributes: @kernel@ | @ramdisk@ | @sourceDestCheck@ . To change an instance attribute, use 'ModifyInstanceAttribute' .
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaAttribute :: Lens.Lens' ResetInstanceAttribute Types.InstanceAttributeName
riaAttribute = Lens.field @"attribute"
{-# INLINEABLE riaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaInstanceId :: Lens.Lens' ResetInstanceAttribute Types.InstanceId
riaInstanceId = Lens.field @"instanceId"
{-# INLINEABLE riaInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riaDryRun :: Lens.Lens' ResetInstanceAttribute (Core.Maybe Core.Bool)
riaDryRun = Lens.field @"dryRun"
{-# INLINEABLE riaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ResetInstanceAttribute where
        toQuery ResetInstanceAttribute{..}
          = Core.toQueryPair "Action" ("ResetInstanceAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Attribute" attribute
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ResetInstanceAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ResetInstanceAttribute where
        type Rs ResetInstanceAttribute = ResetInstanceAttributeResponse
        toRequest x@Core.Request{..}
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
          = Response.receiveNull ResetInstanceAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetInstanceAttributeResponse' smart constructor.
data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResetInstanceAttributeResponse' value with any optional fields omitted.
mkResetInstanceAttributeResponse
    :: ResetInstanceAttributeResponse
mkResetInstanceAttributeResponse = ResetInstanceAttributeResponse'
