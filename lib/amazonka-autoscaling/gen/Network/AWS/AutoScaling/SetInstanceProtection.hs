{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetInstanceProtection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the instance protection settings of the specified instances.
--
-- For more information about preventing instances that are part of an Auto Scaling group from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance scale-in protection> in the /Amazon EC2 Auto Scaling User Guide/ .
-- If you exceed your maximum limit of instance IDs, which is 50 per Auto Scaling group, the call fails.
module Network.AWS.AutoScaling.SetInstanceProtection
    (
    -- * Creating a request
      SetInstanceProtection (..)
    , mkSetInstanceProtection
    -- ** Request lenses
    , sipInstanceIds
    , sipAutoScalingGroupName
    , sipProtectedFromScaleIn

    -- * Destructuring the response
    , SetInstanceProtectionResponse (..)
    , mkSetInstanceProtectionResponse
    -- ** Response lenses
    , siprrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetInstanceProtection' smart constructor.
data SetInstanceProtection = SetInstanceProtection'
  { instanceIds :: [Types.XmlStringMaxLen19]
    -- ^ One or more instance IDs. You can specify up to 50 instances.
  , autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , protectedFromScaleIn :: Core.Bool
    -- ^ Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetInstanceProtection' value with any optional fields omitted.
mkSetInstanceProtection
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> Core.Bool -- ^ 'protectedFromScaleIn'
    -> SetInstanceProtection
mkSetInstanceProtection autoScalingGroupName protectedFromScaleIn
  = SetInstanceProtection'{instanceIds = Core.mempty,
                           autoScalingGroupName, protectedFromScaleIn}

-- | One or more instance IDs. You can specify up to 50 instances.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipInstanceIds :: Lens.Lens' SetInstanceProtection [Types.XmlStringMaxLen19]
sipInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE sipInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipAutoScalingGroupName :: Lens.Lens' SetInstanceProtection Types.AutoScalingGroupName
sipAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE sipAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
--
-- /Note:/ Consider using 'protectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipProtectedFromScaleIn :: Lens.Lens' SetInstanceProtection Core.Bool
sipProtectedFromScaleIn = Lens.field @"protectedFromScaleIn"
{-# INLINEABLE sipProtectedFromScaleIn #-}
{-# DEPRECATED protectedFromScaleIn "Use generic-lens or generic-optics with 'protectedFromScaleIn' instead"  #-}

instance Core.ToQuery SetInstanceProtection where
        toQuery SetInstanceProtection{..}
          = Core.toQueryPair "Action" ("SetInstanceProtection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "InstanceIds"
                (Core.toQueryList "member" instanceIds)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "ProtectedFromScaleIn" protectedFromScaleIn

instance Core.ToHeaders SetInstanceProtection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetInstanceProtection where
        type Rs SetInstanceProtection = SetInstanceProtectionResponse
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
          = Response.receiveXMLWrapper "SetInstanceProtectionResult"
              (\ s h x ->
                 SetInstanceProtectionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetInstanceProtectionResponse' smart constructor.
newtype SetInstanceProtectionResponse = SetInstanceProtectionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetInstanceProtectionResponse' value with any optional fields omitted.
mkSetInstanceProtectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetInstanceProtectionResponse
mkSetInstanceProtectionResponse responseStatus
  = SetInstanceProtectionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siprrsResponseStatus :: Lens.Lens' SetInstanceProtectionResponse Core.Int
siprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE siprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
