{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.GetInstanceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests remote access to a fleet instance. Remote access is useful for debugging, gathering benchmarking data, or observing activity in real time. 
--
-- To remotely access an instance, you need credentials that match the operating system of the instance. For a Windows instance, Amazon GameLift returns a user name and password as strings for use with a Windows Remote Desktop client. For a Linux instance, Amazon GameLift returns a user name and RSA private key, also as strings, for use with an SSH client. The private key must be saved in the proper format to a @.pem@ file before using. If you're making this request using the AWS CLI, saving the secret can be handled as part of the GetInstanceAccess request, as shown in one of the examples for this operation. 
-- To request access to a specific instance, specify the IDs of both the instance and the fleet it belongs to. You can retrieve a fleet's instance IDs by calling 'DescribeInstances' . If successful, an 'InstanceAccess' object is returned that contains the instance's IP address and a set of credentials.
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html Remotely Access Fleet Instances> 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html Debug Fleet Issues> 
-- __Related operations__ 
--
--     * 'DescribeInstances' 
--
--
--     * 'GetInstanceAccess' 
--
--
module Network.AWS.GameLift.GetInstanceAccess
    (
    -- * Creating a request
      GetInstanceAccess (..)
    , mkGetInstanceAccess
    -- ** Request lenses
    , giaFleetId
    , giaInstanceId

    -- * Destructuring the response
    , GetInstanceAccessResponse (..)
    , mkGetInstanceAccessResponse
    -- ** Response lenses
    , giarrsInstanceAccess
    , giarrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkGetInstanceAccess' smart constructor.
data GetInstanceAccess = GetInstanceAccess'
  { fleetId :: Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet that contains the instance you want access to. You can use either the fleet ID or ARN value. The fleet can be in any of the following statuses: @ACTIVATING@ , @ACTIVE@ , or @ERROR@ . Fleets with an @ERROR@ status may be accessible for a short time before they are deleted.
  , instanceId :: Types.InstanceId
    -- ^ A unique identifier for an instance you want to get access to. You can access an instance in any status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceAccess' value with any optional fields omitted.
mkGetInstanceAccess
    :: Types.FleetIdOrArn -- ^ 'fleetId'
    -> Types.InstanceId -- ^ 'instanceId'
    -> GetInstanceAccess
mkGetInstanceAccess fleetId instanceId
  = GetInstanceAccess'{fleetId, instanceId}

-- | A unique identifier for a fleet that contains the instance you want access to. You can use either the fleet ID or ARN value. The fleet can be in any of the following statuses: @ACTIVATING@ , @ACTIVE@ , or @ERROR@ . Fleets with an @ERROR@ status may be accessible for a short time before they are deleted.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giaFleetId :: Lens.Lens' GetInstanceAccess Types.FleetIdOrArn
giaFleetId = Lens.field @"fleetId"
{-# INLINEABLE giaFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | A unique identifier for an instance you want to get access to. You can access an instance in any status.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giaInstanceId :: Lens.Lens' GetInstanceAccess Types.InstanceId
giaInstanceId = Lens.field @"instanceId"
{-# INLINEABLE giaInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery GetInstanceAccess where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetInstanceAccess where
        toHeaders GetInstanceAccess{..}
          = Core.pure ("X-Amz-Target", "GameLift.GetInstanceAccess") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetInstanceAccess where
        toJSON GetInstanceAccess{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FleetId" Core..= fleetId),
                  Core.Just ("InstanceId" Core..= instanceId)])

instance Core.AWSRequest GetInstanceAccess where
        type Rs GetInstanceAccess = GetInstanceAccessResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetInstanceAccessResponse' Core.<$>
                   (x Core..:? "InstanceAccess") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkGetInstanceAccessResponse' smart constructor.
data GetInstanceAccessResponse = GetInstanceAccessResponse'
  { instanceAccess :: Core.Maybe Types.InstanceAccess
    -- ^ The connection information for a fleet instance, including IP address and access credentials.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstanceAccessResponse' value with any optional fields omitted.
mkGetInstanceAccessResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetInstanceAccessResponse
mkGetInstanceAccessResponse responseStatus
  = GetInstanceAccessResponse'{instanceAccess = Core.Nothing,
                               responseStatus}

-- | The connection information for a fleet instance, including IP address and access credentials.
--
-- /Note:/ Consider using 'instanceAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giarrsInstanceAccess :: Lens.Lens' GetInstanceAccessResponse (Core.Maybe Types.InstanceAccess)
giarrsInstanceAccess = Lens.field @"instanceAccess"
{-# INLINEABLE giarrsInstanceAccess #-}
{-# DEPRECATED instanceAccess "Use generic-lens or generic-optics with 'instanceAccess' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giarrsResponseStatus :: Lens.Lens' GetInstanceAccessResponse Core.Int
giarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE giarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
