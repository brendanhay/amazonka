{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterRdsDbInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an Amazon RDS instance.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeregisterRdsDbInstance
    (
    -- * Creating a request
      DeregisterRdsDbInstance (..)
    , mkDeregisterRdsDbInstance
    -- ** Request lenses
    , drdiRdsDbInstanceArn

    -- * Destructuring the response
    , DeregisterRdsDbInstanceResponse (..)
    , mkDeregisterRdsDbInstanceResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterRdsDbInstance' smart constructor.
newtype DeregisterRdsDbInstance = DeregisterRdsDbInstance'
  { rdsDbInstanceArn :: Core.Text
    -- ^ The Amazon RDS instance's ARN.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterRdsDbInstance' value with any optional fields omitted.
mkDeregisterRdsDbInstance
    :: Core.Text -- ^ 'rdsDbInstanceArn'
    -> DeregisterRdsDbInstance
mkDeregisterRdsDbInstance rdsDbInstanceArn
  = DeregisterRdsDbInstance'{rdsDbInstanceArn}

-- | The Amazon RDS instance's ARN.
--
-- /Note:/ Consider using 'rdsDbInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdiRdsDbInstanceArn :: Lens.Lens' DeregisterRdsDbInstance Core.Text
drdiRdsDbInstanceArn = Lens.field @"rdsDbInstanceArn"
{-# INLINEABLE drdiRdsDbInstanceArn #-}
{-# DEPRECATED rdsDbInstanceArn "Use generic-lens or generic-optics with 'rdsDbInstanceArn' instead"  #-}

instance Core.ToQuery DeregisterRdsDbInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterRdsDbInstance where
        toHeaders DeregisterRdsDbInstance{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.DeregisterRdsDbInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterRdsDbInstance where
        toJSON DeregisterRdsDbInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RdsDbInstanceArn" Core..= rdsDbInstanceArn)])

instance Core.AWSRequest DeregisterRdsDbInstance where
        type Rs DeregisterRdsDbInstance = DeregisterRdsDbInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeregisterRdsDbInstanceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterRdsDbInstanceResponse' smart constructor.
data DeregisterRdsDbInstanceResponse = DeregisterRdsDbInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterRdsDbInstanceResponse' value with any optional fields omitted.
mkDeregisterRdsDbInstanceResponse
    :: DeregisterRdsDbInstanceResponse
mkDeregisterRdsDbInstanceResponse
  = DeregisterRdsDbInstanceResponse'
