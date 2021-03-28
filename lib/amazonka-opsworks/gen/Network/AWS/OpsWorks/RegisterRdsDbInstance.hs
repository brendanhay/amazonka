{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterRdsDbInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Amazon RDS instance with a stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RegisterRdsDbInstance
    (
    -- * Creating a request
      RegisterRdsDbInstance (..)
    , mkRegisterRdsDbInstance
    -- ** Request lenses
    , rrdiStackId
    , rrdiRdsDbInstanceArn
    , rrdiDbUser
    , rrdiDbPassword

    -- * Destructuring the response
    , RegisterRdsDbInstanceResponse (..)
    , mkRegisterRdsDbInstanceResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterRdsDbInstance' smart constructor.
data RegisterRdsDbInstance = RegisterRdsDbInstance'
  { stackId :: Core.Text
    -- ^ The stack ID.
  , rdsDbInstanceArn :: Core.Text
    -- ^ The Amazon RDS instance's ARN.
  , dbUser :: Core.Text
    -- ^ The database's master user name.
  , dbPassword :: Core.Text
    -- ^ The database password.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterRdsDbInstance' value with any optional fields omitted.
mkRegisterRdsDbInstance
    :: Core.Text -- ^ 'stackId'
    -> Core.Text -- ^ 'rdsDbInstanceArn'
    -> Core.Text -- ^ 'dbUser'
    -> Core.Text -- ^ 'dbPassword'
    -> RegisterRdsDbInstance
mkRegisterRdsDbInstance stackId rdsDbInstanceArn dbUser dbPassword
  = RegisterRdsDbInstance'{stackId, rdsDbInstanceArn, dbUser,
                           dbPassword}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdiStackId :: Lens.Lens' RegisterRdsDbInstance Core.Text
rrdiStackId = Lens.field @"stackId"
{-# INLINEABLE rrdiStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The Amazon RDS instance's ARN.
--
-- /Note:/ Consider using 'rdsDbInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdiRdsDbInstanceArn :: Lens.Lens' RegisterRdsDbInstance Core.Text
rrdiRdsDbInstanceArn = Lens.field @"rdsDbInstanceArn"
{-# INLINEABLE rrdiRdsDbInstanceArn #-}
{-# DEPRECATED rdsDbInstanceArn "Use generic-lens or generic-optics with 'rdsDbInstanceArn' instead"  #-}

-- | The database's master user name.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdiDbUser :: Lens.Lens' RegisterRdsDbInstance Core.Text
rrdiDbUser = Lens.field @"dbUser"
{-# INLINEABLE rrdiDbUser #-}
{-# DEPRECATED dbUser "Use generic-lens or generic-optics with 'dbUser' instead"  #-}

-- | The database password.
--
-- /Note:/ Consider using 'dbPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrdiDbPassword :: Lens.Lens' RegisterRdsDbInstance Core.Text
rrdiDbPassword = Lens.field @"dbPassword"
{-# INLINEABLE rrdiDbPassword #-}
{-# DEPRECATED dbPassword "Use generic-lens or generic-optics with 'dbPassword' instead"  #-}

instance Core.ToQuery RegisterRdsDbInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterRdsDbInstance where
        toHeaders RegisterRdsDbInstance{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.RegisterRdsDbInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterRdsDbInstance where
        toJSON RegisterRdsDbInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StackId" Core..= stackId),
                  Core.Just ("RdsDbInstanceArn" Core..= rdsDbInstanceArn),
                  Core.Just ("DbUser" Core..= dbUser),
                  Core.Just ("DbPassword" Core..= dbPassword)])

instance Core.AWSRequest RegisterRdsDbInstance where
        type Rs RegisterRdsDbInstance = RegisterRdsDbInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull RegisterRdsDbInstanceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterRdsDbInstanceResponse' smart constructor.
data RegisterRdsDbInstanceResponse = RegisterRdsDbInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterRdsDbInstanceResponse' value with any optional fields omitted.
mkRegisterRdsDbInstanceResponse
    :: RegisterRdsDbInstanceResponse
mkRegisterRdsDbInstanceResponse = RegisterRdsDbInstanceResponse'
