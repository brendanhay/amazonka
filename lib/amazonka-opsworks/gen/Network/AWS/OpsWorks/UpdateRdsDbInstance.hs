{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateRdsDbInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon RDS instance.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateRdsDbInstance
    (
    -- * Creating a request
      UpdateRdsDbInstance (..)
    , mkUpdateRdsDbInstance
    -- ** Request lenses
    , urdiRdsDbInstanceArn
    , urdiDbPassword
    , urdiDbUser

    -- * Destructuring the response
    , UpdateRdsDbInstanceResponse (..)
    , mkUpdateRdsDbInstanceResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRdsDbInstance' smart constructor.
data UpdateRdsDbInstance = UpdateRdsDbInstance'
  { rdsDbInstanceArn :: Core.Text
    -- ^ The Amazon RDS instance's ARN.
  , dbPassword :: Core.Maybe Core.Text
    -- ^ The database password.
  , dbUser :: Core.Maybe Core.Text
    -- ^ The master user name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRdsDbInstance' value with any optional fields omitted.
mkUpdateRdsDbInstance
    :: Core.Text -- ^ 'rdsDbInstanceArn'
    -> UpdateRdsDbInstance
mkUpdateRdsDbInstance rdsDbInstanceArn
  = UpdateRdsDbInstance'{rdsDbInstanceArn, dbPassword = Core.Nothing,
                         dbUser = Core.Nothing}

-- | The Amazon RDS instance's ARN.
--
-- /Note:/ Consider using 'rdsDbInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdiRdsDbInstanceArn :: Lens.Lens' UpdateRdsDbInstance Core.Text
urdiRdsDbInstanceArn = Lens.field @"rdsDbInstanceArn"
{-# INLINEABLE urdiRdsDbInstanceArn #-}
{-# DEPRECATED rdsDbInstanceArn "Use generic-lens or generic-optics with 'rdsDbInstanceArn' instead"  #-}

-- | The database password.
--
-- /Note:/ Consider using 'dbPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdiDbPassword :: Lens.Lens' UpdateRdsDbInstance (Core.Maybe Core.Text)
urdiDbPassword = Lens.field @"dbPassword"
{-# INLINEABLE urdiDbPassword #-}
{-# DEPRECATED dbPassword "Use generic-lens or generic-optics with 'dbPassword' instead"  #-}

-- | The master user name.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdiDbUser :: Lens.Lens' UpdateRdsDbInstance (Core.Maybe Core.Text)
urdiDbUser = Lens.field @"dbUser"
{-# INLINEABLE urdiDbUser #-}
{-# DEPRECATED dbUser "Use generic-lens or generic-optics with 'dbUser' instead"  #-}

instance Core.ToQuery UpdateRdsDbInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRdsDbInstance where
        toHeaders UpdateRdsDbInstance{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.UpdateRdsDbInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRdsDbInstance where
        toJSON UpdateRdsDbInstance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RdsDbInstanceArn" Core..= rdsDbInstanceArn),
                  ("DbPassword" Core..=) Core.<$> dbPassword,
                  ("DbUser" Core..=) Core.<$> dbUser])

instance Core.AWSRequest UpdateRdsDbInstance where
        type Rs UpdateRdsDbInstance = UpdateRdsDbInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateRdsDbInstanceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRdsDbInstanceResponse' smart constructor.
data UpdateRdsDbInstanceResponse = UpdateRdsDbInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRdsDbInstanceResponse' value with any optional fields omitted.
mkUpdateRdsDbInstanceResponse
    :: UpdateRdsDbInstanceResponse
mkUpdateRdsDbInstanceResponse = UpdateRdsDbInstanceResponse'
