{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified app.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeleteApp
    (
    -- * Creating a request
      DeleteApp (..)
    , mkDeleteApp
    -- ** Request lenses
    , daAppId

    -- * Destructuring the response
    , DeleteAppResponse (..)
    , mkDeleteAppResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApp' smart constructor.
newtype DeleteApp = DeleteApp'
  { appId :: Core.Text
    -- ^ The app ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApp' value with any optional fields omitted.
mkDeleteApp
    :: Core.Text -- ^ 'appId'
    -> DeleteApp
mkDeleteApp appId = DeleteApp'{appId}

-- | The app ID.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAppId :: Lens.Lens' DeleteApp Core.Text
daAppId = Lens.field @"appId"
{-# INLINEABLE daAppId #-}
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'appId' instead"  #-}

instance Core.ToQuery DeleteApp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApp where
        toHeaders DeleteApp{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.DeleteApp") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteApp where
        toJSON DeleteApp{..}
          = Core.object (Core.catMaybes [Core.Just ("AppId" Core..= appId)])

instance Core.AWSRequest DeleteApp where
        type Rs DeleteApp = DeleteAppResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteAppResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAppResponse' value with any optional fields omitted.
mkDeleteAppResponse
    :: DeleteAppResponse
mkDeleteAppResponse = DeleteAppResponse'
