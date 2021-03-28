{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.StartStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a stack's instances.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.StartStack
    (
    -- * Creating a request
      StartStack (..)
    , mkStartStack
    -- ** Request lenses
    , ssgStackId

    -- * Destructuring the response
    , StartStackResponse (..)
    , mkStartStackResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartStack' smart constructor.
newtype StartStack = StartStack'
  { stackId :: Core.Text
    -- ^ The stack ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartStack' value with any optional fields omitted.
mkStartStack
    :: Core.Text -- ^ 'stackId'
    -> StartStack
mkStartStack stackId = StartStack'{stackId}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgStackId :: Lens.Lens' StartStack Core.Text
ssgStackId = Lens.field @"stackId"
{-# INLINEABLE ssgStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery StartStack where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartStack where
        toHeaders StartStack{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.StartStack")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartStack where
        toJSON StartStack{..}
          = Core.object
              (Core.catMaybes [Core.Just ("StackId" Core..= stackId)])

instance Core.AWSRequest StartStack where
        type Rs StartStack = StartStackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull StartStackResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartStackResponse' smart constructor.
data StartStackResponse = StartStackResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartStackResponse' value with any optional fields omitted.
mkStartStackResponse
    :: StartStackResponse
mkStartStackResponse = StartStackResponse'
