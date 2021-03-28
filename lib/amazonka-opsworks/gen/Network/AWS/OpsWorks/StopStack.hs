{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.StopStack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a specified stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.StopStack
    (
    -- * Creating a request
      StopStack (..)
    , mkStopStack
    -- ** Request lenses
    , ssfStackId

    -- * Destructuring the response
    , StopStackResponse (..)
    , mkStopStackResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopStack' smart constructor.
newtype StopStack = StopStack'
  { stackId :: Core.Text
    -- ^ The stack ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopStack' value with any optional fields omitted.
mkStopStack
    :: Core.Text -- ^ 'stackId'
    -> StopStack
mkStopStack stackId = StopStack'{stackId}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssfStackId :: Lens.Lens' StopStack Core.Text
ssfStackId = Lens.field @"stackId"
{-# INLINEABLE ssfStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery StopStack where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopStack where
        toHeaders StopStack{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.StopStack") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopStack where
        toJSON StopStack{..}
          = Core.object
              (Core.catMaybes [Core.Just ("StackId" Core..= stackId)])

instance Core.AWSRequest StopStack where
        type Rs StopStack = StopStackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull StopStackResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopStackResponse' smart constructor.
data StopStackResponse = StopStackResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopStackResponse' value with any optional fields omitted.
mkStopStackResponse
    :: StopStackResponse
mkStopStackResponse = StopStackResponse'
