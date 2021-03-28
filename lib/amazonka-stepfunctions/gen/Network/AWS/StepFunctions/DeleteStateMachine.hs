{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DeleteStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a state machine. This is an asynchronous operation: It sets the state machine's status to @DELETING@ and begins the deletion process. 
module Network.AWS.StepFunctions.DeleteStateMachine
    (
    -- * Creating a request
      DeleteStateMachine (..)
    , mkDeleteStateMachine
    -- ** Request lenses
    , dStateMachineArn

    -- * Destructuring the response
    , DeleteStateMachineResponse (..)
    , mkDeleteStateMachineResponse
    -- ** Response lenses
    , dsmrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StepFunctions.Types as Types

-- | /See:/ 'mkDeleteStateMachine' smart constructor.
newtype DeleteStateMachine = DeleteStateMachine'
  { stateMachineArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the state machine to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStateMachine' value with any optional fields omitted.
mkDeleteStateMachine
    :: Types.Arn -- ^ 'stateMachineArn'
    -> DeleteStateMachine
mkDeleteStateMachine stateMachineArn
  = DeleteStateMachine'{stateMachineArn}

-- | The Amazon Resource Name (ARN) of the state machine to delete.
--
-- /Note:/ Consider using 'stateMachineArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStateMachineArn :: Lens.Lens' DeleteStateMachine Types.Arn
dStateMachineArn = Lens.field @"stateMachineArn"
{-# INLINEABLE dStateMachineArn #-}
{-# DEPRECATED stateMachineArn "Use generic-lens or generic-optics with 'stateMachineArn' instead"  #-}

instance Core.ToQuery DeleteStateMachine where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteStateMachine where
        toHeaders DeleteStateMachine{..}
          = Core.pure ("X-Amz-Target", "AWSStepFunctions.DeleteStateMachine")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON DeleteStateMachine where
        toJSON DeleteStateMachine{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("stateMachineArn" Core..= stateMachineArn)])

instance Core.AWSRequest DeleteStateMachine where
        type Rs DeleteStateMachine = DeleteStateMachineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteStateMachineResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteStateMachineResponse' smart constructor.
newtype DeleteStateMachineResponse = DeleteStateMachineResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStateMachineResponse' value with any optional fields omitted.
mkDeleteStateMachineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteStateMachineResponse
mkDeleteStateMachineResponse responseStatus
  = DeleteStateMachineResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrfrsResponseStatus :: Lens.Lens' DeleteStateMachineResponse Core.Int
dsmrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsmrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
