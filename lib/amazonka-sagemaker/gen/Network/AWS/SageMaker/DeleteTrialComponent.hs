{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified trial component. A trial component must be disassociated from all trials before the trial component can be deleted. To disassociate a trial component from a trial, call the 'DisassociateTrialComponent' API.
module Network.AWS.SageMaker.DeleteTrialComponent
    (
    -- * Creating a request
      DeleteTrialComponent (..)
    , mkDeleteTrialComponent
    -- ** Request lenses
    , dTrialComponentName

    -- * Destructuring the response
    , DeleteTrialComponentResponse (..)
    , mkDeleteTrialComponentResponse
    -- ** Response lenses
    , dtcrfrsTrialComponentArn
    , dtcrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteTrialComponent' smart constructor.
newtype DeleteTrialComponent = DeleteTrialComponent'
  { trialComponentName :: Types.TrialComponentName
    -- ^ The name of the component to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrialComponent' value with any optional fields omitted.
mkDeleteTrialComponent
    :: Types.TrialComponentName -- ^ 'trialComponentName'
    -> DeleteTrialComponent
mkDeleteTrialComponent trialComponentName
  = DeleteTrialComponent'{trialComponentName}

-- | The name of the component to delete.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTrialComponentName :: Lens.Lens' DeleteTrialComponent Types.TrialComponentName
dTrialComponentName = Lens.field @"trialComponentName"
{-# INLINEABLE dTrialComponentName #-}
{-# DEPRECATED trialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead"  #-}

instance Core.ToQuery DeleteTrialComponent where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteTrialComponent where
        toHeaders DeleteTrialComponent{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteTrialComponent")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteTrialComponent where
        toJSON DeleteTrialComponent{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TrialComponentName" Core..= trialComponentName)])

instance Core.AWSRequest DeleteTrialComponent where
        type Rs DeleteTrialComponent = DeleteTrialComponentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteTrialComponentResponse' Core.<$>
                   (x Core..:? "TrialComponentArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteTrialComponentResponse' smart constructor.
data DeleteTrialComponentResponse = DeleteTrialComponentResponse'
  { trialComponentArn :: Core.Maybe Types.TrialComponentArn
    -- ^ The Amazon Resource Name (ARN) of the component is being deleted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteTrialComponentResponse' value with any optional fields omitted.
mkDeleteTrialComponentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteTrialComponentResponse
mkDeleteTrialComponentResponse responseStatus
  = DeleteTrialComponentResponse'{trialComponentArn = Core.Nothing,
                                  responseStatus}

-- | The Amazon Resource Name (ARN) of the component is being deleted.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrfrsTrialComponentArn :: Lens.Lens' DeleteTrialComponentResponse (Core.Maybe Types.TrialComponentArn)
dtcrfrsTrialComponentArn = Lens.field @"trialComponentArn"
{-# INLINEABLE dtcrfrsTrialComponentArn #-}
{-# DEPRECATED trialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcrfrsResponseStatus :: Lens.Lens' DeleteTrialComponentResponse Core.Int
dtcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dtcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
