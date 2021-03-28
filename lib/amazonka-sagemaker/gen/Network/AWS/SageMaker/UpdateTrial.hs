{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the display name of a trial.
module Network.AWS.SageMaker.UpdateTrial
    (
    -- * Creating a request
      UpdateTrial (..)
    , mkUpdateTrial
    -- ** Request lenses
    , utTrialName
    , utDisplayName

    -- * Destructuring the response
    , UpdateTrialResponse (..)
    , mkUpdateTrialResponse
    -- ** Response lenses
    , utrrsTrialArn
    , utrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateTrial' smart constructor.
data UpdateTrial = UpdateTrial'
  { trialName :: Types.TrialName
    -- ^ The name of the trial to update.
  , displayName :: Core.Maybe Types.DisplayName
    -- ^ The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrial' value with any optional fields omitted.
mkUpdateTrial
    :: Types.TrialName -- ^ 'trialName'
    -> UpdateTrial
mkUpdateTrial trialName
  = UpdateTrial'{trialName, displayName = Core.Nothing}

-- | The name of the trial to update.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTrialName :: Lens.Lens' UpdateTrial Types.TrialName
utTrialName = Lens.field @"trialName"
{-# INLINEABLE utTrialName #-}
{-# DEPRECATED trialName "Use generic-lens or generic-optics with 'trialName' instead"  #-}

-- | The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDisplayName :: Lens.Lens' UpdateTrial (Core.Maybe Types.DisplayName)
utDisplayName = Lens.field @"displayName"
{-# INLINEABLE utDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

instance Core.ToQuery UpdateTrial where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTrial where
        toHeaders UpdateTrial{..}
          = Core.pure ("X-Amz-Target", "SageMaker.UpdateTrial") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateTrial where
        toJSON UpdateTrial{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TrialName" Core..= trialName),
                  ("DisplayName" Core..=) Core.<$> displayName])

instance Core.AWSRequest UpdateTrial where
        type Rs UpdateTrial = UpdateTrialResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateTrialResponse' Core.<$>
                   (x Core..:? "TrialArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTrialResponse' smart constructor.
data UpdateTrialResponse = UpdateTrialResponse'
  { trialArn :: Core.Maybe Types.TrialArn
    -- ^ The Amazon Resource Name (ARN) of the trial.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrialResponse' value with any optional fields omitted.
mkUpdateTrialResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTrialResponse
mkUpdateTrialResponse responseStatus
  = UpdateTrialResponse'{trialArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsTrialArn :: Lens.Lens' UpdateTrialResponse (Core.Maybe Types.TrialArn)
utrrsTrialArn = Lens.field @"trialArn"
{-# INLINEABLE utrrsTrialArn #-}
{-# DEPRECATED trialArn "Use generic-lens or generic-optics with 'trialArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrrsResponseStatus :: Lens.Lens' UpdateTrialResponse Core.Int
utrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
