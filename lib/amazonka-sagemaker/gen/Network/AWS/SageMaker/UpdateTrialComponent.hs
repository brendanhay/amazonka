{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more properties of a trial component.
module Network.AWS.SageMaker.UpdateTrialComponent
  ( -- * Creating a request
    UpdateTrialComponent (..),
    mkUpdateTrialComponent,

    -- ** Request lenses
    utcTrialComponentName,
    utcDisplayName,
    utcEndTime,
    utcInputArtifacts,
    utcInputArtifactsToRemove,
    utcOutputArtifacts,
    utcOutputArtifactsToRemove,
    utcParameters,
    utcParametersToRemove,
    utcStartTime,
    utcStatus,

    -- * Destructuring the response
    UpdateTrialComponentResponse (..),
    mkUpdateTrialComponentResponse,

    -- ** Response lenses
    utcrrsTrialComponentArn,
    utcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateTrialComponent' smart constructor.
data UpdateTrialComponent = UpdateTrialComponent'
  { -- | The name of the component to update.
    trialComponentName :: Types.TrialComponentName,
    -- | The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
    displayName :: Core.Maybe Types.DisplayName,
    -- | When the component ended.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | Replaces all of the component's input artifacts with the specified artifacts.
    inputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact),
    -- | The input artifacts to remove from the component.
    inputArtifactsToRemove :: Core.Maybe [Types.TrialComponentKey256],
    -- | Replaces all of the component's output artifacts with the specified artifacts.
    outputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact),
    -- | The output artifacts to remove from the component.
    outputArtifactsToRemove :: Core.Maybe [Types.TrialComponentKey256],
    -- | Replaces all of the component's hyperparameters with the specified hyperparameters.
    parameters :: Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue),
    -- | The hyperparameters to remove from the component.
    parametersToRemove :: Core.Maybe [Types.TrialComponentKey256],
    -- | When the component started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The new status of the component.
    status :: Core.Maybe Types.TrialComponentStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateTrialComponent' value with any optional fields omitted.
mkUpdateTrialComponent ::
  -- | 'trialComponentName'
  Types.TrialComponentName ->
  UpdateTrialComponent
mkUpdateTrialComponent trialComponentName =
  UpdateTrialComponent'
    { trialComponentName,
      displayName = Core.Nothing,
      endTime = Core.Nothing,
      inputArtifacts = Core.Nothing,
      inputArtifactsToRemove = Core.Nothing,
      outputArtifacts = Core.Nothing,
      outputArtifactsToRemove = Core.Nothing,
      parameters = Core.Nothing,
      parametersToRemove = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing
    }

-- | The name of the component to update.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcTrialComponentName :: Lens.Lens' UpdateTrialComponent Types.TrialComponentName
utcTrialComponentName = Lens.field @"trialComponentName"
{-# DEPRECATED utcTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcDisplayName :: Lens.Lens' UpdateTrialComponent (Core.Maybe Types.DisplayName)
utcDisplayName = Lens.field @"displayName"
{-# DEPRECATED utcDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcEndTime :: Lens.Lens' UpdateTrialComponent (Core.Maybe Core.NominalDiffTime)
utcEndTime = Lens.field @"endTime"
{-# DEPRECATED utcEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Replaces all of the component's input artifacts with the specified artifacts.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcInputArtifacts :: Lens.Lens' UpdateTrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
utcInputArtifacts = Lens.field @"inputArtifacts"
{-# DEPRECATED utcInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The input artifacts to remove from the component.
--
-- /Note:/ Consider using 'inputArtifactsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcInputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Core.Maybe [Types.TrialComponentKey256])
utcInputArtifactsToRemove = Lens.field @"inputArtifactsToRemove"
{-# DEPRECATED utcInputArtifactsToRemove "Use generic-lens or generic-optics with 'inputArtifactsToRemove' instead." #-}

-- | Replaces all of the component's output artifacts with the specified artifacts.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcOutputArtifacts :: Lens.Lens' UpdateTrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
utcOutputArtifacts = Lens.field @"outputArtifacts"
{-# DEPRECATED utcOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | The output artifacts to remove from the component.
--
-- /Note:/ Consider using 'outputArtifactsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcOutputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Core.Maybe [Types.TrialComponentKey256])
utcOutputArtifactsToRemove = Lens.field @"outputArtifactsToRemove"
{-# DEPRECATED utcOutputArtifactsToRemove "Use generic-lens or generic-optics with 'outputArtifactsToRemove' instead." #-}

-- | Replaces all of the component's hyperparameters with the specified hyperparameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcParameters :: Lens.Lens' UpdateTrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue))
utcParameters = Lens.field @"parameters"
{-# DEPRECATED utcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The hyperparameters to remove from the component.
--
-- /Note:/ Consider using 'parametersToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcParametersToRemove :: Lens.Lens' UpdateTrialComponent (Core.Maybe [Types.TrialComponentKey256])
utcParametersToRemove = Lens.field @"parametersToRemove"
{-# DEPRECATED utcParametersToRemove "Use generic-lens or generic-optics with 'parametersToRemove' instead." #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcStartTime :: Lens.Lens' UpdateTrialComponent (Core.Maybe Core.NominalDiffTime)
utcStartTime = Lens.field @"startTime"
{-# DEPRECATED utcStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The new status of the component.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcStatus :: Lens.Lens' UpdateTrialComponent (Core.Maybe Types.TrialComponentStatus)
utcStatus = Lens.field @"status"
{-# DEPRECATED utcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON UpdateTrialComponent where
  toJSON UpdateTrialComponent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrialComponentName" Core..= trialComponentName),
            ("DisplayName" Core..=) Core.<$> displayName,
            ("EndTime" Core..=) Core.<$> endTime,
            ("InputArtifacts" Core..=) Core.<$> inputArtifacts,
            ("InputArtifactsToRemove" Core..=) Core.<$> inputArtifactsToRemove,
            ("OutputArtifacts" Core..=) Core.<$> outputArtifacts,
            ("OutputArtifactsToRemove" Core..=)
              Core.<$> outputArtifactsToRemove,
            ("Parameters" Core..=) Core.<$> parameters,
            ("ParametersToRemove" Core..=) Core.<$> parametersToRemove,
            ("StartTime" Core..=) Core.<$> startTime,
            ("Status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest UpdateTrialComponent where
  type Rs UpdateTrialComponent = UpdateTrialComponentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.UpdateTrialComponent")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrialComponentResponse'
            Core.<$> (x Core..:? "TrialComponentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateTrialComponentResponse' smart constructor.
data UpdateTrialComponentResponse = UpdateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Core.Maybe Types.TrialComponentArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTrialComponentResponse' value with any optional fields omitted.
mkUpdateTrialComponentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateTrialComponentResponse
mkUpdateTrialComponentResponse responseStatus =
  UpdateTrialComponentResponse'
    { trialComponentArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcrrsTrialComponentArn :: Lens.Lens' UpdateTrialComponentResponse (Core.Maybe Types.TrialComponentArn)
utcrrsTrialComponentArn = Lens.field @"trialComponentArn"
{-# DEPRECATED utcrrsTrialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcrrsResponseStatus :: Lens.Lens' UpdateTrialComponentResponse Core.Int
utcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED utcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
