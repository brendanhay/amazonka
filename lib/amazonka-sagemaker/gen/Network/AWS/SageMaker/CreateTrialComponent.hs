{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a /trial component/ , which is a stage of a machine learning /trial/ . A trial is composed of one or more trial components. A trial component can be used in multiple trials.
--
-- Trial components include pre-processing jobs, training jobs, and batch transform jobs.
-- When you use Amazon SageMaker Studio or the Amazon SageMaker Python SDK, all experiments, trials, and trial components are automatically tracked, logged, and indexed. When you use the AWS SDK for Python (Boto), you must use the logging APIs provided by the SDK.
-- You can add tags to a trial component and then use the 'Search' API to search for the tags.
module Network.AWS.SageMaker.CreateTrialComponent
  ( -- * Creating a request
    CreateTrialComponent (..),
    mkCreateTrialComponent,

    -- ** Request lenses
    ctcTrialComponentName,
    ctcDisplayName,
    ctcEndTime,
    ctcInputArtifacts,
    ctcOutputArtifacts,
    ctcParameters,
    ctcStartTime,
    ctcStatus,
    ctcTags,

    -- * Destructuring the response
    CreateTrialComponentResponse (..),
    mkCreateTrialComponentResponse,

    -- ** Response lenses
    ctcrrsTrialComponentArn,
    ctcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateTrialComponent' smart constructor.
data CreateTrialComponent = CreateTrialComponent'
  { -- | The name of the component. The name must be unique in your AWS account and is not case-sensitive.
    trialComponentName :: Types.ExperimentEntityName,
    -- | The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
    displayName :: Core.Maybe Types.ExperimentEntityName,
    -- | When the component ended.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The input artifacts for the component. Examples of input artifacts are datasets, algorithms, hyperparameters, source code, and instance types.
    inputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact),
    -- | The output artifacts for the component. Examples of output artifacts are metrics, snapshots, logs, and images.
    outputArtifacts :: Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact),
    -- | The hyperparameters for the component.
    parameters :: Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue),
    -- | When the component started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the component. States include:
    --
    --
    --     * InProgress
    --
    --
    --     * Completed
    --
    --
    --     * Failed
    status :: Core.Maybe Types.TrialComponentStatus,
    -- | A list of tags to associate with the component. You can use 'Search' API to search on the tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTrialComponent' value with any optional fields omitted.
mkCreateTrialComponent ::
  -- | 'trialComponentName'
  Types.ExperimentEntityName ->
  CreateTrialComponent
mkCreateTrialComponent trialComponentName =
  CreateTrialComponent'
    { trialComponentName,
      displayName = Core.Nothing,
      endTime = Core.Nothing,
      inputArtifacts = Core.Nothing,
      outputArtifacts = Core.Nothing,
      parameters = Core.Nothing,
      startTime = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the component. The name must be unique in your AWS account and is not case-sensitive.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcTrialComponentName :: Lens.Lens' CreateTrialComponent Types.ExperimentEntityName
ctcTrialComponentName = Lens.field @"trialComponentName"
{-# DEPRECATED ctcTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcDisplayName :: Lens.Lens' CreateTrialComponent (Core.Maybe Types.ExperimentEntityName)
ctcDisplayName = Lens.field @"displayName"
{-# DEPRECATED ctcDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcEndTime :: Lens.Lens' CreateTrialComponent (Core.Maybe Core.NominalDiffTime)
ctcEndTime = Lens.field @"endTime"
{-# DEPRECATED ctcEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The input artifacts for the component. Examples of input artifacts are datasets, algorithms, hyperparameters, source code, and instance types.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcInputArtifacts :: Lens.Lens' CreateTrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
ctcInputArtifacts = Lens.field @"inputArtifacts"
{-# DEPRECATED ctcInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The output artifacts for the component. Examples of output artifacts are metrics, snapshots, logs, and images.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcOutputArtifacts :: Lens.Lens' CreateTrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey64 Types.TrialComponentArtifact))
ctcOutputArtifacts = Lens.field @"outputArtifacts"
{-# DEPRECATED ctcOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | The hyperparameters for the component.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcParameters :: Lens.Lens' CreateTrialComponent (Core.Maybe (Core.HashMap Types.TrialComponentKey256 Types.TrialComponentParameterValue))
ctcParameters = Lens.field @"parameters"
{-# DEPRECATED ctcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcStartTime :: Lens.Lens' CreateTrialComponent (Core.Maybe Core.NominalDiffTime)
ctcStartTime = Lens.field @"startTime"
{-# DEPRECATED ctcStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The status of the component. States include:
--
--
--     * InProgress
--
--
--     * Completed
--
--
--     * Failed
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcStatus :: Lens.Lens' CreateTrialComponent (Core.Maybe Types.TrialComponentStatus)
ctcStatus = Lens.field @"status"
{-# DEPRECATED ctcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A list of tags to associate with the component. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcTags :: Lens.Lens' CreateTrialComponent (Core.Maybe [Types.Tag])
ctcTags = Lens.field @"tags"
{-# DEPRECATED ctcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateTrialComponent where
  toJSON CreateTrialComponent {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrialComponentName" Core..= trialComponentName),
            ("DisplayName" Core..=) Core.<$> displayName,
            ("EndTime" Core..=) Core.<$> endTime,
            ("InputArtifacts" Core..=) Core.<$> inputArtifacts,
            ("OutputArtifacts" Core..=) Core.<$> outputArtifacts,
            ("Parameters" Core..=) Core.<$> parameters,
            ("StartTime" Core..=) Core.<$> startTime,
            ("Status" Core..=) Core.<$> status,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateTrialComponent where
  type Rs CreateTrialComponent = CreateTrialComponentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateTrialComponent")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrialComponentResponse'
            Core.<$> (x Core..:? "TrialComponentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTrialComponentResponse' smart constructor.
data CreateTrialComponentResponse = CreateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Core.Maybe Types.TrialComponentArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrialComponentResponse' value with any optional fields omitted.
mkCreateTrialComponentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTrialComponentResponse
mkCreateTrialComponentResponse responseStatus =
  CreateTrialComponentResponse'
    { trialComponentArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcrrsTrialComponentArn :: Lens.Lens' CreateTrialComponentResponse (Core.Maybe Types.TrialComponentArn)
ctcrrsTrialComponentArn = Lens.field @"trialComponentArn"
{-# DEPRECATED ctcrrsTrialComponentArn "Use generic-lens or generic-optics with 'trialComponentArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcrrsResponseStatus :: Lens.Lens' CreateTrialComponentResponse Core.Int
ctcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
