{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateTrial
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon SageMaker /trial/ . A trial is a set of steps called /trial components/ that produce a machine learning model. A trial is part of a single Amazon SageMaker /experiment/ .
--
-- When you use Amazon SageMaker Studio or the Amazon SageMaker Python SDK, all experiments, trials, and trial components are automatically tracked, logged, and indexed. When you use the AWS SDK for Python (Boto), you must use the logging APIs provided by the SDK.
-- You can add tags to a trial and then use the 'Search' API to search for the tags.
-- To get a list of all your trials, call the 'ListTrials' API. To view a trial's properties, call the 'DescribeTrial' API. To create a trial component, call the 'CreateTrialComponent' API.
module Network.AWS.SageMaker.CreateTrial
  ( -- * Creating a request
    CreateTrial (..),
    mkCreateTrial,

    -- ** Request lenses
    ctTrialName,
    ctExperimentName,
    ctDisplayName,
    ctTags,

    -- * Destructuring the response
    CreateTrialResponse (..),
    mkCreateTrialResponse,

    -- ** Response lenses
    ctrrsTrialArn,
    ctrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateTrial' smart constructor.
data CreateTrial = CreateTrial'
  { -- | The name of the trial. The name must be unique in your AWS account and is not case-sensitive.
    trialName :: Types.TrialName,
    -- | The name of the experiment to associate the trial with.
    experimentName :: Types.ExperimentName,
    -- | The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
    displayName :: Core.Maybe Types.DisplayName,
    -- | A list of tags to associate with the trial. You can use 'Search' API to search on the tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrial' value with any optional fields omitted.
mkCreateTrial ::
  -- | 'trialName'
  Types.TrialName ->
  -- | 'experimentName'
  Types.ExperimentName ->
  CreateTrial
mkCreateTrial trialName experimentName =
  CreateTrial'
    { trialName,
      experimentName,
      displayName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the trial. The name must be unique in your AWS account and is not case-sensitive.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTrialName :: Lens.Lens' CreateTrial Types.TrialName
ctTrialName = Lens.field @"trialName"
{-# DEPRECATED ctTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

-- | The name of the experiment to associate the trial with.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctExperimentName :: Lens.Lens' CreateTrial Types.ExperimentName
ctExperimentName = Lens.field @"experimentName"
{-# DEPRECATED ctExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDisplayName :: Lens.Lens' CreateTrial (Core.Maybe Types.DisplayName)
ctDisplayName = Lens.field @"displayName"
{-# DEPRECATED ctDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | A list of tags to associate with the trial. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTrial (Core.Maybe [Types.Tag])
ctTags = Lens.field @"tags"
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateTrial where
  toJSON CreateTrial {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrialName" Core..= trialName),
            Core.Just ("ExperimentName" Core..= experimentName),
            ("DisplayName" Core..=) Core.<$> displayName,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateTrial where
  type Rs CreateTrial = CreateTrialResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateTrial")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrialResponse'
            Core.<$> (x Core..:? "TrialArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTrialResponse' smart constructor.
data CreateTrialResponse = CreateTrialResponse'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Core.Maybe Types.TrialArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrialResponse' value with any optional fields omitted.
mkCreateTrialResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTrialResponse
mkCreateTrialResponse responseStatus =
  CreateTrialResponse' {trialArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsTrialArn :: Lens.Lens' CreateTrialResponse (Core.Maybe Types.TrialArn)
ctrrsTrialArn = Lens.field @"trialArn"
{-# DEPRECATED ctrrsTrialArn "Use generic-lens or generic-optics with 'trialArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTrialResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
