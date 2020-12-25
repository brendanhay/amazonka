{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateExperiment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SageMaker /experiment/ . An experiment is a collection of /trials/ that are observed, compared and evaluated as a group. A trial is a set of steps, called /trial components/ , that produce a machine learning model.
--
-- The goal of an experiment is to determine the components that produce the best model. Multiple trials are performed, each one isolating and measuring the impact of a change to one or more inputs, while keeping the remaining inputs constant.
-- When you use Amazon SageMaker Studio or the Amazon SageMaker Python SDK, all experiments, trials, and trial components are automatically tracked, logged, and indexed. When you use the AWS SDK for Python (Boto), you must use the logging APIs provided by the SDK.
-- You can add tags to experiments, trials, trial components and then use the 'Search' API to search for the tags.
-- To add a description to an experiment, specify the optional @Description@ parameter. To add a description later, or to change the description, call the 'UpdateExperiment' API.
-- To get a list of all your experiments, call the 'ListExperiments' API. To view an experiment's properties, call the 'DescribeExperiment' API. To get a list of all the trials associated with an experiment, call the 'ListTrials' API. To create a trial call the 'CreateTrial' API.
module Network.AWS.SageMaker.CreateExperiment
  ( -- * Creating a request
    CreateExperiment (..),
    mkCreateExperiment,

    -- ** Request lenses
    cExperimentName,
    cDescription,
    cDisplayName,
    cTags,

    -- * Destructuring the response
    CreateExperimentResponse (..),
    mkCreateExperimentResponse,

    -- ** Response lenses
    cerrsExperimentArn,
    cerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateExperiment' smart constructor.
data CreateExperiment = CreateExperiment'
  { -- | The name of the experiment. The name must be unique in your AWS account and is not case-sensitive.
    experimentName :: Types.ExperimentEntityName,
    -- | The description of the experiment.
    description :: Core.Maybe Types.Description,
    -- | The name of the experiment as displayed. The name doesn't need to be unique. If you don't specify @DisplayName@ , the value in @ExperimentName@ is displayed.
    displayName :: Core.Maybe Types.ExperimentEntityName,
    -- | A list of tags to associate with the experiment. You can use 'Search' API to search on the tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExperiment' value with any optional fields omitted.
mkCreateExperiment ::
  -- | 'experimentName'
  Types.ExperimentEntityName ->
  CreateExperiment
mkCreateExperiment experimentName =
  CreateExperiment'
    { experimentName,
      description = Core.Nothing,
      displayName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the experiment. The name must be unique in your AWS account and is not case-sensitive.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExperimentName :: Lens.Lens' CreateExperiment Types.ExperimentEntityName
cExperimentName = Lens.field @"experimentName"
{-# DEPRECATED cExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The description of the experiment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreateExperiment (Core.Maybe Types.Description)
cDescription = Lens.field @"description"
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the experiment as displayed. The name doesn't need to be unique. If you don't specify @DisplayName@ , the value in @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDisplayName :: Lens.Lens' CreateExperiment (Core.Maybe Types.ExperimentEntityName)
cDisplayName = Lens.field @"displayName"
{-# DEPRECATED cDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | A list of tags to associate with the experiment. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateExperiment (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateExperiment where
  toJSON CreateExperiment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ExperimentName" Core..= experimentName),
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateExperiment where
  type Rs CreateExperiment = CreateExperimentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateExperiment")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExperimentResponse'
            Core.<$> (x Core..:? "ExperimentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateExperimentResponse' smart constructor.
data CreateExperimentResponse = CreateExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Core.Maybe Types.ExperimentArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExperimentResponse' value with any optional fields omitted.
mkCreateExperimentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateExperimentResponse
mkCreateExperimentResponse responseStatus =
  CreateExperimentResponse'
    { experimentArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsExperimentArn :: Lens.Lens' CreateExperimentResponse (Core.Maybe Types.ExperimentArn)
cerrsExperimentArn = Lens.field @"experimentArn"
{-# DEPRECATED cerrsExperimentArn "Use generic-lens or generic-optics with 'experimentArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerrsResponseStatus :: Lens.Lens' CreateExperimentResponse Core.Int
cerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
