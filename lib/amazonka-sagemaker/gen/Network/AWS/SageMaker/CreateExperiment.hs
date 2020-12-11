{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cDisplayName,
    cDescription,
    cTags,
    cExperimentName,

    -- * Destructuring the response
    CreateExperimentResponse (..),
    mkCreateExperimentResponse,

    -- ** Response lenses
    crsExperimentARN,
    crsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateExperiment' smart constructor.
data CreateExperiment = CreateExperiment'
  { displayName ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    experimentName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateExperiment' with the minimum fields required to make a request.
--
-- * 'description' - The description of the experiment.
-- * 'displayName' - The name of the experiment as displayed. The name doesn't need to be unique. If you don't specify @DisplayName@ , the value in @ExperimentName@ is displayed.
-- * 'experimentName' - The name of the experiment. The name must be unique in your AWS account and is not case-sensitive.
-- * 'tags' - A list of tags to associate with the experiment. You can use 'Search' API to search on the tags.
mkCreateExperiment ::
  -- | 'experimentName'
  Lude.Text ->
  CreateExperiment
mkCreateExperiment pExperimentName_ =
  CreateExperiment'
    { displayName = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      experimentName = pExperimentName_
    }

-- | The name of the experiment as displayed. The name doesn't need to be unique. If you don't specify @DisplayName@ , the value in @ExperimentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDisplayName :: Lens.Lens' CreateExperiment (Lude.Maybe Lude.Text)
cDisplayName = Lens.lens (displayName :: CreateExperiment -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: CreateExperiment)
{-# DEPRECATED cDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The description of the experiment.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreateExperiment (Lude.Maybe Lude.Text)
cDescription = Lens.lens (description :: CreateExperiment -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateExperiment)
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to associate with the experiment. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateExperiment (Lude.Maybe [Tag])
cTags = Lens.lens (tags :: CreateExperiment -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateExperiment)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the experiment. The name must be unique in your AWS account and is not case-sensitive.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExperimentName :: Lens.Lens' CreateExperiment Lude.Text
cExperimentName = Lens.lens (experimentName :: CreateExperiment -> Lude.Text) (\s a -> s {experimentName = a} :: CreateExperiment)
{-# DEPRECATED cExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

instance Lude.AWSRequest CreateExperiment where
  type Rs CreateExperiment = CreateExperimentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateExperimentResponse'
            Lude.<$> (x Lude..?> "ExperimentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateExperiment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateExperiment" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateExperiment where
  toJSON CreateExperiment' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisplayName" Lude..=) Lude.<$> displayName,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ExperimentName" Lude..= experimentName)
          ]
      )

instance Lude.ToPath CreateExperiment where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateExperiment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateExperimentResponse' smart constructor.
data CreateExperimentResponse = CreateExperimentResponse'
  { experimentARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateExperimentResponse' with the minimum fields required to make a request.
--
-- * 'experimentARN' - The Amazon Resource Name (ARN) of the experiment.
-- * 'responseStatus' - The response status code.
mkCreateExperimentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateExperimentResponse
mkCreateExperimentResponse pResponseStatus_ =
  CreateExperimentResponse'
    { experimentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment.
--
-- /Note:/ Consider using 'experimentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsExperimentARN :: Lens.Lens' CreateExperimentResponse (Lude.Maybe Lude.Text)
crsExperimentARN = Lens.lens (experimentARN :: CreateExperimentResponse -> Lude.Maybe Lude.Text) (\s a -> s {experimentARN = a} :: CreateExperimentResponse)
{-# DEPRECATED crsExperimentARN "Use generic-lens or generic-optics with 'experimentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateExperimentResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateExperimentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateExperimentResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
