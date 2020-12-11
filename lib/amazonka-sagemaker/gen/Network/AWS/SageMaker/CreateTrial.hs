{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ctDisplayName,
    ctTags,
    ctTrialName,
    ctExperimentName,

    -- * Destructuring the response
    CreateTrialResponse (..),
    mkCreateTrialResponse,

    -- ** Response lenses
    ctrsTrialARN,
    ctrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateTrial' smart constructor.
data CreateTrial = CreateTrial'
  { displayName ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    trialName :: Lude.Text,
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

-- | Creates a value of 'CreateTrial' with the minimum fields required to make a request.
--
-- * 'displayName' - The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
-- * 'experimentName' - The name of the experiment to associate the trial with.
-- * 'tags' - A list of tags to associate with the trial. You can use 'Search' API to search on the tags.
-- * 'trialName' - The name of the trial. The name must be unique in your AWS account and is not case-sensitive.
mkCreateTrial ::
  -- | 'trialName'
  Lude.Text ->
  -- | 'experimentName'
  Lude.Text ->
  CreateTrial
mkCreateTrial pTrialName_ pExperimentName_ =
  CreateTrial'
    { displayName = Lude.Nothing,
      tags = Lude.Nothing,
      trialName = pTrialName_,
      experimentName = pExperimentName_
    }

-- | The name of the trial as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDisplayName :: Lens.Lens' CreateTrial (Lude.Maybe Lude.Text)
ctDisplayName = Lens.lens (displayName :: CreateTrial -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: CreateTrial)
{-# DEPRECATED ctDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | A list of tags to associate with the trial. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTrial (Lude.Maybe [Tag])
ctTags = Lens.lens (tags :: CreateTrial -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTrial)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the trial. The name must be unique in your AWS account and is not case-sensitive.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTrialName :: Lens.Lens' CreateTrial Lude.Text
ctTrialName = Lens.lens (trialName :: CreateTrial -> Lude.Text) (\s a -> s {trialName = a} :: CreateTrial)
{-# DEPRECATED ctTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

-- | The name of the experiment to associate the trial with.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctExperimentName :: Lens.Lens' CreateTrial Lude.Text
ctExperimentName = Lens.lens (experimentName :: CreateTrial -> Lude.Text) (\s a -> s {experimentName = a} :: CreateTrial)
{-# DEPRECATED ctExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

instance Lude.AWSRequest CreateTrial where
  type Rs CreateTrial = CreateTrialResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTrialResponse'
            Lude.<$> (x Lude..?> "TrialArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTrial where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateTrial" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTrial where
  toJSON CreateTrial' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisplayName" Lude..=) Lude.<$> displayName,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("TrialName" Lude..= trialName),
            Lude.Just ("ExperimentName" Lude..= experimentName)
          ]
      )

instance Lude.ToPath CreateTrial where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrial where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTrialResponse' smart constructor.
data CreateTrialResponse = CreateTrialResponse'
  { trialARN ::
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

-- | Creates a value of 'CreateTrialResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trialARN' - The Amazon Resource Name (ARN) of the trial.
mkCreateTrialResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrialResponse
mkCreateTrialResponse pResponseStatus_ =
  CreateTrialResponse'
    { trialARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial.
--
-- /Note:/ Consider using 'trialARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsTrialARN :: Lens.Lens' CreateTrialResponse (Lude.Maybe Lude.Text)
ctrsTrialARN = Lens.lens (trialARN :: CreateTrialResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialARN = a} :: CreateTrialResponse)
{-# DEPRECATED ctrsTrialARN "Use generic-lens or generic-optics with 'trialARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTrialResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTrialResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrialResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
