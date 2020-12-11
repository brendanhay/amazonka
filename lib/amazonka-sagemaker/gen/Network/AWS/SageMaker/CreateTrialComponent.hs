{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ctcStatus,
    ctcOutputArtifacts,
    ctcStartTime,
    ctcEndTime,
    ctcParameters,
    ctcDisplayName,
    ctcInputArtifacts,
    ctcTags,
    ctcTrialComponentName,

    -- * Destructuring the response
    CreateTrialComponentResponse (..),
    mkCreateTrialComponentResponse,

    -- ** Response lenses
    ctcrsTrialComponentARN,
    ctcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateTrialComponent' smart constructor.
data CreateTrialComponent = CreateTrialComponent'
  { status ::
      Lude.Maybe TrialComponentStatus,
    outputArtifacts ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (TrialComponentArtifact)
        ),
    startTime :: Lude.Maybe Lude.Timestamp,
    endTime :: Lude.Maybe Lude.Timestamp,
    parameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (TrialComponentParameterValue)
        ),
    displayName :: Lude.Maybe Lude.Text,
    inputArtifacts ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (TrialComponentArtifact)
        ),
    tags :: Lude.Maybe [Tag],
    trialComponentName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrialComponent' with the minimum fields required to make a request.
--
-- * 'displayName' - The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
-- * 'endTime' - When the component ended.
-- * 'inputArtifacts' - The input artifacts for the component. Examples of input artifacts are datasets, algorithms, hyperparameters, source code, and instance types.
-- * 'outputArtifacts' - The output artifacts for the component. Examples of output artifacts are metrics, snapshots, logs, and images.
-- * 'parameters' - The hyperparameters for the component.
-- * 'startTime' - When the component started.
-- * 'status' - The status of the component. States include:
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
-- * 'tags' - A list of tags to associate with the component. You can use 'Search' API to search on the tags.
-- * 'trialComponentName' - The name of the component. The name must be unique in your AWS account and is not case-sensitive.
mkCreateTrialComponent ::
  -- | 'trialComponentName'
  Lude.Text ->
  CreateTrialComponent
mkCreateTrialComponent pTrialComponentName_ =
  CreateTrialComponent'
    { status = Lude.Nothing,
      outputArtifacts = Lude.Nothing,
      startTime = Lude.Nothing,
      endTime = Lude.Nothing,
      parameters = Lude.Nothing,
      displayName = Lude.Nothing,
      inputArtifacts = Lude.Nothing,
      tags = Lude.Nothing,
      trialComponentName = pTrialComponentName_
    }

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
ctcStatus :: Lens.Lens' CreateTrialComponent (Lude.Maybe TrialComponentStatus)
ctcStatus = Lens.lens (status :: CreateTrialComponent -> Lude.Maybe TrialComponentStatus) (\s a -> s {status = a} :: CreateTrialComponent)
{-# DEPRECATED ctcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The output artifacts for the component. Examples of output artifacts are metrics, snapshots, logs, and images.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcOutputArtifacts :: Lens.Lens' CreateTrialComponent (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
ctcOutputArtifacts = Lens.lens (outputArtifacts :: CreateTrialComponent -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {outputArtifacts = a} :: CreateTrialComponent)
{-# DEPRECATED ctcOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcStartTime :: Lens.Lens' CreateTrialComponent (Lude.Maybe Lude.Timestamp)
ctcStartTime = Lens.lens (startTime :: CreateTrialComponent -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: CreateTrialComponent)
{-# DEPRECATED ctcStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcEndTime :: Lens.Lens' CreateTrialComponent (Lude.Maybe Lude.Timestamp)
ctcEndTime = Lens.lens (endTime :: CreateTrialComponent -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: CreateTrialComponent)
{-# DEPRECATED ctcEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The hyperparameters for the component.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcParameters :: Lens.Lens' CreateTrialComponent (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue)))
ctcParameters = Lens.lens (parameters :: CreateTrialComponent -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue))) (\s a -> s {parameters = a} :: CreateTrialComponent)
{-# DEPRECATED ctcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcDisplayName :: Lens.Lens' CreateTrialComponent (Lude.Maybe Lude.Text)
ctcDisplayName = Lens.lens (displayName :: CreateTrialComponent -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: CreateTrialComponent)
{-# DEPRECATED ctcDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The input artifacts for the component. Examples of input artifacts are datasets, algorithms, hyperparameters, source code, and instance types.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcInputArtifacts :: Lens.Lens' CreateTrialComponent (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
ctcInputArtifacts = Lens.lens (inputArtifacts :: CreateTrialComponent -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {inputArtifacts = a} :: CreateTrialComponent)
{-# DEPRECATED ctcInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | A list of tags to associate with the component. You can use 'Search' API to search on the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcTags :: Lens.Lens' CreateTrialComponent (Lude.Maybe [Tag])
ctcTags = Lens.lens (tags :: CreateTrialComponent -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTrialComponent)
{-# DEPRECATED ctcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the component. The name must be unique in your AWS account and is not case-sensitive.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcTrialComponentName :: Lens.Lens' CreateTrialComponent Lude.Text
ctcTrialComponentName = Lens.lens (trialComponentName :: CreateTrialComponent -> Lude.Text) (\s a -> s {trialComponentName = a} :: CreateTrialComponent)
{-# DEPRECATED ctcTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

instance Lude.AWSRequest CreateTrialComponent where
  type Rs CreateTrialComponent = CreateTrialComponentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTrialComponentResponse'
            Lude.<$> (x Lude..?> "TrialComponentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTrialComponent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateTrialComponent" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTrialComponent where
  toJSON CreateTrialComponent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("OutputArtifacts" Lude..=) Lude.<$> outputArtifacts,
            ("StartTime" Lude..=) Lude.<$> startTime,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("InputArtifacts" Lude..=) Lude.<$> inputArtifacts,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("TrialComponentName" Lude..= trialComponentName)
          ]
      )

instance Lude.ToPath CreateTrialComponent where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrialComponent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTrialComponentResponse' smart constructor.
data CreateTrialComponentResponse = CreateTrialComponentResponse'
  { trialComponentARN ::
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

-- | Creates a value of 'CreateTrialComponentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
mkCreateTrialComponentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrialComponentResponse
mkCreateTrialComponentResponse pResponseStatus_ =
  CreateTrialComponentResponse'
    { trialComponentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcrsTrialComponentARN :: Lens.Lens' CreateTrialComponentResponse (Lude.Maybe Lude.Text)
ctcrsTrialComponentARN = Lens.lens (trialComponentARN :: CreateTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: CreateTrialComponentResponse)
{-# DEPRECATED ctcrsTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctcrsResponseStatus :: Lens.Lens' CreateTrialComponentResponse Lude.Int
ctcrsResponseStatus = Lens.lens (responseStatus :: CreateTrialComponentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrialComponentResponse)
{-# DEPRECATED ctcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
