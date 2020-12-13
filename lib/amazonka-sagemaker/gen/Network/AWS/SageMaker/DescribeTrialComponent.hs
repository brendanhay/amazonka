{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeTrialComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trials component's properties.
module Network.AWS.SageMaker.DescribeTrialComponent
  ( -- * Creating a request
    DescribeTrialComponent (..),
    mkDescribeTrialComponent,

    -- ** Request lenses
    dTrialComponentName,

    -- * Destructuring the response
    DescribeTrialComponentResponse (..),
    mkDescribeTrialComponentResponse,

    -- ** Response lenses
    dtcgrsCreationTime,
    dtcgrsStatus,
    dtcgrsMetrics,
    dtcgrsOutputArtifacts,
    dtcgrsStartTime,
    dtcgrsCreatedBy,
    dtcgrsLastModifiedTime,
    dtcgrsEndTime,
    dtcgrsTrialComponentName,
    dtcgrsParameters,
    dtcgrsSource,
    dtcgrsDisplayName,
    dtcgrsLastModifiedBy,
    dtcgrsTrialComponentARN,
    dtcgrsInputArtifacts,
    dtcgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeTrialComponent' smart constructor.
newtype DescribeTrialComponent = DescribeTrialComponent'
  { -- | The name of the trial component to describe.
    trialComponentName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrialComponent' with the minimum fields required to make a request.
--
-- * 'trialComponentName' - The name of the trial component to describe.
mkDescribeTrialComponent ::
  -- | 'trialComponentName'
  Lude.Text ->
  DescribeTrialComponent
mkDescribeTrialComponent pTrialComponentName_ =
  DescribeTrialComponent'
    { trialComponentName =
        pTrialComponentName_
    }

-- | The name of the trial component to describe.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTrialComponentName :: Lens.Lens' DescribeTrialComponent Lude.Text
dTrialComponentName = Lens.lens (trialComponentName :: DescribeTrialComponent -> Lude.Text) (\s a -> s {trialComponentName = a} :: DescribeTrialComponent)
{-# DEPRECATED dTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

instance Lude.AWSRequest DescribeTrialComponent where
  type Rs DescribeTrialComponent = DescribeTrialComponentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTrialComponentResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Metrics" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "OutputArtifacts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "StartTime")
            Lude.<*> (x Lude..?> "CreatedBy")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "EndTime")
            Lude.<*> (x Lude..?> "TrialComponentName")
            Lude.<*> (x Lude..?> "Parameters" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Source")
            Lude.<*> (x Lude..?> "DisplayName")
            Lude.<*> (x Lude..?> "LastModifiedBy")
            Lude.<*> (x Lude..?> "TrialComponentArn")
            Lude.<*> (x Lude..?> "InputArtifacts" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrialComponent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeTrialComponent" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTrialComponent where
  toJSON DescribeTrialComponent' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("TrialComponentName" Lude..= trialComponentName)]
      )

instance Lude.ToPath DescribeTrialComponent where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrialComponent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTrialComponentResponse' smart constructor.
data DescribeTrialComponentResponse = DescribeTrialComponentResponse'
  { -- | When the component was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
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
    status :: Lude.Maybe TrialComponentStatus,
    -- | The metrics for the component.
    metrics :: Lude.Maybe [TrialComponentMetricSummary],
    -- | The output artifacts of the component.
    outputArtifacts :: Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)),
    -- | When the component started.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | Who created the component.
    createdBy :: Lude.Maybe UserContext,
    -- | When the component was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | When the component ended.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the trial component.
    trialComponentName :: Lude.Maybe Lude.Text,
    -- | The hyperparameters of the component.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue)),
    -- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
    source :: Lude.Maybe TrialComponentSource,
    -- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
    displayName :: Lude.Maybe Lude.Text,
    -- | Who last modified the component.
    lastModifiedBy :: Lude.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentARN :: Lude.Maybe Lude.Text,
    -- | The input artifacts of the component.
    inputArtifacts :: Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrialComponentResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the component was created.
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
-- * 'metrics' - The metrics for the component.
-- * 'outputArtifacts' - The output artifacts of the component.
-- * 'startTime' - When the component started.
-- * 'createdBy' - Who created the component.
-- * 'lastModifiedTime' - When the component was last modified.
-- * 'endTime' - When the component ended.
-- * 'trialComponentName' - The name of the trial component.
-- * 'parameters' - The hyperparameters of the component.
-- * 'source' - The Amazon Resource Name (ARN) of the source and, optionally, the job type.
-- * 'displayName' - The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
-- * 'lastModifiedBy' - Who last modified the component.
-- * 'trialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
-- * 'inputArtifacts' - The input artifacts of the component.
-- * 'responseStatus' - The response status code.
mkDescribeTrialComponentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrialComponentResponse
mkDescribeTrialComponentResponse pResponseStatus_ =
  DescribeTrialComponentResponse'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      metrics = Lude.Nothing,
      outputArtifacts = Lude.Nothing,
      startTime = Lude.Nothing,
      createdBy = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      endTime = Lude.Nothing,
      trialComponentName = Lude.Nothing,
      parameters = Lude.Nothing,
      source = Lude.Nothing,
      displayName = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      trialComponentARN = Lude.Nothing,
      inputArtifacts = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the component was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsCreationTime :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Timestamp)
dtcgrsCreationTime = Lens.lens (creationTime :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

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
dtcgrsStatus :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe TrialComponentStatus)
dtcgrsStatus = Lens.lens (status :: DescribeTrialComponentResponse -> Lude.Maybe TrialComponentStatus) (\s a -> s {status = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The metrics for the component.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsMetrics :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe [TrialComponentMetricSummary])
dtcgrsMetrics = Lens.lens (metrics :: DescribeTrialComponentResponse -> Lude.Maybe [TrialComponentMetricSummary]) (\s a -> s {metrics = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The output artifacts of the component.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsOutputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
dtcgrsOutputArtifacts = Lens.lens (outputArtifacts :: DescribeTrialComponentResponse -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {outputArtifacts = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsStartTime :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Timestamp)
dtcgrsStartTime = Lens.lens (startTime :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Who created the component.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsCreatedBy :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe UserContext)
dtcgrsCreatedBy = Lens.lens (createdBy :: DescribeTrialComponentResponse -> Lude.Maybe UserContext) (\s a -> s {createdBy = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | When the component was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsLastModifiedTime :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Timestamp)
dtcgrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsEndTime :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Timestamp)
dtcgrsEndTime = Lens.lens (endTime :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsTrialComponentName :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Text)
dtcgrsTrialComponentName = Lens.lens (trialComponentName :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentName = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The hyperparameters of the component.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsParameters :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue)))
dtcgrsParameters = Lens.lens (parameters :: DescribeTrialComponentResponse -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue))) (\s a -> s {parameters = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsSource :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe TrialComponentSource)
dtcgrsSource = Lens.lens (source :: DescribeTrialComponentResponse -> Lude.Maybe TrialComponentSource) (\s a -> s {source = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsDisplayName :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Text)
dtcgrsDisplayName = Lens.lens (displayName :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Who last modified the component.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsLastModifiedBy :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe UserContext)
dtcgrsLastModifiedBy = Lens.lens (lastModifiedBy :: DescribeTrialComponentResponse -> Lude.Maybe UserContext) (\s a -> s {lastModifiedBy = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsTrialComponentARN :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Text)
dtcgrsTrialComponentARN = Lens.lens (trialComponentARN :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | The input artifacts of the component.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsInputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
dtcgrsInputArtifacts = Lens.lens (inputArtifacts :: DescribeTrialComponentResponse -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {inputArtifacts = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtcgrsResponseStatus :: Lens.Lens' DescribeTrialComponentResponse Lude.Int
dtcgrsResponseStatus = Lens.lens (responseStatus :: DescribeTrialComponentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED dtcgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
