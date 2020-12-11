{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    desTrialComponentName,

    -- * Destructuring the response
    DescribeTrialComponentResponse (..),
    mkDescribeTrialComponentResponse,

    -- ** Response lenses
    desersCreationTime,
    desersStatus,
    desersMetrics,
    desersOutputArtifacts,
    desersStartTime,
    desersCreatedBy,
    desersLastModifiedTime,
    desersEndTime,
    desersTrialComponentName,
    desersParameters,
    desersSource,
    desersDisplayName,
    desersLastModifiedBy,
    desersTrialComponentARN,
    desersInputArtifacts,
    desersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeTrialComponent' smart constructor.
newtype DescribeTrialComponent = DescribeTrialComponent'
  { trialComponentName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
desTrialComponentName :: Lens.Lens' DescribeTrialComponent Lude.Text
desTrialComponentName = Lens.lens (trialComponentName :: DescribeTrialComponent -> Lude.Text) (\s a -> s {trialComponentName = a} :: DescribeTrialComponent)
{-# DEPRECATED desTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

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
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status ::
      Lude.Maybe
        TrialComponentStatus,
    metrics ::
      Lude.Maybe
        [TrialComponentMetricSummary],
    outputArtifacts ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (TrialComponentArtifact)
        ),
    startTime ::
      Lude.Maybe Lude.Timestamp,
    createdBy ::
      Lude.Maybe UserContext,
    lastModifiedTime ::
      Lude.Maybe Lude.Timestamp,
    endTime ::
      Lude.Maybe Lude.Timestamp,
    trialComponentName ::
      Lude.Maybe Lude.Text,
    parameters ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (TrialComponentParameterValue)
        ),
    source ::
      Lude.Maybe
        TrialComponentSource,
    displayName ::
      Lude.Maybe Lude.Text,
    lastModifiedBy ::
      Lude.Maybe UserContext,
    trialComponentARN ::
      Lude.Maybe Lude.Text,
    inputArtifacts ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (TrialComponentArtifact)
        ),
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

-- | Creates a value of 'DescribeTrialComponentResponse' with the minimum fields required to make a request.
--
-- * 'createdBy' - Who created the component.
-- * 'creationTime' - When the component was created.
-- * 'displayName' - The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
-- * 'endTime' - When the component ended.
-- * 'inputArtifacts' - The input artifacts of the component.
-- * 'lastModifiedBy' - Who last modified the component.
-- * 'lastModifiedTime' - When the component was last modified.
-- * 'metrics' - The metrics for the component.
-- * 'outputArtifacts' - The output artifacts of the component.
-- * 'parameters' - The hyperparameters of the component.
-- * 'responseStatus' - The response status code.
-- * 'source' - The Amazon Resource Name (ARN) of the source and, optionally, the job type.
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
-- * 'trialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
-- * 'trialComponentName' - The name of the trial component.
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
desersCreationTime :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Timestamp)
desersCreationTime = Lens.lens (creationTime :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

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
desersStatus :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe TrialComponentStatus)
desersStatus = Lens.lens (status :: DescribeTrialComponentResponse -> Lude.Maybe TrialComponentStatus) (\s a -> s {status = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The metrics for the component.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersMetrics :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe [TrialComponentMetricSummary])
desersMetrics = Lens.lens (metrics :: DescribeTrialComponentResponse -> Lude.Maybe [TrialComponentMetricSummary]) (\s a -> s {metrics = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The output artifacts of the component.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersOutputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
desersOutputArtifacts = Lens.lens (outputArtifacts :: DescribeTrialComponentResponse -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {outputArtifacts = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersStartTime :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Timestamp)
desersStartTime = Lens.lens (startTime :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Who created the component.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersCreatedBy :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe UserContext)
desersCreatedBy = Lens.lens (createdBy :: DescribeTrialComponentResponse -> Lude.Maybe UserContext) (\s a -> s {createdBy = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | When the component was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersLastModifiedTime :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Timestamp)
desersLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersEndTime :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Timestamp)
desersEndTime = Lens.lens (endTime :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the trial component.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersTrialComponentName :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Text)
desersTrialComponentName = Lens.lens (trialComponentName :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentName = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

-- | The hyperparameters of the component.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersParameters :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue)))
desersParameters = Lens.lens (parameters :: DescribeTrialComponentResponse -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue))) (\s a -> s {parameters = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersSource :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe TrialComponentSource)
desersSource = Lens.lens (source :: DescribeTrialComponentResponse -> Lude.Maybe TrialComponentSource) (\s a -> s {source = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersDisplayName :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Text)
desersDisplayName = Lens.lens (displayName :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Who last modified the component.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersLastModifiedBy :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe UserContext)
desersLastModifiedBy = Lens.lens (lastModifiedBy :: DescribeTrialComponentResponse -> Lude.Maybe UserContext) (\s a -> s {lastModifiedBy = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersTrialComponentARN :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe Lude.Text)
desersTrialComponentARN = Lens.lens (trialComponentARN :: DescribeTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | The input artifacts of the component.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersInputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
desersInputArtifacts = Lens.lens (inputArtifacts :: DescribeTrialComponentResponse -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {inputArtifacts = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desersResponseStatus :: Lens.Lens' DescribeTrialComponentResponse Lude.Int
desersResponseStatus = Lens.lens (responseStatus :: DescribeTrialComponentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrialComponentResponse)
{-# DEPRECATED desersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
