{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    utcStatus,
    utcParametersToRemove,
    utcOutputArtifacts,
    utcStartTime,
    utcOutputArtifactsToRemove,
    utcEndTime,
    utcParameters,
    utcDisplayName,
    utcInputArtifacts,
    utcInputArtifactsToRemove,
    utcTrialComponentName,

    -- * Destructuring the response
    UpdateTrialComponentResponse (..),
    mkUpdateTrialComponentResponse,

    -- ** Response lenses
    utcrsTrialComponentARN,
    utcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateTrialComponent' smart constructor.
data UpdateTrialComponent = UpdateTrialComponent'
  { status ::
      Lude.Maybe TrialComponentStatus,
    parametersToRemove :: Lude.Maybe [Lude.Text],
    outputArtifacts ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (TrialComponentArtifact)
        ),
    startTime :: Lude.Maybe Lude.Timestamp,
    outputArtifactsToRemove :: Lude.Maybe [Lude.Text],
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
    inputArtifactsToRemove :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'UpdateTrialComponent' with the minimum fields required to make a request.
--
-- * 'displayName' - The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
-- * 'endTime' - When the component ended.
-- * 'inputArtifacts' - Replaces all of the component's input artifacts with the specified artifacts.
-- * 'inputArtifactsToRemove' - The input artifacts to remove from the component.
-- * 'outputArtifacts' - Replaces all of the component's output artifacts with the specified artifacts.
-- * 'outputArtifactsToRemove' - The output artifacts to remove from the component.
-- * 'parameters' - Replaces all of the component's hyperparameters with the specified hyperparameters.
-- * 'parametersToRemove' - The hyperparameters to remove from the component.
-- * 'startTime' - When the component started.
-- * 'status' - The new status of the component.
-- * 'trialComponentName' - The name of the component to update.
mkUpdateTrialComponent ::
  -- | 'trialComponentName'
  Lude.Text ->
  UpdateTrialComponent
mkUpdateTrialComponent pTrialComponentName_ =
  UpdateTrialComponent'
    { status = Lude.Nothing,
      parametersToRemove = Lude.Nothing,
      outputArtifacts = Lude.Nothing,
      startTime = Lude.Nothing,
      outputArtifactsToRemove = Lude.Nothing,
      endTime = Lude.Nothing,
      parameters = Lude.Nothing,
      displayName = Lude.Nothing,
      inputArtifacts = Lude.Nothing,
      inputArtifactsToRemove = Lude.Nothing,
      trialComponentName = pTrialComponentName_
    }

-- | The new status of the component.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcStatus :: Lens.Lens' UpdateTrialComponent (Lude.Maybe TrialComponentStatus)
utcStatus = Lens.lens (status :: UpdateTrialComponent -> Lude.Maybe TrialComponentStatus) (\s a -> s {status = a} :: UpdateTrialComponent)
{-# DEPRECATED utcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The hyperparameters to remove from the component.
--
-- /Note:/ Consider using 'parametersToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcParametersToRemove :: Lens.Lens' UpdateTrialComponent (Lude.Maybe [Lude.Text])
utcParametersToRemove = Lens.lens (parametersToRemove :: UpdateTrialComponent -> Lude.Maybe [Lude.Text]) (\s a -> s {parametersToRemove = a} :: UpdateTrialComponent)
{-# DEPRECATED utcParametersToRemove "Use generic-lens or generic-optics with 'parametersToRemove' instead." #-}

-- | Replaces all of the component's output artifacts with the specified artifacts.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcOutputArtifacts :: Lens.Lens' UpdateTrialComponent (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
utcOutputArtifacts = Lens.lens (outputArtifacts :: UpdateTrialComponent -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {outputArtifacts = a} :: UpdateTrialComponent)
{-# DEPRECATED utcOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | When the component started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcStartTime :: Lens.Lens' UpdateTrialComponent (Lude.Maybe Lude.Timestamp)
utcStartTime = Lens.lens (startTime :: UpdateTrialComponent -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: UpdateTrialComponent)
{-# DEPRECATED utcStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The output artifacts to remove from the component.
--
-- /Note:/ Consider using 'outputArtifactsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcOutputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Lude.Maybe [Lude.Text])
utcOutputArtifactsToRemove = Lens.lens (outputArtifactsToRemove :: UpdateTrialComponent -> Lude.Maybe [Lude.Text]) (\s a -> s {outputArtifactsToRemove = a} :: UpdateTrialComponent)
{-# DEPRECATED utcOutputArtifactsToRemove "Use generic-lens or generic-optics with 'outputArtifactsToRemove' instead." #-}

-- | When the component ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcEndTime :: Lens.Lens' UpdateTrialComponent (Lude.Maybe Lude.Timestamp)
utcEndTime = Lens.lens (endTime :: UpdateTrialComponent -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: UpdateTrialComponent)
{-# DEPRECATED utcEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Replaces all of the component's hyperparameters with the specified hyperparameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcParameters :: Lens.Lens' UpdateTrialComponent (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue)))
utcParameters = Lens.lens (parameters :: UpdateTrialComponent -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentParameterValue))) (\s a -> s {parameters = a} :: UpdateTrialComponent)
{-# DEPRECATED utcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The name of the component as displayed. The name doesn't need to be unique. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcDisplayName :: Lens.Lens' UpdateTrialComponent (Lude.Maybe Lude.Text)
utcDisplayName = Lens.lens (displayName :: UpdateTrialComponent -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: UpdateTrialComponent)
{-# DEPRECATED utcDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Replaces all of the component's input artifacts with the specified artifacts.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcInputArtifacts :: Lens.Lens' UpdateTrialComponent (Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact)))
utcInputArtifacts = Lens.lens (inputArtifacts :: UpdateTrialComponent -> Lude.Maybe (Lude.HashMap Lude.Text (TrialComponentArtifact))) (\s a -> s {inputArtifacts = a} :: UpdateTrialComponent)
{-# DEPRECATED utcInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The input artifacts to remove from the component.
--
-- /Note:/ Consider using 'inputArtifactsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcInputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Lude.Maybe [Lude.Text])
utcInputArtifactsToRemove = Lens.lens (inputArtifactsToRemove :: UpdateTrialComponent -> Lude.Maybe [Lude.Text]) (\s a -> s {inputArtifactsToRemove = a} :: UpdateTrialComponent)
{-# DEPRECATED utcInputArtifactsToRemove "Use generic-lens or generic-optics with 'inputArtifactsToRemove' instead." #-}

-- | The name of the component to update.
--
-- /Note:/ Consider using 'trialComponentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcTrialComponentName :: Lens.Lens' UpdateTrialComponent Lude.Text
utcTrialComponentName = Lens.lens (trialComponentName :: UpdateTrialComponent -> Lude.Text) (\s a -> s {trialComponentName = a} :: UpdateTrialComponent)
{-# DEPRECATED utcTrialComponentName "Use generic-lens or generic-optics with 'trialComponentName' instead." #-}

instance Lude.AWSRequest UpdateTrialComponent where
  type Rs UpdateTrialComponent = UpdateTrialComponentResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTrialComponentResponse'
            Lude.<$> (x Lude..?> "TrialComponentArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTrialComponent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateTrialComponent" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTrialComponent where
  toJSON UpdateTrialComponent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("ParametersToRemove" Lude..=) Lude.<$> parametersToRemove,
            ("OutputArtifacts" Lude..=) Lude.<$> outputArtifacts,
            ("StartTime" Lude..=) Lude.<$> startTime,
            ("OutputArtifactsToRemove" Lude..=)
              Lude.<$> outputArtifactsToRemove,
            ("EndTime" Lude..=) Lude.<$> endTime,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("DisplayName" Lude..=) Lude.<$> displayName,
            ("InputArtifacts" Lude..=) Lude.<$> inputArtifacts,
            ("InputArtifactsToRemove" Lude..=) Lude.<$> inputArtifactsToRemove,
            Lude.Just ("TrialComponentName" Lude..= trialComponentName)
          ]
      )

instance Lude.ToPath UpdateTrialComponent where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTrialComponent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTrialComponentResponse' smart constructor.
data UpdateTrialComponentResponse = UpdateTrialComponentResponse'
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

-- | Creates a value of 'UpdateTrialComponentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
mkUpdateTrialComponentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTrialComponentResponse
mkUpdateTrialComponentResponse pResponseStatus_ =
  UpdateTrialComponentResponse'
    { trialComponentARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial component.
--
-- /Note:/ Consider using 'trialComponentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcrsTrialComponentARN :: Lens.Lens' UpdateTrialComponentResponse (Lude.Maybe Lude.Text)
utcrsTrialComponentARN = Lens.lens (trialComponentARN :: UpdateTrialComponentResponse -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentARN = a} :: UpdateTrialComponentResponse)
{-# DEPRECATED utcrsTrialComponentARN "Use generic-lens or generic-optics with 'trialComponentARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utcrsResponseStatus :: Lens.Lens' UpdateTrialComponentResponse Lude.Int
utcrsResponseStatus = Lens.lens (responseStatus :: UpdateTrialComponentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTrialComponentResponse)
{-# DEPRECATED utcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
