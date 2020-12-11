{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StartProjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the running of the version of a model. Starting a model takes a while to complete. To check the current state of the model, use 'DescribeProjectVersions' .
--
-- Once the model is running, you can detect custom labels in new images by calling 'DetectCustomLabels' .
-- This operation requires permissions to perform the @rekognition:StartProjectVersion@ action.
module Network.AWS.Rekognition.StartProjectVersion
  ( -- * Creating a request
    StartProjectVersion (..),
    mkStartProjectVersion,

    -- ** Request lenses
    spvProjectVersionARN,
    spvMinInferenceUnits,

    -- * Destructuring the response
    StartProjectVersionResponse (..),
    mkStartProjectVersionResponse,

    -- ** Response lenses
    spvrsStatus,
    spvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartProjectVersion' smart constructor.
data StartProjectVersion = StartProjectVersion'
  { projectVersionARN ::
      Lude.Text,
    minInferenceUnits :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartProjectVersion' with the minimum fields required to make a request.
--
-- * 'minInferenceUnits' - The minimum number of inference units to use. A single inference unit represents 1 hour of processing and can support up to 5 Transaction Pers Second (TPS). Use a higher number to increase the TPS throughput of your model. You are charged for the number of inference units that you use.
-- * 'projectVersionARN' - The Amazon Resource Name(ARN) of the model version that you want to start.
mkStartProjectVersion ::
  -- | 'projectVersionARN'
  Lude.Text ->
  -- | 'minInferenceUnits'
  Lude.Natural ->
  StartProjectVersion
mkStartProjectVersion pProjectVersionARN_ pMinInferenceUnits_ =
  StartProjectVersion'
    { projectVersionARN = pProjectVersionARN_,
      minInferenceUnits = pMinInferenceUnits_
    }

-- | The Amazon Resource Name(ARN) of the model version that you want to start.
--
-- /Note:/ Consider using 'projectVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spvProjectVersionARN :: Lens.Lens' StartProjectVersion Lude.Text
spvProjectVersionARN = Lens.lens (projectVersionARN :: StartProjectVersion -> Lude.Text) (\s a -> s {projectVersionARN = a} :: StartProjectVersion)
{-# DEPRECATED spvProjectVersionARN "Use generic-lens or generic-optics with 'projectVersionARN' instead." #-}

-- | The minimum number of inference units to use. A single inference unit represents 1 hour of processing and can support up to 5 Transaction Pers Second (TPS). Use a higher number to increase the TPS throughput of your model. You are charged for the number of inference units that you use.
--
-- /Note:/ Consider using 'minInferenceUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spvMinInferenceUnits :: Lens.Lens' StartProjectVersion Lude.Natural
spvMinInferenceUnits = Lens.lens (minInferenceUnits :: StartProjectVersion -> Lude.Natural) (\s a -> s {minInferenceUnits = a} :: StartProjectVersion)
{-# DEPRECATED spvMinInferenceUnits "Use generic-lens or generic-optics with 'minInferenceUnits' instead." #-}

instance Lude.AWSRequest StartProjectVersion where
  type Rs StartProjectVersion = StartProjectVersionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartProjectVersionResponse'
            Lude.<$> (x Lude..?> "Status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartProjectVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StartProjectVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartProjectVersion where
  toJSON StartProjectVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ProjectVersionArn" Lude..= projectVersionARN),
            Lude.Just ("MinInferenceUnits" Lude..= minInferenceUnits)
          ]
      )

instance Lude.ToPath StartProjectVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery StartProjectVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartProjectVersionResponse' smart constructor.
data StartProjectVersionResponse = StartProjectVersionResponse'
  { status ::
      Lude.Maybe ProjectVersionStatus,
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

-- | Creates a value of 'StartProjectVersionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - The current running status of the model.
mkStartProjectVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartProjectVersionResponse
mkStartProjectVersionResponse pResponseStatus_ =
  StartProjectVersionResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current running status of the model.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spvrsStatus :: Lens.Lens' StartProjectVersionResponse (Lude.Maybe ProjectVersionStatus)
spvrsStatus = Lens.lens (status :: StartProjectVersionResponse -> Lude.Maybe ProjectVersionStatus) (\s a -> s {status = a} :: StartProjectVersionResponse)
{-# DEPRECATED spvrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spvrsResponseStatus :: Lens.Lens' StartProjectVersionResponse Lude.Int
spvrsResponseStatus = Lens.lens (responseStatus :: StartProjectVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartProjectVersionResponse)
{-# DEPRECATED spvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
