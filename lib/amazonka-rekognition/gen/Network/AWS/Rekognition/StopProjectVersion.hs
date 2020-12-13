{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.StopProjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running model. The operation might take a while to complete. To check the current status, call 'DescribeProjectVersions' .
module Network.AWS.Rekognition.StopProjectVersion
  ( -- * Creating a request
    StopProjectVersion (..),
    mkStopProjectVersion,

    -- ** Request lenses
    spvProjectVersionARN,

    -- * Destructuring the response
    StopProjectVersionResponse (..),
    mkStopProjectVersionResponse,

    -- ** Response lenses
    srsStatus,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopProjectVersion' smart constructor.
newtype StopProjectVersion = StopProjectVersion'
  { -- | The Amazon Resource Name (ARN) of the model version that you want to delete.
    --
    -- This operation requires permissions to perform the @rekognition:StopProjectVersion@ action.
    projectVersionARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopProjectVersion' with the minimum fields required to make a request.
--
-- * 'projectVersionARN' - The Amazon Resource Name (ARN) of the model version that you want to delete.
--
-- This operation requires permissions to perform the @rekognition:StopProjectVersion@ action.
mkStopProjectVersion ::
  -- | 'projectVersionARN'
  Lude.Text ->
  StopProjectVersion
mkStopProjectVersion pProjectVersionARN_ =
  StopProjectVersion' {projectVersionARN = pProjectVersionARN_}

-- | The Amazon Resource Name (ARN) of the model version that you want to delete.
--
-- This operation requires permissions to perform the @rekognition:StopProjectVersion@ action.
--
-- /Note:/ Consider using 'projectVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spvProjectVersionARN :: Lens.Lens' StopProjectVersion Lude.Text
spvProjectVersionARN = Lens.lens (projectVersionARN :: StopProjectVersion -> Lude.Text) (\s a -> s {projectVersionARN = a} :: StopProjectVersion)
{-# DEPRECATED spvProjectVersionARN "Use generic-lens or generic-optics with 'projectVersionARN' instead." #-}

instance Lude.AWSRequest StopProjectVersion where
  type Rs StopProjectVersion = StopProjectVersionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopProjectVersionResponse'
            Lude.<$> (x Lude..?> "Status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopProjectVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.StopProjectVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopProjectVersion where
  toJSON StopProjectVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ProjectVersionArn" Lude..= projectVersionARN)]
      )

instance Lude.ToPath StopProjectVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery StopProjectVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopProjectVersionResponse' smart constructor.
data StopProjectVersionResponse = StopProjectVersionResponse'
  { -- | The current status of the stop operation.
    status :: Lude.Maybe ProjectVersionStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopProjectVersionResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the stop operation.
-- * 'responseStatus' - The response status code.
mkStopProjectVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopProjectVersionResponse
mkStopProjectVersionResponse pResponseStatus_ =
  StopProjectVersionResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current status of the stop operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStatus :: Lens.Lens' StopProjectVersionResponse (Lude.Maybe ProjectVersionStatus)
srsStatus = Lens.lens (status :: StopProjectVersionResponse -> Lude.Maybe ProjectVersionStatus) (\s a -> s {status = a} :: StopProjectVersionResponse)
{-# DEPRECATED srsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopProjectVersionResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopProjectVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopProjectVersionResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
