{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteProjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Rekognition Custom Labels model.
--
-- You can't delete a model if it is running or if it is training. To check the status of a model, use the @Status@ field returned from 'DescribeProjectVersions' . To stop a running model call 'StopProjectVersion' . If the model is training, wait until it finishes.
-- This operation requires permissions to perform the @rekognition:DeleteProjectVersion@ action.
module Network.AWS.Rekognition.DeleteProjectVersion
  ( -- * Creating a request
    DeleteProjectVersion (..),
    mkDeleteProjectVersion,

    -- ** Request lenses
    dpvProjectVersionARN,

    -- * Destructuring the response
    DeleteProjectVersionResponse (..),
    mkDeleteProjectVersionResponse,

    -- ** Response lenses
    dpvfrsStatus,
    dpvfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProjectVersion' smart constructor.
newtype DeleteProjectVersion = DeleteProjectVersion'
  { -- | The Amazon Resource Name (ARN) of the model version that you want to delete.
    projectVersionARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProjectVersion' with the minimum fields required to make a request.
--
-- * 'projectVersionARN' - The Amazon Resource Name (ARN) of the model version that you want to delete.
mkDeleteProjectVersion ::
  -- | 'projectVersionARN'
  Lude.Text ->
  DeleteProjectVersion
mkDeleteProjectVersion pProjectVersionARN_ =
  DeleteProjectVersion' {projectVersionARN = pProjectVersionARN_}

-- | The Amazon Resource Name (ARN) of the model version that you want to delete.
--
-- /Note:/ Consider using 'projectVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvProjectVersionARN :: Lens.Lens' DeleteProjectVersion Lude.Text
dpvProjectVersionARN = Lens.lens (projectVersionARN :: DeleteProjectVersion -> Lude.Text) (\s a -> s {projectVersionARN = a} :: DeleteProjectVersion)
{-# DEPRECATED dpvProjectVersionARN "Use generic-lens or generic-optics with 'projectVersionARN' instead." #-}

instance Lude.AWSRequest DeleteProjectVersion where
  type Rs DeleteProjectVersion = DeleteProjectVersionResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteProjectVersionResponse'
            Lude.<$> (x Lude..?> "Status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProjectVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DeleteProjectVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProjectVersion where
  toJSON DeleteProjectVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ProjectVersionArn" Lude..= projectVersionARN)]
      )

instance Lude.ToPath DeleteProjectVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProjectVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProjectVersionResponse' smart constructor.
data DeleteProjectVersionResponse = DeleteProjectVersionResponse'
  { -- | The status of the deletion operation.
    status :: Lude.Maybe ProjectVersionStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProjectVersionResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the deletion operation.
-- * 'responseStatus' - The response status code.
mkDeleteProjectVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProjectVersionResponse
mkDeleteProjectVersionResponse pResponseStatus_ =
  DeleteProjectVersionResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the deletion operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvfrsStatus :: Lens.Lens' DeleteProjectVersionResponse (Lude.Maybe ProjectVersionStatus)
dpvfrsStatus = Lens.lens (status :: DeleteProjectVersionResponse -> Lude.Maybe ProjectVersionStatus) (\s a -> s {status = a} :: DeleteProjectVersionResponse)
{-# DEPRECATED dpvfrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvfrsResponseStatus :: Lens.Lens' DeleteProjectVersionResponse Lude.Int
dpvfrsResponseStatus = Lens.lens (responseStatus :: DeleteProjectVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProjectVersionResponse)
{-# DEPRECATED dpvfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
