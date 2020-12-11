{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of a SageMaker image. The container image the version represents isn't deleted.
module Network.AWS.SageMaker.DeleteImageVersion
  ( -- * Creating a request
    DeleteImageVersion (..),
    mkDeleteImageVersion,

    -- ** Request lenses
    divImageName,
    divVersion,

    -- * Destructuring the response
    DeleteImageVersionResponse (..),
    mkDeleteImageVersionResponse,

    -- ** Response lenses
    divrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteImageVersion' smart constructor.
data DeleteImageVersion = DeleteImageVersion'
  { imageName ::
      Lude.Text,
    version :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImageVersion' with the minimum fields required to make a request.
--
-- * 'imageName' - The name of the image.
-- * 'version' - The version to delete.
mkDeleteImageVersion ::
  -- | 'imageName'
  Lude.Text ->
  -- | 'version'
  Lude.Natural ->
  DeleteImageVersion
mkDeleteImageVersion pImageName_ pVersion_ =
  DeleteImageVersion' {imageName = pImageName_, version = pVersion_}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divImageName :: Lens.Lens' DeleteImageVersion Lude.Text
divImageName = Lens.lens (imageName :: DeleteImageVersion -> Lude.Text) (\s a -> s {imageName = a} :: DeleteImageVersion)
{-# DEPRECATED divImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

-- | The version to delete.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divVersion :: Lens.Lens' DeleteImageVersion Lude.Natural
divVersion = Lens.lens (version :: DeleteImageVersion -> Lude.Natural) (\s a -> s {version = a} :: DeleteImageVersion)
{-# DEPRECATED divVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest DeleteImageVersion where
  type Rs DeleteImageVersion = DeleteImageVersionResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteImageVersionResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteImageVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteImageVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteImageVersion where
  toJSON DeleteImageVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ImageName" Lude..= imageName),
            Lude.Just ("Version" Lude..= version)
          ]
      )

instance Lude.ToPath DeleteImageVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteImageVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteImageVersionResponse' smart constructor.
newtype DeleteImageVersionResponse = DeleteImageVersionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteImageVersionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteImageVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteImageVersionResponse
mkDeleteImageVersionResponse pResponseStatus_ =
  DeleteImageVersionResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divrsResponseStatus :: Lens.Lens' DeleteImageVersionResponse Lude.Int
divrsResponseStatus = Lens.lens (responseStatus :: DeleteImageVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteImageVersionResponse)
{-# DEPRECATED divrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
