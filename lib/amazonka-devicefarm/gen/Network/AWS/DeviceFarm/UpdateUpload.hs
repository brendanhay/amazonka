{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.UpdateUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an uploaded test spec.
module Network.AWS.DeviceFarm.UpdateUpload
  ( -- * Creating a request
    UpdateUpload (..),
    mkUpdateUpload,

    -- ** Request lenses
    uuEditContent,
    uuName,
    uuContentType,
    uuArn,

    -- * Destructuring the response
    UpdateUploadResponse (..),
    mkUpdateUploadResponse,

    -- ** Response lenses
    uursUpload,
    uursResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUpload' smart constructor.
data UpdateUpload = UpdateUpload'
  { editContent ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    contentType :: Lude.Maybe Lude.Text,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUpload' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the uploaded test spec.
-- * 'contentType' - The upload's content type (for example, @application/x-yaml@ ).
-- * 'editContent' - Set to true if the YAML file has changed and must be updated. Otherwise, set to false.
-- * 'name' - The upload's test spec file name. The name must not contain any forward slashes (/). The test spec file name must end with the @.yaml@ or @.yml@ file extension.
mkUpdateUpload ::
  -- | 'arn'
  Lude.Text ->
  UpdateUpload
mkUpdateUpload pArn_ =
  UpdateUpload'
    { editContent = Lude.Nothing,
      name = Lude.Nothing,
      contentType = Lude.Nothing,
      arn = pArn_
    }

-- | Set to true if the YAML file has changed and must be updated. Otherwise, set to false.
--
-- /Note:/ Consider using 'editContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuEditContent :: Lens.Lens' UpdateUpload (Lude.Maybe Lude.Bool)
uuEditContent = Lens.lens (editContent :: UpdateUpload -> Lude.Maybe Lude.Bool) (\s a -> s {editContent = a} :: UpdateUpload)
{-# DEPRECATED uuEditContent "Use generic-lens or generic-optics with 'editContent' instead." #-}

-- | The upload's test spec file name. The name must not contain any forward slashes (/). The test spec file name must end with the @.yaml@ or @.yml@ file extension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuName :: Lens.Lens' UpdateUpload (Lude.Maybe Lude.Text)
uuName = Lens.lens (name :: UpdateUpload -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateUpload)
{-# DEPRECATED uuName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The upload's content type (for example, @application/x-yaml@ ).
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuContentType :: Lens.Lens' UpdateUpload (Lude.Maybe Lude.Text)
uuContentType = Lens.lens (contentType :: UpdateUpload -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: UpdateUpload)
{-# DEPRECATED uuContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The Amazon Resource Name (ARN) of the uploaded test spec.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuArn :: Lens.Lens' UpdateUpload Lude.Text
uuArn = Lens.lens (arn :: UpdateUpload -> Lude.Text) (\s a -> s {arn = a} :: UpdateUpload)
{-# DEPRECATED uuArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest UpdateUpload where
  type Rs UpdateUpload = UpdateUploadResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateUploadResponse'
            Lude.<$> (x Lude..?> "upload") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateUpload where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.UpdateUpload" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUpload where
  toJSON UpdateUpload' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("editContent" Lude..=) Lude.<$> editContent,
            ("name" Lude..=) Lude.<$> name,
            ("contentType" Lude..=) Lude.<$> contentType,
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath UpdateUpload where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUpload where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUploadResponse' smart constructor.
data UpdateUploadResponse = UpdateUploadResponse'
  { upload ::
      Lude.Maybe Upload,
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

-- | Creates a value of 'UpdateUploadResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'upload' - A test spec uploaded to Device Farm.
mkUpdateUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateUploadResponse
mkUpdateUploadResponse pResponseStatus_ =
  UpdateUploadResponse'
    { upload = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A test spec uploaded to Device Farm.
--
-- /Note:/ Consider using 'upload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uursUpload :: Lens.Lens' UpdateUploadResponse (Lude.Maybe Upload)
uursUpload = Lens.lens (upload :: UpdateUploadResponse -> Lude.Maybe Upload) (\s a -> s {upload = a} :: UpdateUploadResponse)
{-# DEPRECATED uursUpload "Use generic-lens or generic-optics with 'upload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uursResponseStatus :: Lens.Lens' UpdateUploadResponse Lude.Int
uursResponseStatus = Lens.lens (responseStatus :: UpdateUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUploadResponse)
{-# DEPRECATED uursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
