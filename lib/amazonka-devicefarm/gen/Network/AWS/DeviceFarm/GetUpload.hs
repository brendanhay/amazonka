{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an upload.
module Network.AWS.DeviceFarm.GetUpload
  ( -- * Creating a request
    GetUpload (..),
    mkGetUpload,

    -- ** Request lenses
    guArn,

    -- * Destructuring the response
    GetUploadResponse (..),
    mkGetUploadResponse,

    -- ** Response lenses
    gursUpload,
    gursResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the get upload operation.
--
-- /See:/ 'mkGetUpload' smart constructor.
newtype GetUpload = GetUpload'
  { -- | The upload's ARN.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUpload' with the minimum fields required to make a request.
--
-- * 'arn' - The upload's ARN.
mkGetUpload ::
  -- | 'arn'
  Lude.Text ->
  GetUpload
mkGetUpload pArn_ = GetUpload' {arn = pArn_}

-- | The upload's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guArn :: Lens.Lens' GetUpload Lude.Text
guArn = Lens.lens (arn :: GetUpload -> Lude.Text) (\s a -> s {arn = a} :: GetUpload)
{-# DEPRECATED guArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetUpload where
  type Rs GetUpload = GetUploadResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUploadResponse'
            Lude.<$> (x Lude..?> "upload") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUpload where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.GetUpload" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetUpload where
  toJSON GetUpload' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath GetUpload where
  toPath = Lude.const "/"

instance Lude.ToQuery GetUpload where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a get upload request.
--
-- /See:/ 'mkGetUploadResponse' smart constructor.
data GetUploadResponse = GetUploadResponse'
  { -- | An app or a set of one or more tests to upload or that have been uploaded.
    upload :: Lude.Maybe Upload,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUploadResponse' with the minimum fields required to make a request.
--
-- * 'upload' - An app or a set of one or more tests to upload or that have been uploaded.
-- * 'responseStatus' - The response status code.
mkGetUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUploadResponse
mkGetUploadResponse pResponseStatus_ =
  GetUploadResponse'
    { upload = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An app or a set of one or more tests to upload or that have been uploaded.
--
-- /Note:/ Consider using 'upload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursUpload :: Lens.Lens' GetUploadResponse (Lude.Maybe Upload)
gursUpload = Lens.lens (upload :: GetUploadResponse -> Lude.Maybe Upload) (\s a -> s {upload = a} :: GetUploadResponse)
{-# DEPRECATED gursUpload "Use generic-lens or generic-optics with 'upload' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gursResponseStatus :: Lens.Lens' GetUploadResponse Lude.Int
gursResponseStatus = Lens.lens (responseStatus :: GetUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUploadResponse)
{-# DEPRECATED gursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
