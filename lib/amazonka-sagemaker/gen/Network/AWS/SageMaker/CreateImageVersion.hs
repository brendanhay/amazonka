{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of the SageMaker image specified by @ImageName@ . The version represents the Amazon Container Registry (ECR) container image specified by @BaseImage@ .
module Network.AWS.SageMaker.CreateImageVersion
  ( -- * Creating a request
    CreateImageVersion (..),
    mkCreateImageVersion,

    -- ** Request lenses
    civClientToken,
    civBaseImage,
    civImageName,

    -- * Destructuring the response
    CreateImageVersionResponse (..),
    mkCreateImageVersionResponse,

    -- ** Response lenses
    civrsImageVersionARN,
    civrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateImageVersion' smart constructor.
data CreateImageVersion = CreateImageVersion'
  { -- | A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK for Python (Boto3), add a unique value to the call.
    clientToken :: Lude.Text,
    -- | The registry path of the container image to use as the starting point for this version. The path is an Amazon Container Registry (ECR) URI in the following format:
    --
    -- @<acct-id>.dkr.ecr.<region>.amazonaws.com/<repo-name[:tag] or [@digest]>@
    baseImage :: Lude.Text,
    -- | The @ImageName@ of the @Image@ to create a version of.
    imageName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImageVersion' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK for Python (Boto3), add a unique value to the call.
-- * 'baseImage' - The registry path of the container image to use as the starting point for this version. The path is an Amazon Container Registry (ECR) URI in the following format:
--
-- @<acct-id>.dkr.ecr.<region>.amazonaws.com/<repo-name[:tag] or [@digest]>@
-- * 'imageName' - The @ImageName@ of the @Image@ to create a version of.
mkCreateImageVersion ::
  -- | 'clientToken'
  Lude.Text ->
  -- | 'baseImage'
  Lude.Text ->
  -- | 'imageName'
  Lude.Text ->
  CreateImageVersion
mkCreateImageVersion pClientToken_ pBaseImage_ pImageName_ =
  CreateImageVersion'
    { clientToken = pClientToken_,
      baseImage = pBaseImage_,
      imageName = pImageName_
    }

-- | A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK for Python (Boto3), add a unique value to the call.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civClientToken :: Lens.Lens' CreateImageVersion Lude.Text
civClientToken = Lens.lens (clientToken :: CreateImageVersion -> Lude.Text) (\s a -> s {clientToken = a} :: CreateImageVersion)
{-# DEPRECATED civClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The registry path of the container image to use as the starting point for this version. The path is an Amazon Container Registry (ECR) URI in the following format:
--
-- @<acct-id>.dkr.ecr.<region>.amazonaws.com/<repo-name[:tag] or [@digest]>@
--
-- /Note:/ Consider using 'baseImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civBaseImage :: Lens.Lens' CreateImageVersion Lude.Text
civBaseImage = Lens.lens (baseImage :: CreateImageVersion -> Lude.Text) (\s a -> s {baseImage = a} :: CreateImageVersion)
{-# DEPRECATED civBaseImage "Use generic-lens or generic-optics with 'baseImage' instead." #-}

-- | The @ImageName@ of the @Image@ to create a version of.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civImageName :: Lens.Lens' CreateImageVersion Lude.Text
civImageName = Lens.lens (imageName :: CreateImageVersion -> Lude.Text) (\s a -> s {imageName = a} :: CreateImageVersion)
{-# DEPRECATED civImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

instance Lude.AWSRequest CreateImageVersion where
  type Rs CreateImageVersion = CreateImageVersionResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateImageVersionResponse'
            Lude.<$> (x Lude..?> "ImageVersionArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateImageVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateImageVersion" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateImageVersion where
  toJSON CreateImageVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("BaseImage" Lude..= baseImage),
            Lude.Just ("ImageName" Lude..= imageName)
          ]
      )

instance Lude.ToPath CreateImageVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateImageVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateImageVersionResponse' smart constructor.
data CreateImageVersionResponse = CreateImageVersionResponse'
  { -- | The Amazon Resource Name (ARN) of the image version.
    imageVersionARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImageVersionResponse' with the minimum fields required to make a request.
--
-- * 'imageVersionARN' - The Amazon Resource Name (ARN) of the image version.
-- * 'responseStatus' - The response status code.
mkCreateImageVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateImageVersionResponse
mkCreateImageVersionResponse pResponseStatus_ =
  CreateImageVersionResponse'
    { imageVersionARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the image version.
--
-- /Note:/ Consider using 'imageVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsImageVersionARN :: Lens.Lens' CreateImageVersionResponse (Lude.Maybe Lude.Text)
civrsImageVersionARN = Lens.lens (imageVersionARN :: CreateImageVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageVersionARN = a} :: CreateImageVersionResponse)
{-# DEPRECATED civrsImageVersionARN "Use generic-lens or generic-optics with 'imageVersionARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrsResponseStatus :: Lens.Lens' CreateImageVersionResponse Lude.Int
civrsResponseStatus = Lens.lens (responseStatus :: CreateImageVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateImageVersionResponse)
{-# DEPRECATED civrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
