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
    civBaseImage,
    civClientToken,
    civImageName,

    -- * Destructuring the response
    CreateImageVersionResponse (..),
    mkCreateImageVersionResponse,

    -- ** Response lenses
    civrrsImageVersionArn,
    civrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateImageVersion' smart constructor.
data CreateImageVersion = CreateImageVersion'
  { -- | The registry path of the container image to use as the starting point for this version. The path is an Amazon Container Registry (ECR) URI in the following format:
    --
    -- @<acct-id>.dkr.ecr.<region>.amazonaws.com/<repo-name[:tag] or [@digest]>@
    baseImage :: Types.ImageBaseImage,
    -- | A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK for Python (Boto3), add a unique value to the call.
    clientToken :: Types.ClientToken,
    -- | The @ImageName@ of the @Image@ to create a version of.
    imageName :: Types.ImageName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageVersion' value with any optional fields omitted.
mkCreateImageVersion ::
  -- | 'baseImage'
  Types.ImageBaseImage ->
  -- | 'clientToken'
  Types.ClientToken ->
  -- | 'imageName'
  Types.ImageName ->
  CreateImageVersion
mkCreateImageVersion baseImage clientToken imageName =
  CreateImageVersion' {baseImage, clientToken, imageName}

-- | The registry path of the container image to use as the starting point for this version. The path is an Amazon Container Registry (ECR) URI in the following format:
--
-- @<acct-id>.dkr.ecr.<region>.amazonaws.com/<repo-name[:tag] or [@digest]>@
--
-- /Note:/ Consider using 'baseImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civBaseImage :: Lens.Lens' CreateImageVersion Types.ImageBaseImage
civBaseImage = Lens.field @"baseImage"
{-# DEPRECATED civBaseImage "Use generic-lens or generic-optics with 'baseImage' instead." #-}

-- | A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK for Python (Boto3), add a unique value to the call.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civClientToken :: Lens.Lens' CreateImageVersion Types.ClientToken
civClientToken = Lens.field @"clientToken"
{-# DEPRECATED civClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The @ImageName@ of the @Image@ to create a version of.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civImageName :: Lens.Lens' CreateImageVersion Types.ImageName
civImageName = Lens.field @"imageName"
{-# DEPRECATED civImageName "Use generic-lens or generic-optics with 'imageName' instead." #-}

instance Core.FromJSON CreateImageVersion where
  toJSON CreateImageVersion {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BaseImage" Core..= baseImage),
            Core.Just ("ClientToken" Core..= clientToken),
            Core.Just ("ImageName" Core..= imageName)
          ]
      )

instance Core.AWSRequest CreateImageVersion where
  type Rs CreateImageVersion = CreateImageVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateImageVersion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageVersionResponse'
            Core.<$> (x Core..:? "ImageVersionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateImageVersionResponse' smart constructor.
data CreateImageVersionResponse = CreateImageVersionResponse'
  { -- | The Amazon Resource Name (ARN) of the image version.
    imageVersionArn :: Core.Maybe Types.ImageVersionArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageVersionResponse' value with any optional fields omitted.
mkCreateImageVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateImageVersionResponse
mkCreateImageVersionResponse responseStatus =
  CreateImageVersionResponse'
    { imageVersionArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the image version.
--
-- /Note:/ Consider using 'imageVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsImageVersionArn :: Lens.Lens' CreateImageVersionResponse (Core.Maybe Types.ImageVersionArn)
civrrsImageVersionArn = Lens.field @"imageVersionArn"
{-# DEPRECATED civrrsImageVersionArn "Use generic-lens or generic-optics with 'imageVersionArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsResponseStatus :: Lens.Lens' CreateImageVersionResponse Core.Int
civrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED civrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
