{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateImageVersion (..)
    , mkCreateImageVersion
    -- ** Request lenses
    , civBaseImage
    , civClientToken
    , civImageName

    -- * Destructuring the response
    , CreateImageVersionResponse (..)
    , mkCreateImageVersionResponse
    -- ** Response lenses
    , civrrsImageVersionArn
    , civrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateImageVersion' smart constructor.
data CreateImageVersion = CreateImageVersion'
  { baseImage :: Types.ImageBaseImage
    -- ^ The registry path of the container image to use as the starting point for this version. The path is an Amazon Container Registry (ECR) URI in the following format:
--
-- @<acct-id>.dkr.ecr.<region>.amazonaws.com/<repo-name[:tag] or [@digest]>@ 
  , clientToken :: Types.ClientToken
    -- ^ A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK for Python (Boto3), add a unique value to the call.
  , imageName :: Types.ImageName
    -- ^ The @ImageName@ of the @Image@ to create a version of.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageVersion' value with any optional fields omitted.
mkCreateImageVersion
    :: Types.ImageBaseImage -- ^ 'baseImage'
    -> Types.ClientToken -- ^ 'clientToken'
    -> Types.ImageName -- ^ 'imageName'
    -> CreateImageVersion
mkCreateImageVersion baseImage clientToken imageName
  = CreateImageVersion'{baseImage, clientToken, imageName}

-- | The registry path of the container image to use as the starting point for this version. The path is an Amazon Container Registry (ECR) URI in the following format:
--
-- @<acct-id>.dkr.ecr.<region>.amazonaws.com/<repo-name[:tag] or [@digest]>@ 
--
-- /Note:/ Consider using 'baseImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civBaseImage :: Lens.Lens' CreateImageVersion Types.ImageBaseImage
civBaseImage = Lens.field @"baseImage"
{-# INLINEABLE civBaseImage #-}
{-# DEPRECATED baseImage "Use generic-lens or generic-optics with 'baseImage' instead"  #-}

-- | A unique ID. If not specified, the AWS CLI and AWS SDKs, such as the SDK for Python (Boto3), add a unique value to the call.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civClientToken :: Lens.Lens' CreateImageVersion Types.ClientToken
civClientToken = Lens.field @"clientToken"
{-# INLINEABLE civClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The @ImageName@ of the @Image@ to create a version of.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civImageName :: Lens.Lens' CreateImageVersion Types.ImageName
civImageName = Lens.field @"imageName"
{-# INLINEABLE civImageName #-}
{-# DEPRECATED imageName "Use generic-lens or generic-optics with 'imageName' instead"  #-}

instance Core.ToQuery CreateImageVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateImageVersion where
        toHeaders CreateImageVersion{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateImageVersion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateImageVersion where
        toJSON CreateImageVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("BaseImage" Core..= baseImage),
                  Core.Just ("ClientToken" Core..= clientToken),
                  Core.Just ("ImageName" Core..= imageName)])

instance Core.AWSRequest CreateImageVersion where
        type Rs CreateImageVersion = CreateImageVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateImageVersionResponse' Core.<$>
                   (x Core..:? "ImageVersionArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateImageVersionResponse' smart constructor.
data CreateImageVersionResponse = CreateImageVersionResponse'
  { imageVersionArn :: Core.Maybe Types.ImageVersionArn
    -- ^ The Amazon Resource Name (ARN) of the image version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageVersionResponse' value with any optional fields omitted.
mkCreateImageVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateImageVersionResponse
mkCreateImageVersionResponse responseStatus
  = CreateImageVersionResponse'{imageVersionArn = Core.Nothing,
                                responseStatus}

-- | The Amazon Resource Name (ARN) of the image version.
--
-- /Note:/ Consider using 'imageVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsImageVersionArn :: Lens.Lens' CreateImageVersionResponse (Core.Maybe Types.ImageVersionArn)
civrrsImageVersionArn = Lens.field @"imageVersionArn"
{-# INLINEABLE civrrsImageVersionArn #-}
{-# DEPRECATED imageVersionArn "Use generic-lens or generic-optics with 'imageVersionArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
civrrsResponseStatus :: Lens.Lens' CreateImageVersionResponse Core.Int
civrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE civrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
