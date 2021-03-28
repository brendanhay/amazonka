{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom SageMaker image. A SageMaker image is a set of image versions. Each image version represents a container image stored in Amazon Container Registry (ECR). For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/studio-byoi.html Bring your own SageMaker image> .
module Network.AWS.SageMaker.CreateImage
    (
    -- * Creating a request
      CreateImage (..)
    , mkCreateImage
    -- ** Request lenses
    , cifImageName
    , cifRoleArn
    , cifDescription
    , cifDisplayName
    , cifTags

    -- * Destructuring the response
    , CreateImageResponse (..)
    , mkCreateImageResponse
    -- ** Response lenses
    , cirrsImageArn
    , cirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateImage' smart constructor.
data CreateImage = CreateImage'
  { imageName :: Types.ImageName
    -- ^ The name of the image. Must be unique to your account.
  , roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the image.
  , displayName :: Core.Maybe Types.ImageDisplayName
    -- ^ The display name of the image. If not provided, @ImageName@ is displayed.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags to apply to the image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImage' value with any optional fields omitted.
mkCreateImage
    :: Types.ImageName -- ^ 'imageName'
    -> Types.RoleArn -- ^ 'roleArn'
    -> CreateImage
mkCreateImage imageName roleArn
  = CreateImage'{imageName, roleArn, description = Core.Nothing,
                 displayName = Core.Nothing, tags = Core.Nothing}

-- | The name of the image. Must be unique to your account.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifImageName :: Lens.Lens' CreateImage Types.ImageName
cifImageName = Lens.field @"imageName"
{-# INLINEABLE cifImageName #-}
{-# DEPRECATED imageName "Use generic-lens or generic-optics with 'imageName' instead"  #-}

-- | The Amazon Resource Name (ARN) of an IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifRoleArn :: Lens.Lens' CreateImage Types.RoleArn
cifRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cifRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifDescription :: Lens.Lens' CreateImage (Core.Maybe Types.Description)
cifDescription = Lens.field @"description"
{-# INLINEABLE cifDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The display name of the image. If not provided, @ImageName@ is displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifDisplayName :: Lens.Lens' CreateImage (Core.Maybe Types.ImageDisplayName)
cifDisplayName = Lens.field @"displayName"
{-# INLINEABLE cifDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | A list of tags to apply to the image.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifTags :: Lens.Lens' CreateImage (Core.Maybe [Types.Tag])
cifTags = Lens.field @"tags"
{-# INLINEABLE cifTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateImage where
        toHeaders CreateImage{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateImage") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateImage where
        toJSON CreateImage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ImageName" Core..= imageName),
                  Core.Just ("RoleArn" Core..= roleArn),
                  ("Description" Core..=) Core.<$> description,
                  ("DisplayName" Core..=) Core.<$> displayName,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateImage where
        type Rs CreateImage = CreateImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateImageResponse' Core.<$>
                   (x Core..:? "ImageArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateImageResponse' smart constructor.
data CreateImageResponse = CreateImageResponse'
  { imageArn :: Core.Maybe Types.ImageArn
    -- ^ The Amazon Resource Name (ARN) of the image.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateImageResponse' value with any optional fields omitted.
mkCreateImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateImageResponse
mkCreateImageResponse responseStatus
  = CreateImageResponse'{imageArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the image.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsImageArn :: Lens.Lens' CreateImageResponse (Core.Maybe Types.ImageArn)
cirrsImageArn = Lens.field @"imageArn"
{-# INLINEABLE cirrsImageArn #-}
{-# DEPRECATED imageArn "Use generic-lens or generic-optics with 'imageArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CreateImageResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
