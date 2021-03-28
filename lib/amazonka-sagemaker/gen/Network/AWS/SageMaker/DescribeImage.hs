{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a SageMaker image.
module Network.AWS.SageMaker.DescribeImage
    (
    -- * Creating a request
      DescribeImage (..)
    , mkDescribeImage
    -- ** Request lenses
    , diImageName

    -- * Destructuring the response
    , DescribeImageResponse (..)
    , mkDescribeImageResponse
    -- ** Response lenses
    , dirfrsCreationTime
    , dirfrsDescription
    , dirfrsDisplayName
    , dirfrsFailureReason
    , dirfrsImageArn
    , dirfrsImageName
    , dirfrsImageStatus
    , dirfrsLastModifiedTime
    , dirfrsRoleArn
    , dirfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeImage' smart constructor.
newtype DescribeImage = DescribeImage'
  { imageName :: Types.ImageName
    -- ^ The name of the image to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImage' value with any optional fields omitted.
mkDescribeImage
    :: Types.ImageName -- ^ 'imageName'
    -> DescribeImage
mkDescribeImage imageName = DescribeImage'{imageName}

-- | The name of the image to describe.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diImageName :: Lens.Lens' DescribeImage Types.ImageName
diImageName = Lens.field @"imageName"
{-# INLINEABLE diImageName #-}
{-# DEPRECATED imageName "Use generic-lens or generic-optics with 'imageName' instead"  #-}

instance Core.ToQuery DescribeImage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeImage where
        toHeaders DescribeImage{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeImage") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeImage where
        toJSON DescribeImage{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ImageName" Core..= imageName)])

instance Core.AWSRequest DescribeImage where
        type Rs DescribeImage = DescribeImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeImageResponse' Core.<$>
                   (x Core..:? "CreationTime") Core.<*> x Core..:? "Description"
                     Core.<*> x Core..:? "DisplayName"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "ImageArn"
                     Core.<*> x Core..:? "ImageName"
                     Core.<*> x Core..:? "ImageStatus"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "RoleArn"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeImageResponse' smart constructor.
data DescribeImageResponse = DescribeImageResponse'
  { creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the image was created.
  , description :: Core.Maybe Types.Description
    -- ^ The description of the image.
  , displayName :: Core.Maybe Types.ImageDisplayName
    -- ^ The name of the image as displayed.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ When a create, update, or delete operation fails, the reason for the failure.
  , imageArn :: Core.Maybe Types.ImageArn
    -- ^ The Amazon Resource Name (ARN) of the image.
  , imageName :: Core.Maybe Types.ImageName
    -- ^ The name of the image.
  , imageStatus :: Core.Maybe Types.ImageStatus
    -- ^ The status of the image.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the image was last modified.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeImageResponse' value with any optional fields omitted.
mkDescribeImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeImageResponse
mkDescribeImageResponse responseStatus
  = DescribeImageResponse'{creationTime = Core.Nothing,
                           description = Core.Nothing, displayName = Core.Nothing,
                           failureReason = Core.Nothing, imageArn = Core.Nothing,
                           imageName = Core.Nothing, imageStatus = Core.Nothing,
                           lastModifiedTime = Core.Nothing, roleArn = Core.Nothing,
                           responseStatus}

-- | When the image was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsCreationTime :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.NominalDiffTime)
dirfrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dirfrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The description of the image.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsDescription :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.Description)
dirfrsDescription = Lens.field @"description"
{-# INLINEABLE dirfrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the image as displayed.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsDisplayName :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.ImageDisplayName)
dirfrsDisplayName = Lens.field @"displayName"
{-# INLINEABLE dirfrsDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | When a create, update, or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsFailureReason :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.FailureReason)
dirfrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE dirfrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The Amazon Resource Name (ARN) of the image.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsImageArn :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.ImageArn)
dirfrsImageArn = Lens.field @"imageArn"
{-# INLINEABLE dirfrsImageArn #-}
{-# DEPRECATED imageArn "Use generic-lens or generic-optics with 'imageArn' instead"  #-}

-- | The name of the image.
--
-- /Note:/ Consider using 'imageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsImageName :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.ImageName)
dirfrsImageName = Lens.field @"imageName"
{-# INLINEABLE dirfrsImageName #-}
{-# DEPRECATED imageName "Use generic-lens or generic-optics with 'imageName' instead"  #-}

-- | The status of the image.
--
-- /Note:/ Consider using 'imageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsImageStatus :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.ImageStatus)
dirfrsImageStatus = Lens.field @"imageStatus"
{-# INLINEABLE dirfrsImageStatus #-}
{-# DEPRECATED imageStatus "Use generic-lens or generic-optics with 'imageStatus' instead"  #-}

-- | When the image was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsLastModifiedTime :: Lens.Lens' DescribeImageResponse (Core.Maybe Core.NominalDiffTime)
dirfrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE dirfrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsRoleArn :: Lens.Lens' DescribeImageResponse (Core.Maybe Types.RoleArn)
dirfrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dirfrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsResponseStatus :: Lens.Lens' DescribeImageResponse Core.Int
dirfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
