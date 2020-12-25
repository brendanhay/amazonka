{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersion
  ( ImageVersion (..),

    -- * Smart constructor
    mkImageVersion,

    -- * Lenses
    ivCreationTime,
    ivImageArn,
    ivImageVersionArn,
    ivImageVersionStatus,
    ivLastModifiedTime,
    ivVersion,
    ivFailureReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.FailureReason as Types
import qualified Network.AWS.SageMaker.Types.ImageArn as Types
import qualified Network.AWS.SageMaker.Types.ImageVersionArn as Types
import qualified Network.AWS.SageMaker.Types.ImageVersionStatus as Types

-- | A version of a SageMaker @Image@ . A version represents an existing container image.
--
-- /See:/ 'mkImageVersion' smart constructor.
data ImageVersion = ImageVersion'
  { -- | When the version was created.
    creationTime :: Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the image the version is based on.
    imageArn :: Types.ImageArn,
    -- | The ARN of the version.
    imageVersionArn :: Types.ImageVersionArn,
    -- | The status of the version.
    imageVersionStatus :: Types.ImageVersionStatus,
    -- | When the version was last modified.
    lastModifiedTime :: Core.NominalDiffTime,
    -- | The version number.
    version :: Core.Natural,
    -- | When a create or delete operation fails, the reason for the failure.
    failureReason :: Core.Maybe Types.FailureReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ImageVersion' value with any optional fields omitted.
mkImageVersion ::
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'imageArn'
  Types.ImageArn ->
  -- | 'imageVersionArn'
  Types.ImageVersionArn ->
  -- | 'imageVersionStatus'
  Types.ImageVersionStatus ->
  -- | 'lastModifiedTime'
  Core.NominalDiffTime ->
  -- | 'version'
  Core.Natural ->
  ImageVersion
mkImageVersion
  creationTime
  imageArn
  imageVersionArn
  imageVersionStatus
  lastModifiedTime
  version =
    ImageVersion'
      { creationTime,
        imageArn,
        imageVersionArn,
        imageVersionStatus,
        lastModifiedTime,
        version,
        failureReason = Core.Nothing
      }

-- | When the version was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivCreationTime :: Lens.Lens' ImageVersion Core.NominalDiffTime
ivCreationTime = Lens.field @"creationTime"
{-# DEPRECATED ivCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the image the version is based on.
--
-- /Note:/ Consider using 'imageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivImageArn :: Lens.Lens' ImageVersion Types.ImageArn
ivImageArn = Lens.field @"imageArn"
{-# DEPRECATED ivImageArn "Use generic-lens or generic-optics with 'imageArn' instead." #-}

-- | The ARN of the version.
--
-- /Note:/ Consider using 'imageVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivImageVersionArn :: Lens.Lens' ImageVersion Types.ImageVersionArn
ivImageVersionArn = Lens.field @"imageVersionArn"
{-# DEPRECATED ivImageVersionArn "Use generic-lens or generic-optics with 'imageVersionArn' instead." #-}

-- | The status of the version.
--
-- /Note:/ Consider using 'imageVersionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivImageVersionStatus :: Lens.Lens' ImageVersion Types.ImageVersionStatus
ivImageVersionStatus = Lens.field @"imageVersionStatus"
{-# DEPRECATED ivImageVersionStatus "Use generic-lens or generic-optics with 'imageVersionStatus' instead." #-}

-- | When the version was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivLastModifiedTime :: Lens.Lens' ImageVersion Core.NominalDiffTime
ivLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED ivLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivVersion :: Lens.Lens' ImageVersion Core.Natural
ivVersion = Lens.field @"version"
{-# DEPRECATED ivVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | When a create or delete operation fails, the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivFailureReason :: Lens.Lens' ImageVersion (Core.Maybe Types.FailureReason)
ivFailureReason = Lens.field @"failureReason"
{-# DEPRECATED ivFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

instance Core.FromJSON ImageVersion where
  parseJSON =
    Core.withObject "ImageVersion" Core.$
      \x ->
        ImageVersion'
          Core.<$> (x Core..: "CreationTime")
          Core.<*> (x Core..: "ImageArn")
          Core.<*> (x Core..: "ImageVersionArn")
          Core.<*> (x Core..: "ImageVersionStatus")
          Core.<*> (x Core..: "LastModifiedTime")
          Core.<*> (x Core..: "Version")
          Core.<*> (x Core..:? "FailureReason")
