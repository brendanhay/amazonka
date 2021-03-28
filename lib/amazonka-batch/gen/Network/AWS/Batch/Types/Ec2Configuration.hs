{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Ec2Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.Ec2Configuration
  ( Ec2Configuration (..)
  -- * Smart constructor
  , mkEc2Configuration
  -- * Lenses
  , ecImageType
  , ecImageIdOverride
  ) where

import qualified Network.AWS.Batch.Types.ImageIdOverride as Types
import qualified Network.AWS.Batch.Types.ImageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information used to select Amazon Machine Images (AMIs) for instances in the compute environment. If the @Ec2Configuration@ is not specified, the default is @ECS_AL1@ .
--
-- /See:/ 'mkEc2Configuration' smart constructor.
data Ec2Configuration = Ec2Configuration'
  { imageType :: Types.ImageType
    -- ^ The image type to match with the instance type to pick an AMI. If the @imageIdOverride@ parameter is not specified, then a recent <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized AMI> will be used.
--
--
--     * ECS_AL2
--
--     * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#al2ami Amazon Linux 2> − Default for all AWS Graviton-based instance families (for example, @C6g@ , @M6g@ , @R6g@ , and @T4g@ ) and can be used for all non-GPU instance types.
--
--
--     * ECS_AL2_NVIDIA
--
--     * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#gpuami Amazon Linux 2 (GPU)> −Default for all GPU instance families (for example @P4@ and @G4@ ) and can be used for all non-AWS Graviton-based instance types.
--
--
--     * ECS_AL1
--
--     * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#alami Amazon Linux> −Default for all non-GPU, non-AWS-Graviton instance families. Amazon Linux is reaching the end-of-life of standard support. For more information, see <https://aws.amazon.com/amazon-linux-ami/ Amazon Linux AMI> .
--
--
  , imageIdOverride :: Core.Maybe Types.ImageIdOverride
    -- ^ The AMI ID used for instances launched in the compute environment that match the image type. This setting overrides the @imageId@ set in the @computeResource@ object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ec2Configuration' value with any optional fields omitted.
mkEc2Configuration
    :: Types.ImageType -- ^ 'imageType'
    -> Ec2Configuration
mkEc2Configuration imageType
  = Ec2Configuration'{imageType, imageIdOverride = Core.Nothing}

-- | The image type to match with the instance type to pick an AMI. If the @imageIdOverride@ parameter is not specified, then a recent <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized AMI> will be used.
--
--
--     * ECS_AL2
--
--     * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#al2ami Amazon Linux 2> − Default for all AWS Graviton-based instance families (for example, @C6g@ , @M6g@ , @R6g@ , and @T4g@ ) and can be used for all non-GPU instance types.
--
--
--     * ECS_AL2_NVIDIA
--
--     * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#gpuami Amazon Linux 2 (GPU)> −Default for all GPU instance families (for example @P4@ and @G4@ ) and can be used for all non-AWS Graviton-based instance types.
--
--
--     * ECS_AL1
--
--     * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#alami Amazon Linux> −Default for all non-GPU, non-AWS-Graviton instance families. Amazon Linux is reaching the end-of-life of standard support. For more information, see <https://aws.amazon.com/amazon-linux-ami/ Amazon Linux AMI> .
--
--
--
-- /Note:/ Consider using 'imageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecImageType :: Lens.Lens' Ec2Configuration Types.ImageType
ecImageType = Lens.field @"imageType"
{-# INLINEABLE ecImageType #-}
{-# DEPRECATED imageType "Use generic-lens or generic-optics with 'imageType' instead"  #-}

-- | The AMI ID used for instances launched in the compute environment that match the image type. This setting overrides the @imageId@ set in the @computeResource@ object.
--
-- /Note:/ Consider using 'imageIdOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecImageIdOverride :: Lens.Lens' Ec2Configuration (Core.Maybe Types.ImageIdOverride)
ecImageIdOverride = Lens.field @"imageIdOverride"
{-# INLINEABLE ecImageIdOverride #-}
{-# DEPRECATED imageIdOverride "Use generic-lens or generic-optics with 'imageIdOverride' instead"  #-}

instance Core.FromJSON Ec2Configuration where
        toJSON Ec2Configuration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("imageType" Core..= imageType),
                  ("imageIdOverride" Core..=) Core.<$> imageIdOverride])

instance Core.FromJSON Ec2Configuration where
        parseJSON
          = Core.withObject "Ec2Configuration" Core.$
              \ x ->
                Ec2Configuration' Core.<$>
                  (x Core..: "imageType") Core.<*> x Core..:? "imageIdOverride"
