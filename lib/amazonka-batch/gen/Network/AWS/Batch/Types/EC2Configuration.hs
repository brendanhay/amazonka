{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.EC2Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.EC2Configuration
  ( EC2Configuration (..),

    -- * Smart constructor
    mkEC2Configuration,

    -- * Lenses
    ecImageIdOverride,
    ecImageType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information used to select Amazon Machine Images (AMIs) for instances in the compute environment. If the @Ec2Configuration@ is not specified, the default is @ECS_AL1@ .
--
-- /See:/ 'mkEC2Configuration' smart constructor.
data EC2Configuration = EC2Configuration'
  { imageIdOverride ::
      Lude.Maybe Lude.Text,
    imageType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2Configuration' with the minimum fields required to make a request.
--
-- * 'imageIdOverride' - The AMI ID used for instances launched in the compute environment that match the image type. This setting overrides the @imageId@ set in the @computeResource@ object.
-- * 'imageType' - The image type to match with the instance type to pick an AMI. If the @imageIdOverride@ parameter is not specified, then a recent <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized AMI> will be used.
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
mkEC2Configuration ::
  -- | 'imageType'
  Lude.Text ->
  EC2Configuration
mkEC2Configuration pImageType_ =
  EC2Configuration'
    { imageIdOverride = Lude.Nothing,
      imageType = pImageType_
    }

-- | The AMI ID used for instances launched in the compute environment that match the image type. This setting overrides the @imageId@ set in the @computeResource@ object.
--
-- /Note:/ Consider using 'imageIdOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecImageIdOverride :: Lens.Lens' EC2Configuration (Lude.Maybe Lude.Text)
ecImageIdOverride = Lens.lens (imageIdOverride :: EC2Configuration -> Lude.Maybe Lude.Text) (\s a -> s {imageIdOverride = a} :: EC2Configuration)
{-# DEPRECATED ecImageIdOverride "Use generic-lens or generic-optics with 'imageIdOverride' instead." #-}

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
ecImageType :: Lens.Lens' EC2Configuration Lude.Text
ecImageType = Lens.lens (imageType :: EC2Configuration -> Lude.Text) (\s a -> s {imageType = a} :: EC2Configuration)
{-# DEPRECATED ecImageType "Use generic-lens or generic-optics with 'imageType' instead." #-}

instance Lude.FromJSON EC2Configuration where
  parseJSON =
    Lude.withObject
      "EC2Configuration"
      ( \x ->
          EC2Configuration'
            Lude.<$> (x Lude..:? "imageIdOverride") Lude.<*> (x Lude..: "imageType")
      )

instance Lude.ToJSON EC2Configuration where
  toJSON EC2Configuration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("imageIdOverride" Lude..=) Lude.<$> imageIdOverride,
            Lude.Just ("imageType" Lude..= imageType)
          ]
      )
