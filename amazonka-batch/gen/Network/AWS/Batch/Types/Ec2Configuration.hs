{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Ec2Configuration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Ec2Configuration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information used to select Amazon Machine Images (AMIs) for
-- instances in the compute environment. If the @Ec2Configuration@ isn\'t
-- specified, the default is @ECS_AL1@.
--
-- This object isn\'t applicable to jobs running on Fargate resources.
--
-- /See:/ 'newEc2Configuration' smart constructor.
data Ec2Configuration = Ec2Configuration'
  { -- | The AMI ID used for instances launched in the compute environment that
    -- match the image type. This setting overrides the @imageId@ set in the
    -- @computeResource@ object.
    imageIdOverride :: Prelude.Maybe Prelude.Text,
    -- | The image type to match with the instance type to select an AMI. If the
    -- @imageIdOverride@ parameter isn\'t specified, then a recent
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized AMI>
    -- is used.
    --
    -- [ECS_AL2]
    --     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#al2ami Amazon Linux 2>−
    --     Default for all AWS Graviton-based instance families (for example,
    --     @C6g@, @M6g@, @R6g@, and @T4g@) and can be used for all non-GPU
    --     instance types.
    --
    -- [ECS_AL2_NVIDIA]
    --     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#gpuami Amazon Linux 2 (GPU)>−Default
    --     for all GPU instance families (for example @P4@ and @G4@) and can be
    --     used for all non-AWS Graviton-based instance types.
    --
    -- [ECS_AL1]
    --     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#alami Amazon Linux>−Default
    --     for all non-GPU, non-AWS Graviton instance families. Amazon Linux is
    --     reaching the end-of-life of standard support. For more information,
    --     see <http://aws.amazon.com/amazon-linux-ami/ Amazon Linux AMI>.
    imageType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Ec2Configuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageIdOverride', 'ec2Configuration_imageIdOverride' - The AMI ID used for instances launched in the compute environment that
-- match the image type. This setting overrides the @imageId@ set in the
-- @computeResource@ object.
--
-- 'imageType', 'ec2Configuration_imageType' - The image type to match with the instance type to select an AMI. If the
-- @imageIdOverride@ parameter isn\'t specified, then a recent
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized AMI>
-- is used.
--
-- [ECS_AL2]
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#al2ami Amazon Linux 2>−
--     Default for all AWS Graviton-based instance families (for example,
--     @C6g@, @M6g@, @R6g@, and @T4g@) and can be used for all non-GPU
--     instance types.
--
-- [ECS_AL2_NVIDIA]
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#gpuami Amazon Linux 2 (GPU)>−Default
--     for all GPU instance families (for example @P4@ and @G4@) and can be
--     used for all non-AWS Graviton-based instance types.
--
-- [ECS_AL1]
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#alami Amazon Linux>−Default
--     for all non-GPU, non-AWS Graviton instance families. Amazon Linux is
--     reaching the end-of-life of standard support. For more information,
--     see <http://aws.amazon.com/amazon-linux-ami/ Amazon Linux AMI>.
newEc2Configuration ::
  -- | 'imageType'
  Prelude.Text ->
  Ec2Configuration
newEc2Configuration pImageType_ =
  Ec2Configuration'
    { imageIdOverride =
        Prelude.Nothing,
      imageType = pImageType_
    }

-- | The AMI ID used for instances launched in the compute environment that
-- match the image type. This setting overrides the @imageId@ set in the
-- @computeResource@ object.
ec2Configuration_imageIdOverride :: Lens.Lens' Ec2Configuration (Prelude.Maybe Prelude.Text)
ec2Configuration_imageIdOverride = Lens.lens (\Ec2Configuration' {imageIdOverride} -> imageIdOverride) (\s@Ec2Configuration' {} a -> s {imageIdOverride = a} :: Ec2Configuration)

-- | The image type to match with the instance type to select an AMI. If the
-- @imageIdOverride@ parameter isn\'t specified, then a recent
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized AMI>
-- is used.
--
-- [ECS_AL2]
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#al2ami Amazon Linux 2>−
--     Default for all AWS Graviton-based instance families (for example,
--     @C6g@, @M6g@, @R6g@, and @T4g@) and can be used for all non-GPU
--     instance types.
--
-- [ECS_AL2_NVIDIA]
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#gpuami Amazon Linux 2 (GPU)>−Default
--     for all GPU instance families (for example @P4@ and @G4@) and can be
--     used for all non-AWS Graviton-based instance types.
--
-- [ECS_AL1]
--     <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#alami Amazon Linux>−Default
--     for all non-GPU, non-AWS Graviton instance families. Amazon Linux is
--     reaching the end-of-life of standard support. For more information,
--     see <http://aws.amazon.com/amazon-linux-ami/ Amazon Linux AMI>.
ec2Configuration_imageType :: Lens.Lens' Ec2Configuration Prelude.Text
ec2Configuration_imageType = Lens.lens (\Ec2Configuration' {imageType} -> imageType) (\s@Ec2Configuration' {} a -> s {imageType = a} :: Ec2Configuration)

instance Prelude.FromJSON Ec2Configuration where
  parseJSON =
    Prelude.withObject
      "Ec2Configuration"
      ( \x ->
          Ec2Configuration'
            Prelude.<$> (x Prelude..:? "imageIdOverride")
            Prelude.<*> (x Prelude..: "imageType")
      )

instance Prelude.Hashable Ec2Configuration

instance Prelude.NFData Ec2Configuration

instance Prelude.ToJSON Ec2Configuration where
  toJSON Ec2Configuration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("imageIdOverride" Prelude..=)
              Prelude.<$> imageIdOverride,
            Prelude.Just ("imageType" Prelude..= imageType)
          ]
      )
