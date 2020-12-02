{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.EC2Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.EC2Configuration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information used to select Amazon Machine Images (AMIs) for instances in the compute environment. If the @Ec2Configuration@ is not specified, the default is @ECS_AL1@ .
--
--
--
-- /See:/ 'ec2Configuration' smart constructor.
data EC2Configuration = EC2Configuration'
  { _ecImageIdOverride ::
      !(Maybe Text),
    _ecImageType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecImageIdOverride' - The AMI ID used for instances launched in the compute environment that match the image type. This setting overrides the @imageId@ set in the @computeResource@ object.
--
-- * 'ecImageType' - The image type to match with the instance type to pick an AMI. If the @imageIdOverride@ parameter is not specified, then a recent <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized AMI> will be used.     * ECS_AL2    * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#al2ami Amazon Linux 2> − Default for all AWS Graviton-based instance families (for example, @C6g@ , @M6g@ , @R6g@ , and @T4g@ ) and can be used for all non-GPU instance types.     * ECS_AL2_NVIDIA    * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#gpuami Amazon Linux 2 (GPU)> −Default for all GPU instance families (for example @P4@ and @G4@ ) and can be used for all non-AWS Graviton-based instance types.     * ECS_AL1    * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#alami Amazon Linux> −Default for all non-GPU, non-AWS-Graviton instance families. Amazon Linux is reaching the end-of-life of standard support. For more information, see <https://aws.amazon.com/amazon-linux-ami/ Amazon Linux AMI> .
ec2Configuration ::
  -- | 'ecImageType'
  Text ->
  EC2Configuration
ec2Configuration pImageType_ =
  EC2Configuration'
    { _ecImageIdOverride = Nothing,
      _ecImageType = pImageType_
    }

-- | The AMI ID used for instances launched in the compute environment that match the image type. This setting overrides the @imageId@ set in the @computeResource@ object.
ecImageIdOverride :: Lens' EC2Configuration (Maybe Text)
ecImageIdOverride = lens _ecImageIdOverride (\s a -> s {_ecImageIdOverride = a})

-- | The image type to match with the instance type to pick an AMI. If the @imageIdOverride@ parameter is not specified, then a recent <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html Amazon ECS-optimized AMI> will be used.     * ECS_AL2    * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#al2ami Amazon Linux 2> − Default for all AWS Graviton-based instance families (for example, @C6g@ , @M6g@ , @R6g@ , and @T4g@ ) and can be used for all non-GPU instance types.     * ECS_AL2_NVIDIA    * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#gpuami Amazon Linux 2 (GPU)> −Default for all GPU instance families (for example @P4@ and @G4@ ) and can be used for all non-AWS Graviton-based instance types.     * ECS_AL1    * <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-optimized_AMI.html#alami Amazon Linux> −Default for all non-GPU, non-AWS-Graviton instance families. Amazon Linux is reaching the end-of-life of standard support. For more information, see <https://aws.amazon.com/amazon-linux-ami/ Amazon Linux AMI> .
ecImageType :: Lens' EC2Configuration Text
ecImageType = lens _ecImageType (\s a -> s {_ecImageType = a})

instance FromJSON EC2Configuration where
  parseJSON =
    withObject
      "EC2Configuration"
      ( \x ->
          EC2Configuration'
            <$> (x .:? "imageIdOverride") <*> (x .: "imageType")
      )

instance Hashable EC2Configuration

instance NFData EC2Configuration

instance ToJSON EC2Configuration where
  toJSON EC2Configuration' {..} =
    object
      ( catMaybes
          [ ("imageIdOverride" .=) <$> _ecImageIdOverride,
            Just ("imageType" .= _ecImageType)
          ]
      )
