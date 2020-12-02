{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageContainerDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Docker container for the model package.
--
--
--
-- /See:/ 'modelPackageContainerDefinition' smart constructor.
data ModelPackageContainerDefinition = ModelPackageContainerDefinition'
  { _mpcdModelDataURL ::
      !(Maybe Text),
    _mpcdImageDigest ::
      !(Maybe Text),
    _mpcdContainerHostname ::
      !(Maybe Text),
    _mpcdProductId ::
      !(Maybe Text),
    _mpcdImage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelPackageContainerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpcdModelDataURL' - The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
--
-- * 'mpcdImageDigest' - An MD5 hash of the training algorithm that identifies the Docker image used for training.
--
-- * 'mpcdContainerHostname' - The DNS host name for the Docker container.
--
-- * 'mpcdProductId' - The AWS Marketplace product ID of the model package.
--
-- * 'mpcdImage' - The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
modelPackageContainerDefinition ::
  -- | 'mpcdImage'
  Text ->
  ModelPackageContainerDefinition
modelPackageContainerDefinition pImage_ =
  ModelPackageContainerDefinition'
    { _mpcdModelDataURL = Nothing,
      _mpcdImageDigest = Nothing,
      _mpcdContainerHostname = Nothing,
      _mpcdProductId = Nothing,
      _mpcdImage = pImage_
    }

-- | The Amazon S3 path where the model artifacts, which result from model training, are stored. This path must point to a single @gzip@ compressed tar archive (@.tar.gz@ suffix).
mpcdModelDataURL :: Lens' ModelPackageContainerDefinition (Maybe Text)
mpcdModelDataURL = lens _mpcdModelDataURL (\s a -> s {_mpcdModelDataURL = a})

-- | An MD5 hash of the training algorithm that identifies the Docker image used for training.
mpcdImageDigest :: Lens' ModelPackageContainerDefinition (Maybe Text)
mpcdImageDigest = lens _mpcdImageDigest (\s a -> s {_mpcdImageDigest = a})

-- | The DNS host name for the Docker container.
mpcdContainerHostname :: Lens' ModelPackageContainerDefinition (Maybe Text)
mpcdContainerHostname = lens _mpcdContainerHostname (\s a -> s {_mpcdContainerHostname = a})

-- | The AWS Marketplace product ID of the model package.
mpcdProductId :: Lens' ModelPackageContainerDefinition (Maybe Text)
mpcdProductId = lens _mpcdProductId (\s a -> s {_mpcdProductId = a})

-- | The Amazon EC2 Container Registry (Amazon ECR) path where inference code is stored. If you are using your own custom algorithm instead of an algorithm provided by Amazon SageMaker, the inference code must meet Amazon SageMaker requirements. Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
mpcdImage :: Lens' ModelPackageContainerDefinition Text
mpcdImage = lens _mpcdImage (\s a -> s {_mpcdImage = a})

instance FromJSON ModelPackageContainerDefinition where
  parseJSON =
    withObject
      "ModelPackageContainerDefinition"
      ( \x ->
          ModelPackageContainerDefinition'
            <$> (x .:? "ModelDataUrl")
            <*> (x .:? "ImageDigest")
            <*> (x .:? "ContainerHostname")
            <*> (x .:? "ProductId")
            <*> (x .: "Image")
      )

instance Hashable ModelPackageContainerDefinition

instance NFData ModelPackageContainerDefinition

instance ToJSON ModelPackageContainerDefinition where
  toJSON ModelPackageContainerDefinition' {..} =
    object
      ( catMaybes
          [ ("ModelDataUrl" .=) <$> _mpcdModelDataURL,
            ("ImageDigest" .=) <$> _mpcdImageDigest,
            ("ContainerHostname" .=) <$> _mpcdContainerHostname,
            ("ProductId" .=) <$> _mpcdProductId,
            Just ("Image" .= _mpcdImage)
          ]
      )
