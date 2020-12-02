{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DeployedImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DeployedImage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Gets the Amazon EC2 Container Registry path of the docker image of the model that is hosted in this 'ProductionVariant' .
--
--
-- If you used the @registry/repository[:tag]@ form to specify the image path of the primary container when you created the model hosted in this @ProductionVariant@ , the path resolves to a path of the form @registry/repository[@digest]@ . A digest is a hash value that identifies a specific version of an image. For information about Amazon ECR paths, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/docker-pull-ecr-image.html Pulling an Image> in the /Amazon ECR User Guide/ .
--
--
-- /See:/ 'deployedImage' smart constructor.
data DeployedImage = DeployedImage'
  { _diResolvedImage ::
      !(Maybe Text),
    _diSpecifiedImage :: !(Maybe Text),
    _diResolutionTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeployedImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diResolvedImage' - The specific digest path of the image hosted in this @ProductionVariant@ .
--
-- * 'diSpecifiedImage' - The image path you specified when you created the model.
--
-- * 'diResolutionTime' - The date and time when the image path for the model resolved to the @ResolvedImage@
deployedImage ::
  DeployedImage
deployedImage =
  DeployedImage'
    { _diResolvedImage = Nothing,
      _diSpecifiedImage = Nothing,
      _diResolutionTime = Nothing
    }

-- | The specific digest path of the image hosted in this @ProductionVariant@ .
diResolvedImage :: Lens' DeployedImage (Maybe Text)
diResolvedImage = lens _diResolvedImage (\s a -> s {_diResolvedImage = a})

-- | The image path you specified when you created the model.
diSpecifiedImage :: Lens' DeployedImage (Maybe Text)
diSpecifiedImage = lens _diSpecifiedImage (\s a -> s {_diSpecifiedImage = a})

-- | The date and time when the image path for the model resolved to the @ResolvedImage@
diResolutionTime :: Lens' DeployedImage (Maybe UTCTime)
diResolutionTime = lens _diResolutionTime (\s a -> s {_diResolutionTime = a}) . mapping _Time

instance FromJSON DeployedImage where
  parseJSON =
    withObject
      "DeployedImage"
      ( \x ->
          DeployedImage'
            <$> (x .:? "ResolvedImage")
            <*> (x .:? "SpecifiedImage")
            <*> (x .:? "ResolutionTime")
      )

instance Hashable DeployedImage

instance NFData DeployedImage
