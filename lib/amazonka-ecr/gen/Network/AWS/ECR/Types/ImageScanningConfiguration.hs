{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanningConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The image scanning configuration for a repository.
--
--
--
-- /See:/ 'imageScanningConfiguration' smart constructor.
newtype ImageScanningConfiguration = ImageScanningConfiguration'
  { _iscScanOnPush ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageScanningConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iscScanOnPush' - The setting that determines whether images are scanned after being pushed to a repository. If set to @true@ , images will be scanned after being pushed. If this parameter is not specified, it will default to @false@ and images will not be scanned unless a scan is manually started with the 'StartImageScan' API.
imageScanningConfiguration ::
  ImageScanningConfiguration
imageScanningConfiguration =
  ImageScanningConfiguration' {_iscScanOnPush = Nothing}

-- | The setting that determines whether images are scanned after being pushed to a repository. If set to @true@ , images will be scanned after being pushed. If this parameter is not specified, it will default to @false@ and images will not be scanned unless a scan is manually started with the 'StartImageScan' API.
iscScanOnPush :: Lens' ImageScanningConfiguration (Maybe Bool)
iscScanOnPush = lens _iscScanOnPush (\s a -> s {_iscScanOnPush = a})

instance FromJSON ImageScanningConfiguration where
  parseJSON =
    withObject
      "ImageScanningConfiguration"
      (\x -> ImageScanningConfiguration' <$> (x .:? "scanOnPush"))

instance Hashable ImageScanningConfiguration

instance NFData ImageScanningConfiguration

instance ToJSON ImageScanningConfiguration where
  toJSON ImageScanningConfiguration' {..} =
    object (catMaybes [("scanOnPush" .=) <$> _iscScanOnPush])
