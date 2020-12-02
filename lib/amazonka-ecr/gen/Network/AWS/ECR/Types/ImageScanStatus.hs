{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanStatus where

import Network.AWS.ECR.Types.ScanStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The current status of an image scan.
--
--
--
-- /See:/ 'imageScanStatus' smart constructor.
data ImageScanStatus = ImageScanStatus'
  { _issStatus ::
      !(Maybe ScanStatus),
    _issDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageScanStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'issStatus' - The current state of an image scan.
--
-- * 'issDescription' - The description of the image scan status.
imageScanStatus ::
  ImageScanStatus
imageScanStatus =
  ImageScanStatus' {_issStatus = Nothing, _issDescription = Nothing}

-- | The current state of an image scan.
issStatus :: Lens' ImageScanStatus (Maybe ScanStatus)
issStatus = lens _issStatus (\s a -> s {_issStatus = a})

-- | The description of the image scan status.
issDescription :: Lens' ImageScanStatus (Maybe Text)
issDescription = lens _issDescription (\s a -> s {_issDescription = a})

instance FromJSON ImageScanStatus where
  parseJSON =
    withObject
      "ImageScanStatus"
      ( \x ->
          ImageScanStatus' <$> (x .:? "status") <*> (x .:? "description")
      )

instance Hashable ImageScanStatus

instance NFData ImageScanStatus
