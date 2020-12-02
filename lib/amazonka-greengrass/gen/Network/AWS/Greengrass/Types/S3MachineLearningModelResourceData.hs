{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData where

import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Attributes that define an Amazon S3 machine learning resource.
--
-- /See:/ 's3MachineLearningModelResourceData' smart constructor.
data S3MachineLearningModelResourceData = S3MachineLearningModelResourceData'
  { _smlmrdOwnerSetting ::
      !( Maybe
           ResourceDownloadOwnerSetting
       ),
    _smlmrdDestinationPath ::
      !(Maybe Text),
    _smlmrdS3URI ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3MachineLearningModelResourceData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smlmrdOwnerSetting' - Undocumented member.
--
-- * 'smlmrdDestinationPath' - The absolute local path of the resource inside the Lambda environment.
--
-- * 'smlmrdS3URI' - The URI of the source model in an S3 bucket. The model package must be in tar.gz or .zip format.
s3MachineLearningModelResourceData ::
  S3MachineLearningModelResourceData
s3MachineLearningModelResourceData =
  S3MachineLearningModelResourceData'
    { _smlmrdOwnerSetting =
        Nothing,
      _smlmrdDestinationPath = Nothing,
      _smlmrdS3URI = Nothing
    }

-- | Undocumented member.
smlmrdOwnerSetting :: Lens' S3MachineLearningModelResourceData (Maybe ResourceDownloadOwnerSetting)
smlmrdOwnerSetting = lens _smlmrdOwnerSetting (\s a -> s {_smlmrdOwnerSetting = a})

-- | The absolute local path of the resource inside the Lambda environment.
smlmrdDestinationPath :: Lens' S3MachineLearningModelResourceData (Maybe Text)
smlmrdDestinationPath = lens _smlmrdDestinationPath (\s a -> s {_smlmrdDestinationPath = a})

-- | The URI of the source model in an S3 bucket. The model package must be in tar.gz or .zip format.
smlmrdS3URI :: Lens' S3MachineLearningModelResourceData (Maybe Text)
smlmrdS3URI = lens _smlmrdS3URI (\s a -> s {_smlmrdS3URI = a})

instance FromJSON S3MachineLearningModelResourceData where
  parseJSON =
    withObject
      "S3MachineLearningModelResourceData"
      ( \x ->
          S3MachineLearningModelResourceData'
            <$> (x .:? "OwnerSetting")
            <*> (x .:? "DestinationPath")
            <*> (x .:? "S3Uri")
      )

instance Hashable S3MachineLearningModelResourceData

instance NFData S3MachineLearningModelResourceData

instance ToJSON S3MachineLearningModelResourceData where
  toJSON S3MachineLearningModelResourceData' {..} =
    object
      ( catMaybes
          [ ("OwnerSetting" .=) <$> _smlmrdOwnerSetting,
            ("DestinationPath" .=) <$> _smlmrdDestinationPath,
            ("S3Uri" .=) <$> _smlmrdS3URI
          ]
      )
