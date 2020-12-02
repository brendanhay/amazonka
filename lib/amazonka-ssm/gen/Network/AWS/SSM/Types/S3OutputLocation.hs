{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.S3OutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.S3OutputLocation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 's3OutputLocation' smart constructor.
data S3OutputLocation = S3OutputLocation'
  { _solOutputS3KeyPrefix ::
      !(Maybe Text),
    _solOutputS3Region :: !(Maybe Text),
    _solOutputS3BucketName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3OutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'solOutputS3KeyPrefix' - The S3 bucket subfolder.
--
-- * 'solOutputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
--
-- * 'solOutputS3BucketName' - The name of the S3 bucket.
s3OutputLocation ::
  S3OutputLocation
s3OutputLocation =
  S3OutputLocation'
    { _solOutputS3KeyPrefix = Nothing,
      _solOutputS3Region = Nothing,
      _solOutputS3BucketName = Nothing
    }

-- | The S3 bucket subfolder.
solOutputS3KeyPrefix :: Lens' S3OutputLocation (Maybe Text)
solOutputS3KeyPrefix = lens _solOutputS3KeyPrefix (\s a -> s {_solOutputS3KeyPrefix = a})

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
solOutputS3Region :: Lens' S3OutputLocation (Maybe Text)
solOutputS3Region = lens _solOutputS3Region (\s a -> s {_solOutputS3Region = a})

-- | The name of the S3 bucket.
solOutputS3BucketName :: Lens' S3OutputLocation (Maybe Text)
solOutputS3BucketName = lens _solOutputS3BucketName (\s a -> s {_solOutputS3BucketName = a})

instance FromJSON S3OutputLocation where
  parseJSON =
    withObject
      "S3OutputLocation"
      ( \x ->
          S3OutputLocation'
            <$> (x .:? "OutputS3KeyPrefix")
            <*> (x .:? "OutputS3Region")
            <*> (x .:? "OutputS3BucketName")
      )

instance Hashable S3OutputLocation

instance NFData S3OutputLocation

instance ToJSON S3OutputLocation where
  toJSON S3OutputLocation' {..} =
    object
      ( catMaybes
          [ ("OutputS3KeyPrefix" .=) <$> _solOutputS3KeyPrefix,
            ("OutputS3Region" .=) <$> _solOutputS3Region,
            ("OutputS3BucketName" .=) <$> _solOutputS3BucketName
          ]
      )
