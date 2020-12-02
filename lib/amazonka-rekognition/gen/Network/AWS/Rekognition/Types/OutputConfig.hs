{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.OutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.OutputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The S3 bucket and folder location where training output is placed.
--
--
--
-- /See:/ 'outputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { _ocS3KeyPrefix :: !(Maybe Text),
    _ocS3Bucket :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ocS3KeyPrefix' - The prefix applied to the training output files.
--
-- * 'ocS3Bucket' - The S3 bucket where training output is placed.
outputConfig ::
  OutputConfig
outputConfig =
  OutputConfig' {_ocS3KeyPrefix = Nothing, _ocS3Bucket = Nothing}

-- | The prefix applied to the training output files.
ocS3KeyPrefix :: Lens' OutputConfig (Maybe Text)
ocS3KeyPrefix = lens _ocS3KeyPrefix (\s a -> s {_ocS3KeyPrefix = a})

-- | The S3 bucket where training output is placed.
ocS3Bucket :: Lens' OutputConfig (Maybe Text)
ocS3Bucket = lens _ocS3Bucket (\s a -> s {_ocS3Bucket = a})

instance FromJSON OutputConfig where
  parseJSON =
    withObject
      "OutputConfig"
      ( \x ->
          OutputConfig' <$> (x .:? "S3KeyPrefix") <*> (x .:? "S3Bucket")
      )

instance Hashable OutputConfig

instance NFData OutputConfig

instance ToJSON OutputConfig where
  toJSON OutputConfig' {..} =
    object
      ( catMaybes
          [ ("S3KeyPrefix" .=) <$> _ocS3KeyPrefix,
            ("S3Bucket" .=) <$> _ocS3Bucket
          ]
      )
