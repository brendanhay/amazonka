{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CheckpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CheckpointConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the output location for managed spot training checkpoint data.
--
--
--
-- /See:/ 'checkpointConfig' smart constructor.
data CheckpointConfig = CheckpointConfig'
  { _ccLocalPath ::
      !(Maybe Text),
    _ccS3URI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CheckpointConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccLocalPath' - (Optional) The local directory where checkpoints are written. The default directory is @/opt/ml/checkpoints/@ .
--
-- * 'ccS3URI' - Identifies the S3 path where you want Amazon SageMaker to store checkpoints. For example, @s3://bucket-name/key-name-prefix@ .
checkpointConfig ::
  -- | 'ccS3URI'
  Text ->
  CheckpointConfig
checkpointConfig pS3URI_ =
  CheckpointConfig' {_ccLocalPath = Nothing, _ccS3URI = pS3URI_}

-- | (Optional) The local directory where checkpoints are written. The default directory is @/opt/ml/checkpoints/@ .
ccLocalPath :: Lens' CheckpointConfig (Maybe Text)
ccLocalPath = lens _ccLocalPath (\s a -> s {_ccLocalPath = a})

-- | Identifies the S3 path where you want Amazon SageMaker to store checkpoints. For example, @s3://bucket-name/key-name-prefix@ .
ccS3URI :: Lens' CheckpointConfig Text
ccS3URI = lens _ccS3URI (\s a -> s {_ccS3URI = a})

instance FromJSON CheckpointConfig where
  parseJSON =
    withObject
      "CheckpointConfig"
      ( \x ->
          CheckpointConfig' <$> (x .:? "LocalPath") <*> (x .: "S3Uri")
      )

instance Hashable CheckpointConfig

instance NFData CheckpointConfig

instance ToJSON CheckpointConfig where
  toJSON CheckpointConfig' {..} =
    object
      ( catMaybes
          [("LocalPath" .=) <$> _ccLocalPath, Just ("S3Uri" .= _ccS3URI)]
      )
