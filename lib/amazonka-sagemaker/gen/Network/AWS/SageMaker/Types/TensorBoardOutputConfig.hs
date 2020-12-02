{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TensorBoardOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TensorBoardOutputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration of storage locations for TensorBoard output.
--
--
--
-- /See:/ 'tensorBoardOutputConfig' smart constructor.
data TensorBoardOutputConfig = TensorBoardOutputConfig'
  { _tbocLocalPath ::
      !(Maybe Text),
    _tbocS3OutputPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TensorBoardOutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbocLocalPath' - Path to local storage location for tensorBoard output. Defaults to @/opt/ml/output/tensorboard@ .
--
-- * 'tbocS3OutputPath' - Path to Amazon S3 storage location for TensorBoard output.
tensorBoardOutputConfig ::
  -- | 'tbocS3OutputPath'
  Text ->
  TensorBoardOutputConfig
tensorBoardOutputConfig pS3OutputPath_ =
  TensorBoardOutputConfig'
    { _tbocLocalPath = Nothing,
      _tbocS3OutputPath = pS3OutputPath_
    }

-- | Path to local storage location for tensorBoard output. Defaults to @/opt/ml/output/tensorboard@ .
tbocLocalPath :: Lens' TensorBoardOutputConfig (Maybe Text)
tbocLocalPath = lens _tbocLocalPath (\s a -> s {_tbocLocalPath = a})

-- | Path to Amazon S3 storage location for TensorBoard output.
tbocS3OutputPath :: Lens' TensorBoardOutputConfig Text
tbocS3OutputPath = lens _tbocS3OutputPath (\s a -> s {_tbocS3OutputPath = a})

instance FromJSON TensorBoardOutputConfig where
  parseJSON =
    withObject
      "TensorBoardOutputConfig"
      ( \x ->
          TensorBoardOutputConfig'
            <$> (x .:? "LocalPath") <*> (x .: "S3OutputPath")
      )

instance Hashable TensorBoardOutputConfig

instance NFData TensorBoardOutputConfig

instance ToJSON TensorBoardOutputConfig where
  toJSON TensorBoardOutputConfig' {..} =
    object
      ( catMaybes
          [ ("LocalPath" .=) <$> _tbocLocalPath,
            Just ("S3OutputPath" .= _tbocS3OutputPath)
          ]
      )
