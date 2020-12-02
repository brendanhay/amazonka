{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLS3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLS3DataSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AutoMLS3DataType

-- | The Amazon S3 data source.
--
--
--
-- /See:/ 'autoMLS3DataSource' smart constructor.
data AutoMLS3DataSource = AutoMLS3DataSource'
  { _amlsdsS3DataType ::
      !AutoMLS3DataType,
    _amlsdsS3URI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLS3DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amlsdsS3DataType' - The data type.
--
-- * 'amlsdsS3URI' - The URL to the Amazon S3 data source.
autoMLS3DataSource ::
  -- | 'amlsdsS3DataType'
  AutoMLS3DataType ->
  -- | 'amlsdsS3URI'
  Text ->
  AutoMLS3DataSource
autoMLS3DataSource pS3DataType_ pS3URI_ =
  AutoMLS3DataSource'
    { _amlsdsS3DataType = pS3DataType_,
      _amlsdsS3URI = pS3URI_
    }

-- | The data type.
amlsdsS3DataType :: Lens' AutoMLS3DataSource AutoMLS3DataType
amlsdsS3DataType = lens _amlsdsS3DataType (\s a -> s {_amlsdsS3DataType = a})

-- | The URL to the Amazon S3 data source.
amlsdsS3URI :: Lens' AutoMLS3DataSource Text
amlsdsS3URI = lens _amlsdsS3URI (\s a -> s {_amlsdsS3URI = a})

instance FromJSON AutoMLS3DataSource where
  parseJSON =
    withObject
      "AutoMLS3DataSource"
      ( \x ->
          AutoMLS3DataSource' <$> (x .: "S3DataType") <*> (x .: "S3Uri")
      )

instance Hashable AutoMLS3DataSource

instance NFData AutoMLS3DataSource

instance ToJSON AutoMLS3DataSource where
  toJSON AutoMLS3DataSource' {..} =
    object
      ( catMaybes
          [ Just ("S3DataType" .= _amlsdsS3DataType),
            Just ("S3Uri" .= _amlsdsS3URI)
          ]
      )
