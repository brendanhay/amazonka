{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformDataSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.TransformS3DataSource

-- | Describes the location of the channel data.
--
--
--
-- /See:/ 'transformDataSource' smart constructor.
newtype TransformDataSource = TransformDataSource'
  { _tdsS3DataSource ::
      TransformS3DataSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdsS3DataSource' - The S3 location of the data source that is associated with a channel.
transformDataSource ::
  -- | 'tdsS3DataSource'
  TransformS3DataSource ->
  TransformDataSource
transformDataSource pS3DataSource_ =
  TransformDataSource' {_tdsS3DataSource = pS3DataSource_}

-- | The S3 location of the data source that is associated with a channel.
tdsS3DataSource :: Lens' TransformDataSource TransformS3DataSource
tdsS3DataSource = lens _tdsS3DataSource (\s a -> s {_tdsS3DataSource = a})

instance FromJSON TransformDataSource where
  parseJSON =
    withObject
      "TransformDataSource"
      (\x -> TransformDataSource' <$> (x .: "S3DataSource"))

instance Hashable TransformDataSource

instance NFData TransformDataSource

instance ToJSON TransformDataSource where
  toJSON TransformDataSource' {..} =
    object (catMaybes [Just ("S3DataSource" .= _tdsS3DataSource)])
