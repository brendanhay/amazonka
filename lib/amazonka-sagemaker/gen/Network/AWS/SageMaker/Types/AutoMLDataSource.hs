{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLDataSource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AutoMLS3DataSource

-- | The data source for the Autopilot job.
--
--
--
-- /See:/ 'autoMLDataSource' smart constructor.
newtype AutoMLDataSource = AutoMLDataSource'
  { _amldsS3DataSource ::
      AutoMLS3DataSource
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amldsS3DataSource' - The Amazon S3 location of the input data.
autoMLDataSource ::
  -- | 'amldsS3DataSource'
  AutoMLS3DataSource ->
  AutoMLDataSource
autoMLDataSource pS3DataSource_ =
  AutoMLDataSource' {_amldsS3DataSource = pS3DataSource_}

-- | The Amazon S3 location of the input data.
amldsS3DataSource :: Lens' AutoMLDataSource AutoMLS3DataSource
amldsS3DataSource = lens _amldsS3DataSource (\s a -> s {_amldsS3DataSource = a})

instance FromJSON AutoMLDataSource where
  parseJSON =
    withObject
      "AutoMLDataSource"
      (\x -> AutoMLDataSource' <$> (x .: "S3DataSource"))

instance Hashable AutoMLDataSource

instance NFData AutoMLDataSource

instance ToJSON AutoMLDataSource where
  toJSON AutoMLDataSource' {..} =
    object (catMaybes [Just ("S3DataSource" .= _amldsS3DataSource)])
