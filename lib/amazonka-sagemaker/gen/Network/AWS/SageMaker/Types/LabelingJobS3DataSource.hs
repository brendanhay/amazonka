{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobS3DataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobS3DataSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon S3 location of the input data objects.
--
--
--
-- /See:/ 'labelingJobS3DataSource' smart constructor.
newtype LabelingJobS3DataSource = LabelingJobS3DataSource'
  { _ljsdsManifestS3URI ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobS3DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljsdsManifestS3URI' - The Amazon S3 location of the manifest file that describes the input data objects.
labelingJobS3DataSource ::
  -- | 'ljsdsManifestS3URI'
  Text ->
  LabelingJobS3DataSource
labelingJobS3DataSource pManifestS3URI_ =
  LabelingJobS3DataSource' {_ljsdsManifestS3URI = pManifestS3URI_}

-- | The Amazon S3 location of the manifest file that describes the input data objects.
ljsdsManifestS3URI :: Lens' LabelingJobS3DataSource Text
ljsdsManifestS3URI = lens _ljsdsManifestS3URI (\s a -> s {_ljsdsManifestS3URI = a})

instance FromJSON LabelingJobS3DataSource where
  parseJSON =
    withObject
      "LabelingJobS3DataSource"
      (\x -> LabelingJobS3DataSource' <$> (x .: "ManifestS3Uri"))

instance Hashable LabelingJobS3DataSource

instance NFData LabelingJobS3DataSource

instance ToJSON LabelingJobS3DataSource where
  toJSON LabelingJobS3DataSource' {..} =
    object
      (catMaybes [Just ("ManifestS3Uri" .= _ljsdsManifestS3URI)])
