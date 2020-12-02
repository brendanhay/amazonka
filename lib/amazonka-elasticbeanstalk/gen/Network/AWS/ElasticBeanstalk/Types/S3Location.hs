{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.S3Location where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The bucket and key of an item stored in Amazon S3.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slS3Key :: !(Maybe Text),
    _slS3Bucket :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slS3Key' - The Amazon S3 key where the data is located.
--
-- * 'slS3Bucket' - The Amazon S3 bucket where the data is located.
s3Location ::
  S3Location
s3Location = S3Location' {_slS3Key = Nothing, _slS3Bucket = Nothing}

-- | The Amazon S3 key where the data is located.
slS3Key :: Lens' S3Location (Maybe Text)
slS3Key = lens _slS3Key (\s a -> s {_slS3Key = a})

-- | The Amazon S3 bucket where the data is located.
slS3Bucket :: Lens' S3Location (Maybe Text)
slS3Bucket = lens _slS3Bucket (\s a -> s {_slS3Bucket = a})

instance FromXML S3Location where
  parseXML x = S3Location' <$> (x .@? "S3Key") <*> (x .@? "S3Bucket")

instance Hashable S3Location

instance NFData S3Location

instance ToQuery S3Location where
  toQuery S3Location' {..} =
    mconcat ["S3Key" =: _slS3Key, "S3Bucket" =: _slS3Bucket]
