{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.S3Location where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon S3 location where the source code files provided with the project request are stored.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slBucketKey :: !(Maybe Text),
    _slBucketName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBucketKey' - The Amazon S3 object key where the source code files provided with the project request are stored.
--
-- * 'slBucketName' - The Amazon S3 bucket name where the source code files provided with the project request are stored.
s3Location ::
  S3Location
s3Location =
  S3Location' {_slBucketKey = Nothing, _slBucketName = Nothing}

-- | The Amazon S3 object key where the source code files provided with the project request are stored.
slBucketKey :: Lens' S3Location (Maybe Text)
slBucketKey = lens _slBucketKey (\s a -> s {_slBucketKey = a})

-- | The Amazon S3 bucket name where the source code files provided with the project request are stored.
slBucketName :: Lens' S3Location (Maybe Text)
slBucketName = lens _slBucketName (\s a -> s {_slBucketName = a})

instance Hashable S3Location

instance NFData S3Location

instance ToJSON S3Location where
  toJSON S3Location' {..} =
    object
      ( catMaybes
          [ ("bucketKey" .=) <$> _slBucketKey,
            ("bucketName" .=) <$> _slBucketName
          ]
      )
