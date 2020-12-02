{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.BucketPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.BucketPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information on the current bucket policies for the S3 bucket.
--
--
--
-- /See:/ 'bucketPolicy' smart constructor.
data BucketPolicy = BucketPolicy'
  { _bpAllowsPublicWriteAccess ::
      !(Maybe Bool),
    _bpAllowsPublicReadAccess :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BucketPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpAllowsPublicWriteAccess' - A value that indicates whether public write access for the bucket is enabled through a bucket policy.
--
-- * 'bpAllowsPublicReadAccess' - A value that indicates whether public read access for the bucket is enabled through a bucket policy.
bucketPolicy ::
  BucketPolicy
bucketPolicy =
  BucketPolicy'
    { _bpAllowsPublicWriteAccess = Nothing,
      _bpAllowsPublicReadAccess = Nothing
    }

-- | A value that indicates whether public write access for the bucket is enabled through a bucket policy.
bpAllowsPublicWriteAccess :: Lens' BucketPolicy (Maybe Bool)
bpAllowsPublicWriteAccess = lens _bpAllowsPublicWriteAccess (\s a -> s {_bpAllowsPublicWriteAccess = a})

-- | A value that indicates whether public read access for the bucket is enabled through a bucket policy.
bpAllowsPublicReadAccess :: Lens' BucketPolicy (Maybe Bool)
bpAllowsPublicReadAccess = lens _bpAllowsPublicReadAccess (\s a -> s {_bpAllowsPublicReadAccess = a})

instance FromJSON BucketPolicy where
  parseJSON =
    withObject
      "BucketPolicy"
      ( \x ->
          BucketPolicy'
            <$> (x .:? "allowsPublicWriteAccess")
            <*> (x .:? "allowsPublicReadAccess")
      )

instance Hashable BucketPolicy

instance NFData BucketPolicy
