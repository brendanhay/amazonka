{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.BucketLevelPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.BucketLevelPermissions where

import Network.AWS.GuardDuty.Types.AccessControlList
import Network.AWS.GuardDuty.Types.BlockPublicAccess
import Network.AWS.GuardDuty.Types.BucketPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the bucket level permissions for the S3 bucket.
--
--
--
-- /See:/ 'bucketLevelPermissions' smart constructor.
data BucketLevelPermissions = BucketLevelPermissions'
  { _blpAccessControlList ::
      !(Maybe AccessControlList),
    _blpBlockPublicAccess ::
      !(Maybe BlockPublicAccess),
    _blpBucketPolicy :: !(Maybe BucketPolicy)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BucketLevelPermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blpAccessControlList' - Contains information on how Access Control Policies are applied to the bucket.
--
-- * 'blpBlockPublicAccess' - Contains information on which account level S3 Block Public Access settings are applied to the S3 bucket.
--
-- * 'blpBucketPolicy' - Contains information on the bucket policies for the S3 bucket.
bucketLevelPermissions ::
  BucketLevelPermissions
bucketLevelPermissions =
  BucketLevelPermissions'
    { _blpAccessControlList = Nothing,
      _blpBlockPublicAccess = Nothing,
      _blpBucketPolicy = Nothing
    }

-- | Contains information on how Access Control Policies are applied to the bucket.
blpAccessControlList :: Lens' BucketLevelPermissions (Maybe AccessControlList)
blpAccessControlList = lens _blpAccessControlList (\s a -> s {_blpAccessControlList = a})

-- | Contains information on which account level S3 Block Public Access settings are applied to the S3 bucket.
blpBlockPublicAccess :: Lens' BucketLevelPermissions (Maybe BlockPublicAccess)
blpBlockPublicAccess = lens _blpBlockPublicAccess (\s a -> s {_blpBlockPublicAccess = a})

-- | Contains information on the bucket policies for the S3 bucket.
blpBucketPolicy :: Lens' BucketLevelPermissions (Maybe BucketPolicy)
blpBucketPolicy = lens _blpBucketPolicy (\s a -> s {_blpBucketPolicy = a})

instance FromJSON BucketLevelPermissions where
  parseJSON =
    withObject
      "BucketLevelPermissions"
      ( \x ->
          BucketLevelPermissions'
            <$> (x .:? "accessControlList")
            <*> (x .:? "blockPublicAccess")
            <*> (x .:? "bucketPolicy")
      )

instance Hashable BucketLevelPermissions

instance NFData BucketLevelPermissions
