{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PermissionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PermissionConfiguration where

import Network.AWS.GuardDuty.Types.AccountLevelPermissions
import Network.AWS.GuardDuty.Types.BucketLevelPermissions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about how permissions are configured for the S3 bucket.
--
--
--
-- /See:/ 'permissionConfiguration' smart constructor.
data PermissionConfiguration = PermissionConfiguration'
  { _pcBucketLevelPermissions ::
      !(Maybe BucketLevelPermissions),
    _pcAccountLevelPermissions ::
      !(Maybe AccountLevelPermissions)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PermissionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcBucketLevelPermissions' - Contains information about the bucket level permissions for the S3 bucket.
--
-- * 'pcAccountLevelPermissions' - Contains information about the account level permissions on the S3 bucket.
permissionConfiguration ::
  PermissionConfiguration
permissionConfiguration =
  PermissionConfiguration'
    { _pcBucketLevelPermissions = Nothing,
      _pcAccountLevelPermissions = Nothing
    }

-- | Contains information about the bucket level permissions for the S3 bucket.
pcBucketLevelPermissions :: Lens' PermissionConfiguration (Maybe BucketLevelPermissions)
pcBucketLevelPermissions = lens _pcBucketLevelPermissions (\s a -> s {_pcBucketLevelPermissions = a})

-- | Contains information about the account level permissions on the S3 bucket.
pcAccountLevelPermissions :: Lens' PermissionConfiguration (Maybe AccountLevelPermissions)
pcAccountLevelPermissions = lens _pcAccountLevelPermissions (\s a -> s {_pcAccountLevelPermissions = a})

instance FromJSON PermissionConfiguration where
  parseJSON =
    withObject
      "PermissionConfiguration"
      ( \x ->
          PermissionConfiguration'
            <$> (x .:? "bucketLevelPermissions")
            <*> (x .:? "accountLevelPermissions")
      )

instance Hashable PermissionConfiguration

instance NFData PermissionConfiguration
