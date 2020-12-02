{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use this to store data store data in an S3 bucket that you manage. When customer-managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
--
--
--
-- /See:/ 'customerManagedDatastoreS3Storage' smart constructor.
data CustomerManagedDatastoreS3Storage = CustomerManagedDatastoreS3Storage'
  { _cmdssKeyPrefix ::
      !(Maybe Text),
    _cmdssBucket :: !Text,
    _cmdssRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomerManagedDatastoreS3Storage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmdssKeyPrefix' - Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
--
-- * 'cmdssBucket' - The name of the S3 bucket in which data store data is stored.
--
-- * 'cmdssRoleARN' - The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
customerManagedDatastoreS3Storage ::
  -- | 'cmdssBucket'
  Text ->
  -- | 'cmdssRoleARN'
  Text ->
  CustomerManagedDatastoreS3Storage
customerManagedDatastoreS3Storage pBucket_ pRoleARN_ =
  CustomerManagedDatastoreS3Storage'
    { _cmdssKeyPrefix = Nothing,
      _cmdssBucket = pBucket_,
      _cmdssRoleARN = pRoleARN_
    }

-- | Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
cmdssKeyPrefix :: Lens' CustomerManagedDatastoreS3Storage (Maybe Text)
cmdssKeyPrefix = lens _cmdssKeyPrefix (\s a -> s {_cmdssKeyPrefix = a})

-- | The name of the S3 bucket in which data store data is stored.
cmdssBucket :: Lens' CustomerManagedDatastoreS3Storage Text
cmdssBucket = lens _cmdssBucket (\s a -> s {_cmdssBucket = a})

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
cmdssRoleARN :: Lens' CustomerManagedDatastoreS3Storage Text
cmdssRoleARN = lens _cmdssRoleARN (\s a -> s {_cmdssRoleARN = a})

instance FromJSON CustomerManagedDatastoreS3Storage where
  parseJSON =
    withObject
      "CustomerManagedDatastoreS3Storage"
      ( \x ->
          CustomerManagedDatastoreS3Storage'
            <$> (x .:? "keyPrefix") <*> (x .: "bucket") <*> (x .: "roleArn")
      )

instance Hashable CustomerManagedDatastoreS3Storage

instance NFData CustomerManagedDatastoreS3Storage

instance ToJSON CustomerManagedDatastoreS3Storage where
  toJSON CustomerManagedDatastoreS3Storage' {..} =
    object
      ( catMaybes
          [ ("keyPrefix" .=) <$> _cmdssKeyPrefix,
            Just ("bucket" .= _cmdssBucket),
            Just ("roleArn" .= _cmdssRoleARN)
          ]
      )
