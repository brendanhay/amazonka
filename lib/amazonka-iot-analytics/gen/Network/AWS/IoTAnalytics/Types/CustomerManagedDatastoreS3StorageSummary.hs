{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to store data store data in an S3 bucket that you manage.
--
--
--
-- /See:/ 'customerManagedDatastoreS3StorageSummary' smart constructor.
data CustomerManagedDatastoreS3StorageSummary = CustomerManagedDatastoreS3StorageSummary'
  { _cmdsssBucket ::
      !( Maybe
           Text
       ),
    _cmdsssKeyPrefix ::
      !( Maybe
           Text
       ),
    _cmdsssRoleARN ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomerManagedDatastoreS3StorageSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmdsssBucket' - The name of the S3 bucket in which data store data is stored.
--
-- * 'cmdsssKeyPrefix' - Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
--
-- * 'cmdsssRoleARN' - The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
customerManagedDatastoreS3StorageSummary ::
  CustomerManagedDatastoreS3StorageSummary
customerManagedDatastoreS3StorageSummary =
  CustomerManagedDatastoreS3StorageSummary'
    { _cmdsssBucket =
        Nothing,
      _cmdsssKeyPrefix = Nothing,
      _cmdsssRoleARN = Nothing
    }

-- | The name of the S3 bucket in which data store data is stored.
cmdsssBucket :: Lens' CustomerManagedDatastoreS3StorageSummary (Maybe Text)
cmdsssBucket = lens _cmdsssBucket (\s a -> s {_cmdsssBucket = a})

-- | Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
cmdsssKeyPrefix :: Lens' CustomerManagedDatastoreS3StorageSummary (Maybe Text)
cmdsssKeyPrefix = lens _cmdsssKeyPrefix (\s a -> s {_cmdsssKeyPrefix = a})

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
cmdsssRoleARN :: Lens' CustomerManagedDatastoreS3StorageSummary (Maybe Text)
cmdsssRoleARN = lens _cmdsssRoleARN (\s a -> s {_cmdsssRoleARN = a})

instance FromJSON CustomerManagedDatastoreS3StorageSummary where
  parseJSON =
    withObject
      "CustomerManagedDatastoreS3StorageSummary"
      ( \x ->
          CustomerManagedDatastoreS3StorageSummary'
            <$> (x .:? "bucket") <*> (x .:? "keyPrefix") <*> (x .:? "roleArn")
      )

instance Hashable CustomerManagedDatastoreS3StorageSummary

instance NFData CustomerManagedDatastoreS3StorageSummary
