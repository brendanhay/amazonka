{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to store channel data in an S3 bucket that you manage.
--
--
--
-- /See:/ 'customerManagedChannelS3StorageSummary' smart constructor.
data CustomerManagedChannelS3StorageSummary = CustomerManagedChannelS3StorageSummary'
  { _cmcsssBucket ::
      !(Maybe Text),
    _cmcsssKeyPrefix ::
      !(Maybe Text),
    _cmcsssRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomerManagedChannelS3StorageSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmcsssBucket' - The name of the S3 bucket in which channel data is stored.
--
-- * 'cmcsssKeyPrefix' - Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). The prefix must end with a forward slash (/).
--
-- * 'cmcsssRoleARN' - The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
customerManagedChannelS3StorageSummary ::
  CustomerManagedChannelS3StorageSummary
customerManagedChannelS3StorageSummary =
  CustomerManagedChannelS3StorageSummary'
    { _cmcsssBucket = Nothing,
      _cmcsssKeyPrefix = Nothing,
      _cmcsssRoleARN = Nothing
    }

-- | The name of the S3 bucket in which channel data is stored.
cmcsssBucket :: Lens' CustomerManagedChannelS3StorageSummary (Maybe Text)
cmcsssBucket = lens _cmcsssBucket (\s a -> s {_cmcsssBucket = a})

-- | Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). The prefix must end with a forward slash (/).
cmcsssKeyPrefix :: Lens' CustomerManagedChannelS3StorageSummary (Maybe Text)
cmcsssKeyPrefix = lens _cmcsssKeyPrefix (\s a -> s {_cmcsssKeyPrefix = a})

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
cmcsssRoleARN :: Lens' CustomerManagedChannelS3StorageSummary (Maybe Text)
cmcsssRoleARN = lens _cmcsssRoleARN (\s a -> s {_cmcsssRoleARN = a})

instance FromJSON CustomerManagedChannelS3StorageSummary where
  parseJSON =
    withObject
      "CustomerManagedChannelS3StorageSummary"
      ( \x ->
          CustomerManagedChannelS3StorageSummary'
            <$> (x .:? "bucket") <*> (x .:? "keyPrefix") <*> (x .:? "roleArn")
      )

instance Hashable CustomerManagedChannelS3StorageSummary

instance NFData CustomerManagedChannelS3StorageSummary
