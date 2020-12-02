{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
--
--
-- /See:/ 'customerManagedChannelS3Storage' smart constructor.
data CustomerManagedChannelS3Storage = CustomerManagedChannelS3Storage'
  { _cmcssKeyPrefix ::
      !(Maybe Text),
    _cmcssBucket :: !Text,
    _cmcssRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomerManagedChannelS3Storage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmcssKeyPrefix' - Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
--
-- * 'cmcssBucket' - The name of the S3 bucket in which channel data is stored.
--
-- * 'cmcssRoleARN' - The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
customerManagedChannelS3Storage ::
  -- | 'cmcssBucket'
  Text ->
  -- | 'cmcssRoleARN'
  Text ->
  CustomerManagedChannelS3Storage
customerManagedChannelS3Storage pBucket_ pRoleARN_ =
  CustomerManagedChannelS3Storage'
    { _cmcssKeyPrefix = Nothing,
      _cmcssBucket = pBucket_,
      _cmcssRoleARN = pRoleARN_
    }

-- | Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
cmcssKeyPrefix :: Lens' CustomerManagedChannelS3Storage (Maybe Text)
cmcssKeyPrefix = lens _cmcssKeyPrefix (\s a -> s {_cmcssKeyPrefix = a})

-- | The name of the S3 bucket in which channel data is stored.
cmcssBucket :: Lens' CustomerManagedChannelS3Storage Text
cmcssBucket = lens _cmcssBucket (\s a -> s {_cmcssBucket = a})

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
cmcssRoleARN :: Lens' CustomerManagedChannelS3Storage Text
cmcssRoleARN = lens _cmcssRoleARN (\s a -> s {_cmcssRoleARN = a})

instance FromJSON CustomerManagedChannelS3Storage where
  parseJSON =
    withObject
      "CustomerManagedChannelS3Storage"
      ( \x ->
          CustomerManagedChannelS3Storage'
            <$> (x .:? "keyPrefix") <*> (x .: "bucket") <*> (x .: "roleArn")
      )

instance Hashable CustomerManagedChannelS3Storage

instance NFData CustomerManagedChannelS3Storage

instance ToJSON CustomerManagedChannelS3Storage where
  toJSON CustomerManagedChannelS3Storage' {..} =
    object
      ( catMaybes
          [ ("keyPrefix" .=) <$> _cmcssKeyPrefix,
            Just ("bucket" .= _cmcssBucket),
            Just ("roleArn" .= _cmcssRoleARN)
          ]
      )
