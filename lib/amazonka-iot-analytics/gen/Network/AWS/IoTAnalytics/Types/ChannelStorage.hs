{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStorage where

import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Where channel data is stored. You may choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . This cannot be changed after creation of the channel.
--
--
--
-- /See:/ 'channelStorage' smart constructor.
data ChannelStorage = ChannelStorage'
  { _csServiceManagedS3 ::
      !(Maybe ServiceManagedChannelS3Storage),
    _csCustomerManagedS3 ::
      !(Maybe CustomerManagedChannelS3Storage)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelStorage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csServiceManagedS3' - Use this to store channel data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
-- * 'csCustomerManagedS3' - Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
channelStorage ::
  ChannelStorage
channelStorage =
  ChannelStorage'
    { _csServiceManagedS3 = Nothing,
      _csCustomerManagedS3 = Nothing
    }

-- | Use this to store channel data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
csServiceManagedS3 :: Lens' ChannelStorage (Maybe ServiceManagedChannelS3Storage)
csServiceManagedS3 = lens _csServiceManagedS3 (\s a -> s {_csServiceManagedS3 = a})

-- | Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
csCustomerManagedS3 :: Lens' ChannelStorage (Maybe CustomerManagedChannelS3Storage)
csCustomerManagedS3 = lens _csCustomerManagedS3 (\s a -> s {_csCustomerManagedS3 = a})

instance FromJSON ChannelStorage where
  parseJSON =
    withObject
      "ChannelStorage"
      ( \x ->
          ChannelStorage'
            <$> (x .:? "serviceManagedS3") <*> (x .:? "customerManagedS3")
      )

instance Hashable ChannelStorage

instance NFData ChannelStorage

instance ToJSON ChannelStorage where
  toJSON ChannelStorage' {..} =
    object
      ( catMaybes
          [ ("serviceManagedS3" .=) <$> _csServiceManagedS3,
            ("customerManagedS3" .=) <$> _csCustomerManagedS3
          ]
      )
