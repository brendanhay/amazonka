{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStorageSummary where

import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Where channel data is stored.
--
--
--
-- /See:/ 'channelStorageSummary' smart constructor.
data ChannelStorageSummary = ChannelStorageSummary'
  { _cssServiceManagedS3 ::
      !(Maybe ServiceManagedChannelS3StorageSummary),
    _cssCustomerManagedS3 ::
      !(Maybe CustomerManagedChannelS3StorageSummary)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelStorageSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssServiceManagedS3' - Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
--
-- * 'cssCustomerManagedS3' - Used to store channel data in an S3 bucket that you manage.
channelStorageSummary ::
  ChannelStorageSummary
channelStorageSummary =
  ChannelStorageSummary'
    { _cssServiceManagedS3 = Nothing,
      _cssCustomerManagedS3 = Nothing
    }

-- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
cssServiceManagedS3 :: Lens' ChannelStorageSummary (Maybe ServiceManagedChannelS3StorageSummary)
cssServiceManagedS3 = lens _cssServiceManagedS3 (\s a -> s {_cssServiceManagedS3 = a})

-- | Used to store channel data in an S3 bucket that you manage.
cssCustomerManagedS3 :: Lens' ChannelStorageSummary (Maybe CustomerManagedChannelS3StorageSummary)
cssCustomerManagedS3 = lens _cssCustomerManagedS3 (\s a -> s {_cssCustomerManagedS3 = a})

instance FromJSON ChannelStorageSummary where
  parseJSON =
    withObject
      "ChannelStorageSummary"
      ( \x ->
          ChannelStorageSummary'
            <$> (x .:? "serviceManagedS3") <*> (x .:? "customerManagedS3")
      )

instance Hashable ChannelStorageSummary

instance NFData ChannelStorageSummary
