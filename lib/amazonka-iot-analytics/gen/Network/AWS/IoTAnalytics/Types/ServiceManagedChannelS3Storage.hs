{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use this to store channel data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
--
--
-- /See:/ 'serviceManagedChannelS3Storage' smart constructor.
data ServiceManagedChannelS3Storage = ServiceManagedChannelS3Storage'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceManagedChannelS3Storage' with the minimum fields required to make a request.
serviceManagedChannelS3Storage ::
  ServiceManagedChannelS3Storage
serviceManagedChannelS3Storage = ServiceManagedChannelS3Storage'

instance FromJSON ServiceManagedChannelS3Storage where
  parseJSON =
    withObject
      "ServiceManagedChannelS3Storage"
      (\x -> pure ServiceManagedChannelS3Storage')

instance Hashable ServiceManagedChannelS3Storage

instance NFData ServiceManagedChannelS3Storage

instance ToJSON ServiceManagedChannelS3Storage where
  toJSON = const (Object mempty)
