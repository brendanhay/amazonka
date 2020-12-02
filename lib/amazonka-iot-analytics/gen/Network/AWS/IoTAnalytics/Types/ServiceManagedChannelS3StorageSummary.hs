{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
--
--
--
-- /See:/ 'serviceManagedChannelS3StorageSummary' smart constructor.
data ServiceManagedChannelS3StorageSummary = ServiceManagedChannelS3StorageSummary'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceManagedChannelS3StorageSummary' with the minimum fields required to make a request.
serviceManagedChannelS3StorageSummary ::
  ServiceManagedChannelS3StorageSummary
serviceManagedChannelS3StorageSummary =
  ServiceManagedChannelS3StorageSummary'

instance FromJSON ServiceManagedChannelS3StorageSummary where
  parseJSON =
    withObject
      "ServiceManagedChannelS3StorageSummary"
      (\x -> pure ServiceManagedChannelS3StorageSummary')

instance Hashable ServiceManagedChannelS3StorageSummary

instance NFData ServiceManagedChannelS3StorageSummary
