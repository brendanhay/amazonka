{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
--
--
--
-- /See:/ 'serviceManagedDatastoreS3StorageSummary' smart constructor.
data ServiceManagedDatastoreS3StorageSummary = ServiceManagedDatastoreS3StorageSummary'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceManagedDatastoreS3StorageSummary' with the minimum fields required to make a request.
serviceManagedDatastoreS3StorageSummary ::
  ServiceManagedDatastoreS3StorageSummary
serviceManagedDatastoreS3StorageSummary =
  ServiceManagedDatastoreS3StorageSummary'

instance FromJSON ServiceManagedDatastoreS3StorageSummary where
  parseJSON =
    withObject
      "ServiceManagedDatastoreS3StorageSummary"
      (\x -> pure ServiceManagedDatastoreS3StorageSummary')

instance Hashable ServiceManagedDatastoreS3StorageSummary

instance NFData ServiceManagedDatastoreS3StorageSummary
