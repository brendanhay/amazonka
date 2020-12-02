{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use this to store data store data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
--
--
--
-- /See:/ 'serviceManagedDatastoreS3Storage' smart constructor.
data ServiceManagedDatastoreS3Storage = ServiceManagedDatastoreS3Storage'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceManagedDatastoreS3Storage' with the minimum fields required to make a request.
serviceManagedDatastoreS3Storage ::
  ServiceManagedDatastoreS3Storage
serviceManagedDatastoreS3Storage =
  ServiceManagedDatastoreS3Storage'

instance FromJSON ServiceManagedDatastoreS3Storage where
  parseJSON =
    withObject
      "ServiceManagedDatastoreS3Storage"
      (\x -> pure ServiceManagedDatastoreS3Storage')

instance Hashable ServiceManagedDatastoreS3Storage

instance NFData ServiceManagedDatastoreS3Storage

instance ToJSON ServiceManagedDatastoreS3Storage where
  toJSON = const (Object mempty)
