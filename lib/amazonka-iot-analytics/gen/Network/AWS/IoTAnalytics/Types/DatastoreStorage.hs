{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStorage where

import Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
--
--
--
-- /See:/ 'datastoreStorage' smart constructor.
data DatastoreStorage = DatastoreStorage'
  { _dsServiceManagedS3 ::
      !(Maybe ServiceManagedDatastoreS3Storage),
    _dsCustomerManagedS3 ::
      !(Maybe CustomerManagedDatastoreS3Storage)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatastoreStorage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsServiceManagedS3' - Use this to store data store data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
--
-- * 'dsCustomerManagedS3' - Use this to store data store data in an S3 bucket that you manage. When customer managed storage is selected, the @retentionPeriod@ parameter is ignored. The choice of service-managed or customer-managed S3 storage cannot be changed after creation of the data store.
datastoreStorage ::
  DatastoreStorage
datastoreStorage =
  DatastoreStorage'
    { _dsServiceManagedS3 = Nothing,
      _dsCustomerManagedS3 = Nothing
    }

-- | Use this to store data store data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
dsServiceManagedS3 :: Lens' DatastoreStorage (Maybe ServiceManagedDatastoreS3Storage)
dsServiceManagedS3 = lens _dsServiceManagedS3 (\s a -> s {_dsServiceManagedS3 = a})

-- | Use this to store data store data in an S3 bucket that you manage. When customer managed storage is selected, the @retentionPeriod@ parameter is ignored. The choice of service-managed or customer-managed S3 storage cannot be changed after creation of the data store.
dsCustomerManagedS3 :: Lens' DatastoreStorage (Maybe CustomerManagedDatastoreS3Storage)
dsCustomerManagedS3 = lens _dsCustomerManagedS3 (\s a -> s {_dsCustomerManagedS3 = a})

instance FromJSON DatastoreStorage where
  parseJSON =
    withObject
      "DatastoreStorage"
      ( \x ->
          DatastoreStorage'
            <$> (x .:? "serviceManagedS3") <*> (x .:? "customerManagedS3")
      )

instance Hashable DatastoreStorage

instance NFData DatastoreStorage

instance ToJSON DatastoreStorage where
  toJSON DatastoreStorage' {..} =
    object
      ( catMaybes
          [ ("serviceManagedS3" .=) <$> _dsServiceManagedS3,
            ("customerManagedS3" .=) <$> _dsCustomerManagedS3
          ]
      )
