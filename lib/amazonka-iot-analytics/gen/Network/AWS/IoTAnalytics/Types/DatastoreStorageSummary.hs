{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary where

import Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Where data store data is stored.
--
--
--
-- /See:/ 'datastoreStorageSummary' smart constructor.
data DatastoreStorageSummary = DatastoreStorageSummary'
  { _dssServiceManagedS3 ::
      !( Maybe
           ServiceManagedDatastoreS3StorageSummary
       ),
    _dssCustomerManagedS3 ::
      !( Maybe
           CustomerManagedDatastoreS3StorageSummary
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatastoreStorageSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssServiceManagedS3' - Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
--
-- * 'dssCustomerManagedS3' - Used to store data store data in an S3 bucket that you manage.
datastoreStorageSummary ::
  DatastoreStorageSummary
datastoreStorageSummary =
  DatastoreStorageSummary'
    { _dssServiceManagedS3 = Nothing,
      _dssCustomerManagedS3 = Nothing
    }

-- | Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
dssServiceManagedS3 :: Lens' DatastoreStorageSummary (Maybe ServiceManagedDatastoreS3StorageSummary)
dssServiceManagedS3 = lens _dssServiceManagedS3 (\s a -> s {_dssServiceManagedS3 = a})

-- | Used to store data store data in an S3 bucket that you manage.
dssCustomerManagedS3 :: Lens' DatastoreStorageSummary (Maybe CustomerManagedDatastoreS3StorageSummary)
dssCustomerManagedS3 = lens _dssCustomerManagedS3 (\s a -> s {_dssCustomerManagedS3 = a})

instance FromJSON DatastoreStorageSummary where
  parseJSON =
    withObject
      "DatastoreStorageSummary"
      ( \x ->
          DatastoreStorageSummary'
            <$> (x .:? "serviceManagedS3") <*> (x .:? "customerManagedS3")
      )

instance Hashable DatastoreStorageSummary

instance NFData DatastoreStorageSummary
