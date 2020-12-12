{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStorage
  ( DatastoreStorage (..),

    -- * Smart constructor
    mkDatastoreStorage,

    -- * Lenses
    dsServiceManagedS3,
    dsCustomerManagedS3,
  )
where

import Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
--
-- /See:/ 'mkDatastoreStorage' smart constructor.
data DatastoreStorage = DatastoreStorage'
  { serviceManagedS3 ::
      Lude.Maybe ServiceManagedDatastoreS3Storage,
    customerManagedS3 ::
      Lude.Maybe CustomerManagedDatastoreS3Storage
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatastoreStorage' with the minimum fields required to make a request.
--
-- * 'customerManagedS3' - Use this to store data store data in an S3 bucket that you manage. When customer managed storage is selected, the @retentionPeriod@ parameter is ignored. The choice of service-managed or customer-managed S3 storage cannot be changed after creation of the data store.
-- * 'serviceManagedS3' - Use this to store data store data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
mkDatastoreStorage ::
  DatastoreStorage
mkDatastoreStorage =
  DatastoreStorage'
    { serviceManagedS3 = Lude.Nothing,
      customerManagedS3 = Lude.Nothing
    }

-- | Use this to store data store data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
--
-- /Note:/ Consider using 'serviceManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServiceManagedS3 :: Lens.Lens' DatastoreStorage (Lude.Maybe ServiceManagedDatastoreS3Storage)
dsServiceManagedS3 = Lens.lens (serviceManagedS3 :: DatastoreStorage -> Lude.Maybe ServiceManagedDatastoreS3Storage) (\s a -> s {serviceManagedS3 = a} :: DatastoreStorage)
{-# DEPRECATED dsServiceManagedS3 "Use generic-lens or generic-optics with 'serviceManagedS3' instead." #-}

-- | Use this to store data store data in an S3 bucket that you manage. When customer managed storage is selected, the @retentionPeriod@ parameter is ignored. The choice of service-managed or customer-managed S3 storage cannot be changed after creation of the data store.
--
-- /Note:/ Consider using 'customerManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCustomerManagedS3 :: Lens.Lens' DatastoreStorage (Lude.Maybe CustomerManagedDatastoreS3Storage)
dsCustomerManagedS3 = Lens.lens (customerManagedS3 :: DatastoreStorage -> Lude.Maybe CustomerManagedDatastoreS3Storage) (\s a -> s {customerManagedS3 = a} :: DatastoreStorage)
{-# DEPRECATED dsCustomerManagedS3 "Use generic-lens or generic-optics with 'customerManagedS3' instead." #-}

instance Lude.FromJSON DatastoreStorage where
  parseJSON =
    Lude.withObject
      "DatastoreStorage"
      ( \x ->
          DatastoreStorage'
            Lude.<$> (x Lude..:? "serviceManagedS3")
            Lude.<*> (x Lude..:? "customerManagedS3")
      )

instance Lude.ToJSON DatastoreStorage where
  toJSON DatastoreStorage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serviceManagedS3" Lude..=) Lude.<$> serviceManagedS3,
            ("customerManagedS3" Lude..=) Lude.<$> customerManagedS3
          ]
      )
