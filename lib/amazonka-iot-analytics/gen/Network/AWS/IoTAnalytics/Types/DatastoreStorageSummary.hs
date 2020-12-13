{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary
  ( DatastoreStorageSummary (..),

    -- * Smart constructor
    mkDatastoreStorageSummary,

    -- * Lenses
    dssServiceManagedS3,
    dssCustomerManagedS3,
  )
where

import Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Where data store data is stored.
--
-- /See:/ 'mkDatastoreStorageSummary' smart constructor.
data DatastoreStorageSummary = DatastoreStorageSummary'
  { -- | Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
    serviceManagedS3 :: Lude.Maybe ServiceManagedDatastoreS3StorageSummary,
    -- | Used to store data store data in an S3 bucket that you manage.
    customerManagedS3 :: Lude.Maybe CustomerManagedDatastoreS3StorageSummary
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatastoreStorageSummary' with the minimum fields required to make a request.
--
-- * 'serviceManagedS3' - Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
-- * 'customerManagedS3' - Used to store data store data in an S3 bucket that you manage.
mkDatastoreStorageSummary ::
  DatastoreStorageSummary
mkDatastoreStorageSummary =
  DatastoreStorageSummary'
    { serviceManagedS3 = Lude.Nothing,
      customerManagedS3 = Lude.Nothing
    }

-- | Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
--
-- /Note:/ Consider using 'serviceManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServiceManagedS3 :: Lens.Lens' DatastoreStorageSummary (Lude.Maybe ServiceManagedDatastoreS3StorageSummary)
dssServiceManagedS3 = Lens.lens (serviceManagedS3 :: DatastoreStorageSummary -> Lude.Maybe ServiceManagedDatastoreS3StorageSummary) (\s a -> s {serviceManagedS3 = a} :: DatastoreStorageSummary)
{-# DEPRECATED dssServiceManagedS3 "Use generic-lens or generic-optics with 'serviceManagedS3' instead." #-}

-- | Used to store data store data in an S3 bucket that you manage.
--
-- /Note:/ Consider using 'customerManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssCustomerManagedS3 :: Lens.Lens' DatastoreStorageSummary (Lude.Maybe CustomerManagedDatastoreS3StorageSummary)
dssCustomerManagedS3 = Lens.lens (customerManagedS3 :: DatastoreStorageSummary -> Lude.Maybe CustomerManagedDatastoreS3StorageSummary) (\s a -> s {customerManagedS3 = a} :: DatastoreStorageSummary)
{-# DEPRECATED dssCustomerManagedS3 "Use generic-lens or generic-optics with 'customerManagedS3' instead." #-}

instance Lude.FromJSON DatastoreStorageSummary where
  parseJSON =
    Lude.withObject
      "DatastoreStorageSummary"
      ( \x ->
          DatastoreStorageSummary'
            Lude.<$> (x Lude..:? "serviceManagedS3")
            Lude.<*> (x Lude..:? "customerManagedS3")
      )
