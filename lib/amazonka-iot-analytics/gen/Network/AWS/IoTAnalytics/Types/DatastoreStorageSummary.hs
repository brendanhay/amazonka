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
    dssCustomerManagedS3,
    dssServiceManagedS3,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary as Types
import qualified Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3StorageSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Where data store data is stored.
--
-- /See:/ 'mkDatastoreStorageSummary' smart constructor.
data DatastoreStorageSummary = DatastoreStorageSummary'
  { -- | Used to store data store data in an S3 bucket that you manage.
    customerManagedS3 :: Core.Maybe Types.CustomerManagedDatastoreS3StorageSummary,
    -- | Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
    serviceManagedS3 :: Core.Maybe Types.ServiceManagedDatastoreS3StorageSummary
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatastoreStorageSummary' value with any optional fields omitted.
mkDatastoreStorageSummary ::
  DatastoreStorageSummary
mkDatastoreStorageSummary =
  DatastoreStorageSummary'
    { customerManagedS3 = Core.Nothing,
      serviceManagedS3 = Core.Nothing
    }

-- | Used to store data store data in an S3 bucket that you manage.
--
-- /Note:/ Consider using 'customerManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssCustomerManagedS3 :: Lens.Lens' DatastoreStorageSummary (Core.Maybe Types.CustomerManagedDatastoreS3StorageSummary)
dssCustomerManagedS3 = Lens.field @"customerManagedS3"
{-# DEPRECATED dssCustomerManagedS3 "Use generic-lens or generic-optics with 'customerManagedS3' instead." #-}

-- | Used to store data store data in an S3 bucket managed by AWS IoT Analytics.
--
-- /Note:/ Consider using 'serviceManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServiceManagedS3 :: Lens.Lens' DatastoreStorageSummary (Core.Maybe Types.ServiceManagedDatastoreS3StorageSummary)
dssServiceManagedS3 = Lens.field @"serviceManagedS3"
{-# DEPRECATED dssServiceManagedS3 "Use generic-lens or generic-optics with 'serviceManagedS3' instead." #-}

instance Core.FromJSON DatastoreStorageSummary where
  parseJSON =
    Core.withObject "DatastoreStorageSummary" Core.$
      \x ->
        DatastoreStorageSummary'
          Core.<$> (x Core..:? "customerManagedS3")
          Core.<*> (x Core..:? "serviceManagedS3")
