{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatastoreStorage
  ( DatastoreStorage (..)
  -- * Smart constructor
  , mkDatastoreStorage
  -- * Lenses
  , dsCustomerManagedS3
  , dsServiceManagedS3
  ) where

import qualified Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage as Types
import qualified Network.AWS.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
--
-- /See:/ 'mkDatastoreStorage' smart constructor.
data DatastoreStorage = DatastoreStorage'
  { customerManagedS3 :: Core.Maybe Types.CustomerManagedDatastoreS3Storage
    -- ^ Use this to store data store data in an S3 bucket that you manage. When customer managed storage is selected, the @retentionPeriod@ parameter is ignored. The choice of service-managed or customer-managed S3 storage cannot be changed after creation of the data store.
  , serviceManagedS3 :: Core.Maybe Types.ServiceManagedDatastoreS3Storage
    -- ^ Use this to store data store data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatastoreStorage' value with any optional fields omitted.
mkDatastoreStorage
    :: DatastoreStorage
mkDatastoreStorage
  = DatastoreStorage'{customerManagedS3 = Core.Nothing,
                      serviceManagedS3 = Core.Nothing}

-- | Use this to store data store data in an S3 bucket that you manage. When customer managed storage is selected, the @retentionPeriod@ parameter is ignored. The choice of service-managed or customer-managed S3 storage cannot be changed after creation of the data store.
--
-- /Note:/ Consider using 'customerManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCustomerManagedS3 :: Lens.Lens' DatastoreStorage (Core.Maybe Types.CustomerManagedDatastoreS3Storage)
dsCustomerManagedS3 = Lens.field @"customerManagedS3"
{-# INLINEABLE dsCustomerManagedS3 #-}
{-# DEPRECATED customerManagedS3 "Use generic-lens or generic-optics with 'customerManagedS3' instead"  #-}

-- | Use this to store data store data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
--
-- /Note:/ Consider using 'serviceManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsServiceManagedS3 :: Lens.Lens' DatastoreStorage (Core.Maybe Types.ServiceManagedDatastoreS3Storage)
dsServiceManagedS3 = Lens.field @"serviceManagedS3"
{-# INLINEABLE dsServiceManagedS3 #-}
{-# DEPRECATED serviceManagedS3 "Use generic-lens or generic-optics with 'serviceManagedS3' instead"  #-}

instance Core.FromJSON DatastoreStorage where
        toJSON DatastoreStorage{..}
          = Core.object
              (Core.catMaybes
                 [("customerManagedS3" Core..=) Core.<$> customerManagedS3,
                  ("serviceManagedS3" Core..=) Core.<$> serviceManagedS3])

instance Core.FromJSON DatastoreStorage where
        parseJSON
          = Core.withObject "DatastoreStorage" Core.$
              \ x ->
                DatastoreStorage' Core.<$>
                  (x Core..:? "customerManagedS3") Core.<*>
                    x Core..:? "serviceManagedS3"
