{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
  ( CustomerManagedDatastoreS3Storage (..),

    -- * Smart constructor
    mkCustomerManagedDatastoreS3Storage,

    -- * Lenses
    cmdssBucket,
    cmdssRoleArn,
    cmdssKeyPrefix,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.Bucket as Types
import qualified Network.AWS.IoTAnalytics.Types.KeyPrefix as Types
import qualified Network.AWS.IoTAnalytics.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use this to store data store data in an S3 bucket that you manage. When customer-managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the data store is created.
--
-- /See:/ 'mkCustomerManagedDatastoreS3Storage' smart constructor.
data CustomerManagedDatastoreS3Storage = CustomerManagedDatastoreS3Storage'
  { -- | The name of the S3 bucket in which data store data is stored.
    bucket :: Types.Bucket,
    -- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
    roleArn :: Types.RoleArn,
    -- | Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
    keyPrefix :: Core.Maybe Types.KeyPrefix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomerManagedDatastoreS3Storage' value with any optional fields omitted.
mkCustomerManagedDatastoreS3Storage ::
  -- | 'bucket'
  Types.Bucket ->
  -- | 'roleArn'
  Types.RoleArn ->
  CustomerManagedDatastoreS3Storage
mkCustomerManagedDatastoreS3Storage bucket roleArn =
  CustomerManagedDatastoreS3Storage'
    { bucket,
      roleArn,
      keyPrefix = Core.Nothing
    }

-- | The name of the S3 bucket in which data store data is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdssBucket :: Lens.Lens' CustomerManagedDatastoreS3Storage Types.Bucket
cmdssBucket = Lens.field @"bucket"
{-# DEPRECATED cmdssBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdssRoleArn :: Lens.Lens' CustomerManagedDatastoreS3Storage Types.RoleArn
cmdssRoleArn = Lens.field @"roleArn"
{-# DEPRECATED cmdssRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
--
-- /Note:/ Consider using 'keyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdssKeyPrefix :: Lens.Lens' CustomerManagedDatastoreS3Storage (Core.Maybe Types.KeyPrefix)
cmdssKeyPrefix = Lens.field @"keyPrefix"
{-# DEPRECATED cmdssKeyPrefix "Use generic-lens or generic-optics with 'keyPrefix' instead." #-}

instance Core.FromJSON CustomerManagedDatastoreS3Storage where
  toJSON CustomerManagedDatastoreS3Storage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("bucket" Core..= bucket),
            Core.Just ("roleArn" Core..= roleArn),
            ("keyPrefix" Core..=) Core.<$> keyPrefix
          ]
      )

instance Core.FromJSON CustomerManagedDatastoreS3Storage where
  parseJSON =
    Core.withObject "CustomerManagedDatastoreS3Storage" Core.$
      \x ->
        CustomerManagedDatastoreS3Storage'
          Core.<$> (x Core..: "bucket")
          Core.<*> (x Core..: "roleArn")
          Core.<*> (x Core..:? "keyPrefix")
