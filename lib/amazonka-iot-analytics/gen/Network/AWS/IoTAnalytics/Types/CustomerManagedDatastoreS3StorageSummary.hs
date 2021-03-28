{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.CustomerManagedDatastoreS3StorageSummary
  ( CustomerManagedDatastoreS3StorageSummary (..)
  -- * Smart constructor
  , mkCustomerManagedDatastoreS3StorageSummary
  -- * Lenses
  , cmdsssBucket
  , cmdsssKeyPrefix
  , cmdsssRoleArn
  ) where

import qualified Network.AWS.IoTAnalytics.Types.BucketName as Types
import qualified Network.AWS.IoTAnalytics.Types.RoleArn as Types
import qualified Network.AWS.IoTAnalytics.Types.S3KeyPrefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used to store data store data in an S3 bucket that you manage.
--
-- /See:/ 'mkCustomerManagedDatastoreS3StorageSummary' smart constructor.
data CustomerManagedDatastoreS3StorageSummary = CustomerManagedDatastoreS3StorageSummary'
  { bucket :: Core.Maybe Types.BucketName
    -- ^ The name of the S3 bucket in which data store data is stored.
  , keyPrefix :: Core.Maybe Types.S3KeyPrefix
    -- ^ Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomerManagedDatastoreS3StorageSummary' value with any optional fields omitted.
mkCustomerManagedDatastoreS3StorageSummary
    :: CustomerManagedDatastoreS3StorageSummary
mkCustomerManagedDatastoreS3StorageSummary
  = CustomerManagedDatastoreS3StorageSummary'{bucket = Core.Nothing,
                                              keyPrefix = Core.Nothing, roleArn = Core.Nothing}

-- | The name of the S3 bucket in which data store data is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdsssBucket :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Core.Maybe Types.BucketName)
cmdsssBucket = Lens.field @"bucket"
{-# INLINEABLE cmdsssBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Optional. The prefix used to create the keys of the data store data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
--
-- /Note:/ Consider using 'keyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdsssKeyPrefix :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Core.Maybe Types.S3KeyPrefix)
cmdsssKeyPrefix = Lens.field @"keyPrefix"
{-# INLINEABLE cmdsssKeyPrefix #-}
{-# DEPRECATED keyPrefix "Use generic-lens or generic-optics with 'keyPrefix' instead"  #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdsssRoleArn :: Lens.Lens' CustomerManagedDatastoreS3StorageSummary (Core.Maybe Types.RoleArn)
cmdsssRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cmdsssRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON CustomerManagedDatastoreS3StorageSummary
         where
        parseJSON
          = Core.withObject "CustomerManagedDatastoreS3StorageSummary" Core.$
              \ x ->
                CustomerManagedDatastoreS3StorageSummary' Core.<$>
                  (x Core..:? "bucket") Core.<*> x Core..:? "keyPrefix" Core.<*>
                    x Core..:? "roleArn"
