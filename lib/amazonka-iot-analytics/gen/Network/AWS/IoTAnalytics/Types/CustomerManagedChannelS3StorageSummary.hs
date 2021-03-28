{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
  ( CustomerManagedChannelS3StorageSummary (..)
  -- * Smart constructor
  , mkCustomerManagedChannelS3StorageSummary
  -- * Lenses
  , cmcsssBucket
  , cmcsssKeyPrefix
  , cmcsssRoleArn
  ) where

import qualified Network.AWS.IoTAnalytics.Types.Bucket as Types
import qualified Network.AWS.IoTAnalytics.Types.KeyPrefix as Types
import qualified Network.AWS.IoTAnalytics.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used to store channel data in an S3 bucket that you manage.
--
-- /See:/ 'mkCustomerManagedChannelS3StorageSummary' smart constructor.
data CustomerManagedChannelS3StorageSummary = CustomerManagedChannelS3StorageSummary'
  { bucket :: Core.Maybe Types.Bucket
    -- ^ The name of the S3 bucket in which channel data is stored.
  , keyPrefix :: Core.Maybe Types.KeyPrefix
    -- ^ Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). The prefix must end with a forward slash (/).
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomerManagedChannelS3StorageSummary' value with any optional fields omitted.
mkCustomerManagedChannelS3StorageSummary
    :: CustomerManagedChannelS3StorageSummary
mkCustomerManagedChannelS3StorageSummary
  = CustomerManagedChannelS3StorageSummary'{bucket = Core.Nothing,
                                            keyPrefix = Core.Nothing, roleArn = Core.Nothing}

-- | The name of the S3 bucket in which channel data is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcsssBucket :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Core.Maybe Types.Bucket)
cmcsssBucket = Lens.field @"bucket"
{-# INLINEABLE cmcsssBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). The prefix must end with a forward slash (/).
--
-- /Note:/ Consider using 'keyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcsssKeyPrefix :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Core.Maybe Types.KeyPrefix)
cmcsssKeyPrefix = Lens.field @"keyPrefix"
{-# INLINEABLE cmcsssKeyPrefix #-}
{-# DEPRECATED keyPrefix "Use generic-lens or generic-optics with 'keyPrefix' instead"  #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcsssRoleArn :: Lens.Lens' CustomerManagedChannelS3StorageSummary (Core.Maybe Types.RoleArn)
cmcsssRoleArn = Lens.field @"roleArn"
{-# INLINEABLE cmcsssRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON CustomerManagedChannelS3StorageSummary where
        parseJSON
          = Core.withObject "CustomerManagedChannelS3StorageSummary" Core.$
              \ x ->
                CustomerManagedChannelS3StorageSummary' Core.<$>
                  (x Core..:? "bucket") Core.<*> x Core..:? "keyPrefix" Core.<*>
                    x Core..:? "roleArn"
