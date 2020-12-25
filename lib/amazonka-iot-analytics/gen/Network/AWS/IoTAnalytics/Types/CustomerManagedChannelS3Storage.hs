{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
  ( CustomerManagedChannelS3Storage (..),

    -- * Smart constructor
    mkCustomerManagedChannelS3Storage,

    -- * Lenses
    cmcssBucket,
    cmcssRoleArn,
    cmcssKeyPrefix,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.BucketName as Types
import qualified Network.AWS.IoTAnalytics.Types.RoleArn as Types
import qualified Network.AWS.IoTAnalytics.Types.S3KeyPrefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
-- /See:/ 'mkCustomerManagedChannelS3Storage' smart constructor.
data CustomerManagedChannelS3Storage = CustomerManagedChannelS3Storage'
  { -- | The name of the S3 bucket in which channel data is stored.
    bucket :: Types.BucketName,
    -- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
    roleArn :: Types.RoleArn,
    -- | Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
    keyPrefix :: Core.Maybe Types.S3KeyPrefix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomerManagedChannelS3Storage' value with any optional fields omitted.
mkCustomerManagedChannelS3Storage ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'roleArn'
  Types.RoleArn ->
  CustomerManagedChannelS3Storage
mkCustomerManagedChannelS3Storage bucket roleArn =
  CustomerManagedChannelS3Storage'
    { bucket,
      roleArn,
      keyPrefix = Core.Nothing
    }

-- | The name of the S3 bucket in which channel data is stored.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcssBucket :: Lens.Lens' CustomerManagedChannelS3Storage Types.BucketName
cmcssBucket = Lens.field @"bucket"
{-# DEPRECATED cmcssBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcssRoleArn :: Lens.Lens' CustomerManagedChannelS3Storage Types.RoleArn
cmcssRoleArn = Lens.field @"roleArn"
{-# DEPRECATED cmcssRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Optional. The prefix used to create the keys of the channel data objects. Each object in an S3 bucket has a key that is its unique identifier in the bucket. Each object in a bucket has exactly one key. The prefix must end with a forward slash (/).
--
-- /Note:/ Consider using 'keyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmcssKeyPrefix :: Lens.Lens' CustomerManagedChannelS3Storage (Core.Maybe Types.S3KeyPrefix)
cmcssKeyPrefix = Lens.field @"keyPrefix"
{-# DEPRECATED cmcssKeyPrefix "Use generic-lens or generic-optics with 'keyPrefix' instead." #-}

instance Core.FromJSON CustomerManagedChannelS3Storage where
  toJSON CustomerManagedChannelS3Storage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("bucket" Core..= bucket),
            Core.Just ("roleArn" Core..= roleArn),
            ("keyPrefix" Core..=) Core.<$> keyPrefix
          ]
      )

instance Core.FromJSON CustomerManagedChannelS3Storage where
  parseJSON =
    Core.withObject "CustomerManagedChannelS3Storage" Core.$
      \x ->
        CustomerManagedChannelS3Storage'
          Core.<$> (x Core..: "bucket")
          Core.<*> (x Core..: "roleArn")
          Core.<*> (x Core..:? "keyPrefix")
