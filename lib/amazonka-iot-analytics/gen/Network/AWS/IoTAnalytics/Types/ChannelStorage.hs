{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStorage
  ( ChannelStorage (..),

    -- * Smart constructor
    mkChannelStorage,

    -- * Lenses
    csCustomerManagedS3,
    csServiceManagedS3,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage as Types
import qualified Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Where channel data is stored. You may choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . This cannot be changed after creation of the channel.
--
-- /See:/ 'mkChannelStorage' smart constructor.
data ChannelStorage = ChannelStorage'
  { -- | Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
    customerManagedS3 :: Core.Maybe Types.CustomerManagedChannelS3Storage,
    -- | Use this to store channel data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
    serviceManagedS3 :: Core.Maybe Types.ServiceManagedChannelS3Storage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelStorage' value with any optional fields omitted.
mkChannelStorage ::
  ChannelStorage
mkChannelStorage =
  ChannelStorage'
    { customerManagedS3 = Core.Nothing,
      serviceManagedS3 = Core.Nothing
    }

-- | Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
-- /Note:/ Consider using 'customerManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomerManagedS3 :: Lens.Lens' ChannelStorage (Core.Maybe Types.CustomerManagedChannelS3Storage)
csCustomerManagedS3 = Lens.field @"customerManagedS3"
{-# DEPRECATED csCustomerManagedS3 "Use generic-lens or generic-optics with 'customerManagedS3' instead." #-}

-- | Use this to store channel data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
-- /Note:/ Consider using 'serviceManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceManagedS3 :: Lens.Lens' ChannelStorage (Core.Maybe Types.ServiceManagedChannelS3Storage)
csServiceManagedS3 = Lens.field @"serviceManagedS3"
{-# DEPRECATED csServiceManagedS3 "Use generic-lens or generic-optics with 'serviceManagedS3' instead." #-}

instance Core.FromJSON ChannelStorage where
  toJSON ChannelStorage {..} =
    Core.object
      ( Core.catMaybes
          [ ("customerManagedS3" Core..=) Core.<$> customerManagedS3,
            ("serviceManagedS3" Core..=) Core.<$> serviceManagedS3
          ]
      )

instance Core.FromJSON ChannelStorage where
  parseJSON =
    Core.withObject "ChannelStorage" Core.$
      \x ->
        ChannelStorage'
          Core.<$> (x Core..:? "customerManagedS3")
          Core.<*> (x Core..:? "serviceManagedS3")
