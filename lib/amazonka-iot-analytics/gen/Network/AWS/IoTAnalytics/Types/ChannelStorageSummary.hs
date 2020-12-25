{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
  ( ChannelStorageSummary (..),

    -- * Smart constructor
    mkChannelStorageSummary,

    -- * Lenses
    cssCustomerManagedS3,
    cssServiceManagedS3,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary as Types
import qualified Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Where channel data is stored.
--
-- /See:/ 'mkChannelStorageSummary' smart constructor.
data ChannelStorageSummary = ChannelStorageSummary'
  { -- | Used to store channel data in an S3 bucket that you manage.
    customerManagedS3 :: Core.Maybe Types.CustomerManagedChannelS3StorageSummary,
    -- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
    serviceManagedS3 :: Core.Maybe Types.ServiceManagedChannelS3StorageSummary
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChannelStorageSummary' value with any optional fields omitted.
mkChannelStorageSummary ::
  ChannelStorageSummary
mkChannelStorageSummary =
  ChannelStorageSummary'
    { customerManagedS3 = Core.Nothing,
      serviceManagedS3 = Core.Nothing
    }

-- | Used to store channel data in an S3 bucket that you manage.
--
-- /Note:/ Consider using 'customerManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssCustomerManagedS3 :: Lens.Lens' ChannelStorageSummary (Core.Maybe Types.CustomerManagedChannelS3StorageSummary)
cssCustomerManagedS3 = Lens.field @"customerManagedS3"
{-# DEPRECATED cssCustomerManagedS3 "Use generic-lens or generic-optics with 'customerManagedS3' instead." #-}

-- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
--
-- /Note:/ Consider using 'serviceManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssServiceManagedS3 :: Lens.Lens' ChannelStorageSummary (Core.Maybe Types.ServiceManagedChannelS3StorageSummary)
cssServiceManagedS3 = Lens.field @"serviceManagedS3"
{-# DEPRECATED cssServiceManagedS3 "Use generic-lens or generic-optics with 'serviceManagedS3' instead." #-}

instance Core.FromJSON ChannelStorageSummary where
  parseJSON =
    Core.withObject "ChannelStorageSummary" Core.$
      \x ->
        ChannelStorageSummary'
          Core.<$> (x Core..:? "customerManagedS3")
          Core.<*> (x Core..:? "serviceManagedS3")
