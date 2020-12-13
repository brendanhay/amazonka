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
    cssServiceManagedS3,
    cssCustomerManagedS3,
  )
where

import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Where channel data is stored.
--
-- /See:/ 'mkChannelStorageSummary' smart constructor.
data ChannelStorageSummary = ChannelStorageSummary'
  { -- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
    serviceManagedS3 :: Lude.Maybe ServiceManagedChannelS3StorageSummary,
    -- | Used to store channel data in an S3 bucket that you manage.
    customerManagedS3 :: Lude.Maybe CustomerManagedChannelS3StorageSummary
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelStorageSummary' with the minimum fields required to make a request.
--
-- * 'serviceManagedS3' - Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
-- * 'customerManagedS3' - Used to store channel data in an S3 bucket that you manage.
mkChannelStorageSummary ::
  ChannelStorageSummary
mkChannelStorageSummary =
  ChannelStorageSummary'
    { serviceManagedS3 = Lude.Nothing,
      customerManagedS3 = Lude.Nothing
    }

-- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
--
-- /Note:/ Consider using 'serviceManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssServiceManagedS3 :: Lens.Lens' ChannelStorageSummary (Lude.Maybe ServiceManagedChannelS3StorageSummary)
cssServiceManagedS3 = Lens.lens (serviceManagedS3 :: ChannelStorageSummary -> Lude.Maybe ServiceManagedChannelS3StorageSummary) (\s a -> s {serviceManagedS3 = a} :: ChannelStorageSummary)
{-# DEPRECATED cssServiceManagedS3 "Use generic-lens or generic-optics with 'serviceManagedS3' instead." #-}

-- | Used to store channel data in an S3 bucket that you manage.
--
-- /Note:/ Consider using 'customerManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cssCustomerManagedS3 :: Lens.Lens' ChannelStorageSummary (Lude.Maybe CustomerManagedChannelS3StorageSummary)
cssCustomerManagedS3 = Lens.lens (customerManagedS3 :: ChannelStorageSummary -> Lude.Maybe CustomerManagedChannelS3StorageSummary) (\s a -> s {customerManagedS3 = a} :: ChannelStorageSummary)
{-# DEPRECATED cssCustomerManagedS3 "Use generic-lens or generic-optics with 'customerManagedS3' instead." #-}

instance Lude.FromJSON ChannelStorageSummary where
  parseJSON =
    Lude.withObject
      "ChannelStorageSummary"
      ( \x ->
          ChannelStorageSummary'
            Lude.<$> (x Lude..:? "serviceManagedS3")
            Lude.<*> (x Lude..:? "customerManagedS3")
      )
