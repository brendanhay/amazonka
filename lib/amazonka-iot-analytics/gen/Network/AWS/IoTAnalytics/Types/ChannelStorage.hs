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
    csServiceManagedS3,
    csCustomerManagedS3,
  )
where

import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Where channel data is stored. You may choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . This cannot be changed after creation of the channel.
--
-- /See:/ 'mkChannelStorage' smart constructor.
data ChannelStorage = ChannelStorage'
  { serviceManagedS3 ::
      Lude.Maybe ServiceManagedChannelS3Storage,
    customerManagedS3 ::
      Lude.Maybe CustomerManagedChannelS3Storage
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelStorage' with the minimum fields required to make a request.
--
-- * 'customerManagedS3' - Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
-- * 'serviceManagedS3' - Use this to store channel data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
mkChannelStorage ::
  ChannelStorage
mkChannelStorage =
  ChannelStorage'
    { serviceManagedS3 = Lude.Nothing,
      customerManagedS3 = Lude.Nothing
    }

-- | Use this to store channel data in an S3 bucket managed by AWS IoT Analytics. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
-- /Note:/ Consider using 'serviceManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceManagedS3 :: Lens.Lens' ChannelStorage (Lude.Maybe ServiceManagedChannelS3Storage)
csServiceManagedS3 = Lens.lens (serviceManagedS3 :: ChannelStorage -> Lude.Maybe ServiceManagedChannelS3Storage) (\s a -> s {serviceManagedS3 = a} :: ChannelStorage)
{-# DEPRECATED csServiceManagedS3 "Use generic-lens or generic-optics with 'serviceManagedS3' instead." #-}

-- | Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the @retentionPeriod@ parameter is ignored. You cannot change the choice of service-managed or customer-managed S3 storage after the channel is created.
--
-- /Note:/ Consider using 'customerManagedS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomerManagedS3 :: Lens.Lens' ChannelStorage (Lude.Maybe CustomerManagedChannelS3Storage)
csCustomerManagedS3 = Lens.lens (customerManagedS3 :: ChannelStorage -> Lude.Maybe CustomerManagedChannelS3Storage) (\s a -> s {customerManagedS3 = a} :: ChannelStorage)
{-# DEPRECATED csCustomerManagedS3 "Use generic-lens or generic-optics with 'customerManagedS3' instead." #-}

instance Lude.FromJSON ChannelStorage where
  parseJSON =
    Lude.withObject
      "ChannelStorage"
      ( \x ->
          ChannelStorage'
            Lude.<$> (x Lude..:? "serviceManagedS3")
            Lude.<*> (x Lude..:? "customerManagedS3")
      )

instance Lude.ToJSON ChannelStorage where
  toJSON ChannelStorage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("serviceManagedS3" Lude..=) Lude.<$> serviceManagedS3,
            ("customerManagedS3" Lude..=) Lude.<$> customerManagedS3
          ]
      )
