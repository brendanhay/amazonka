{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStorage where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
import qualified Network.AWS.Lens as Lens

-- | Where channel data is stored. You may choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. This cannot be changed after creation of the
-- channel.
--
-- /See:/ 'newChannelStorage' smart constructor.
data ChannelStorage = ChannelStorage'
  { -- | Use this to store channel data in an S3 bucket managed by AWS IoT
    -- Analytics. You cannot change the choice of service-managed or
    -- customer-managed S3 storage after the channel is created.
    serviceManagedS3 :: Core.Maybe ServiceManagedChannelS3Storage,
    -- | Use this to store channel data in an S3 bucket that you manage. If
    -- customer managed storage is selected, the @retentionPeriod@ parameter is
    -- ignored. You cannot change the choice of service-managed or
    -- customer-managed S3 storage after the channel is created.
    customerManagedS3 :: Core.Maybe CustomerManagedChannelS3Storage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChannelStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceManagedS3', 'channelStorage_serviceManagedS3' - Use this to store channel data in an S3 bucket managed by AWS IoT
-- Analytics. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the channel is created.
--
-- 'customerManagedS3', 'channelStorage_customerManagedS3' - Use this to store channel data in an S3 bucket that you manage. If
-- customer managed storage is selected, the @retentionPeriod@ parameter is
-- ignored. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the channel is created.
newChannelStorage ::
  ChannelStorage
newChannelStorage =
  ChannelStorage'
    { serviceManagedS3 = Core.Nothing,
      customerManagedS3 = Core.Nothing
    }

-- | Use this to store channel data in an S3 bucket managed by AWS IoT
-- Analytics. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the channel is created.
channelStorage_serviceManagedS3 :: Lens.Lens' ChannelStorage (Core.Maybe ServiceManagedChannelS3Storage)
channelStorage_serviceManagedS3 = Lens.lens (\ChannelStorage' {serviceManagedS3} -> serviceManagedS3) (\s@ChannelStorage' {} a -> s {serviceManagedS3 = a} :: ChannelStorage)

-- | Use this to store channel data in an S3 bucket that you manage. If
-- customer managed storage is selected, the @retentionPeriod@ parameter is
-- ignored. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the channel is created.
channelStorage_customerManagedS3 :: Lens.Lens' ChannelStorage (Core.Maybe CustomerManagedChannelS3Storage)
channelStorage_customerManagedS3 = Lens.lens (\ChannelStorage' {customerManagedS3} -> customerManagedS3) (\s@ChannelStorage' {} a -> s {customerManagedS3 = a} :: ChannelStorage)

instance Core.FromJSON ChannelStorage where
  parseJSON =
    Core.withObject
      "ChannelStorage"
      ( \x ->
          ChannelStorage'
            Core.<$> (x Core..:? "serviceManagedS3")
            Core.<*> (x Core..:? "customerManagedS3")
      )

instance Core.Hashable ChannelStorage

instance Core.NFData ChannelStorage

instance Core.ToJSON ChannelStorage where
  toJSON ChannelStorage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("serviceManagedS3" Core..=)
              Core.<$> serviceManagedS3,
            ("customerManagedS3" Core..=)
              Core.<$> customerManagedS3
          ]
      )
