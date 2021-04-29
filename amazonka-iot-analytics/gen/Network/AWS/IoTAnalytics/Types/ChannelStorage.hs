{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    serviceManagedS3 :: Prelude.Maybe ServiceManagedChannelS3Storage,
    -- | Use this to store channel data in an S3 bucket that you manage. If
    -- customer managed storage is selected, the @retentionPeriod@ parameter is
    -- ignored. You cannot change the choice of service-managed or
    -- customer-managed S3 storage after the channel is created.
    customerManagedS3 :: Prelude.Maybe CustomerManagedChannelS3Storage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { serviceManagedS3 = Prelude.Nothing,
      customerManagedS3 = Prelude.Nothing
    }

-- | Use this to store channel data in an S3 bucket managed by AWS IoT
-- Analytics. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the channel is created.
channelStorage_serviceManagedS3 :: Lens.Lens' ChannelStorage (Prelude.Maybe ServiceManagedChannelS3Storage)
channelStorage_serviceManagedS3 = Lens.lens (\ChannelStorage' {serviceManagedS3} -> serviceManagedS3) (\s@ChannelStorage' {} a -> s {serviceManagedS3 = a} :: ChannelStorage)

-- | Use this to store channel data in an S3 bucket that you manage. If
-- customer managed storage is selected, the @retentionPeriod@ parameter is
-- ignored. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the channel is created.
channelStorage_customerManagedS3 :: Lens.Lens' ChannelStorage (Prelude.Maybe CustomerManagedChannelS3Storage)
channelStorage_customerManagedS3 = Lens.lens (\ChannelStorage' {customerManagedS3} -> customerManagedS3) (\s@ChannelStorage' {} a -> s {customerManagedS3 = a} :: ChannelStorage)

instance Prelude.FromJSON ChannelStorage where
  parseJSON =
    Prelude.withObject
      "ChannelStorage"
      ( \x ->
          ChannelStorage'
            Prelude.<$> (x Prelude..:? "serviceManagedS3")
            Prelude.<*> (x Prelude..:? "customerManagedS3")
      )

instance Prelude.Hashable ChannelStorage

instance Prelude.NFData ChannelStorage

instance Prelude.ToJSON ChannelStorage where
  toJSON ChannelStorage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("serviceManagedS3" Prelude..=)
              Prelude.<$> serviceManagedS3,
            ("customerManagedS3" Prelude..=)
              Prelude.<$> customerManagedS3
          ]
      )
