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
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStorageSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
import qualified Network.AWS.Lens as Lens

-- | Where channel data is stored.
--
-- /See:/ 'newChannelStorageSummary' smart constructor.
data ChannelStorageSummary = ChannelStorageSummary'
  { -- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
    serviceManagedS3 :: Core.Maybe ServiceManagedChannelS3StorageSummary,
    -- | Used to store channel data in an S3 bucket that you manage.
    customerManagedS3 :: Core.Maybe CustomerManagedChannelS3StorageSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChannelStorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceManagedS3', 'channelStorageSummary_serviceManagedS3' - Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
--
-- 'customerManagedS3', 'channelStorageSummary_customerManagedS3' - Used to store channel data in an S3 bucket that you manage.
newChannelStorageSummary ::
  ChannelStorageSummary
newChannelStorageSummary =
  ChannelStorageSummary'
    { serviceManagedS3 =
        Core.Nothing,
      customerManagedS3 = Core.Nothing
    }

-- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
channelStorageSummary_serviceManagedS3 :: Lens.Lens' ChannelStorageSummary (Core.Maybe ServiceManagedChannelS3StorageSummary)
channelStorageSummary_serviceManagedS3 = Lens.lens (\ChannelStorageSummary' {serviceManagedS3} -> serviceManagedS3) (\s@ChannelStorageSummary' {} a -> s {serviceManagedS3 = a} :: ChannelStorageSummary)

-- | Used to store channel data in an S3 bucket that you manage.
channelStorageSummary_customerManagedS3 :: Lens.Lens' ChannelStorageSummary (Core.Maybe CustomerManagedChannelS3StorageSummary)
channelStorageSummary_customerManagedS3 = Lens.lens (\ChannelStorageSummary' {customerManagedS3} -> customerManagedS3) (\s@ChannelStorageSummary' {} a -> s {customerManagedS3 = a} :: ChannelStorageSummary)

instance Core.FromJSON ChannelStorageSummary where
  parseJSON =
    Core.withObject
      "ChannelStorageSummary"
      ( \x ->
          ChannelStorageSummary'
            Core.<$> (x Core..:? "serviceManagedS3")
            Core.<*> (x Core..:? "customerManagedS3")
      )

instance Core.Hashable ChannelStorageSummary

instance Core.NFData ChannelStorageSummary
