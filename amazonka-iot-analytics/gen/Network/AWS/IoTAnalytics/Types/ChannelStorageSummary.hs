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
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStorageSummary where

import Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3StorageSummary
import Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Where channel data is stored.
--
-- /See:/ 'newChannelStorageSummary' smart constructor.
data ChannelStorageSummary = ChannelStorageSummary'
  { -- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
    serviceManagedS3 :: Prelude.Maybe ServiceManagedChannelS3StorageSummary,
    -- | Used to store channel data in an S3 bucket that you manage.
    customerManagedS3 :: Prelude.Maybe CustomerManagedChannelS3StorageSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      customerManagedS3 = Prelude.Nothing
    }

-- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
channelStorageSummary_serviceManagedS3 :: Lens.Lens' ChannelStorageSummary (Prelude.Maybe ServiceManagedChannelS3StorageSummary)
channelStorageSummary_serviceManagedS3 = Lens.lens (\ChannelStorageSummary' {serviceManagedS3} -> serviceManagedS3) (\s@ChannelStorageSummary' {} a -> s {serviceManagedS3 = a} :: ChannelStorageSummary)

-- | Used to store channel data in an S3 bucket that you manage.
channelStorageSummary_customerManagedS3 :: Lens.Lens' ChannelStorageSummary (Prelude.Maybe CustomerManagedChannelS3StorageSummary)
channelStorageSummary_customerManagedS3 = Lens.lens (\ChannelStorageSummary' {customerManagedS3} -> customerManagedS3) (\s@ChannelStorageSummary' {} a -> s {customerManagedS3 = a} :: ChannelStorageSummary)

instance Prelude.FromJSON ChannelStorageSummary where
  parseJSON =
    Prelude.withObject
      "ChannelStorageSummary"
      ( \x ->
          ChannelStorageSummary'
            Prelude.<$> (x Prelude..:? "serviceManagedS3")
            Prelude.<*> (x Prelude..:? "customerManagedS3")
      )

instance Prelude.Hashable ChannelStorageSummary

instance Prelude.NFData ChannelStorageSummary
