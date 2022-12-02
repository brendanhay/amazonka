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
-- Module      : Amazonka.IoTAnalytics.Types.ChannelStorage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ChannelStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.CustomerManagedChannelS3Storage
import Amazonka.IoTAnalytics.Types.ServiceManagedChannelS3Storage
import qualified Amazonka.Prelude as Prelude

-- | Where channel data is stored. You may choose one of @serviceManagedS3@,
-- @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. This can\'t be changed after creation of the
-- channel.
--
-- /See:/ 'newChannelStorage' smart constructor.
data ChannelStorage = ChannelStorage'
  { -- | Used to store channel data in an S3 bucket that you manage. If customer
    -- managed storage is selected, the @retentionPeriod@ parameter is ignored.
    -- You can\'t change the choice of S3 storage after the data store is
    -- created.
    customerManagedS3 :: Prelude.Maybe CustomerManagedChannelS3Storage,
    -- | Used to store channel data in an S3 bucket managed by IoT Analytics. You
    -- can\'t change the choice of S3 storage after the data store is created.
    serviceManagedS3 :: Prelude.Maybe ServiceManagedChannelS3Storage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerManagedS3', 'channelStorage_customerManagedS3' - Used to store channel data in an S3 bucket that you manage. If customer
-- managed storage is selected, the @retentionPeriod@ parameter is ignored.
-- You can\'t change the choice of S3 storage after the data store is
-- created.
--
-- 'serviceManagedS3', 'channelStorage_serviceManagedS3' - Used to store channel data in an S3 bucket managed by IoT Analytics. You
-- can\'t change the choice of S3 storage after the data store is created.
newChannelStorage ::
  ChannelStorage
newChannelStorage =
  ChannelStorage'
    { customerManagedS3 =
        Prelude.Nothing,
      serviceManagedS3 = Prelude.Nothing
    }

-- | Used to store channel data in an S3 bucket that you manage. If customer
-- managed storage is selected, the @retentionPeriod@ parameter is ignored.
-- You can\'t change the choice of S3 storage after the data store is
-- created.
channelStorage_customerManagedS3 :: Lens.Lens' ChannelStorage (Prelude.Maybe CustomerManagedChannelS3Storage)
channelStorage_customerManagedS3 = Lens.lens (\ChannelStorage' {customerManagedS3} -> customerManagedS3) (\s@ChannelStorage' {} a -> s {customerManagedS3 = a} :: ChannelStorage)

-- | Used to store channel data in an S3 bucket managed by IoT Analytics. You
-- can\'t change the choice of S3 storage after the data store is created.
channelStorage_serviceManagedS3 :: Lens.Lens' ChannelStorage (Prelude.Maybe ServiceManagedChannelS3Storage)
channelStorage_serviceManagedS3 = Lens.lens (\ChannelStorage' {serviceManagedS3} -> serviceManagedS3) (\s@ChannelStorage' {} a -> s {serviceManagedS3 = a} :: ChannelStorage)

instance Data.FromJSON ChannelStorage where
  parseJSON =
    Data.withObject
      "ChannelStorage"
      ( \x ->
          ChannelStorage'
            Prelude.<$> (x Data..:? "customerManagedS3")
            Prelude.<*> (x Data..:? "serviceManagedS3")
      )

instance Prelude.Hashable ChannelStorage where
  hashWithSalt _salt ChannelStorage' {..} =
    _salt `Prelude.hashWithSalt` customerManagedS3
      `Prelude.hashWithSalt` serviceManagedS3

instance Prelude.NFData ChannelStorage where
  rnf ChannelStorage' {..} =
    Prelude.rnf customerManagedS3
      `Prelude.seq` Prelude.rnf serviceManagedS3

instance Data.ToJSON ChannelStorage where
  toJSON ChannelStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("customerManagedS3" Data..=)
              Prelude.<$> customerManagedS3,
            ("serviceManagedS3" Data..=)
              Prelude.<$> serviceManagedS3
          ]
      )
