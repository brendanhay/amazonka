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
-- Module      : Amazonka.Kafka.Types.ProvisionedThroughput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ProvisionedThroughput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about provisioned throughput for EBS storage
-- volumes attached to kafka broker nodes.
--
-- /See:/ 'newProvisionedThroughput' smart constructor.
data ProvisionedThroughput = ProvisionedThroughput'
  { -- | Provisioned throughput is enabled or not.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Throughput value of the EBS volumes for the data drive on each kafka
    -- broker node in MiB per second.
    volumeThroughput :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedThroughput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'provisionedThroughput_enabled' - Provisioned throughput is enabled or not.
--
-- 'volumeThroughput', 'provisionedThroughput_volumeThroughput' - Throughput value of the EBS volumes for the data drive on each kafka
-- broker node in MiB per second.
newProvisionedThroughput ::
  ProvisionedThroughput
newProvisionedThroughput =
  ProvisionedThroughput'
    { enabled = Prelude.Nothing,
      volumeThroughput = Prelude.Nothing
    }

-- | Provisioned throughput is enabled or not.
provisionedThroughput_enabled :: Lens.Lens' ProvisionedThroughput (Prelude.Maybe Prelude.Bool)
provisionedThroughput_enabled = Lens.lens (\ProvisionedThroughput' {enabled} -> enabled) (\s@ProvisionedThroughput' {} a -> s {enabled = a} :: ProvisionedThroughput)

-- | Throughput value of the EBS volumes for the data drive on each kafka
-- broker node in MiB per second.
provisionedThroughput_volumeThroughput :: Lens.Lens' ProvisionedThroughput (Prelude.Maybe Prelude.Int)
provisionedThroughput_volumeThroughput = Lens.lens (\ProvisionedThroughput' {volumeThroughput} -> volumeThroughput) (\s@ProvisionedThroughput' {} a -> s {volumeThroughput = a} :: ProvisionedThroughput)

instance Data.FromJSON ProvisionedThroughput where
  parseJSON =
    Data.withObject
      "ProvisionedThroughput"
      ( \x ->
          ProvisionedThroughput'
            Prelude.<$> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "volumeThroughput")
      )

instance Prelude.Hashable ProvisionedThroughput where
  hashWithSalt _salt ProvisionedThroughput' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` volumeThroughput

instance Prelude.NFData ProvisionedThroughput where
  rnf ProvisionedThroughput' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf volumeThroughput

instance Data.ToJSON ProvisionedThroughput where
  toJSON ProvisionedThroughput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enabled" Data..=) Prelude.<$> enabled,
            ("volumeThroughput" Data..=)
              Prelude.<$> volumeThroughput
          ]
      )
