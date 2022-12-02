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
-- Module      : Amazonka.Kafka.Types.EBSStorageInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.EBSStorageInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.ProvisionedThroughput
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the EBS storage volumes attached to Apache
-- Kafka broker nodes.
--
-- /See:/ 'newEBSStorageInfo' smart constructor.
data EBSStorageInfo = EBSStorageInfo'
  { -- | EBS volume provisioned throughput information.
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughput,
    -- | The size in GiB of the EBS volume for the data drive on each broker
    -- node.
    volumeSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EBSStorageInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughput', 'eBSStorageInfo_provisionedThroughput' - EBS volume provisioned throughput information.
--
-- 'volumeSize', 'eBSStorageInfo_volumeSize' - The size in GiB of the EBS volume for the data drive on each broker
-- node.
newEBSStorageInfo ::
  EBSStorageInfo
newEBSStorageInfo =
  EBSStorageInfo'
    { provisionedThroughput =
        Prelude.Nothing,
      volumeSize = Prelude.Nothing
    }

-- | EBS volume provisioned throughput information.
eBSStorageInfo_provisionedThroughput :: Lens.Lens' EBSStorageInfo (Prelude.Maybe ProvisionedThroughput)
eBSStorageInfo_provisionedThroughput = Lens.lens (\EBSStorageInfo' {provisionedThroughput} -> provisionedThroughput) (\s@EBSStorageInfo' {} a -> s {provisionedThroughput = a} :: EBSStorageInfo)

-- | The size in GiB of the EBS volume for the data drive on each broker
-- node.
eBSStorageInfo_volumeSize :: Lens.Lens' EBSStorageInfo (Prelude.Maybe Prelude.Natural)
eBSStorageInfo_volumeSize = Lens.lens (\EBSStorageInfo' {volumeSize} -> volumeSize) (\s@EBSStorageInfo' {} a -> s {volumeSize = a} :: EBSStorageInfo)

instance Data.FromJSON EBSStorageInfo where
  parseJSON =
    Data.withObject
      "EBSStorageInfo"
      ( \x ->
          EBSStorageInfo'
            Prelude.<$> (x Data..:? "provisionedThroughput")
            Prelude.<*> (x Data..:? "volumeSize")
      )

instance Prelude.Hashable EBSStorageInfo where
  hashWithSalt _salt EBSStorageInfo' {..} =
    _salt `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` volumeSize

instance Prelude.NFData EBSStorageInfo where
  rnf EBSStorageInfo' {..} =
    Prelude.rnf provisionedThroughput
      `Prelude.seq` Prelude.rnf volumeSize

instance Data.ToJSON EBSStorageInfo where
  toJSON EBSStorageInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("provisionedThroughput" Data..=)
              Prelude.<$> provisionedThroughput,
            ("volumeSize" Data..=) Prelude.<$> volumeSize
          ]
      )
