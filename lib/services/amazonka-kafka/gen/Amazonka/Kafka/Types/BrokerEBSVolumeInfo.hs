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
-- Module      : Amazonka.Kafka.Types.BrokerEBSVolumeInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.BrokerEBSVolumeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.ProvisionedThroughput
import qualified Amazonka.Prelude as Prelude

-- | Specifies the EBS volume upgrade information. The broker identifier must
-- be set to the keyword ALL. This means the changes apply to all the
-- brokers in the cluster.
--
-- /See:/ 'newBrokerEBSVolumeInfo' smart constructor.
data BrokerEBSVolumeInfo = BrokerEBSVolumeInfo'
  { -- | EBS volume provisioned throughput information.
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughput,
    -- | Size of the EBS volume to update.
    volumeSizeGB :: Prelude.Maybe Prelude.Int,
    -- | The ID of the broker to update.
    kafkaBrokerNodeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrokerEBSVolumeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughput', 'brokerEBSVolumeInfo_provisionedThroughput' - EBS volume provisioned throughput information.
--
-- 'volumeSizeGB', 'brokerEBSVolumeInfo_volumeSizeGB' - Size of the EBS volume to update.
--
-- 'kafkaBrokerNodeId', 'brokerEBSVolumeInfo_kafkaBrokerNodeId' - The ID of the broker to update.
newBrokerEBSVolumeInfo ::
  -- | 'kafkaBrokerNodeId'
  Prelude.Text ->
  BrokerEBSVolumeInfo
newBrokerEBSVolumeInfo pKafkaBrokerNodeId_ =
  BrokerEBSVolumeInfo'
    { provisionedThroughput =
        Prelude.Nothing,
      volumeSizeGB = Prelude.Nothing,
      kafkaBrokerNodeId = pKafkaBrokerNodeId_
    }

-- | EBS volume provisioned throughput information.
brokerEBSVolumeInfo_provisionedThroughput :: Lens.Lens' BrokerEBSVolumeInfo (Prelude.Maybe ProvisionedThroughput)
brokerEBSVolumeInfo_provisionedThroughput = Lens.lens (\BrokerEBSVolumeInfo' {provisionedThroughput} -> provisionedThroughput) (\s@BrokerEBSVolumeInfo' {} a -> s {provisionedThroughput = a} :: BrokerEBSVolumeInfo)

-- | Size of the EBS volume to update.
brokerEBSVolumeInfo_volumeSizeGB :: Lens.Lens' BrokerEBSVolumeInfo (Prelude.Maybe Prelude.Int)
brokerEBSVolumeInfo_volumeSizeGB = Lens.lens (\BrokerEBSVolumeInfo' {volumeSizeGB} -> volumeSizeGB) (\s@BrokerEBSVolumeInfo' {} a -> s {volumeSizeGB = a} :: BrokerEBSVolumeInfo)

-- | The ID of the broker to update.
brokerEBSVolumeInfo_kafkaBrokerNodeId :: Lens.Lens' BrokerEBSVolumeInfo Prelude.Text
brokerEBSVolumeInfo_kafkaBrokerNodeId = Lens.lens (\BrokerEBSVolumeInfo' {kafkaBrokerNodeId} -> kafkaBrokerNodeId) (\s@BrokerEBSVolumeInfo' {} a -> s {kafkaBrokerNodeId = a} :: BrokerEBSVolumeInfo)

instance Data.FromJSON BrokerEBSVolumeInfo where
  parseJSON =
    Data.withObject
      "BrokerEBSVolumeInfo"
      ( \x ->
          BrokerEBSVolumeInfo'
            Prelude.<$> (x Data..:? "provisionedThroughput")
            Prelude.<*> (x Data..:? "volumeSizeGB")
            Prelude.<*> (x Data..: "kafkaBrokerNodeId")
      )

instance Prelude.Hashable BrokerEBSVolumeInfo where
  hashWithSalt _salt BrokerEBSVolumeInfo' {..} =
    _salt
      `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` volumeSizeGB
      `Prelude.hashWithSalt` kafkaBrokerNodeId

instance Prelude.NFData BrokerEBSVolumeInfo where
  rnf BrokerEBSVolumeInfo' {..} =
    Prelude.rnf provisionedThroughput `Prelude.seq`
      Prelude.rnf volumeSizeGB `Prelude.seq`
        Prelude.rnf kafkaBrokerNodeId

instance Data.ToJSON BrokerEBSVolumeInfo where
  toJSON BrokerEBSVolumeInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("provisionedThroughput" Data..=)
              Prelude.<$> provisionedThroughput,
            ("volumeSizeGB" Data..=) Prelude.<$> volumeSizeGB,
            Prelude.Just
              ("kafkaBrokerNodeId" Data..= kafkaBrokerNodeId)
          ]
      )
