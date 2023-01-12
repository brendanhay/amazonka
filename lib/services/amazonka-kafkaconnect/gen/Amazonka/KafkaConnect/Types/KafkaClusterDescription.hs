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
-- Module      : Amazonka.KafkaConnect.Types.KafkaClusterDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.KafkaClusterDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.ApacheKafkaClusterDescription
import qualified Amazonka.Prelude as Prelude

-- | Details of how to connect to the Apache Kafka cluster.
--
-- /See:/ 'newKafkaClusterDescription' smart constructor.
data KafkaClusterDescription = KafkaClusterDescription'
  { -- | The Apache Kafka cluster to which the connector is connected.
    apacheKafkaCluster :: Prelude.Maybe ApacheKafkaClusterDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KafkaClusterDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apacheKafkaCluster', 'kafkaClusterDescription_apacheKafkaCluster' - The Apache Kafka cluster to which the connector is connected.
newKafkaClusterDescription ::
  KafkaClusterDescription
newKafkaClusterDescription =
  KafkaClusterDescription'
    { apacheKafkaCluster =
        Prelude.Nothing
    }

-- | The Apache Kafka cluster to which the connector is connected.
kafkaClusterDescription_apacheKafkaCluster :: Lens.Lens' KafkaClusterDescription (Prelude.Maybe ApacheKafkaClusterDescription)
kafkaClusterDescription_apacheKafkaCluster = Lens.lens (\KafkaClusterDescription' {apacheKafkaCluster} -> apacheKafkaCluster) (\s@KafkaClusterDescription' {} a -> s {apacheKafkaCluster = a} :: KafkaClusterDescription)

instance Data.FromJSON KafkaClusterDescription where
  parseJSON =
    Data.withObject
      "KafkaClusterDescription"
      ( \x ->
          KafkaClusterDescription'
            Prelude.<$> (x Data..:? "apacheKafkaCluster")
      )

instance Prelude.Hashable KafkaClusterDescription where
  hashWithSalt _salt KafkaClusterDescription' {..} =
    _salt `Prelude.hashWithSalt` apacheKafkaCluster

instance Prelude.NFData KafkaClusterDescription where
  rnf KafkaClusterDescription' {..} =
    Prelude.rnf apacheKafkaCluster
