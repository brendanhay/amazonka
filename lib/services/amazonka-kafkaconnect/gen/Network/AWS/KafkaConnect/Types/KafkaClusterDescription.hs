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
-- Module      : Network.AWS.KafkaConnect.Types.KafkaClusterDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KafkaConnect.Types.KafkaClusterDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.KafkaConnect.Types.ApacheKafkaClusterDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON KafkaClusterDescription where
  parseJSON =
    Core.withObject
      "KafkaClusterDescription"
      ( \x ->
          KafkaClusterDescription'
            Prelude.<$> (x Core..:? "apacheKafkaCluster")
      )

instance Prelude.Hashable KafkaClusterDescription

instance Prelude.NFData KafkaClusterDescription
