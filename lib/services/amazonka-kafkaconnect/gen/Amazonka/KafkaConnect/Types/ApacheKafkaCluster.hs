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
-- Module      : Amazonka.KafkaConnect.Types.ApacheKafkaCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.ApacheKafkaCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.Vpc
import qualified Amazonka.Prelude as Prelude

-- | The details of the Apache Kafka cluster to which the connector is
-- connected.
--
-- /See:/ 'newApacheKafkaCluster' smart constructor.
data ApacheKafkaCluster = ApacheKafkaCluster'
  { -- | The bootstrap servers of the cluster.
    bootstrapServers :: Prelude.Text,
    -- | Details of an Amazon VPC which has network connectivity to the Apache
    -- Kafka cluster.
    vpc :: Vpc
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApacheKafkaCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bootstrapServers', 'apacheKafkaCluster_bootstrapServers' - The bootstrap servers of the cluster.
--
-- 'vpc', 'apacheKafkaCluster_vpc' - Details of an Amazon VPC which has network connectivity to the Apache
-- Kafka cluster.
newApacheKafkaCluster ::
  -- | 'bootstrapServers'
  Prelude.Text ->
  -- | 'vpc'
  Vpc ->
  ApacheKafkaCluster
newApacheKafkaCluster pBootstrapServers_ pVpc_ =
  ApacheKafkaCluster'
    { bootstrapServers =
        pBootstrapServers_,
      vpc = pVpc_
    }

-- | The bootstrap servers of the cluster.
apacheKafkaCluster_bootstrapServers :: Lens.Lens' ApacheKafkaCluster Prelude.Text
apacheKafkaCluster_bootstrapServers = Lens.lens (\ApacheKafkaCluster' {bootstrapServers} -> bootstrapServers) (\s@ApacheKafkaCluster' {} a -> s {bootstrapServers = a} :: ApacheKafkaCluster)

-- | Details of an Amazon VPC which has network connectivity to the Apache
-- Kafka cluster.
apacheKafkaCluster_vpc :: Lens.Lens' ApacheKafkaCluster Vpc
apacheKafkaCluster_vpc = Lens.lens (\ApacheKafkaCluster' {vpc} -> vpc) (\s@ApacheKafkaCluster' {} a -> s {vpc = a} :: ApacheKafkaCluster)

instance Prelude.Hashable ApacheKafkaCluster where
  hashWithSalt _salt ApacheKafkaCluster' {..} =
    _salt
      `Prelude.hashWithSalt` bootstrapServers
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData ApacheKafkaCluster where
  rnf ApacheKafkaCluster' {..} =
    Prelude.rnf bootstrapServers
      `Prelude.seq` Prelude.rnf vpc

instance Data.ToJSON ApacheKafkaCluster where
  toJSON ApacheKafkaCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("bootstrapServers" Data..= bootstrapServers),
            Prelude.Just ("vpc" Data..= vpc)
          ]
      )
