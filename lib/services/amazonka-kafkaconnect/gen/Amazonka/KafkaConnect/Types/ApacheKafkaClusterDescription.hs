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
-- Module      : Amazonka.KafkaConnect.Types.ApacheKafkaClusterDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.ApacheKafkaClusterDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.VpcDescription
import qualified Amazonka.Prelude as Prelude

-- | The description of the Apache Kafka cluster to which the connector is
-- connected.
--
-- /See:/ 'newApacheKafkaClusterDescription' smart constructor.
data ApacheKafkaClusterDescription = ApacheKafkaClusterDescription'
  { -- | Details of an Amazon VPC which has network connectivity to the Apache
    -- Kafka cluster.
    vpc :: Prelude.Maybe VpcDescription,
    -- | The bootstrap servers of the cluster.
    bootstrapServers :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApacheKafkaClusterDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpc', 'apacheKafkaClusterDescription_vpc' - Details of an Amazon VPC which has network connectivity to the Apache
-- Kafka cluster.
--
-- 'bootstrapServers', 'apacheKafkaClusterDescription_bootstrapServers' - The bootstrap servers of the cluster.
newApacheKafkaClusterDescription ::
  ApacheKafkaClusterDescription
newApacheKafkaClusterDescription =
  ApacheKafkaClusterDescription'
    { vpc =
        Prelude.Nothing,
      bootstrapServers = Prelude.Nothing
    }

-- | Details of an Amazon VPC which has network connectivity to the Apache
-- Kafka cluster.
apacheKafkaClusterDescription_vpc :: Lens.Lens' ApacheKafkaClusterDescription (Prelude.Maybe VpcDescription)
apacheKafkaClusterDescription_vpc = Lens.lens (\ApacheKafkaClusterDescription' {vpc} -> vpc) (\s@ApacheKafkaClusterDescription' {} a -> s {vpc = a} :: ApacheKafkaClusterDescription)

-- | The bootstrap servers of the cluster.
apacheKafkaClusterDescription_bootstrapServers :: Lens.Lens' ApacheKafkaClusterDescription (Prelude.Maybe Prelude.Text)
apacheKafkaClusterDescription_bootstrapServers = Lens.lens (\ApacheKafkaClusterDescription' {bootstrapServers} -> bootstrapServers) (\s@ApacheKafkaClusterDescription' {} a -> s {bootstrapServers = a} :: ApacheKafkaClusterDescription)

instance Data.FromJSON ApacheKafkaClusterDescription where
  parseJSON =
    Data.withObject
      "ApacheKafkaClusterDescription"
      ( \x ->
          ApacheKafkaClusterDescription'
            Prelude.<$> (x Data..:? "vpc")
            Prelude.<*> (x Data..:? "bootstrapServers")
      )

instance
  Prelude.Hashable
    ApacheKafkaClusterDescription
  where
  hashWithSalt _salt ApacheKafkaClusterDescription' {..} =
    _salt `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` bootstrapServers

instance Prelude.NFData ApacheKafkaClusterDescription where
  rnf ApacheKafkaClusterDescription' {..} =
    Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf bootstrapServers
