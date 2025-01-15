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
-- Module      : Amazonka.Pipes.Types.UpdatePipeSourceSelfManagedKafkaParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.UpdatePipeSourceSelfManagedKafkaParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationCredentials
import Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationVpc
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a self-managed Apache Kafka stream as a source.
--
-- /See:/ 'newUpdatePipeSourceSelfManagedKafkaParameters' smart constructor.
data UpdatePipeSourceSelfManagedKafkaParameters = UpdatePipeSourceSelfManagedKafkaParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The credentials needed to access the resource.
    credentials :: Prelude.Maybe SelfManagedKafkaAccessConfigurationCredentials,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the Secrets Manager secret used for certification.
    serverRootCaCertificate :: Prelude.Maybe Prelude.Text,
    -- | This structure specifies the VPC subnets and security groups for the
    -- stream, and whether a public IP address is to be used.
    vpc :: Prelude.Maybe SelfManagedKafkaAccessConfigurationVpc
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeSourceSelfManagedKafkaParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'updatePipeSourceSelfManagedKafkaParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'credentials', 'updatePipeSourceSelfManagedKafkaParameters_credentials' - The credentials needed to access the resource.
--
-- 'maximumBatchingWindowInSeconds', 'updatePipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'serverRootCaCertificate', 'updatePipeSourceSelfManagedKafkaParameters_serverRootCaCertificate' - The ARN of the Secrets Manager secret used for certification.
--
-- 'vpc', 'updatePipeSourceSelfManagedKafkaParameters_vpc' - This structure specifies the VPC subnets and security groups for the
-- stream, and whether a public IP address is to be used.
newUpdatePipeSourceSelfManagedKafkaParameters ::
  UpdatePipeSourceSelfManagedKafkaParameters
newUpdatePipeSourceSelfManagedKafkaParameters =
  UpdatePipeSourceSelfManagedKafkaParameters'
    { batchSize =
        Prelude.Nothing,
      credentials = Prelude.Nothing,
      maximumBatchingWindowInSeconds =
        Prelude.Nothing,
      serverRootCaCertificate =
        Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | The maximum number of records to include in each batch.
updatePipeSourceSelfManagedKafkaParameters_batchSize :: Lens.Lens' UpdatePipeSourceSelfManagedKafkaParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceSelfManagedKafkaParameters_batchSize = Lens.lens (\UpdatePipeSourceSelfManagedKafkaParameters' {batchSize} -> batchSize) (\s@UpdatePipeSourceSelfManagedKafkaParameters' {} a -> s {batchSize = a} :: UpdatePipeSourceSelfManagedKafkaParameters)

-- | The credentials needed to access the resource.
updatePipeSourceSelfManagedKafkaParameters_credentials :: Lens.Lens' UpdatePipeSourceSelfManagedKafkaParameters (Prelude.Maybe SelfManagedKafkaAccessConfigurationCredentials)
updatePipeSourceSelfManagedKafkaParameters_credentials = Lens.lens (\UpdatePipeSourceSelfManagedKafkaParameters' {credentials} -> credentials) (\s@UpdatePipeSourceSelfManagedKafkaParameters' {} a -> s {credentials = a} :: UpdatePipeSourceSelfManagedKafkaParameters)

-- | The maximum length of a time to wait for events.
updatePipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds :: Lens.Lens' UpdatePipeSourceSelfManagedKafkaParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds = Lens.lens (\UpdatePipeSourceSelfManagedKafkaParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdatePipeSourceSelfManagedKafkaParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdatePipeSourceSelfManagedKafkaParameters)

-- | The ARN of the Secrets Manager secret used for certification.
updatePipeSourceSelfManagedKafkaParameters_serverRootCaCertificate :: Lens.Lens' UpdatePipeSourceSelfManagedKafkaParameters (Prelude.Maybe Prelude.Text)
updatePipeSourceSelfManagedKafkaParameters_serverRootCaCertificate = Lens.lens (\UpdatePipeSourceSelfManagedKafkaParameters' {serverRootCaCertificate} -> serverRootCaCertificate) (\s@UpdatePipeSourceSelfManagedKafkaParameters' {} a -> s {serverRootCaCertificate = a} :: UpdatePipeSourceSelfManagedKafkaParameters)

-- | This structure specifies the VPC subnets and security groups for the
-- stream, and whether a public IP address is to be used.
updatePipeSourceSelfManagedKafkaParameters_vpc :: Lens.Lens' UpdatePipeSourceSelfManagedKafkaParameters (Prelude.Maybe SelfManagedKafkaAccessConfigurationVpc)
updatePipeSourceSelfManagedKafkaParameters_vpc = Lens.lens (\UpdatePipeSourceSelfManagedKafkaParameters' {vpc} -> vpc) (\s@UpdatePipeSourceSelfManagedKafkaParameters' {} a -> s {vpc = a} :: UpdatePipeSourceSelfManagedKafkaParameters)

instance
  Prelude.Hashable
    UpdatePipeSourceSelfManagedKafkaParameters
  where
  hashWithSalt
    _salt
    UpdatePipeSourceSelfManagedKafkaParameters' {..} =
      _salt
        `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` credentials
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` serverRootCaCertificate
        `Prelude.hashWithSalt` vpc

instance
  Prelude.NFData
    UpdatePipeSourceSelfManagedKafkaParameters
  where
  rnf UpdatePipeSourceSelfManagedKafkaParameters' {..} =
    Prelude.rnf batchSize `Prelude.seq`
      Prelude.rnf credentials `Prelude.seq`
        Prelude.rnf maximumBatchingWindowInSeconds `Prelude.seq`
          Prelude.rnf serverRootCaCertificate `Prelude.seq`
            Prelude.rnf vpc

instance
  Data.ToJSON
    UpdatePipeSourceSelfManagedKafkaParameters
  where
  toJSON
    UpdatePipeSourceSelfManagedKafkaParameters' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("BatchSize" Data..=) Prelude.<$> batchSize,
              ("Credentials" Data..=) Prelude.<$> credentials,
              ("MaximumBatchingWindowInSeconds" Data..=)
                Prelude.<$> maximumBatchingWindowInSeconds,
              ("ServerRootCaCertificate" Data..=)
                Prelude.<$> serverRootCaCertificate,
              ("Vpc" Data..=) Prelude.<$> vpc
            ]
        )
