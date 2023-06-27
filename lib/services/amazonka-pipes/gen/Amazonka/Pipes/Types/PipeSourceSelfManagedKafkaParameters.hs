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
-- Module      : Amazonka.Pipes.Types.PipeSourceSelfManagedKafkaParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeSourceSelfManagedKafkaParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationCredentials
import Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationVpc
import Amazonka.Pipes.Types.SelfManagedKafkaStartPosition
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a self-managed Apache Kafka stream as a source.
--
-- /See:/ 'newPipeSourceSelfManagedKafkaParameters' smart constructor.
data PipeSourceSelfManagedKafkaParameters = PipeSourceSelfManagedKafkaParameters'
  { -- | An array of server URLs.
    additionalBootstrapServers :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The name of the destination queue to consume.
    consumerGroupID :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The credentials needed to access the resource.
    credentials :: Prelude.Maybe SelfManagedKafkaAccessConfigurationCredentials,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the Secrets Manager secret used for certification.
    serverRootCaCertificate :: Prelude.Maybe Prelude.Text,
    -- | (Streams only) The position in a stream from which to start reading.
    startingPosition :: Prelude.Maybe SelfManagedKafkaStartPosition,
    -- | This structure specifies the VPC subnets and security groups for the
    -- stream, and whether a public IP address is to be used.
    vpc :: Prelude.Maybe SelfManagedKafkaAccessConfigurationVpc,
    -- | The name of the topic that the pipe will read from.
    topicName :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeSourceSelfManagedKafkaParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalBootstrapServers', 'pipeSourceSelfManagedKafkaParameters_additionalBootstrapServers' - An array of server URLs.
--
-- 'batchSize', 'pipeSourceSelfManagedKafkaParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'consumerGroupID', 'pipeSourceSelfManagedKafkaParameters_consumerGroupID' - The name of the destination queue to consume.
--
-- 'credentials', 'pipeSourceSelfManagedKafkaParameters_credentials' - The credentials needed to access the resource.
--
-- 'maximumBatchingWindowInSeconds', 'pipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'serverRootCaCertificate', 'pipeSourceSelfManagedKafkaParameters_serverRootCaCertificate' - The ARN of the Secrets Manager secret used for certification.
--
-- 'startingPosition', 'pipeSourceSelfManagedKafkaParameters_startingPosition' - (Streams only) The position in a stream from which to start reading.
--
-- 'vpc', 'pipeSourceSelfManagedKafkaParameters_vpc' - This structure specifies the VPC subnets and security groups for the
-- stream, and whether a public IP address is to be used.
--
-- 'topicName', 'pipeSourceSelfManagedKafkaParameters_topicName' - The name of the topic that the pipe will read from.
newPipeSourceSelfManagedKafkaParameters ::
  -- | 'topicName'
  Prelude.Text ->
  PipeSourceSelfManagedKafkaParameters
newPipeSourceSelfManagedKafkaParameters pTopicName_ =
  PipeSourceSelfManagedKafkaParameters'
    { additionalBootstrapServers =
        Prelude.Nothing,
      batchSize = Prelude.Nothing,
      consumerGroupID = Prelude.Nothing,
      credentials = Prelude.Nothing,
      maximumBatchingWindowInSeconds =
        Prelude.Nothing,
      serverRootCaCertificate =
        Prelude.Nothing,
      startingPosition = Prelude.Nothing,
      vpc = Prelude.Nothing,
      topicName =
        Data._Sensitive Lens.# pTopicName_
    }

-- | An array of server URLs.
pipeSourceSelfManagedKafkaParameters_additionalBootstrapServers :: Lens.Lens' PipeSourceSelfManagedKafkaParameters (Prelude.Maybe [Prelude.Text])
pipeSourceSelfManagedKafkaParameters_additionalBootstrapServers = Lens.lens (\PipeSourceSelfManagedKafkaParameters' {additionalBootstrapServers} -> additionalBootstrapServers) (\s@PipeSourceSelfManagedKafkaParameters' {} a -> s {additionalBootstrapServers = a} :: PipeSourceSelfManagedKafkaParameters) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in each batch.
pipeSourceSelfManagedKafkaParameters_batchSize :: Lens.Lens' PipeSourceSelfManagedKafkaParameters (Prelude.Maybe Prelude.Natural)
pipeSourceSelfManagedKafkaParameters_batchSize = Lens.lens (\PipeSourceSelfManagedKafkaParameters' {batchSize} -> batchSize) (\s@PipeSourceSelfManagedKafkaParameters' {} a -> s {batchSize = a} :: PipeSourceSelfManagedKafkaParameters)

-- | The name of the destination queue to consume.
pipeSourceSelfManagedKafkaParameters_consumerGroupID :: Lens.Lens' PipeSourceSelfManagedKafkaParameters (Prelude.Maybe Prelude.Text)
pipeSourceSelfManagedKafkaParameters_consumerGroupID = Lens.lens (\PipeSourceSelfManagedKafkaParameters' {consumerGroupID} -> consumerGroupID) (\s@PipeSourceSelfManagedKafkaParameters' {} a -> s {consumerGroupID = a} :: PipeSourceSelfManagedKafkaParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The credentials needed to access the resource.
pipeSourceSelfManagedKafkaParameters_credentials :: Lens.Lens' PipeSourceSelfManagedKafkaParameters (Prelude.Maybe SelfManagedKafkaAccessConfigurationCredentials)
pipeSourceSelfManagedKafkaParameters_credentials = Lens.lens (\PipeSourceSelfManagedKafkaParameters' {credentials} -> credentials) (\s@PipeSourceSelfManagedKafkaParameters' {} a -> s {credentials = a} :: PipeSourceSelfManagedKafkaParameters)

-- | The maximum length of a time to wait for events.
pipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds :: Lens.Lens' PipeSourceSelfManagedKafkaParameters (Prelude.Maybe Prelude.Natural)
pipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds = Lens.lens (\PipeSourceSelfManagedKafkaParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@PipeSourceSelfManagedKafkaParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: PipeSourceSelfManagedKafkaParameters)

-- | The ARN of the Secrets Manager secret used for certification.
pipeSourceSelfManagedKafkaParameters_serverRootCaCertificate :: Lens.Lens' PipeSourceSelfManagedKafkaParameters (Prelude.Maybe Prelude.Text)
pipeSourceSelfManagedKafkaParameters_serverRootCaCertificate = Lens.lens (\PipeSourceSelfManagedKafkaParameters' {serverRootCaCertificate} -> serverRootCaCertificate) (\s@PipeSourceSelfManagedKafkaParameters' {} a -> s {serverRootCaCertificate = a} :: PipeSourceSelfManagedKafkaParameters)

-- | (Streams only) The position in a stream from which to start reading.
pipeSourceSelfManagedKafkaParameters_startingPosition :: Lens.Lens' PipeSourceSelfManagedKafkaParameters (Prelude.Maybe SelfManagedKafkaStartPosition)
pipeSourceSelfManagedKafkaParameters_startingPosition = Lens.lens (\PipeSourceSelfManagedKafkaParameters' {startingPosition} -> startingPosition) (\s@PipeSourceSelfManagedKafkaParameters' {} a -> s {startingPosition = a} :: PipeSourceSelfManagedKafkaParameters)

-- | This structure specifies the VPC subnets and security groups for the
-- stream, and whether a public IP address is to be used.
pipeSourceSelfManagedKafkaParameters_vpc :: Lens.Lens' PipeSourceSelfManagedKafkaParameters (Prelude.Maybe SelfManagedKafkaAccessConfigurationVpc)
pipeSourceSelfManagedKafkaParameters_vpc = Lens.lens (\PipeSourceSelfManagedKafkaParameters' {vpc} -> vpc) (\s@PipeSourceSelfManagedKafkaParameters' {} a -> s {vpc = a} :: PipeSourceSelfManagedKafkaParameters)

-- | The name of the topic that the pipe will read from.
pipeSourceSelfManagedKafkaParameters_topicName :: Lens.Lens' PipeSourceSelfManagedKafkaParameters Prelude.Text
pipeSourceSelfManagedKafkaParameters_topicName = Lens.lens (\PipeSourceSelfManagedKafkaParameters' {topicName} -> topicName) (\s@PipeSourceSelfManagedKafkaParameters' {} a -> s {topicName = a} :: PipeSourceSelfManagedKafkaParameters) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    PipeSourceSelfManagedKafkaParameters
  where
  parseJSON =
    Data.withObject
      "PipeSourceSelfManagedKafkaParameters"
      ( \x ->
          PipeSourceSelfManagedKafkaParameters'
            Prelude.<$> ( x
                            Data..:? "AdditionalBootstrapServers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "BatchSize")
            Prelude.<*> (x Data..:? "ConsumerGroupID")
            Prelude.<*> (x Data..:? "Credentials")
            Prelude.<*> (x Data..:? "MaximumBatchingWindowInSeconds")
            Prelude.<*> (x Data..:? "ServerRootCaCertificate")
            Prelude.<*> (x Data..:? "StartingPosition")
            Prelude.<*> (x Data..:? "Vpc")
            Prelude.<*> (x Data..: "TopicName")
      )

instance
  Prelude.Hashable
    PipeSourceSelfManagedKafkaParameters
  where
  hashWithSalt
    _salt
    PipeSourceSelfManagedKafkaParameters' {..} =
      _salt
        `Prelude.hashWithSalt` additionalBootstrapServers
        `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` consumerGroupID
        `Prelude.hashWithSalt` credentials
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` serverRootCaCertificate
        `Prelude.hashWithSalt` startingPosition
        `Prelude.hashWithSalt` vpc
        `Prelude.hashWithSalt` topicName

instance
  Prelude.NFData
    PipeSourceSelfManagedKafkaParameters
  where
  rnf PipeSourceSelfManagedKafkaParameters' {..} =
    Prelude.rnf additionalBootstrapServers
      `Prelude.seq` Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf consumerGroupID
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf serverRootCaCertificate
      `Prelude.seq` Prelude.rnf startingPosition
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf topicName

instance
  Data.ToJSON
    PipeSourceSelfManagedKafkaParameters
  where
  toJSON PipeSourceSelfManagedKafkaParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalBootstrapServers" Data..=)
              Prelude.<$> additionalBootstrapServers,
            ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("ConsumerGroupID" Data..=)
              Prelude.<$> consumerGroupID,
            ("Credentials" Data..=) Prelude.<$> credentials,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            ("ServerRootCaCertificate" Data..=)
              Prelude.<$> serverRootCaCertificate,
            ("StartingPosition" Data..=)
              Prelude.<$> startingPosition,
            ("Vpc" Data..=) Prelude.<$> vpc,
            Prelude.Just ("TopicName" Data..= topicName)
          ]
      )
