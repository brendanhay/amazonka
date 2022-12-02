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
-- Module      : Amazonka.Lambda.Types.SourceAccessConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.SourceAccessConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.SourceAccessType
import qualified Amazonka.Prelude as Prelude

-- | To secure and define access to your event source, you can specify the
-- authentication protocol, VPC components, or virtual host.
--
-- /See:/ 'newSourceAccessConfiguration' smart constructor.
data SourceAccessConfiguration = SourceAccessConfiguration'
  { -- | The type of authentication protocol, VPC components, or virtual host for
    -- your event source. For example: @\"Type\":\"SASL_SCRAM_512_AUTH\"@.
    --
    -- -   @BASIC_AUTH@ - (Amazon MQ) The Secrets Manager secret that stores
    --     your broker credentials.
    --
    -- -   @BASIC_AUTH@ - (Self-managed Apache Kafka) The Secrets Manager ARN
    --     of your secret key used for SASL\/PLAIN authentication of your
    --     Apache Kafka brokers.
    --
    -- -   @VPC_SUBNET@ - (Self-managed Apache Kafka) The subnets associated
    --     with your VPC. Lambda connects to these subnets to fetch data from
    --     your self-managed Apache Kafka cluster.
    --
    -- -   @VPC_SECURITY_GROUP@ - (Self-managed Apache Kafka) The VPC security
    --     group used to manage access to your self-managed Apache Kafka
    --     brokers.
    --
    -- -   @SASL_SCRAM_256_AUTH@ - (Self-managed Apache Kafka) The Secrets
    --     Manager ARN of your secret key used for SASL SCRAM-256
    --     authentication of your self-managed Apache Kafka brokers.
    --
    -- -   @SASL_SCRAM_512_AUTH@ - (Amazon MSK, Self-managed Apache Kafka) The
    --     Secrets Manager ARN of your secret key used for SASL SCRAM-512
    --     authentication of your self-managed Apache Kafka brokers.
    --
    -- -   @VIRTUAL_HOST@ - (RabbitMQ) The name of the virtual host in your
    --     RabbitMQ broker. Lambda uses this RabbitMQ host as the event source.
    --     This property cannot be specified in an UpdateEventSourceMapping API
    --     call.
    --
    -- -   @CLIENT_CERTIFICATE_TLS_AUTH@ - (Amazon MSK, self-managed Apache
    --     Kafka) The Secrets Manager ARN of your secret key containing the
    --     certificate chain (X.509 PEM), private key (PKCS#8 PEM), and private
    --     key password (optional) used for mutual TLS authentication of your
    --     MSK\/Apache Kafka brokers.
    --
    -- -   @SERVER_ROOT_CA_CERTIFICATE@ - (Self-managed Apache Kafka) The
    --     Secrets Manager ARN of your secret key containing the root CA
    --     certificate (X.509 PEM) used for TLS encryption of your Apache Kafka
    --     brokers.
    type' :: Prelude.Maybe SourceAccessType,
    -- | The value for your chosen configuration in @Type@. For example:
    -- @\"URI\": \"arn:aws:secretsmanager:us-east-1:01234567890:secret:MyBrokerSecretName\"@.
    uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceAccessConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'sourceAccessConfiguration_type' - The type of authentication protocol, VPC components, or virtual host for
-- your event source. For example: @\"Type\":\"SASL_SCRAM_512_AUTH\"@.
--
-- -   @BASIC_AUTH@ - (Amazon MQ) The Secrets Manager secret that stores
--     your broker credentials.
--
-- -   @BASIC_AUTH@ - (Self-managed Apache Kafka) The Secrets Manager ARN
--     of your secret key used for SASL\/PLAIN authentication of your
--     Apache Kafka brokers.
--
-- -   @VPC_SUBNET@ - (Self-managed Apache Kafka) The subnets associated
--     with your VPC. Lambda connects to these subnets to fetch data from
--     your self-managed Apache Kafka cluster.
--
-- -   @VPC_SECURITY_GROUP@ - (Self-managed Apache Kafka) The VPC security
--     group used to manage access to your self-managed Apache Kafka
--     brokers.
--
-- -   @SASL_SCRAM_256_AUTH@ - (Self-managed Apache Kafka) The Secrets
--     Manager ARN of your secret key used for SASL SCRAM-256
--     authentication of your self-managed Apache Kafka brokers.
--
-- -   @SASL_SCRAM_512_AUTH@ - (Amazon MSK, Self-managed Apache Kafka) The
--     Secrets Manager ARN of your secret key used for SASL SCRAM-512
--     authentication of your self-managed Apache Kafka brokers.
--
-- -   @VIRTUAL_HOST@ - (RabbitMQ) The name of the virtual host in your
--     RabbitMQ broker. Lambda uses this RabbitMQ host as the event source.
--     This property cannot be specified in an UpdateEventSourceMapping API
--     call.
--
-- -   @CLIENT_CERTIFICATE_TLS_AUTH@ - (Amazon MSK, self-managed Apache
--     Kafka) The Secrets Manager ARN of your secret key containing the
--     certificate chain (X.509 PEM), private key (PKCS#8 PEM), and private
--     key password (optional) used for mutual TLS authentication of your
--     MSK\/Apache Kafka brokers.
--
-- -   @SERVER_ROOT_CA_CERTIFICATE@ - (Self-managed Apache Kafka) The
--     Secrets Manager ARN of your secret key containing the root CA
--     certificate (X.509 PEM) used for TLS encryption of your Apache Kafka
--     brokers.
--
-- 'uri', 'sourceAccessConfiguration_uri' - The value for your chosen configuration in @Type@. For example:
-- @\"URI\": \"arn:aws:secretsmanager:us-east-1:01234567890:secret:MyBrokerSecretName\"@.
newSourceAccessConfiguration ::
  SourceAccessConfiguration
newSourceAccessConfiguration =
  SourceAccessConfiguration'
    { type' = Prelude.Nothing,
      uri = Prelude.Nothing
    }

-- | The type of authentication protocol, VPC components, or virtual host for
-- your event source. For example: @\"Type\":\"SASL_SCRAM_512_AUTH\"@.
--
-- -   @BASIC_AUTH@ - (Amazon MQ) The Secrets Manager secret that stores
--     your broker credentials.
--
-- -   @BASIC_AUTH@ - (Self-managed Apache Kafka) The Secrets Manager ARN
--     of your secret key used for SASL\/PLAIN authentication of your
--     Apache Kafka brokers.
--
-- -   @VPC_SUBNET@ - (Self-managed Apache Kafka) The subnets associated
--     with your VPC. Lambda connects to these subnets to fetch data from
--     your self-managed Apache Kafka cluster.
--
-- -   @VPC_SECURITY_GROUP@ - (Self-managed Apache Kafka) The VPC security
--     group used to manage access to your self-managed Apache Kafka
--     brokers.
--
-- -   @SASL_SCRAM_256_AUTH@ - (Self-managed Apache Kafka) The Secrets
--     Manager ARN of your secret key used for SASL SCRAM-256
--     authentication of your self-managed Apache Kafka brokers.
--
-- -   @SASL_SCRAM_512_AUTH@ - (Amazon MSK, Self-managed Apache Kafka) The
--     Secrets Manager ARN of your secret key used for SASL SCRAM-512
--     authentication of your self-managed Apache Kafka brokers.
--
-- -   @VIRTUAL_HOST@ - (RabbitMQ) The name of the virtual host in your
--     RabbitMQ broker. Lambda uses this RabbitMQ host as the event source.
--     This property cannot be specified in an UpdateEventSourceMapping API
--     call.
--
-- -   @CLIENT_CERTIFICATE_TLS_AUTH@ - (Amazon MSK, self-managed Apache
--     Kafka) The Secrets Manager ARN of your secret key containing the
--     certificate chain (X.509 PEM), private key (PKCS#8 PEM), and private
--     key password (optional) used for mutual TLS authentication of your
--     MSK\/Apache Kafka brokers.
--
-- -   @SERVER_ROOT_CA_CERTIFICATE@ - (Self-managed Apache Kafka) The
--     Secrets Manager ARN of your secret key containing the root CA
--     certificate (X.509 PEM) used for TLS encryption of your Apache Kafka
--     brokers.
sourceAccessConfiguration_type :: Lens.Lens' SourceAccessConfiguration (Prelude.Maybe SourceAccessType)
sourceAccessConfiguration_type = Lens.lens (\SourceAccessConfiguration' {type'} -> type') (\s@SourceAccessConfiguration' {} a -> s {type' = a} :: SourceAccessConfiguration)

-- | The value for your chosen configuration in @Type@. For example:
-- @\"URI\": \"arn:aws:secretsmanager:us-east-1:01234567890:secret:MyBrokerSecretName\"@.
sourceAccessConfiguration_uri :: Lens.Lens' SourceAccessConfiguration (Prelude.Maybe Prelude.Text)
sourceAccessConfiguration_uri = Lens.lens (\SourceAccessConfiguration' {uri} -> uri) (\s@SourceAccessConfiguration' {} a -> s {uri = a} :: SourceAccessConfiguration)

instance Data.FromJSON SourceAccessConfiguration where
  parseJSON =
    Data.withObject
      "SourceAccessConfiguration"
      ( \x ->
          SourceAccessConfiguration'
            Prelude.<$> (x Data..:? "Type") Prelude.<*> (x Data..:? "URI")
      )

instance Prelude.Hashable SourceAccessConfiguration where
  hashWithSalt _salt SourceAccessConfiguration' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` uri

instance Prelude.NFData SourceAccessConfiguration where
  rnf SourceAccessConfiguration' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf uri

instance Data.ToJSON SourceAccessConfiguration where
  toJSON SourceAccessConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            ("URI" Data..=) Prelude.<$> uri
          ]
      )
