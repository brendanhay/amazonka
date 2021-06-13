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
-- Module      : Network.AWS.Lambda.Types.SourceAccessConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.SourceAccessConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.SourceAccessType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | You can specify the authentication protocol, or the VPC components to
-- secure access to your event source.
--
-- /See:/ 'newSourceAccessConfiguration' smart constructor.
data SourceAccessConfiguration = SourceAccessConfiguration'
  { -- | The value for your chosen configuration in @Type@. For example:
    -- @\"URI\": \"arn:aws:secretsmanager:us-east-1:01234567890:secret:MyBrokerSecretName\"@.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The type of authentication protocol or the VPC components for your event
    -- source. For example: @\"Type\":\"SASL_SCRAM_512_AUTH\"@.
    --
    -- -   @BASIC_AUTH@ - (MQ) The Secrets Manager secret that stores your
    --     broker credentials.
    --
    -- -   @VPC_SUBNET@ - The subnets associated with your VPC. Lambda connects
    --     to these subnets to fetch data from your Self-Managed Apache Kafka
    --     cluster.
    --
    -- -   @VPC_SECURITY_GROUP@ - The VPC security group used to manage access
    --     to your Self-Managed Apache Kafka brokers.
    --
    -- -   @SASL_SCRAM_256_AUTH@ - The Secrets Manager ARN of your secret key
    --     used for SASL SCRAM-256 authentication of your Self-Managed Apache
    --     Kafka brokers.
    --
    -- -   @SASL_SCRAM_512_AUTH@ - The Secrets Manager ARN of your secret key
    --     used for SASL SCRAM-512 authentication of your Self-Managed Apache
    --     Kafka brokers.
    type' :: Prelude.Maybe SourceAccessType
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
-- 'uri', 'sourceAccessConfiguration_uri' - The value for your chosen configuration in @Type@. For example:
-- @\"URI\": \"arn:aws:secretsmanager:us-east-1:01234567890:secret:MyBrokerSecretName\"@.
--
-- 'type'', 'sourceAccessConfiguration_type' - The type of authentication protocol or the VPC components for your event
-- source. For example: @\"Type\":\"SASL_SCRAM_512_AUTH\"@.
--
-- -   @BASIC_AUTH@ - (MQ) The Secrets Manager secret that stores your
--     broker credentials.
--
-- -   @VPC_SUBNET@ - The subnets associated with your VPC. Lambda connects
--     to these subnets to fetch data from your Self-Managed Apache Kafka
--     cluster.
--
-- -   @VPC_SECURITY_GROUP@ - The VPC security group used to manage access
--     to your Self-Managed Apache Kafka brokers.
--
-- -   @SASL_SCRAM_256_AUTH@ - The Secrets Manager ARN of your secret key
--     used for SASL SCRAM-256 authentication of your Self-Managed Apache
--     Kafka brokers.
--
-- -   @SASL_SCRAM_512_AUTH@ - The Secrets Manager ARN of your secret key
--     used for SASL SCRAM-512 authentication of your Self-Managed Apache
--     Kafka brokers.
newSourceAccessConfiguration ::
  SourceAccessConfiguration
newSourceAccessConfiguration =
  SourceAccessConfiguration'
    { uri = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The value for your chosen configuration in @Type@. For example:
-- @\"URI\": \"arn:aws:secretsmanager:us-east-1:01234567890:secret:MyBrokerSecretName\"@.
sourceAccessConfiguration_uri :: Lens.Lens' SourceAccessConfiguration (Prelude.Maybe Prelude.Text)
sourceAccessConfiguration_uri = Lens.lens (\SourceAccessConfiguration' {uri} -> uri) (\s@SourceAccessConfiguration' {} a -> s {uri = a} :: SourceAccessConfiguration)

-- | The type of authentication protocol or the VPC components for your event
-- source. For example: @\"Type\":\"SASL_SCRAM_512_AUTH\"@.
--
-- -   @BASIC_AUTH@ - (MQ) The Secrets Manager secret that stores your
--     broker credentials.
--
-- -   @VPC_SUBNET@ - The subnets associated with your VPC. Lambda connects
--     to these subnets to fetch data from your Self-Managed Apache Kafka
--     cluster.
--
-- -   @VPC_SECURITY_GROUP@ - The VPC security group used to manage access
--     to your Self-Managed Apache Kafka brokers.
--
-- -   @SASL_SCRAM_256_AUTH@ - The Secrets Manager ARN of your secret key
--     used for SASL SCRAM-256 authentication of your Self-Managed Apache
--     Kafka brokers.
--
-- -   @SASL_SCRAM_512_AUTH@ - The Secrets Manager ARN of your secret key
--     used for SASL SCRAM-512 authentication of your Self-Managed Apache
--     Kafka brokers.
sourceAccessConfiguration_type :: Lens.Lens' SourceAccessConfiguration (Prelude.Maybe SourceAccessType)
sourceAccessConfiguration_type = Lens.lens (\SourceAccessConfiguration' {type'} -> type') (\s@SourceAccessConfiguration' {} a -> s {type' = a} :: SourceAccessConfiguration)

instance Core.FromJSON SourceAccessConfiguration where
  parseJSON =
    Core.withObject
      "SourceAccessConfiguration"
      ( \x ->
          SourceAccessConfiguration'
            Prelude.<$> (x Core..:? "URI") Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable SourceAccessConfiguration

instance Prelude.NFData SourceAccessConfiguration

instance Core.ToJSON SourceAccessConfiguration where
  toJSON SourceAccessConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("URI" Core..=) Prelude.<$> uri,
            ("Type" Core..=) Prelude.<$> type'
          ]
      )
