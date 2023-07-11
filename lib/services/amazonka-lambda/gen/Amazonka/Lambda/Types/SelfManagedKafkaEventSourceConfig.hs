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
-- Module      : Amazonka.Lambda.Types.SelfManagedKafkaEventSourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.SelfManagedKafkaEventSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specific configuration settings for a self-managed Apache Kafka event
-- source.
--
-- /See:/ 'newSelfManagedKafkaEventSourceConfig' smart constructor.
data SelfManagedKafkaEventSourceConfig = SelfManagedKafkaEventSourceConfig'
  { -- | The identifier for the Kafka consumer group to join. The consumer group
    -- ID must be unique among all your Kafka event sources. After creating a
    -- Kafka event source mapping with the consumer group ID specified, you
    -- cannot update this value. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/with-msk.html#services-msk-consumer-group-id Customizable consumer group ID>.
    consumerGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelfManagedKafkaEventSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumerGroupId', 'selfManagedKafkaEventSourceConfig_consumerGroupId' - The identifier for the Kafka consumer group to join. The consumer group
-- ID must be unique among all your Kafka event sources. After creating a
-- Kafka event source mapping with the consumer group ID specified, you
-- cannot update this value. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/with-msk.html#services-msk-consumer-group-id Customizable consumer group ID>.
newSelfManagedKafkaEventSourceConfig ::
  SelfManagedKafkaEventSourceConfig
newSelfManagedKafkaEventSourceConfig =
  SelfManagedKafkaEventSourceConfig'
    { consumerGroupId =
        Prelude.Nothing
    }

-- | The identifier for the Kafka consumer group to join. The consumer group
-- ID must be unique among all your Kafka event sources. After creating a
-- Kafka event source mapping with the consumer group ID specified, you
-- cannot update this value. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/with-msk.html#services-msk-consumer-group-id Customizable consumer group ID>.
selfManagedKafkaEventSourceConfig_consumerGroupId :: Lens.Lens' SelfManagedKafkaEventSourceConfig (Prelude.Maybe Prelude.Text)
selfManagedKafkaEventSourceConfig_consumerGroupId = Lens.lens (\SelfManagedKafkaEventSourceConfig' {consumerGroupId} -> consumerGroupId) (\s@SelfManagedKafkaEventSourceConfig' {} a -> s {consumerGroupId = a} :: SelfManagedKafkaEventSourceConfig)

instance
  Data.FromJSON
    SelfManagedKafkaEventSourceConfig
  where
  parseJSON =
    Data.withObject
      "SelfManagedKafkaEventSourceConfig"
      ( \x ->
          SelfManagedKafkaEventSourceConfig'
            Prelude.<$> (x Data..:? "ConsumerGroupId")
      )

instance
  Prelude.Hashable
    SelfManagedKafkaEventSourceConfig
  where
  hashWithSalt
    _salt
    SelfManagedKafkaEventSourceConfig' {..} =
      _salt `Prelude.hashWithSalt` consumerGroupId

instance
  Prelude.NFData
    SelfManagedKafkaEventSourceConfig
  where
  rnf SelfManagedKafkaEventSourceConfig' {..} =
    Prelude.rnf consumerGroupId

instance
  Data.ToJSON
    SelfManagedKafkaEventSourceConfig
  where
  toJSON SelfManagedKafkaEventSourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConsumerGroupId" Data..=)
              Prelude.<$> consumerGroupId
          ]
      )
