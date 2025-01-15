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
-- Module      : Amazonka.KafkaConnect.Types.FirehoseLogDelivery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.FirehoseLogDelivery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings for delivering logs to Amazon Kinesis Data Firehose.
--
-- /See:/ 'newFirehoseLogDelivery' smart constructor.
data FirehoseLogDelivery = FirehoseLogDelivery'
  { -- | The name of the Kinesis Data Firehose delivery stream that is the
    -- destination for log delivery.
    deliveryStream :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether connector logs get delivered to Amazon Kinesis Data
    -- Firehose.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirehoseLogDelivery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStream', 'firehoseLogDelivery_deliveryStream' - The name of the Kinesis Data Firehose delivery stream that is the
-- destination for log delivery.
--
-- 'enabled', 'firehoseLogDelivery_enabled' - Specifies whether connector logs get delivered to Amazon Kinesis Data
-- Firehose.
newFirehoseLogDelivery ::
  -- | 'enabled'
  Prelude.Bool ->
  FirehoseLogDelivery
newFirehoseLogDelivery pEnabled_ =
  FirehoseLogDelivery'
    { deliveryStream =
        Prelude.Nothing,
      enabled = pEnabled_
    }

-- | The name of the Kinesis Data Firehose delivery stream that is the
-- destination for log delivery.
firehoseLogDelivery_deliveryStream :: Lens.Lens' FirehoseLogDelivery (Prelude.Maybe Prelude.Text)
firehoseLogDelivery_deliveryStream = Lens.lens (\FirehoseLogDelivery' {deliveryStream} -> deliveryStream) (\s@FirehoseLogDelivery' {} a -> s {deliveryStream = a} :: FirehoseLogDelivery)

-- | Specifies whether connector logs get delivered to Amazon Kinesis Data
-- Firehose.
firehoseLogDelivery_enabled :: Lens.Lens' FirehoseLogDelivery Prelude.Bool
firehoseLogDelivery_enabled = Lens.lens (\FirehoseLogDelivery' {enabled} -> enabled) (\s@FirehoseLogDelivery' {} a -> s {enabled = a} :: FirehoseLogDelivery)

instance Prelude.Hashable FirehoseLogDelivery where
  hashWithSalt _salt FirehoseLogDelivery' {..} =
    _salt
      `Prelude.hashWithSalt` deliveryStream
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData FirehoseLogDelivery where
  rnf FirehoseLogDelivery' {..} =
    Prelude.rnf deliveryStream `Prelude.seq`
      Prelude.rnf enabled

instance Data.ToJSON FirehoseLogDelivery where
  toJSON FirehoseLogDelivery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deliveryStream" Data..=)
              Prelude.<$> deliveryStream,
            Prelude.Just ("enabled" Data..= enabled)
          ]
      )
