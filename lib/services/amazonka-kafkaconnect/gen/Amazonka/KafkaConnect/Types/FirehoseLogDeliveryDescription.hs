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
-- Module      : Amazonka.KafkaConnect.Types.FirehoseLogDeliveryDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.FirehoseLogDeliveryDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A description of the settings for delivering logs to Amazon Kinesis Data
-- Firehose.
--
-- /See:/ 'newFirehoseLogDeliveryDescription' smart constructor.
data FirehoseLogDeliveryDescription = FirehoseLogDeliveryDescription'
  { -- | The name of the Kinesis Data Firehose delivery stream that is the
    -- destination for log delivery.
    deliveryStream :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether connector logs get delivered to Amazon Kinesis Data
    -- Firehose.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirehoseLogDeliveryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryStream', 'firehoseLogDeliveryDescription_deliveryStream' - The name of the Kinesis Data Firehose delivery stream that is the
-- destination for log delivery.
--
-- 'enabled', 'firehoseLogDeliveryDescription_enabled' - Specifies whether connector logs get delivered to Amazon Kinesis Data
-- Firehose.
newFirehoseLogDeliveryDescription ::
  FirehoseLogDeliveryDescription
newFirehoseLogDeliveryDescription =
  FirehoseLogDeliveryDescription'
    { deliveryStream =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The name of the Kinesis Data Firehose delivery stream that is the
-- destination for log delivery.
firehoseLogDeliveryDescription_deliveryStream :: Lens.Lens' FirehoseLogDeliveryDescription (Prelude.Maybe Prelude.Text)
firehoseLogDeliveryDescription_deliveryStream = Lens.lens (\FirehoseLogDeliveryDescription' {deliveryStream} -> deliveryStream) (\s@FirehoseLogDeliveryDescription' {} a -> s {deliveryStream = a} :: FirehoseLogDeliveryDescription)

-- | Specifies whether connector logs get delivered to Amazon Kinesis Data
-- Firehose.
firehoseLogDeliveryDescription_enabled :: Lens.Lens' FirehoseLogDeliveryDescription (Prelude.Maybe Prelude.Bool)
firehoseLogDeliveryDescription_enabled = Lens.lens (\FirehoseLogDeliveryDescription' {enabled} -> enabled) (\s@FirehoseLogDeliveryDescription' {} a -> s {enabled = a} :: FirehoseLogDeliveryDescription)

instance Data.FromJSON FirehoseLogDeliveryDescription where
  parseJSON =
    Data.withObject
      "FirehoseLogDeliveryDescription"
      ( \x ->
          FirehoseLogDeliveryDescription'
            Prelude.<$> (x Data..:? "deliveryStream")
            Prelude.<*> (x Data..:? "enabled")
      )

instance
  Prelude.Hashable
    FirehoseLogDeliveryDescription
  where
  hashWithSalt
    _salt
    FirehoseLogDeliveryDescription' {..} =
      _salt `Prelude.hashWithSalt` deliveryStream
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    FirehoseLogDeliveryDescription
  where
  rnf FirehoseLogDeliveryDescription' {..} =
    Prelude.rnf deliveryStream
      `Prelude.seq` Prelude.rnf enabled
