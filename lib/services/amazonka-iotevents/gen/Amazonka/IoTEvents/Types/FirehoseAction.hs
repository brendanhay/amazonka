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
-- Module      : Amazonka.IoTEvents.Types.FirehoseAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.FirehoseAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.Payload
import qualified Amazonka.Prelude as Prelude

-- | Sends information about the detector model instance and the event that
-- triggered the action to an Amazon Kinesis Data Firehose delivery stream.
--
-- /See:/ 'newFirehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
  { -- | A character separator that is used to separate records written to the
    -- Kinesis Data Firehose delivery stream. Valid values are: \'\\n\'
    -- (newline), \'\\t\' (tab), \'\\r\\n\' (Windows newline), \',\' (comma).
    separator :: Prelude.Maybe Prelude.Text,
    -- | You can configure the action payload when you send a message to an
    -- Amazon Kinesis Data Firehose delivery stream.
    payload :: Prelude.Maybe Payload,
    -- | The name of the Kinesis Data Firehose delivery stream where the data is
    -- written.
    deliveryStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirehoseAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'separator', 'firehoseAction_separator' - A character separator that is used to separate records written to the
-- Kinesis Data Firehose delivery stream. Valid values are: \'\\n\'
-- (newline), \'\\t\' (tab), \'\\r\\n\' (Windows newline), \',\' (comma).
--
-- 'payload', 'firehoseAction_payload' - You can configure the action payload when you send a message to an
-- Amazon Kinesis Data Firehose delivery stream.
--
-- 'deliveryStreamName', 'firehoseAction_deliveryStreamName' - The name of the Kinesis Data Firehose delivery stream where the data is
-- written.
newFirehoseAction ::
  -- | 'deliveryStreamName'
  Prelude.Text ->
  FirehoseAction
newFirehoseAction pDeliveryStreamName_ =
  FirehoseAction'
    { separator = Prelude.Nothing,
      payload = Prelude.Nothing,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | A character separator that is used to separate records written to the
-- Kinesis Data Firehose delivery stream. Valid values are: \'\\n\'
-- (newline), \'\\t\' (tab), \'\\r\\n\' (Windows newline), \',\' (comma).
firehoseAction_separator :: Lens.Lens' FirehoseAction (Prelude.Maybe Prelude.Text)
firehoseAction_separator = Lens.lens (\FirehoseAction' {separator} -> separator) (\s@FirehoseAction' {} a -> s {separator = a} :: FirehoseAction)

-- | You can configure the action payload when you send a message to an
-- Amazon Kinesis Data Firehose delivery stream.
firehoseAction_payload :: Lens.Lens' FirehoseAction (Prelude.Maybe Payload)
firehoseAction_payload = Lens.lens (\FirehoseAction' {payload} -> payload) (\s@FirehoseAction' {} a -> s {payload = a} :: FirehoseAction)

-- | The name of the Kinesis Data Firehose delivery stream where the data is
-- written.
firehoseAction_deliveryStreamName :: Lens.Lens' FirehoseAction Prelude.Text
firehoseAction_deliveryStreamName = Lens.lens (\FirehoseAction' {deliveryStreamName} -> deliveryStreamName) (\s@FirehoseAction' {} a -> s {deliveryStreamName = a} :: FirehoseAction)

instance Data.FromJSON FirehoseAction where
  parseJSON =
    Data.withObject
      "FirehoseAction"
      ( \x ->
          FirehoseAction'
            Prelude.<$> (x Data..:? "separator")
            Prelude.<*> (x Data..:? "payload")
            Prelude.<*> (x Data..: "deliveryStreamName")
      )

instance Prelude.Hashable FirehoseAction where
  hashWithSalt _salt FirehoseAction' {..} =
    _salt `Prelude.hashWithSalt` separator
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` deliveryStreamName

instance Prelude.NFData FirehoseAction where
  rnf FirehoseAction' {..} =
    Prelude.rnf separator
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf deliveryStreamName

instance Data.ToJSON FirehoseAction where
  toJSON FirehoseAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("separator" Data..=) Prelude.<$> separator,
            ("payload" Data..=) Prelude.<$> payload,
            Prelude.Just
              ("deliveryStreamName" Data..= deliveryStreamName)
          ]
      )
