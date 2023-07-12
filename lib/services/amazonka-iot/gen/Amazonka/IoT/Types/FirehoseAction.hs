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
-- Module      : Amazonka.IoT.Types.FirehoseAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.FirehoseAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an action that writes data to an Amazon Kinesis Firehose
-- stream.
--
-- /See:/ 'newFirehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
  { -- | Whether to deliver the Kinesis Data Firehose stream as a batch by using
    -- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
    -- . The default value is @false@.
    --
    -- When @batchMode@ is @true@ and the rule\'s SQL statement evaluates to an
    -- Array, each Array element forms one record in the
    -- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
    -- request. The resulting array can\'t have more than 500 records.
    batchMode :: Prelude.Maybe Prelude.Bool,
    -- | A character separator that will be used to separate records written to
    -- the Firehose stream. Valid values are: \'\\n\' (newline), \'\\t\' (tab),
    -- \'\\r\\n\' (Windows newline), \',\' (comma).
    separator :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
    roleArn :: Prelude.Text,
    -- | The delivery stream name.
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
-- 'batchMode', 'firehoseAction_batchMode' - Whether to deliver the Kinesis Data Firehose stream as a batch by using
-- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
-- . The default value is @false@.
--
-- When @batchMode@ is @true@ and the rule\'s SQL statement evaluates to an
-- Array, each Array element forms one record in the
-- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
-- request. The resulting array can\'t have more than 500 records.
--
-- 'separator', 'firehoseAction_separator' - A character separator that will be used to separate records written to
-- the Firehose stream. Valid values are: \'\\n\' (newline), \'\\t\' (tab),
-- \'\\r\\n\' (Windows newline), \',\' (comma).
--
-- 'roleArn', 'firehoseAction_roleArn' - The IAM role that grants access to the Amazon Kinesis Firehose stream.
--
-- 'deliveryStreamName', 'firehoseAction_deliveryStreamName' - The delivery stream name.
newFirehoseAction ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'deliveryStreamName'
  Prelude.Text ->
  FirehoseAction
newFirehoseAction pRoleArn_ pDeliveryStreamName_ =
  FirehoseAction'
    { batchMode = Prelude.Nothing,
      separator = Prelude.Nothing,
      roleArn = pRoleArn_,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | Whether to deliver the Kinesis Data Firehose stream as a batch by using
-- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
-- . The default value is @false@.
--
-- When @batchMode@ is @true@ and the rule\'s SQL statement evaluates to an
-- Array, each Array element forms one record in the
-- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
-- request. The resulting array can\'t have more than 500 records.
firehoseAction_batchMode :: Lens.Lens' FirehoseAction (Prelude.Maybe Prelude.Bool)
firehoseAction_batchMode = Lens.lens (\FirehoseAction' {batchMode} -> batchMode) (\s@FirehoseAction' {} a -> s {batchMode = a} :: FirehoseAction)

-- | A character separator that will be used to separate records written to
-- the Firehose stream. Valid values are: \'\\n\' (newline), \'\\t\' (tab),
-- \'\\r\\n\' (Windows newline), \',\' (comma).
firehoseAction_separator :: Lens.Lens' FirehoseAction (Prelude.Maybe Prelude.Text)
firehoseAction_separator = Lens.lens (\FirehoseAction' {separator} -> separator) (\s@FirehoseAction' {} a -> s {separator = a} :: FirehoseAction)

-- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
firehoseAction_roleArn :: Lens.Lens' FirehoseAction Prelude.Text
firehoseAction_roleArn = Lens.lens (\FirehoseAction' {roleArn} -> roleArn) (\s@FirehoseAction' {} a -> s {roleArn = a} :: FirehoseAction)

-- | The delivery stream name.
firehoseAction_deliveryStreamName :: Lens.Lens' FirehoseAction Prelude.Text
firehoseAction_deliveryStreamName = Lens.lens (\FirehoseAction' {deliveryStreamName} -> deliveryStreamName) (\s@FirehoseAction' {} a -> s {deliveryStreamName = a} :: FirehoseAction)

instance Data.FromJSON FirehoseAction where
  parseJSON =
    Data.withObject
      "FirehoseAction"
      ( \x ->
          FirehoseAction'
            Prelude.<$> (x Data..:? "batchMode")
            Prelude.<*> (x Data..:? "separator")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "deliveryStreamName")
      )

instance Prelude.Hashable FirehoseAction where
  hashWithSalt _salt FirehoseAction' {..} =
    _salt
      `Prelude.hashWithSalt` batchMode
      `Prelude.hashWithSalt` separator
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` deliveryStreamName

instance Prelude.NFData FirehoseAction where
  rnf FirehoseAction' {..} =
    Prelude.rnf batchMode
      `Prelude.seq` Prelude.rnf separator
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf deliveryStreamName

instance Data.ToJSON FirehoseAction where
  toJSON FirehoseAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("batchMode" Data..=) Prelude.<$> batchMode,
            ("separator" Data..=) Prelude.<$> separator,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just
              ("deliveryStreamName" Data..= deliveryStreamName)
          ]
      )
