{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.FirehoseAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.FirehoseAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an action that writes data to an Amazon Kinesis Firehose
-- stream.
--
-- /See:/ 'newFirehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
  { -- | A character separator that will be used to separate records written to
    -- the Firehose stream. Valid values are: \'\\n\' (newline), \'\\t\' (tab),
    -- \'\\r\\n\' (Windows newline), \',\' (comma).
    separator :: Prelude.Maybe Prelude.Text,
    -- | Whether to deliver the Kinesis Data Firehose stream as a batch by using
    -- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
    -- . The default value is @false@.
    --
    -- When @batchMode@ is @true@ and the rule\'s SQL statement evaluates to an
    -- Array, each Array element forms one record in the
    -- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
    -- request. The resulting array can\'t have more than 500 records.
    batchMode :: Prelude.Maybe Prelude.Bool,
    -- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
    roleArn :: Prelude.Text,
    -- | The delivery stream name.
    deliveryStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FirehoseAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'separator', 'firehoseAction_separator' - A character separator that will be used to separate records written to
-- the Firehose stream. Valid values are: \'\\n\' (newline), \'\\t\' (tab),
-- \'\\r\\n\' (Windows newline), \',\' (comma).
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
    { separator = Prelude.Nothing,
      batchMode = Prelude.Nothing,
      roleArn = pRoleArn_,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | A character separator that will be used to separate records written to
-- the Firehose stream. Valid values are: \'\\n\' (newline), \'\\t\' (tab),
-- \'\\r\\n\' (Windows newline), \',\' (comma).
firehoseAction_separator :: Lens.Lens' FirehoseAction (Prelude.Maybe Prelude.Text)
firehoseAction_separator = Lens.lens (\FirehoseAction' {separator} -> separator) (\s@FirehoseAction' {} a -> s {separator = a} :: FirehoseAction)

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

-- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
firehoseAction_roleArn :: Lens.Lens' FirehoseAction Prelude.Text
firehoseAction_roleArn = Lens.lens (\FirehoseAction' {roleArn} -> roleArn) (\s@FirehoseAction' {} a -> s {roleArn = a} :: FirehoseAction)

-- | The delivery stream name.
firehoseAction_deliveryStreamName :: Lens.Lens' FirehoseAction Prelude.Text
firehoseAction_deliveryStreamName = Lens.lens (\FirehoseAction' {deliveryStreamName} -> deliveryStreamName) (\s@FirehoseAction' {} a -> s {deliveryStreamName = a} :: FirehoseAction)

instance Prelude.FromJSON FirehoseAction where
  parseJSON =
    Prelude.withObject
      "FirehoseAction"
      ( \x ->
          FirehoseAction'
            Prelude.<$> (x Prelude..:? "separator")
            Prelude.<*> (x Prelude..:? "batchMode")
            Prelude.<*> (x Prelude..: "roleArn")
            Prelude.<*> (x Prelude..: "deliveryStreamName")
      )

instance Prelude.Hashable FirehoseAction

instance Prelude.NFData FirehoseAction

instance Prelude.ToJSON FirehoseAction where
  toJSON FirehoseAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("separator" Prelude..=) Prelude.<$> separator,
            ("batchMode" Prelude..=) Prelude.<$> batchMode,
            Prelude.Just ("roleArn" Prelude..= roleArn),
            Prelude.Just
              ( "deliveryStreamName"
                  Prelude..= deliveryStreamName
              )
          ]
      )
