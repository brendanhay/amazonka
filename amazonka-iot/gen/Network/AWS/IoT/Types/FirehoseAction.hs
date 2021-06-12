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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an action that writes data to an Amazon Kinesis Firehose
-- stream.
--
-- /See:/ 'newFirehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
  { -- | A character separator that will be used to separate records written to
    -- the Firehose stream. Valid values are: \'\\n\' (newline), \'\\t\' (tab),
    -- \'\\r\\n\' (Windows newline), \',\' (comma).
    separator :: Core.Maybe Core.Text,
    -- | Whether to deliver the Kinesis Data Firehose stream as a batch by using
    -- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
    -- . The default value is @false@.
    --
    -- When @batchMode@ is @true@ and the rule\'s SQL statement evaluates to an
    -- Array, each Array element forms one record in the
    -- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
    -- request. The resulting array can\'t have more than 500 records.
    batchMode :: Core.Maybe Core.Bool,
    -- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
    roleArn :: Core.Text,
    -- | The delivery stream name.
    deliveryStreamName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'deliveryStreamName'
  Core.Text ->
  FirehoseAction
newFirehoseAction pRoleArn_ pDeliveryStreamName_ =
  FirehoseAction'
    { separator = Core.Nothing,
      batchMode = Core.Nothing,
      roleArn = pRoleArn_,
      deliveryStreamName = pDeliveryStreamName_
    }

-- | A character separator that will be used to separate records written to
-- the Firehose stream. Valid values are: \'\\n\' (newline), \'\\t\' (tab),
-- \'\\r\\n\' (Windows newline), \',\' (comma).
firehoseAction_separator :: Lens.Lens' FirehoseAction (Core.Maybe Core.Text)
firehoseAction_separator = Lens.lens (\FirehoseAction' {separator} -> separator) (\s@FirehoseAction' {} a -> s {separator = a} :: FirehoseAction)

-- | Whether to deliver the Kinesis Data Firehose stream as a batch by using
-- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
-- . The default value is @false@.
--
-- When @batchMode@ is @true@ and the rule\'s SQL statement evaluates to an
-- Array, each Array element forms one record in the
-- <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html PutRecordBatch>
-- request. The resulting array can\'t have more than 500 records.
firehoseAction_batchMode :: Lens.Lens' FirehoseAction (Core.Maybe Core.Bool)
firehoseAction_batchMode = Lens.lens (\FirehoseAction' {batchMode} -> batchMode) (\s@FirehoseAction' {} a -> s {batchMode = a} :: FirehoseAction)

-- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
firehoseAction_roleArn :: Lens.Lens' FirehoseAction Core.Text
firehoseAction_roleArn = Lens.lens (\FirehoseAction' {roleArn} -> roleArn) (\s@FirehoseAction' {} a -> s {roleArn = a} :: FirehoseAction)

-- | The delivery stream name.
firehoseAction_deliveryStreamName :: Lens.Lens' FirehoseAction Core.Text
firehoseAction_deliveryStreamName = Lens.lens (\FirehoseAction' {deliveryStreamName} -> deliveryStreamName) (\s@FirehoseAction' {} a -> s {deliveryStreamName = a} :: FirehoseAction)

instance Core.FromJSON FirehoseAction where
  parseJSON =
    Core.withObject
      "FirehoseAction"
      ( \x ->
          FirehoseAction'
            Core.<$> (x Core..:? "separator")
            Core.<*> (x Core..:? "batchMode")
            Core.<*> (x Core..: "roleArn")
            Core.<*> (x Core..: "deliveryStreamName")
      )

instance Core.Hashable FirehoseAction

instance Core.NFData FirehoseAction

instance Core.ToJSON FirehoseAction where
  toJSON FirehoseAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("separator" Core..=) Core.<$> separator,
            ("batchMode" Core..=) Core.<$> batchMode,
            Core.Just ("roleArn" Core..= roleArn),
            Core.Just
              ("deliveryStreamName" Core..= deliveryStreamName)
          ]
      )
