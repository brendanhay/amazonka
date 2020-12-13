{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.FirehoseAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.FirehoseAction
  ( FirehoseAction (..),

    -- * Smart constructor
    mkFirehoseAction,

    -- * Lenses
    faBatchMode,
    faSeparator,
    faDeliveryStreamName,
    faRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action that writes data to an Amazon Kinesis Firehose stream.
--
-- /See:/ 'mkFirehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
  { -- | Whether to deliver the Kinesis Data Firehose stream as a batch by using <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > . The default value is @false@ .
    --
    -- When @batchMode@ is @true@ and the rule's SQL statement evaluates to an Array, each Array element forms one record in the <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > request. The resulting array can't have more than 500 records.
    batchMode :: Lude.Maybe Lude.Bool,
    -- | A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
    separator :: Lude.Maybe Lude.Text,
    -- | The delivery stream name.
    deliveryStreamName :: Lude.Text,
    -- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FirehoseAction' with the minimum fields required to make a request.
--
-- * 'batchMode' - Whether to deliver the Kinesis Data Firehose stream as a batch by using <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > . The default value is @false@ .
--
-- When @batchMode@ is @true@ and the rule's SQL statement evaluates to an Array, each Array element forms one record in the <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > request. The resulting array can't have more than 500 records.
-- * 'separator' - A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
-- * 'deliveryStreamName' - The delivery stream name.
-- * 'roleARN' - The IAM role that grants access to the Amazon Kinesis Firehose stream.
mkFirehoseAction ::
  -- | 'deliveryStreamName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  FirehoseAction
mkFirehoseAction pDeliveryStreamName_ pRoleARN_ =
  FirehoseAction'
    { batchMode = Lude.Nothing,
      separator = Lude.Nothing,
      deliveryStreamName = pDeliveryStreamName_,
      roleARN = pRoleARN_
    }

-- | Whether to deliver the Kinesis Data Firehose stream as a batch by using <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > . The default value is @false@ .
--
-- When @batchMode@ is @true@ and the rule's SQL statement evaluates to an Array, each Array element forms one record in the <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > request. The resulting array can't have more than 500 records.
--
-- /Note:/ Consider using 'batchMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faBatchMode :: Lens.Lens' FirehoseAction (Lude.Maybe Lude.Bool)
faBatchMode = Lens.lens (batchMode :: FirehoseAction -> Lude.Maybe Lude.Bool) (\s a -> s {batchMode = a} :: FirehoseAction)
{-# DEPRECATED faBatchMode "Use generic-lens or generic-optics with 'batchMode' instead." #-}

-- | A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
--
-- /Note:/ Consider using 'separator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faSeparator :: Lens.Lens' FirehoseAction (Lude.Maybe Lude.Text)
faSeparator = Lens.lens (separator :: FirehoseAction -> Lude.Maybe Lude.Text) (\s a -> s {separator = a} :: FirehoseAction)
{-# DEPRECATED faSeparator "Use generic-lens or generic-optics with 'separator' instead." #-}

-- | The delivery stream name.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faDeliveryStreamName :: Lens.Lens' FirehoseAction Lude.Text
faDeliveryStreamName = Lens.lens (deliveryStreamName :: FirehoseAction -> Lude.Text) (\s a -> s {deliveryStreamName = a} :: FirehoseAction)
{-# DEPRECATED faDeliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead." #-}

-- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faRoleARN :: Lens.Lens' FirehoseAction Lude.Text
faRoleARN = Lens.lens (roleARN :: FirehoseAction -> Lude.Text) (\s a -> s {roleARN = a} :: FirehoseAction)
{-# DEPRECATED faRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON FirehoseAction where
  parseJSON =
    Lude.withObject
      "FirehoseAction"
      ( \x ->
          FirehoseAction'
            Lude.<$> (x Lude..:? "batchMode")
            Lude.<*> (x Lude..:? "separator")
            Lude.<*> (x Lude..: "deliveryStreamName")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON FirehoseAction where
  toJSON FirehoseAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("batchMode" Lude..=) Lude.<$> batchMode,
            ("separator" Lude..=) Lude.<$> separator,
            Lude.Just ("deliveryStreamName" Lude..= deliveryStreamName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
