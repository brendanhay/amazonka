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
-- Module      : Network.AWS.SES.Types.KinesisFirehoseDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.KinesisFirehoseDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the delivery stream ARN and the IAM role ARN associated with an
-- Amazon Kinesis Firehose event destination.
--
-- Event destinations, such as Amazon Kinesis Firehose, are associated with
-- configuration sets, which enable you to publish email sending events.
-- For information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newKinesisFirehoseDestination' smart constructor.
data KinesisFirehoseDestination = KinesisFirehoseDestination'
  { -- | The ARN of the IAM role under which Amazon SES publishes email sending
    -- events to the Amazon Kinesis Firehose stream.
    iAMRoleARN :: Prelude.Text,
    -- | The ARN of the Amazon Kinesis Firehose stream that email sending events
    -- should be published to.
    deliveryStreamARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iAMRoleARN', 'kinesisFirehoseDestination_iAMRoleARN' - The ARN of the IAM role under which Amazon SES publishes email sending
-- events to the Amazon Kinesis Firehose stream.
--
-- 'deliveryStreamARN', 'kinesisFirehoseDestination_deliveryStreamARN' - The ARN of the Amazon Kinesis Firehose stream that email sending events
-- should be published to.
newKinesisFirehoseDestination ::
  -- | 'iAMRoleARN'
  Prelude.Text ->
  -- | 'deliveryStreamARN'
  Prelude.Text ->
  KinesisFirehoseDestination
newKinesisFirehoseDestination
  pIAMRoleARN_
  pDeliveryStreamARN_ =
    KinesisFirehoseDestination'
      { iAMRoleARN =
          pIAMRoleARN_,
        deliveryStreamARN = pDeliveryStreamARN_
      }

-- | The ARN of the IAM role under which Amazon SES publishes email sending
-- events to the Amazon Kinesis Firehose stream.
kinesisFirehoseDestination_iAMRoleARN :: Lens.Lens' KinesisFirehoseDestination Prelude.Text
kinesisFirehoseDestination_iAMRoleARN = Lens.lens (\KinesisFirehoseDestination' {iAMRoleARN} -> iAMRoleARN) (\s@KinesisFirehoseDestination' {} a -> s {iAMRoleARN = a} :: KinesisFirehoseDestination)

-- | The ARN of the Amazon Kinesis Firehose stream that email sending events
-- should be published to.
kinesisFirehoseDestination_deliveryStreamARN :: Lens.Lens' KinesisFirehoseDestination Prelude.Text
kinesisFirehoseDestination_deliveryStreamARN = Lens.lens (\KinesisFirehoseDestination' {deliveryStreamARN} -> deliveryStreamARN) (\s@KinesisFirehoseDestination' {} a -> s {deliveryStreamARN = a} :: KinesisFirehoseDestination)

instance Prelude.FromXML KinesisFirehoseDestination where
  parseXML x =
    KinesisFirehoseDestination'
      Prelude.<$> (x Prelude..@ "IAMRoleARN")
      Prelude.<*> (x Prelude..@ "DeliveryStreamARN")

instance Prelude.Hashable KinesisFirehoseDestination

instance Prelude.NFData KinesisFirehoseDestination

instance Prelude.ToQuery KinesisFirehoseDestination where
  toQuery KinesisFirehoseDestination' {..} =
    Prelude.mconcat
      [ "IAMRoleARN" Prelude.=: iAMRoleARN,
        "DeliveryStreamARN" Prelude.=: deliveryStreamARN
      ]
