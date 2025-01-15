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
-- Module      : Amazonka.SES.Types.KinesisFirehoseDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.KinesisFirehoseDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML KinesisFirehoseDestination where
  parseXML x =
    KinesisFirehoseDestination'
      Prelude.<$> (x Data..@ "IAMRoleARN")
      Prelude.<*> (x Data..@ "DeliveryStreamARN")

instance Prelude.Hashable KinesisFirehoseDestination where
  hashWithSalt _salt KinesisFirehoseDestination' {..} =
    _salt
      `Prelude.hashWithSalt` iAMRoleARN
      `Prelude.hashWithSalt` deliveryStreamARN

instance Prelude.NFData KinesisFirehoseDestination where
  rnf KinesisFirehoseDestination' {..} =
    Prelude.rnf iAMRoleARN `Prelude.seq`
      Prelude.rnf deliveryStreamARN

instance Data.ToQuery KinesisFirehoseDestination where
  toQuery KinesisFirehoseDestination' {..} =
    Prelude.mconcat
      [ "IAMRoleARN" Data.=: iAMRoleARN,
        "DeliveryStreamARN" Data.=: deliveryStreamARN
      ]
