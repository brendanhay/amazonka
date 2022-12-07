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
-- Module      : Amazonka.SmsVoice.Types.KinesisFirehoseDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SmsVoice.Types.KinesisFirehoseDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about an event destination that
-- sends data to Amazon Kinesis Data Firehose.
--
-- /See:/ 'newKinesisFirehoseDestination' smart constructor.
data KinesisFirehoseDestination = KinesisFirehoseDestination'
  { -- | The Amazon Resource Name (ARN) of an IAM role that can write data to an
    -- Amazon Kinesis Data Firehose stream.
    deliveryStreamArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Kinesis Data Firehose
    -- destination that you want to use in the event destination.
    iamRoleArn :: Prelude.Maybe Prelude.Text
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
-- 'deliveryStreamArn', 'kinesisFirehoseDestination_deliveryStreamArn' - The Amazon Resource Name (ARN) of an IAM role that can write data to an
-- Amazon Kinesis Data Firehose stream.
--
-- 'iamRoleArn', 'kinesisFirehoseDestination_iamRoleArn' - The Amazon Resource Name (ARN) of the Amazon Kinesis Data Firehose
-- destination that you want to use in the event destination.
newKinesisFirehoseDestination ::
  KinesisFirehoseDestination
newKinesisFirehoseDestination =
  KinesisFirehoseDestination'
    { deliveryStreamArn =
        Prelude.Nothing,
      iamRoleArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of an IAM role that can write data to an
-- Amazon Kinesis Data Firehose stream.
kinesisFirehoseDestination_deliveryStreamArn :: Lens.Lens' KinesisFirehoseDestination (Prelude.Maybe Prelude.Text)
kinesisFirehoseDestination_deliveryStreamArn = Lens.lens (\KinesisFirehoseDestination' {deliveryStreamArn} -> deliveryStreamArn) (\s@KinesisFirehoseDestination' {} a -> s {deliveryStreamArn = a} :: KinesisFirehoseDestination)

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis Data Firehose
-- destination that you want to use in the event destination.
kinesisFirehoseDestination_iamRoleArn :: Lens.Lens' KinesisFirehoseDestination (Prelude.Maybe Prelude.Text)
kinesisFirehoseDestination_iamRoleArn = Lens.lens (\KinesisFirehoseDestination' {iamRoleArn} -> iamRoleArn) (\s@KinesisFirehoseDestination' {} a -> s {iamRoleArn = a} :: KinesisFirehoseDestination)

instance Data.FromJSON KinesisFirehoseDestination where
  parseJSON =
    Data.withObject
      "KinesisFirehoseDestination"
      ( \x ->
          KinesisFirehoseDestination'
            Prelude.<$> (x Data..:? "DeliveryStreamArn")
            Prelude.<*> (x Data..:? "IamRoleArn")
      )

instance Prelude.Hashable KinesisFirehoseDestination where
  hashWithSalt _salt KinesisFirehoseDestination' {..} =
    _salt `Prelude.hashWithSalt` deliveryStreamArn
      `Prelude.hashWithSalt` iamRoleArn

instance Prelude.NFData KinesisFirehoseDestination where
  rnf KinesisFirehoseDestination' {..} =
    Prelude.rnf deliveryStreamArn
      `Prelude.seq` Prelude.rnf iamRoleArn

instance Data.ToJSON KinesisFirehoseDestination where
  toJSON KinesisFirehoseDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeliveryStreamArn" Data..=)
              Prelude.<$> deliveryStreamArn,
            ("IamRoleArn" Data..=) Prelude.<$> iamRoleArn
          ]
      )
