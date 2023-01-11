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
-- Module      : Amazonka.PinpointEmail.Types.KinesisFirehoseDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.KinesisFirehoseDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that defines an Amazon Kinesis Data Firehose destination for
-- email events. You can use Amazon Kinesis Data Firehose to stream data to
-- other services, such as Amazon S3 and Amazon Redshift.
--
-- /See:/ 'newKinesisFirehoseDestination' smart constructor.
data KinesisFirehoseDestination = KinesisFirehoseDestination'
  { -- | The Amazon Resource Name (ARN) of the IAM role that Amazon Pinpoint uses
    -- when sending email events to the Amazon Kinesis Data Firehose stream.
    iamRoleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Kinesis Data Firehose
    -- stream that Amazon Pinpoint sends email events to.
    deliveryStreamArn :: Prelude.Text
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
-- 'iamRoleArn', 'kinesisFirehoseDestination_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role that Amazon Pinpoint uses
-- when sending email events to the Amazon Kinesis Data Firehose stream.
--
-- 'deliveryStreamArn', 'kinesisFirehoseDestination_deliveryStreamArn' - The Amazon Resource Name (ARN) of the Amazon Kinesis Data Firehose
-- stream that Amazon Pinpoint sends email events to.
newKinesisFirehoseDestination ::
  -- | 'iamRoleArn'
  Prelude.Text ->
  -- | 'deliveryStreamArn'
  Prelude.Text ->
  KinesisFirehoseDestination
newKinesisFirehoseDestination
  pIamRoleArn_
  pDeliveryStreamArn_ =
    KinesisFirehoseDestination'
      { iamRoleArn =
          pIamRoleArn_,
        deliveryStreamArn = pDeliveryStreamArn_
      }

-- | The Amazon Resource Name (ARN) of the IAM role that Amazon Pinpoint uses
-- when sending email events to the Amazon Kinesis Data Firehose stream.
kinesisFirehoseDestination_iamRoleArn :: Lens.Lens' KinesisFirehoseDestination Prelude.Text
kinesisFirehoseDestination_iamRoleArn = Lens.lens (\KinesisFirehoseDestination' {iamRoleArn} -> iamRoleArn) (\s@KinesisFirehoseDestination' {} a -> s {iamRoleArn = a} :: KinesisFirehoseDestination)

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis Data Firehose
-- stream that Amazon Pinpoint sends email events to.
kinesisFirehoseDestination_deliveryStreamArn :: Lens.Lens' KinesisFirehoseDestination Prelude.Text
kinesisFirehoseDestination_deliveryStreamArn = Lens.lens (\KinesisFirehoseDestination' {deliveryStreamArn} -> deliveryStreamArn) (\s@KinesisFirehoseDestination' {} a -> s {deliveryStreamArn = a} :: KinesisFirehoseDestination)

instance Data.FromJSON KinesisFirehoseDestination where
  parseJSON =
    Data.withObject
      "KinesisFirehoseDestination"
      ( \x ->
          KinesisFirehoseDestination'
            Prelude.<$> (x Data..: "IamRoleArn")
            Prelude.<*> (x Data..: "DeliveryStreamArn")
      )

instance Prelude.Hashable KinesisFirehoseDestination where
  hashWithSalt _salt KinesisFirehoseDestination' {..} =
    _salt `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` deliveryStreamArn

instance Prelude.NFData KinesisFirehoseDestination where
  rnf KinesisFirehoseDestination' {..} =
    Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf deliveryStreamArn

instance Data.ToJSON KinesisFirehoseDestination where
  toJSON KinesisFirehoseDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IamRoleArn" Data..= iamRoleArn),
            Prelude.Just
              ("DeliveryStreamArn" Data..= deliveryStreamArn)
          ]
      )
