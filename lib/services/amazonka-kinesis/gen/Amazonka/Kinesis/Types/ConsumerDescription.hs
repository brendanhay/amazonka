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
-- Module      : Amazonka.Kinesis.Types.ConsumerDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.ConsumerDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.ConsumerStatus
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the details of a registered consumer. This
-- type of object is returned by DescribeStreamConsumer.
--
-- /See:/ 'newConsumerDescription' smart constructor.
data ConsumerDescription = ConsumerDescription'
  { -- | The name of the consumer is something you choose when you register the
    -- consumer.
    consumerName :: Prelude.Text,
    -- | When you register a consumer, Kinesis Data Streams generates an ARN for
    -- it. You need this ARN to be able to call SubscribeToShard.
    --
    -- If you delete a consumer and then create a new one with the same name,
    -- it won\'t have the same ARN. That\'s because consumer ARNs contain the
    -- creation timestamp. This is important to keep in mind if you have IAM
    -- policies that reference consumer ARNs.
    consumerARN :: Prelude.Text,
    -- | A consumer can\'t read data while in the @CREATING@ or @DELETING@
    -- states.
    consumerStatus :: ConsumerStatus,
    consumerCreationTimestamp :: Data.POSIX,
    -- | The ARN of the stream with which you registered the consumer.
    streamARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConsumerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumerName', 'consumerDescription_consumerName' - The name of the consumer is something you choose when you register the
-- consumer.
--
-- 'consumerARN', 'consumerDescription_consumerARN' - When you register a consumer, Kinesis Data Streams generates an ARN for
-- it. You need this ARN to be able to call SubscribeToShard.
--
-- If you delete a consumer and then create a new one with the same name,
-- it won\'t have the same ARN. That\'s because consumer ARNs contain the
-- creation timestamp. This is important to keep in mind if you have IAM
-- policies that reference consumer ARNs.
--
-- 'consumerStatus', 'consumerDescription_consumerStatus' - A consumer can\'t read data while in the @CREATING@ or @DELETING@
-- states.
--
-- 'consumerCreationTimestamp', 'consumerDescription_consumerCreationTimestamp' -
--
-- 'streamARN', 'consumerDescription_streamARN' - The ARN of the stream with which you registered the consumer.
newConsumerDescription ::
  -- | 'consumerName'
  Prelude.Text ->
  -- | 'consumerARN'
  Prelude.Text ->
  -- | 'consumerStatus'
  ConsumerStatus ->
  -- | 'consumerCreationTimestamp'
  Prelude.UTCTime ->
  -- | 'streamARN'
  Prelude.Text ->
  ConsumerDescription
newConsumerDescription
  pConsumerName_
  pConsumerARN_
  pConsumerStatus_
  pConsumerCreationTimestamp_
  pStreamARN_ =
    ConsumerDescription'
      { consumerName = pConsumerName_,
        consumerARN = pConsumerARN_,
        consumerStatus = pConsumerStatus_,
        consumerCreationTimestamp =
          Data._Time Lens.# pConsumerCreationTimestamp_,
        streamARN = pStreamARN_
      }

-- | The name of the consumer is something you choose when you register the
-- consumer.
consumerDescription_consumerName :: Lens.Lens' ConsumerDescription Prelude.Text
consumerDescription_consumerName = Lens.lens (\ConsumerDescription' {consumerName} -> consumerName) (\s@ConsumerDescription' {} a -> s {consumerName = a} :: ConsumerDescription)

-- | When you register a consumer, Kinesis Data Streams generates an ARN for
-- it. You need this ARN to be able to call SubscribeToShard.
--
-- If you delete a consumer and then create a new one with the same name,
-- it won\'t have the same ARN. That\'s because consumer ARNs contain the
-- creation timestamp. This is important to keep in mind if you have IAM
-- policies that reference consumer ARNs.
consumerDescription_consumerARN :: Lens.Lens' ConsumerDescription Prelude.Text
consumerDescription_consumerARN = Lens.lens (\ConsumerDescription' {consumerARN} -> consumerARN) (\s@ConsumerDescription' {} a -> s {consumerARN = a} :: ConsumerDescription)

-- | A consumer can\'t read data while in the @CREATING@ or @DELETING@
-- states.
consumerDescription_consumerStatus :: Lens.Lens' ConsumerDescription ConsumerStatus
consumerDescription_consumerStatus = Lens.lens (\ConsumerDescription' {consumerStatus} -> consumerStatus) (\s@ConsumerDescription' {} a -> s {consumerStatus = a} :: ConsumerDescription)

consumerDescription_consumerCreationTimestamp :: Lens.Lens' ConsumerDescription Prelude.UTCTime
consumerDescription_consumerCreationTimestamp = Lens.lens (\ConsumerDescription' {consumerCreationTimestamp} -> consumerCreationTimestamp) (\s@ConsumerDescription' {} a -> s {consumerCreationTimestamp = a} :: ConsumerDescription) Prelude.. Data._Time

-- | The ARN of the stream with which you registered the consumer.
consumerDescription_streamARN :: Lens.Lens' ConsumerDescription Prelude.Text
consumerDescription_streamARN = Lens.lens (\ConsumerDescription' {streamARN} -> streamARN) (\s@ConsumerDescription' {} a -> s {streamARN = a} :: ConsumerDescription)

instance Data.FromJSON ConsumerDescription where
  parseJSON =
    Data.withObject
      "ConsumerDescription"
      ( \x ->
          ConsumerDescription'
            Prelude.<$> (x Data..: "ConsumerName")
            Prelude.<*> (x Data..: "ConsumerARN")
            Prelude.<*> (x Data..: "ConsumerStatus")
            Prelude.<*> (x Data..: "ConsumerCreationTimestamp")
            Prelude.<*> (x Data..: "StreamARN")
      )

instance Prelude.Hashable ConsumerDescription where
  hashWithSalt _salt ConsumerDescription' {..} =
    _salt
      `Prelude.hashWithSalt` consumerName
      `Prelude.hashWithSalt` consumerARN
      `Prelude.hashWithSalt` consumerStatus
      `Prelude.hashWithSalt` consumerCreationTimestamp
      `Prelude.hashWithSalt` streamARN

instance Prelude.NFData ConsumerDescription where
  rnf ConsumerDescription' {..} =
    Prelude.rnf consumerName
      `Prelude.seq` Prelude.rnf consumerARN
      `Prelude.seq` Prelude.rnf consumerStatus
      `Prelude.seq` Prelude.rnf consumerCreationTimestamp
      `Prelude.seq` Prelude.rnf streamARN
