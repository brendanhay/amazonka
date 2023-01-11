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
-- Module      : Amazonka.Kinesis.Types.Consumer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.Consumer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.ConsumerStatus
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the details of the consumer you registered.
-- This type of object is returned by RegisterStreamConsumer.
--
-- /See:/ 'newConsumer' smart constructor.
data Consumer = Consumer'
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
    consumerCreationTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Consumer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumerName', 'consumer_consumerName' - The name of the consumer is something you choose when you register the
-- consumer.
--
-- 'consumerARN', 'consumer_consumerARN' - When you register a consumer, Kinesis Data Streams generates an ARN for
-- it. You need this ARN to be able to call SubscribeToShard.
--
-- If you delete a consumer and then create a new one with the same name,
-- it won\'t have the same ARN. That\'s because consumer ARNs contain the
-- creation timestamp. This is important to keep in mind if you have IAM
-- policies that reference consumer ARNs.
--
-- 'consumerStatus', 'consumer_consumerStatus' - A consumer can\'t read data while in the @CREATING@ or @DELETING@
-- states.
--
-- 'consumerCreationTimestamp', 'consumer_consumerCreationTimestamp' -
newConsumer ::
  -- | 'consumerName'
  Prelude.Text ->
  -- | 'consumerARN'
  Prelude.Text ->
  -- | 'consumerStatus'
  ConsumerStatus ->
  -- | 'consumerCreationTimestamp'
  Prelude.UTCTime ->
  Consumer
newConsumer
  pConsumerName_
  pConsumerARN_
  pConsumerStatus_
  pConsumerCreationTimestamp_ =
    Consumer'
      { consumerName = pConsumerName_,
        consumerARN = pConsumerARN_,
        consumerStatus = pConsumerStatus_,
        consumerCreationTimestamp =
          Data._Time Lens.# pConsumerCreationTimestamp_
      }

-- | The name of the consumer is something you choose when you register the
-- consumer.
consumer_consumerName :: Lens.Lens' Consumer Prelude.Text
consumer_consumerName = Lens.lens (\Consumer' {consumerName} -> consumerName) (\s@Consumer' {} a -> s {consumerName = a} :: Consumer)

-- | When you register a consumer, Kinesis Data Streams generates an ARN for
-- it. You need this ARN to be able to call SubscribeToShard.
--
-- If you delete a consumer and then create a new one with the same name,
-- it won\'t have the same ARN. That\'s because consumer ARNs contain the
-- creation timestamp. This is important to keep in mind if you have IAM
-- policies that reference consumer ARNs.
consumer_consumerARN :: Lens.Lens' Consumer Prelude.Text
consumer_consumerARN = Lens.lens (\Consumer' {consumerARN} -> consumerARN) (\s@Consumer' {} a -> s {consumerARN = a} :: Consumer)

-- | A consumer can\'t read data while in the @CREATING@ or @DELETING@
-- states.
consumer_consumerStatus :: Lens.Lens' Consumer ConsumerStatus
consumer_consumerStatus = Lens.lens (\Consumer' {consumerStatus} -> consumerStatus) (\s@Consumer' {} a -> s {consumerStatus = a} :: Consumer)

-- |
consumer_consumerCreationTimestamp :: Lens.Lens' Consumer Prelude.UTCTime
consumer_consumerCreationTimestamp = Lens.lens (\Consumer' {consumerCreationTimestamp} -> consumerCreationTimestamp) (\s@Consumer' {} a -> s {consumerCreationTimestamp = a} :: Consumer) Prelude.. Data._Time

instance Data.FromJSON Consumer where
  parseJSON =
    Data.withObject
      "Consumer"
      ( \x ->
          Consumer'
            Prelude.<$> (x Data..: "ConsumerName")
            Prelude.<*> (x Data..: "ConsumerARN")
            Prelude.<*> (x Data..: "ConsumerStatus")
            Prelude.<*> (x Data..: "ConsumerCreationTimestamp")
      )

instance Prelude.Hashable Consumer where
  hashWithSalt _salt Consumer' {..} =
    _salt `Prelude.hashWithSalt` consumerName
      `Prelude.hashWithSalt` consumerARN
      `Prelude.hashWithSalt` consumerStatus
      `Prelude.hashWithSalt` consumerCreationTimestamp

instance Prelude.NFData Consumer where
  rnf Consumer' {..} =
    Prelude.rnf consumerName
      `Prelude.seq` Prelude.rnf consumerARN
      `Prelude.seq` Prelude.rnf consumerStatus
      `Prelude.seq` Prelude.rnf consumerCreationTimestamp
