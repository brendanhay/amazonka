{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kinesis.DeregisterStreamConsumer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To deregister a consumer, provide its ARN. Alternatively, you can
-- provide the ARN of the data stream and the name you gave the consumer
-- when you registered it. You may also provide all three parameters, as
-- long as they don\'t conflict with each other. If you don\'t know the
-- name or ARN of the consumer that you want to deregister, you can use the
-- ListStreamConsumers operation to get a list of the descriptions of all
-- the consumers that are currently registered with a given data stream.
-- The description of a consumer contains its name and ARN.
--
-- This operation has a limit of five transactions per second per stream.
module Amazonka.Kinesis.DeregisterStreamConsumer
  ( -- * Creating a Request
    DeregisterStreamConsumer (..),
    newDeregisterStreamConsumer,

    -- * Request Lenses
    deregisterStreamConsumer_consumerARN,
    deregisterStreamConsumer_consumerName,
    deregisterStreamConsumer_streamARN,

    -- * Destructuring the Response
    DeregisterStreamConsumerResponse (..),
    newDeregisterStreamConsumerResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterStreamConsumer' smart constructor.
data DeregisterStreamConsumer = DeregisterStreamConsumer'
  { -- | The ARN returned by Kinesis Data Streams when you registered the
    -- consumer. If you don\'t know the ARN of the consumer that you want to
    -- deregister, you can use the ListStreamConsumers operation to get a list
    -- of the descriptions of all the consumers that are currently registered
    -- with a given data stream. The description of a consumer contains its
    -- ARN.
    consumerARN :: Prelude.Maybe Prelude.Text,
    -- | The name that you gave to the consumer.
    consumerName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Kinesis data stream that the consumer is registered with.
    -- For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    streamARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterStreamConsumer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumerARN', 'deregisterStreamConsumer_consumerARN' - The ARN returned by Kinesis Data Streams when you registered the
-- consumer. If you don\'t know the ARN of the consumer that you want to
-- deregister, you can use the ListStreamConsumers operation to get a list
-- of the descriptions of all the consumers that are currently registered
-- with a given data stream. The description of a consumer contains its
-- ARN.
--
-- 'consumerName', 'deregisterStreamConsumer_consumerName' - The name that you gave to the consumer.
--
-- 'streamARN', 'deregisterStreamConsumer_streamARN' - The ARN of the Kinesis data stream that the consumer is registered with.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
newDeregisterStreamConsumer ::
  DeregisterStreamConsumer
newDeregisterStreamConsumer =
  DeregisterStreamConsumer'
    { consumerARN =
        Prelude.Nothing,
      consumerName = Prelude.Nothing,
      streamARN = Prelude.Nothing
    }

-- | The ARN returned by Kinesis Data Streams when you registered the
-- consumer. If you don\'t know the ARN of the consumer that you want to
-- deregister, you can use the ListStreamConsumers operation to get a list
-- of the descriptions of all the consumers that are currently registered
-- with a given data stream. The description of a consumer contains its
-- ARN.
deregisterStreamConsumer_consumerARN :: Lens.Lens' DeregisterStreamConsumer (Prelude.Maybe Prelude.Text)
deregisterStreamConsumer_consumerARN = Lens.lens (\DeregisterStreamConsumer' {consumerARN} -> consumerARN) (\s@DeregisterStreamConsumer' {} a -> s {consumerARN = a} :: DeregisterStreamConsumer)

-- | The name that you gave to the consumer.
deregisterStreamConsumer_consumerName :: Lens.Lens' DeregisterStreamConsumer (Prelude.Maybe Prelude.Text)
deregisterStreamConsumer_consumerName = Lens.lens (\DeregisterStreamConsumer' {consumerName} -> consumerName) (\s@DeregisterStreamConsumer' {} a -> s {consumerName = a} :: DeregisterStreamConsumer)

-- | The ARN of the Kinesis data stream that the consumer is registered with.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
deregisterStreamConsumer_streamARN :: Lens.Lens' DeregisterStreamConsumer (Prelude.Maybe Prelude.Text)
deregisterStreamConsumer_streamARN = Lens.lens (\DeregisterStreamConsumer' {streamARN} -> streamARN) (\s@DeregisterStreamConsumer' {} a -> s {streamARN = a} :: DeregisterStreamConsumer)

instance Core.AWSRequest DeregisterStreamConsumer where
  type
    AWSResponse DeregisterStreamConsumer =
      DeregisterStreamConsumerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeregisterStreamConsumerResponse'

instance Prelude.Hashable DeregisterStreamConsumer where
  hashWithSalt _salt DeregisterStreamConsumer' {..} =
    _salt `Prelude.hashWithSalt` consumerARN
      `Prelude.hashWithSalt` consumerName
      `Prelude.hashWithSalt` streamARN

instance Prelude.NFData DeregisterStreamConsumer where
  rnf DeregisterStreamConsumer' {..} =
    Prelude.rnf consumerARN
      `Prelude.seq` Prelude.rnf consumerName
      `Prelude.seq` Prelude.rnf streamARN

instance Data.ToHeaders DeregisterStreamConsumer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.DeregisterStreamConsumer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterStreamConsumer where
  toJSON DeregisterStreamConsumer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConsumerARN" Data..=) Prelude.<$> consumerARN,
            ("ConsumerName" Data..=) Prelude.<$> consumerName,
            ("StreamARN" Data..=) Prelude.<$> streamARN
          ]
      )

instance Data.ToPath DeregisterStreamConsumer where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterStreamConsumer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterStreamConsumerResponse' smart constructor.
data DeregisterStreamConsumerResponse = DeregisterStreamConsumerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterStreamConsumerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterStreamConsumerResponse ::
  DeregisterStreamConsumerResponse
newDeregisterStreamConsumerResponse =
  DeregisterStreamConsumerResponse'

instance
  Prelude.NFData
    DeregisterStreamConsumerResponse
  where
  rnf _ = ()
