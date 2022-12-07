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
-- Module      : Amazonka.Kinesis.RegisterStreamConsumer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a consumer with a Kinesis data stream. When you use this
-- operation, the consumer you register can then call SubscribeToShard to
-- receive data from the stream using enhanced fan-out, at a rate of up to
-- 2 MiB per second for every shard you subscribe to. This rate is
-- unaffected by the total number of consumers that read from the same
-- stream.
--
-- You can register up to 20 consumers per stream. A given consumer can
-- only be registered with one stream at a time.
--
-- For an example of how to use this operations, see
-- </streams/latest/dev/building-enhanced-consumers-api.html Enhanced Fan-Out Using the Kinesis Data Streams API>.
--
-- The use of this operation has a limit of five transactions per second
-- per account. Also, only 5 consumers can be created simultaneously. In
-- other words, you cannot have more than 5 consumers in a @CREATING@
-- status at the same time. Registering a 6th consumer while there are 5 in
-- a @CREATING@ status results in a @LimitExceededException@.
module Amazonka.Kinesis.RegisterStreamConsumer
  ( -- * Creating a Request
    RegisterStreamConsumer (..),
    newRegisterStreamConsumer,

    -- * Request Lenses
    registerStreamConsumer_streamARN,
    registerStreamConsumer_consumerName,

    -- * Destructuring the Response
    RegisterStreamConsumerResponse (..),
    newRegisterStreamConsumerResponse,

    -- * Response Lenses
    registerStreamConsumerResponse_httpStatus,
    registerStreamConsumerResponse_consumer,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterStreamConsumer' smart constructor.
data RegisterStreamConsumer = RegisterStreamConsumer'
  { -- | The ARN of the Kinesis data stream that you want to register the
    -- consumer with. For more info, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
    streamARN :: Prelude.Text,
    -- | For a given Kinesis data stream, each consumer must have a unique name.
    -- However, consumer names don\'t have to be unique across data streams.
    consumerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterStreamConsumer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamARN', 'registerStreamConsumer_streamARN' - The ARN of the Kinesis data stream that you want to register the
-- consumer with. For more info, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
--
-- 'consumerName', 'registerStreamConsumer_consumerName' - For a given Kinesis data stream, each consumer must have a unique name.
-- However, consumer names don\'t have to be unique across data streams.
newRegisterStreamConsumer ::
  -- | 'streamARN'
  Prelude.Text ->
  -- | 'consumerName'
  Prelude.Text ->
  RegisterStreamConsumer
newRegisterStreamConsumer pStreamARN_ pConsumerName_ =
  RegisterStreamConsumer'
    { streamARN = pStreamARN_,
      consumerName = pConsumerName_
    }

-- | The ARN of the Kinesis data stream that you want to register the
-- consumer with. For more info, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kinesis-streams Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>.
registerStreamConsumer_streamARN :: Lens.Lens' RegisterStreamConsumer Prelude.Text
registerStreamConsumer_streamARN = Lens.lens (\RegisterStreamConsumer' {streamARN} -> streamARN) (\s@RegisterStreamConsumer' {} a -> s {streamARN = a} :: RegisterStreamConsumer)

-- | For a given Kinesis data stream, each consumer must have a unique name.
-- However, consumer names don\'t have to be unique across data streams.
registerStreamConsumer_consumerName :: Lens.Lens' RegisterStreamConsumer Prelude.Text
registerStreamConsumer_consumerName = Lens.lens (\RegisterStreamConsumer' {consumerName} -> consumerName) (\s@RegisterStreamConsumer' {} a -> s {consumerName = a} :: RegisterStreamConsumer)

instance Core.AWSRequest RegisterStreamConsumer where
  type
    AWSResponse RegisterStreamConsumer =
      RegisterStreamConsumerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterStreamConsumerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Consumer")
      )

instance Prelude.Hashable RegisterStreamConsumer where
  hashWithSalt _salt RegisterStreamConsumer' {..} =
    _salt `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` consumerName

instance Prelude.NFData RegisterStreamConsumer where
  rnf RegisterStreamConsumer' {..} =
    Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf consumerName

instance Data.ToHeaders RegisterStreamConsumer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.RegisterStreamConsumer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterStreamConsumer where
  toJSON RegisterStreamConsumer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamARN" Data..= streamARN),
            Prelude.Just ("ConsumerName" Data..= consumerName)
          ]
      )

instance Data.ToPath RegisterStreamConsumer where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterStreamConsumer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterStreamConsumerResponse' smart constructor.
data RegisterStreamConsumerResponse = RegisterStreamConsumerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that represents the details of the consumer you registered.
    -- When you register a consumer, it gets an ARN that is generated by
    -- Kinesis Data Streams.
    consumer :: Consumer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterStreamConsumerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerStreamConsumerResponse_httpStatus' - The response's http status code.
--
-- 'consumer', 'registerStreamConsumerResponse_consumer' - An object that represents the details of the consumer you registered.
-- When you register a consumer, it gets an ARN that is generated by
-- Kinesis Data Streams.
newRegisterStreamConsumerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'consumer'
  Consumer ->
  RegisterStreamConsumerResponse
newRegisterStreamConsumerResponse
  pHttpStatus_
  pConsumer_ =
    RegisterStreamConsumerResponse'
      { httpStatus =
          pHttpStatus_,
        consumer = pConsumer_
      }

-- | The response's http status code.
registerStreamConsumerResponse_httpStatus :: Lens.Lens' RegisterStreamConsumerResponse Prelude.Int
registerStreamConsumerResponse_httpStatus = Lens.lens (\RegisterStreamConsumerResponse' {httpStatus} -> httpStatus) (\s@RegisterStreamConsumerResponse' {} a -> s {httpStatus = a} :: RegisterStreamConsumerResponse)

-- | An object that represents the details of the consumer you registered.
-- When you register a consumer, it gets an ARN that is generated by
-- Kinesis Data Streams.
registerStreamConsumerResponse_consumer :: Lens.Lens' RegisterStreamConsumerResponse Consumer
registerStreamConsumerResponse_consumer = Lens.lens (\RegisterStreamConsumerResponse' {consumer} -> consumer) (\s@RegisterStreamConsumerResponse' {} a -> s {consumer = a} :: RegisterStreamConsumerResponse)

instance
  Prelude.NFData
    RegisterStreamConsumerResponse
  where
  rnf RegisterStreamConsumerResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf consumer
