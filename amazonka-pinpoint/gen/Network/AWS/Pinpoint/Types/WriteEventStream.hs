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
-- Module      : Network.AWS.Pinpoint.Types.WriteEventStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteEventStream where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the Amazon Resource Name (ARN) of an event stream to publish
-- events to and the AWS Identity and Access Management (IAM) role to use
-- when publishing those events.
--
-- /See:/ 'newWriteEventStream' smart constructor.
data WriteEventStream = WriteEventStream'
  { -- | The AWS Identity and Access Management (IAM) role that authorizes Amazon
    -- Pinpoint to publish event data to the stream in your AWS account.
    roleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or
    -- Amazon Kinesis Data Firehose delivery stream that you want to publish
    -- event data to.
    --
    -- For a Kinesis data stream, the ARN format is:
    -- arn:aws:kinesis:region:account-id:stream\/stream_name
    --
    -- For a Kinesis Data Firehose delivery stream, the ARN format is:
    -- arn:aws:firehose:region:account-id:deliverystream\/stream_name
    destinationStreamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WriteEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'writeEventStream_roleArn' - The AWS Identity and Access Management (IAM) role that authorizes Amazon
-- Pinpoint to publish event data to the stream in your AWS account.
--
-- 'destinationStreamArn', 'writeEventStream_destinationStreamArn' - The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or
-- Amazon Kinesis Data Firehose delivery stream that you want to publish
-- event data to.
--
-- For a Kinesis data stream, the ARN format is:
-- arn:aws:kinesis:region:account-id:stream\/stream_name
--
-- For a Kinesis Data Firehose delivery stream, the ARN format is:
-- arn:aws:firehose:region:account-id:deliverystream\/stream_name
newWriteEventStream ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'destinationStreamArn'
  Prelude.Text ->
  WriteEventStream
newWriteEventStream pRoleArn_ pDestinationStreamArn_ =
  WriteEventStream'
    { roleArn = pRoleArn_,
      destinationStreamArn = pDestinationStreamArn_
    }

-- | The AWS Identity and Access Management (IAM) role that authorizes Amazon
-- Pinpoint to publish event data to the stream in your AWS account.
writeEventStream_roleArn :: Lens.Lens' WriteEventStream Prelude.Text
writeEventStream_roleArn = Lens.lens (\WriteEventStream' {roleArn} -> roleArn) (\s@WriteEventStream' {} a -> s {roleArn = a} :: WriteEventStream)

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or
-- Amazon Kinesis Data Firehose delivery stream that you want to publish
-- event data to.
--
-- For a Kinesis data stream, the ARN format is:
-- arn:aws:kinesis:region:account-id:stream\/stream_name
--
-- For a Kinesis Data Firehose delivery stream, the ARN format is:
-- arn:aws:firehose:region:account-id:deliverystream\/stream_name
writeEventStream_destinationStreamArn :: Lens.Lens' WriteEventStream Prelude.Text
writeEventStream_destinationStreamArn = Lens.lens (\WriteEventStream' {destinationStreamArn} -> destinationStreamArn) (\s@WriteEventStream' {} a -> s {destinationStreamArn = a} :: WriteEventStream)

instance Prelude.Hashable WriteEventStream

instance Prelude.NFData WriteEventStream

instance Prelude.ToJSON WriteEventStream where
  toJSON WriteEventStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("RoleArn" Prelude..= roleArn),
            Prelude.Just
              ( "DestinationStreamArn"
                  Prelude..= destinationStreamArn
              )
          ]
      )
