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
-- Module      : Network.AWS.RDS.StopActivityStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a database activity stream that was started using the AWS console,
-- the @start-activity-stream@ AWS CLI command, or the
-- @StartActivityStream@ action.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/DBActivityStreams.html Database Activity Streams>
-- in the /Amazon Aurora User Guide/.
module Network.AWS.RDS.StopActivityStream
  ( -- * Creating a Request
    StopActivityStream (..),
    newStopActivityStream,

    -- * Request Lenses
    stopActivityStream_applyImmediately,
    stopActivityStream_resourceArn,

    -- * Destructuring the Response
    StopActivityStreamResponse (..),
    newStopActivityStreamResponse,

    -- * Response Lenses
    stopActivityStreamResponse_status,
    stopActivityStreamResponse_kmsKeyId,
    stopActivityStreamResponse_kinesisStreamName,
    stopActivityStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopActivityStream' smart constructor.
data StopActivityStream = StopActivityStream'
  { -- | Specifies whether or not the database activity stream is to stop as soon
    -- as possible, regardless of the maintenance window for the database.
    applyImmediately :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the DB cluster for the database
    -- activity stream. For example,
    -- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
    resourceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopActivityStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyImmediately', 'stopActivityStream_applyImmediately' - Specifies whether or not the database activity stream is to stop as soon
-- as possible, regardless of the maintenance window for the database.
--
-- 'resourceArn', 'stopActivityStream_resourceArn' - The Amazon Resource Name (ARN) of the DB cluster for the database
-- activity stream. For example,
-- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
newStopActivityStream ::
  -- | 'resourceArn'
  Core.Text ->
  StopActivityStream
newStopActivityStream pResourceArn_ =
  StopActivityStream'
    { applyImmediately =
        Core.Nothing,
      resourceArn = pResourceArn_
    }

-- | Specifies whether or not the database activity stream is to stop as soon
-- as possible, regardless of the maintenance window for the database.
stopActivityStream_applyImmediately :: Lens.Lens' StopActivityStream (Core.Maybe Core.Bool)
stopActivityStream_applyImmediately = Lens.lens (\StopActivityStream' {applyImmediately} -> applyImmediately) (\s@StopActivityStream' {} a -> s {applyImmediately = a} :: StopActivityStream)

-- | The Amazon Resource Name (ARN) of the DB cluster for the database
-- activity stream. For example,
-- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
stopActivityStream_resourceArn :: Lens.Lens' StopActivityStream Core.Text
stopActivityStream_resourceArn = Lens.lens (\StopActivityStream' {resourceArn} -> resourceArn) (\s@StopActivityStream' {} a -> s {resourceArn = a} :: StopActivityStream)

instance Core.AWSRequest StopActivityStream where
  type
    AWSResponse StopActivityStream =
      StopActivityStreamResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StopActivityStreamResult"
      ( \s h x ->
          StopActivityStreamResponse'
            Core.<$> (x Core..@? "Status")
            Core.<*> (x Core..@? "KmsKeyId")
            Core.<*> (x Core..@? "KinesisStreamName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopActivityStream

instance Core.NFData StopActivityStream

instance Core.ToHeaders StopActivityStream where
  toHeaders = Core.const Core.mempty

instance Core.ToPath StopActivityStream where
  toPath = Core.const "/"

instance Core.ToQuery StopActivityStream where
  toQuery StopActivityStream' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("StopActivityStream" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "ApplyImmediately" Core.=: applyImmediately,
        "ResourceArn" Core.=: resourceArn
      ]

-- | /See:/ 'newStopActivityStreamResponse' smart constructor.
data StopActivityStreamResponse = StopActivityStreamResponse'
  { -- | The status of the database activity stream.
    status :: Core.Maybe ActivityStreamStatus,
    -- | The AWS KMS key identifier used for encrypting messages in the database
    -- activity stream.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The name of the Amazon Kinesis data stream used for the database
    -- activity stream.
    kinesisStreamName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopActivityStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'stopActivityStreamResponse_status' - The status of the database activity stream.
--
-- 'kmsKeyId', 'stopActivityStreamResponse_kmsKeyId' - The AWS KMS key identifier used for encrypting messages in the database
-- activity stream.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- 'kinesisStreamName', 'stopActivityStreamResponse_kinesisStreamName' - The name of the Amazon Kinesis data stream used for the database
-- activity stream.
--
-- 'httpStatus', 'stopActivityStreamResponse_httpStatus' - The response's http status code.
newStopActivityStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopActivityStreamResponse
newStopActivityStreamResponse pHttpStatus_ =
  StopActivityStreamResponse'
    { status = Core.Nothing,
      kmsKeyId = Core.Nothing,
      kinesisStreamName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the database activity stream.
stopActivityStreamResponse_status :: Lens.Lens' StopActivityStreamResponse (Core.Maybe ActivityStreamStatus)
stopActivityStreamResponse_status = Lens.lens (\StopActivityStreamResponse' {status} -> status) (\s@StopActivityStreamResponse' {} a -> s {status = a} :: StopActivityStreamResponse)

-- | The AWS KMS key identifier used for encrypting messages in the database
-- activity stream.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
stopActivityStreamResponse_kmsKeyId :: Lens.Lens' StopActivityStreamResponse (Core.Maybe Core.Text)
stopActivityStreamResponse_kmsKeyId = Lens.lens (\StopActivityStreamResponse' {kmsKeyId} -> kmsKeyId) (\s@StopActivityStreamResponse' {} a -> s {kmsKeyId = a} :: StopActivityStreamResponse)

-- | The name of the Amazon Kinesis data stream used for the database
-- activity stream.
stopActivityStreamResponse_kinesisStreamName :: Lens.Lens' StopActivityStreamResponse (Core.Maybe Core.Text)
stopActivityStreamResponse_kinesisStreamName = Lens.lens (\StopActivityStreamResponse' {kinesisStreamName} -> kinesisStreamName) (\s@StopActivityStreamResponse' {} a -> s {kinesisStreamName = a} :: StopActivityStreamResponse)

-- | The response's http status code.
stopActivityStreamResponse_httpStatus :: Lens.Lens' StopActivityStreamResponse Core.Int
stopActivityStreamResponse_httpStatus = Lens.lens (\StopActivityStreamResponse' {httpStatus} -> httpStatus) (\s@StopActivityStreamResponse' {} a -> s {httpStatus = a} :: StopActivityStreamResponse)

instance Core.NFData StopActivityStreamResponse
