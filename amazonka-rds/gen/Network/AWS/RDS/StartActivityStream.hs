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
-- Module      : Network.AWS.RDS.StartActivityStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a database activity stream to monitor activity on the database.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/DBActivityStreams.html Database Activity Streams>
-- in the /Amazon Aurora User Guide/.
module Network.AWS.RDS.StartActivityStream
  ( -- * Creating a Request
    StartActivityStream (..),
    newStartActivityStream,

    -- * Request Lenses
    startActivityStream_applyImmediately,
    startActivityStream_resourceArn,
    startActivityStream_mode,
    startActivityStream_kmsKeyId,

    -- * Destructuring the Response
    StartActivityStreamResponse (..),
    newStartActivityStreamResponse,

    -- * Response Lenses
    startActivityStreamResponse_status,
    startActivityStreamResponse_mode,
    startActivityStreamResponse_kmsKeyId,
    startActivityStreamResponse_kinesisStreamName,
    startActivityStreamResponse_applyImmediately,
    startActivityStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartActivityStream' smart constructor.
data StartActivityStream = StartActivityStream'
  { -- | Specifies whether or not the database activity stream is to start as
    -- soon as possible, regardless of the maintenance window for the database.
    applyImmediately :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the DB cluster, for example,
    -- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
    resourceArn :: Core.Text,
    -- | Specifies the mode of the database activity stream. Database events such
    -- as a change or access generate an activity stream event. The database
    -- session can handle these events either synchronously or asynchronously.
    mode :: ActivityStreamMode,
    -- | The AWS KMS key identifier for encrypting messages in the database
    -- activity stream. The AWS KMS key identifier is the key ARN, key ID,
    -- alias ARN, or alias name for the AWS KMS customer master key (CMK).
    kmsKeyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartActivityStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyImmediately', 'startActivityStream_applyImmediately' - Specifies whether or not the database activity stream is to start as
-- soon as possible, regardless of the maintenance window for the database.
--
-- 'resourceArn', 'startActivityStream_resourceArn' - The Amazon Resource Name (ARN) of the DB cluster, for example,
-- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
--
-- 'mode', 'startActivityStream_mode' - Specifies the mode of the database activity stream. Database events such
-- as a change or access generate an activity stream event. The database
-- session can handle these events either synchronously or asynchronously.
--
-- 'kmsKeyId', 'startActivityStream_kmsKeyId' - The AWS KMS key identifier for encrypting messages in the database
-- activity stream. The AWS KMS key identifier is the key ARN, key ID,
-- alias ARN, or alias name for the AWS KMS customer master key (CMK).
newStartActivityStream ::
  -- | 'resourceArn'
  Core.Text ->
  -- | 'mode'
  ActivityStreamMode ->
  -- | 'kmsKeyId'
  Core.Text ->
  StartActivityStream
newStartActivityStream
  pResourceArn_
  pMode_
  pKmsKeyId_ =
    StartActivityStream'
      { applyImmediately =
          Core.Nothing,
        resourceArn = pResourceArn_,
        mode = pMode_,
        kmsKeyId = pKmsKeyId_
      }

-- | Specifies whether or not the database activity stream is to start as
-- soon as possible, regardless of the maintenance window for the database.
startActivityStream_applyImmediately :: Lens.Lens' StartActivityStream (Core.Maybe Core.Bool)
startActivityStream_applyImmediately = Lens.lens (\StartActivityStream' {applyImmediately} -> applyImmediately) (\s@StartActivityStream' {} a -> s {applyImmediately = a} :: StartActivityStream)

-- | The Amazon Resource Name (ARN) of the DB cluster, for example,
-- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
startActivityStream_resourceArn :: Lens.Lens' StartActivityStream Core.Text
startActivityStream_resourceArn = Lens.lens (\StartActivityStream' {resourceArn} -> resourceArn) (\s@StartActivityStream' {} a -> s {resourceArn = a} :: StartActivityStream)

-- | Specifies the mode of the database activity stream. Database events such
-- as a change or access generate an activity stream event. The database
-- session can handle these events either synchronously or asynchronously.
startActivityStream_mode :: Lens.Lens' StartActivityStream ActivityStreamMode
startActivityStream_mode = Lens.lens (\StartActivityStream' {mode} -> mode) (\s@StartActivityStream' {} a -> s {mode = a} :: StartActivityStream)

-- | The AWS KMS key identifier for encrypting messages in the database
-- activity stream. The AWS KMS key identifier is the key ARN, key ID,
-- alias ARN, or alias name for the AWS KMS customer master key (CMK).
startActivityStream_kmsKeyId :: Lens.Lens' StartActivityStream Core.Text
startActivityStream_kmsKeyId = Lens.lens (\StartActivityStream' {kmsKeyId} -> kmsKeyId) (\s@StartActivityStream' {} a -> s {kmsKeyId = a} :: StartActivityStream)

instance Core.AWSRequest StartActivityStream where
  type
    AWSResponse StartActivityStream =
      StartActivityStreamResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StartActivityStreamResult"
      ( \s h x ->
          StartActivityStreamResponse'
            Core.<$> (x Core..@? "Status")
            Core.<*> (x Core..@? "Mode")
            Core.<*> (x Core..@? "KmsKeyId")
            Core.<*> (x Core..@? "KinesisStreamName")
            Core.<*> (x Core..@? "ApplyImmediately")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartActivityStream

instance Core.NFData StartActivityStream

instance Core.ToHeaders StartActivityStream where
  toHeaders = Core.const Core.mempty

instance Core.ToPath StartActivityStream where
  toPath = Core.const "/"

instance Core.ToQuery StartActivityStream where
  toQuery StartActivityStream' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("StartActivityStream" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "ApplyImmediately" Core.=: applyImmediately,
        "ResourceArn" Core.=: resourceArn,
        "Mode" Core.=: mode,
        "KmsKeyId" Core.=: kmsKeyId
      ]

-- | /See:/ 'newStartActivityStreamResponse' smart constructor.
data StartActivityStreamResponse = StartActivityStreamResponse'
  { -- | The status of the database activity stream.
    status :: Core.Maybe ActivityStreamStatus,
    -- | The mode of the database activity stream.
    mode :: Core.Maybe ActivityStreamMode,
    -- | The AWS KMS key identifier for encryption of messages in the database
    -- activity stream.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The name of the Amazon Kinesis data stream to be used for the database
    -- activity stream.
    kinesisStreamName :: Core.Maybe Core.Text,
    -- | Indicates whether or not the database activity stream will start as soon
    -- as possible, regardless of the maintenance window for the database.
    applyImmediately :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartActivityStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'startActivityStreamResponse_status' - The status of the database activity stream.
--
-- 'mode', 'startActivityStreamResponse_mode' - The mode of the database activity stream.
--
-- 'kmsKeyId', 'startActivityStreamResponse_kmsKeyId' - The AWS KMS key identifier for encryption of messages in the database
-- activity stream.
--
-- 'kinesisStreamName', 'startActivityStreamResponse_kinesisStreamName' - The name of the Amazon Kinesis data stream to be used for the database
-- activity stream.
--
-- 'applyImmediately', 'startActivityStreamResponse_applyImmediately' - Indicates whether or not the database activity stream will start as soon
-- as possible, regardless of the maintenance window for the database.
--
-- 'httpStatus', 'startActivityStreamResponse_httpStatus' - The response's http status code.
newStartActivityStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartActivityStreamResponse
newStartActivityStreamResponse pHttpStatus_ =
  StartActivityStreamResponse'
    { status = Core.Nothing,
      mode = Core.Nothing,
      kmsKeyId = Core.Nothing,
      kinesisStreamName = Core.Nothing,
      applyImmediately = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the database activity stream.
startActivityStreamResponse_status :: Lens.Lens' StartActivityStreamResponse (Core.Maybe ActivityStreamStatus)
startActivityStreamResponse_status = Lens.lens (\StartActivityStreamResponse' {status} -> status) (\s@StartActivityStreamResponse' {} a -> s {status = a} :: StartActivityStreamResponse)

-- | The mode of the database activity stream.
startActivityStreamResponse_mode :: Lens.Lens' StartActivityStreamResponse (Core.Maybe ActivityStreamMode)
startActivityStreamResponse_mode = Lens.lens (\StartActivityStreamResponse' {mode} -> mode) (\s@StartActivityStreamResponse' {} a -> s {mode = a} :: StartActivityStreamResponse)

-- | The AWS KMS key identifier for encryption of messages in the database
-- activity stream.
startActivityStreamResponse_kmsKeyId :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Core.Text)
startActivityStreamResponse_kmsKeyId = Lens.lens (\StartActivityStreamResponse' {kmsKeyId} -> kmsKeyId) (\s@StartActivityStreamResponse' {} a -> s {kmsKeyId = a} :: StartActivityStreamResponse)

-- | The name of the Amazon Kinesis data stream to be used for the database
-- activity stream.
startActivityStreamResponse_kinesisStreamName :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Core.Text)
startActivityStreamResponse_kinesisStreamName = Lens.lens (\StartActivityStreamResponse' {kinesisStreamName} -> kinesisStreamName) (\s@StartActivityStreamResponse' {} a -> s {kinesisStreamName = a} :: StartActivityStreamResponse)

-- | Indicates whether or not the database activity stream will start as soon
-- as possible, regardless of the maintenance window for the database.
startActivityStreamResponse_applyImmediately :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Core.Bool)
startActivityStreamResponse_applyImmediately = Lens.lens (\StartActivityStreamResponse' {applyImmediately} -> applyImmediately) (\s@StartActivityStreamResponse' {} a -> s {applyImmediately = a} :: StartActivityStreamResponse)

-- | The response's http status code.
startActivityStreamResponse_httpStatus :: Lens.Lens' StartActivityStreamResponse Core.Int
startActivityStreamResponse_httpStatus = Lens.lens (\StartActivityStreamResponse' {httpStatus} -> httpStatus) (\s@StartActivityStreamResponse' {} a -> s {httpStatus = a} :: StartActivityStreamResponse)

instance Core.NFData StartActivityStreamResponse
