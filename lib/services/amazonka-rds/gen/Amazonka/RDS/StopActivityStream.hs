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
-- Module      : Amazonka.RDS.StopActivityStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a database activity stream that was started using the Amazon Web
-- Services console, the @start-activity-stream@ CLI command, or the
-- @StartActivityStream@ action.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/DBActivityStreams.html Database Activity Streams>
-- in the /Amazon Aurora User Guide/.
module Amazonka.RDS.StopActivityStream
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
    stopActivityStreamResponse_kinesisStreamName,
    stopActivityStreamResponse_kmsKeyId,
    stopActivityStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopActivityStream' smart constructor.
data StopActivityStream = StopActivityStream'
  { -- | Specifies whether or not the database activity stream is to stop as soon
    -- as possible, regardless of the maintenance window for the database.
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the DB cluster for the database
    -- activity stream. For example,
    -- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopActivityStream
newStopActivityStream pResourceArn_ =
  StopActivityStream'
    { applyImmediately =
        Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | Specifies whether or not the database activity stream is to stop as soon
-- as possible, regardless of the maintenance window for the database.
stopActivityStream_applyImmediately :: Lens.Lens' StopActivityStream (Prelude.Maybe Prelude.Bool)
stopActivityStream_applyImmediately = Lens.lens (\StopActivityStream' {applyImmediately} -> applyImmediately) (\s@StopActivityStream' {} a -> s {applyImmediately = a} :: StopActivityStream)

-- | The Amazon Resource Name (ARN) of the DB cluster for the database
-- activity stream. For example,
-- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
stopActivityStream_resourceArn :: Lens.Lens' StopActivityStream Prelude.Text
stopActivityStream_resourceArn = Lens.lens (\StopActivityStream' {resourceArn} -> resourceArn) (\s@StopActivityStream' {} a -> s {resourceArn = a} :: StopActivityStream)

instance Core.AWSRequest StopActivityStream where
  type
    AWSResponse StopActivityStream =
      StopActivityStreamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StopActivityStreamResult"
      ( \s h x ->
          StopActivityStreamResponse'
            Prelude.<$> (x Core..@? "Status")
            Prelude.<*> (x Core..@? "KinesisStreamName")
            Prelude.<*> (x Core..@? "KmsKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopActivityStream where
  hashWithSalt _salt StopActivityStream' {..} =
    _salt `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData StopActivityStream where
  rnf StopActivityStream' {..} =
    Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf resourceArn

instance Core.ToHeaders StopActivityStream where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath StopActivityStream where
  toPath = Prelude.const "/"

instance Core.ToQuery StopActivityStream where
  toQuery StopActivityStream' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("StopActivityStream" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "ApplyImmediately" Core.=: applyImmediately,
        "ResourceArn" Core.=: resourceArn
      ]

-- | /See:/ 'newStopActivityStreamResponse' smart constructor.
data StopActivityStreamResponse = StopActivityStreamResponse'
  { -- | The status of the database activity stream.
    status :: Prelude.Maybe ActivityStreamStatus,
    -- | The name of the Amazon Kinesis data stream used for the database
    -- activity stream.
    kinesisStreamName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier used for encrypting messages
    -- in the database activity stream.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'kinesisStreamName', 'stopActivityStreamResponse_kinesisStreamName' - The name of the Amazon Kinesis data stream used for the database
-- activity stream.
--
-- 'kmsKeyId', 'stopActivityStreamResponse_kmsKeyId' - The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- 'httpStatus', 'stopActivityStreamResponse_httpStatus' - The response's http status code.
newStopActivityStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopActivityStreamResponse
newStopActivityStreamResponse pHttpStatus_ =
  StopActivityStreamResponse'
    { status =
        Prelude.Nothing,
      kinesisStreamName = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the database activity stream.
stopActivityStreamResponse_status :: Lens.Lens' StopActivityStreamResponse (Prelude.Maybe ActivityStreamStatus)
stopActivityStreamResponse_status = Lens.lens (\StopActivityStreamResponse' {status} -> status) (\s@StopActivityStreamResponse' {} a -> s {status = a} :: StopActivityStreamResponse)

-- | The name of the Amazon Kinesis data stream used for the database
-- activity stream.
stopActivityStreamResponse_kinesisStreamName :: Lens.Lens' StopActivityStreamResponse (Prelude.Maybe Prelude.Text)
stopActivityStreamResponse_kinesisStreamName = Lens.lens (\StopActivityStreamResponse' {kinesisStreamName} -> kinesisStreamName) (\s@StopActivityStreamResponse' {} a -> s {kinesisStreamName = a} :: StopActivityStreamResponse)

-- | The Amazon Web Services KMS key identifier used for encrypting messages
-- in the database activity stream.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
stopActivityStreamResponse_kmsKeyId :: Lens.Lens' StopActivityStreamResponse (Prelude.Maybe Prelude.Text)
stopActivityStreamResponse_kmsKeyId = Lens.lens (\StopActivityStreamResponse' {kmsKeyId} -> kmsKeyId) (\s@StopActivityStreamResponse' {} a -> s {kmsKeyId = a} :: StopActivityStreamResponse)

-- | The response's http status code.
stopActivityStreamResponse_httpStatus :: Lens.Lens' StopActivityStreamResponse Prelude.Int
stopActivityStreamResponse_httpStatus = Lens.lens (\StopActivityStreamResponse' {httpStatus} -> httpStatus) (\s@StopActivityStreamResponse' {} a -> s {httpStatus = a} :: StopActivityStreamResponse)

instance Prelude.NFData StopActivityStreamResponse where
  rnf StopActivityStreamResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf kinesisStreamName
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf httpStatus
