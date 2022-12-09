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
-- Module      : Amazonka.RDS.StartActivityStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a database activity stream to monitor activity on the database.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/DBActivityStreams.html Database Activity Streams>
-- in the /Amazon Aurora User Guide/.
module Amazonka.RDS.StartActivityStream
  ( -- * Creating a Request
    StartActivityStream (..),
    newStartActivityStream,

    -- * Request Lenses
    startActivityStream_applyImmediately,
    startActivityStream_engineNativeAuditFieldsIncluded,
    startActivityStream_resourceArn,
    startActivityStream_mode,
    startActivityStream_kmsKeyId,

    -- * Destructuring the Response
    StartActivityStreamResponse (..),
    newStartActivityStreamResponse,

    -- * Response Lenses
    startActivityStreamResponse_applyImmediately,
    startActivityStreamResponse_engineNativeAuditFieldsIncluded,
    startActivityStreamResponse_kinesisStreamName,
    startActivityStreamResponse_kmsKeyId,
    startActivityStreamResponse_mode,
    startActivityStreamResponse_status,
    startActivityStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartActivityStream' smart constructor.
data StartActivityStream = StartActivityStream'
  { -- | Specifies whether or not the database activity stream is to start as
    -- soon as possible, regardless of the maintenance window for the database.
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the database activity stream includes engine-native
    -- audit fields. This option only applies to an Oracle DB instance. By
    -- default, no engine-native audit fields are included.
    engineNativeAuditFieldsIncluded :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the DB cluster, for example,
    -- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
    resourceArn :: Prelude.Text,
    -- | Specifies the mode of the database activity stream. Database events such
    -- as a change or access generate an activity stream event. The database
    -- session can handle these events either synchronously or asynchronously.
    mode :: ActivityStreamMode,
    -- | The Amazon Web Services KMS key identifier for encrypting messages in
    -- the database activity stream. The Amazon Web Services KMS key identifier
    -- is the key ARN, key ID, alias ARN, or alias name for the KMS key.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'engineNativeAuditFieldsIncluded', 'startActivityStream_engineNativeAuditFieldsIncluded' - Specifies whether the database activity stream includes engine-native
-- audit fields. This option only applies to an Oracle DB instance. By
-- default, no engine-native audit fields are included.
--
-- 'resourceArn', 'startActivityStream_resourceArn' - The Amazon Resource Name (ARN) of the DB cluster, for example,
-- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
--
-- 'mode', 'startActivityStream_mode' - Specifies the mode of the database activity stream. Database events such
-- as a change or access generate an activity stream event. The database
-- session can handle these events either synchronously or asynchronously.
--
-- 'kmsKeyId', 'startActivityStream_kmsKeyId' - The Amazon Web Services KMS key identifier for encrypting messages in
-- the database activity stream. The Amazon Web Services KMS key identifier
-- is the key ARN, key ID, alias ARN, or alias name for the KMS key.
newStartActivityStream ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'mode'
  ActivityStreamMode ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  StartActivityStream
newStartActivityStream
  pResourceArn_
  pMode_
  pKmsKeyId_ =
    StartActivityStream'
      { applyImmediately =
          Prelude.Nothing,
        engineNativeAuditFieldsIncluded = Prelude.Nothing,
        resourceArn = pResourceArn_,
        mode = pMode_,
        kmsKeyId = pKmsKeyId_
      }

-- | Specifies whether or not the database activity stream is to start as
-- soon as possible, regardless of the maintenance window for the database.
startActivityStream_applyImmediately :: Lens.Lens' StartActivityStream (Prelude.Maybe Prelude.Bool)
startActivityStream_applyImmediately = Lens.lens (\StartActivityStream' {applyImmediately} -> applyImmediately) (\s@StartActivityStream' {} a -> s {applyImmediately = a} :: StartActivityStream)

-- | Specifies whether the database activity stream includes engine-native
-- audit fields. This option only applies to an Oracle DB instance. By
-- default, no engine-native audit fields are included.
startActivityStream_engineNativeAuditFieldsIncluded :: Lens.Lens' StartActivityStream (Prelude.Maybe Prelude.Bool)
startActivityStream_engineNativeAuditFieldsIncluded = Lens.lens (\StartActivityStream' {engineNativeAuditFieldsIncluded} -> engineNativeAuditFieldsIncluded) (\s@StartActivityStream' {} a -> s {engineNativeAuditFieldsIncluded = a} :: StartActivityStream)

-- | The Amazon Resource Name (ARN) of the DB cluster, for example,
-- @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@.
startActivityStream_resourceArn :: Lens.Lens' StartActivityStream Prelude.Text
startActivityStream_resourceArn = Lens.lens (\StartActivityStream' {resourceArn} -> resourceArn) (\s@StartActivityStream' {} a -> s {resourceArn = a} :: StartActivityStream)

-- | Specifies the mode of the database activity stream. Database events such
-- as a change or access generate an activity stream event. The database
-- session can handle these events either synchronously or asynchronously.
startActivityStream_mode :: Lens.Lens' StartActivityStream ActivityStreamMode
startActivityStream_mode = Lens.lens (\StartActivityStream' {mode} -> mode) (\s@StartActivityStream' {} a -> s {mode = a} :: StartActivityStream)

-- | The Amazon Web Services KMS key identifier for encrypting messages in
-- the database activity stream. The Amazon Web Services KMS key identifier
-- is the key ARN, key ID, alias ARN, or alias name for the KMS key.
startActivityStream_kmsKeyId :: Lens.Lens' StartActivityStream Prelude.Text
startActivityStream_kmsKeyId = Lens.lens (\StartActivityStream' {kmsKeyId} -> kmsKeyId) (\s@StartActivityStream' {} a -> s {kmsKeyId = a} :: StartActivityStream)

instance Core.AWSRequest StartActivityStream where
  type
    AWSResponse StartActivityStream =
      StartActivityStreamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StartActivityStreamResult"
      ( \s h x ->
          StartActivityStreamResponse'
            Prelude.<$> (x Data..@? "ApplyImmediately")
            Prelude.<*> (x Data..@? "EngineNativeAuditFieldsIncluded")
            Prelude.<*> (x Data..@? "KinesisStreamName")
            Prelude.<*> (x Data..@? "KmsKeyId")
            Prelude.<*> (x Data..@? "Mode")
            Prelude.<*> (x Data..@? "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartActivityStream where
  hashWithSalt _salt StartActivityStream' {..} =
    _salt `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` engineNativeAuditFieldsIncluded
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData StartActivityStream where
  rnf StartActivityStream' {..} =
    Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf engineNativeAuditFieldsIncluded
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Data.ToHeaders StartActivityStream where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath StartActivityStream where
  toPath = Prelude.const "/"

instance Data.ToQuery StartActivityStream where
  toQuery StartActivityStream' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("StartActivityStream" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "ApplyImmediately" Data.=: applyImmediately,
        "EngineNativeAuditFieldsIncluded"
          Data.=: engineNativeAuditFieldsIncluded,
        "ResourceArn" Data.=: resourceArn,
        "Mode" Data.=: mode,
        "KmsKeyId" Data.=: kmsKeyId
      ]

-- | /See:/ 'newStartActivityStreamResponse' smart constructor.
data StartActivityStreamResponse = StartActivityStreamResponse'
  { -- | Indicates whether or not the database activity stream will start as soon
    -- as possible, regardless of the maintenance window for the database.
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether engine-native audit fields are included in the
    -- database activity stream.
    engineNativeAuditFieldsIncluded :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Amazon Kinesis data stream to be used for the database
    -- activity stream.
    kinesisStreamName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for encryption of messages in
    -- the database activity stream.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The mode of the database activity stream.
    mode :: Prelude.Maybe ActivityStreamMode,
    -- | The status of the database activity stream.
    status :: Prelude.Maybe ActivityStreamStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartActivityStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyImmediately', 'startActivityStreamResponse_applyImmediately' - Indicates whether or not the database activity stream will start as soon
-- as possible, regardless of the maintenance window for the database.
--
-- 'engineNativeAuditFieldsIncluded', 'startActivityStreamResponse_engineNativeAuditFieldsIncluded' - Indicates whether engine-native audit fields are included in the
-- database activity stream.
--
-- 'kinesisStreamName', 'startActivityStreamResponse_kinesisStreamName' - The name of the Amazon Kinesis data stream to be used for the database
-- activity stream.
--
-- 'kmsKeyId', 'startActivityStreamResponse_kmsKeyId' - The Amazon Web Services KMS key identifier for encryption of messages in
-- the database activity stream.
--
-- 'mode', 'startActivityStreamResponse_mode' - The mode of the database activity stream.
--
-- 'status', 'startActivityStreamResponse_status' - The status of the database activity stream.
--
-- 'httpStatus', 'startActivityStreamResponse_httpStatus' - The response's http status code.
newStartActivityStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartActivityStreamResponse
newStartActivityStreamResponse pHttpStatus_ =
  StartActivityStreamResponse'
    { applyImmediately =
        Prelude.Nothing,
      engineNativeAuditFieldsIncluded =
        Prelude.Nothing,
      kinesisStreamName = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      mode = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether or not the database activity stream will start as soon
-- as possible, regardless of the maintenance window for the database.
startActivityStreamResponse_applyImmediately :: Lens.Lens' StartActivityStreamResponse (Prelude.Maybe Prelude.Bool)
startActivityStreamResponse_applyImmediately = Lens.lens (\StartActivityStreamResponse' {applyImmediately} -> applyImmediately) (\s@StartActivityStreamResponse' {} a -> s {applyImmediately = a} :: StartActivityStreamResponse)

-- | Indicates whether engine-native audit fields are included in the
-- database activity stream.
startActivityStreamResponse_engineNativeAuditFieldsIncluded :: Lens.Lens' StartActivityStreamResponse (Prelude.Maybe Prelude.Bool)
startActivityStreamResponse_engineNativeAuditFieldsIncluded = Lens.lens (\StartActivityStreamResponse' {engineNativeAuditFieldsIncluded} -> engineNativeAuditFieldsIncluded) (\s@StartActivityStreamResponse' {} a -> s {engineNativeAuditFieldsIncluded = a} :: StartActivityStreamResponse)

-- | The name of the Amazon Kinesis data stream to be used for the database
-- activity stream.
startActivityStreamResponse_kinesisStreamName :: Lens.Lens' StartActivityStreamResponse (Prelude.Maybe Prelude.Text)
startActivityStreamResponse_kinesisStreamName = Lens.lens (\StartActivityStreamResponse' {kinesisStreamName} -> kinesisStreamName) (\s@StartActivityStreamResponse' {} a -> s {kinesisStreamName = a} :: StartActivityStreamResponse)

-- | The Amazon Web Services KMS key identifier for encryption of messages in
-- the database activity stream.
startActivityStreamResponse_kmsKeyId :: Lens.Lens' StartActivityStreamResponse (Prelude.Maybe Prelude.Text)
startActivityStreamResponse_kmsKeyId = Lens.lens (\StartActivityStreamResponse' {kmsKeyId} -> kmsKeyId) (\s@StartActivityStreamResponse' {} a -> s {kmsKeyId = a} :: StartActivityStreamResponse)

-- | The mode of the database activity stream.
startActivityStreamResponse_mode :: Lens.Lens' StartActivityStreamResponse (Prelude.Maybe ActivityStreamMode)
startActivityStreamResponse_mode = Lens.lens (\StartActivityStreamResponse' {mode} -> mode) (\s@StartActivityStreamResponse' {} a -> s {mode = a} :: StartActivityStreamResponse)

-- | The status of the database activity stream.
startActivityStreamResponse_status :: Lens.Lens' StartActivityStreamResponse (Prelude.Maybe ActivityStreamStatus)
startActivityStreamResponse_status = Lens.lens (\StartActivityStreamResponse' {status} -> status) (\s@StartActivityStreamResponse' {} a -> s {status = a} :: StartActivityStreamResponse)

-- | The response's http status code.
startActivityStreamResponse_httpStatus :: Lens.Lens' StartActivityStreamResponse Prelude.Int
startActivityStreamResponse_httpStatus = Lens.lens (\StartActivityStreamResponse' {httpStatus} -> httpStatus) (\s@StartActivityStreamResponse' {} a -> s {httpStatus = a} :: StartActivityStreamResponse)

instance Prelude.NFData StartActivityStreamResponse where
  rnf StartActivityStreamResponse' {..} =
    Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf engineNativeAuditFieldsIncluded
      `Prelude.seq` Prelude.rnf kinesisStreamName
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
