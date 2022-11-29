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
-- Module      : Amazonka.RDS.ModifyActivityStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the audit policy state of a database activity stream to either
-- locked (default) or unlocked. A locked policy is read-only, whereas an
-- unlocked policy is read\/write. If your activity stream is started and
-- locked, you can unlock it, customize your audit policy, and then lock
-- your activity stream. Restarting the activity stream isn\'t required.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/DBActivityStreams.Modifying.html Modifying a database activity stream>
-- in the /Amazon RDS User Guide/.
--
-- This operation is supported for RDS for Oracle only.
module Amazonka.RDS.ModifyActivityStream
  ( -- * Creating a Request
    ModifyActivityStream (..),
    newModifyActivityStream,

    -- * Request Lenses
    modifyActivityStream_resourceArn,
    modifyActivityStream_auditPolicyState,

    -- * Destructuring the Response
    ModifyActivityStreamResponse (..),
    newModifyActivityStreamResponse,

    -- * Response Lenses
    modifyActivityStreamResponse_status,
    modifyActivityStreamResponse_policyStatus,
    modifyActivityStreamResponse_engineNativeAuditFieldsIncluded,
    modifyActivityStreamResponse_kinesisStreamName,
    modifyActivityStreamResponse_mode,
    modifyActivityStreamResponse_kmsKeyId,
    modifyActivityStreamResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyActivityStream' smart constructor.
data ModifyActivityStream = ModifyActivityStream'
  { -- | The Amazon Resource Name (ARN) of the RDS for Oracle DB instance, for
    -- example, @arn:aws:rds:us-east-1:12345667890:instance:my-orcl-db@.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The audit policy state. When a policy is unlocked, it is read\/write.
    -- When it is locked, it is read-only. You can edit your audit policy only
    -- when the activity stream is unlocked or stopped.
    auditPolicyState :: Prelude.Maybe AuditPolicyState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyActivityStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'modifyActivityStream_resourceArn' - The Amazon Resource Name (ARN) of the RDS for Oracle DB instance, for
-- example, @arn:aws:rds:us-east-1:12345667890:instance:my-orcl-db@.
--
-- 'auditPolicyState', 'modifyActivityStream_auditPolicyState' - The audit policy state. When a policy is unlocked, it is read\/write.
-- When it is locked, it is read-only. You can edit your audit policy only
-- when the activity stream is unlocked or stopped.
newModifyActivityStream ::
  ModifyActivityStream
newModifyActivityStream =
  ModifyActivityStream'
    { resourceArn =
        Prelude.Nothing,
      auditPolicyState = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the RDS for Oracle DB instance, for
-- example, @arn:aws:rds:us-east-1:12345667890:instance:my-orcl-db@.
modifyActivityStream_resourceArn :: Lens.Lens' ModifyActivityStream (Prelude.Maybe Prelude.Text)
modifyActivityStream_resourceArn = Lens.lens (\ModifyActivityStream' {resourceArn} -> resourceArn) (\s@ModifyActivityStream' {} a -> s {resourceArn = a} :: ModifyActivityStream)

-- | The audit policy state. When a policy is unlocked, it is read\/write.
-- When it is locked, it is read-only. You can edit your audit policy only
-- when the activity stream is unlocked or stopped.
modifyActivityStream_auditPolicyState :: Lens.Lens' ModifyActivityStream (Prelude.Maybe AuditPolicyState)
modifyActivityStream_auditPolicyState = Lens.lens (\ModifyActivityStream' {auditPolicyState} -> auditPolicyState) (\s@ModifyActivityStream' {} a -> s {auditPolicyState = a} :: ModifyActivityStream)

instance Core.AWSRequest ModifyActivityStream where
  type
    AWSResponse ModifyActivityStream =
      ModifyActivityStreamResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyActivityStreamResult"
      ( \s h x ->
          ModifyActivityStreamResponse'
            Prelude.<$> (x Core..@? "Status")
            Prelude.<*> (x Core..@? "PolicyStatus")
            Prelude.<*> (x Core..@? "EngineNativeAuditFieldsIncluded")
            Prelude.<*> (x Core..@? "KinesisStreamName")
            Prelude.<*> (x Core..@? "Mode")
            Prelude.<*> (x Core..@? "KmsKeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyActivityStream where
  hashWithSalt _salt ModifyActivityStream' {..} =
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` auditPolicyState

instance Prelude.NFData ModifyActivityStream where
  rnf ModifyActivityStream' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf auditPolicyState

instance Core.ToHeaders ModifyActivityStream where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyActivityStream where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyActivityStream where
  toQuery ModifyActivityStream' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyActivityStream" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "ResourceArn" Core.=: resourceArn,
        "AuditPolicyState" Core.=: auditPolicyState
      ]

-- | /See:/ 'newModifyActivityStreamResponse' smart constructor.
data ModifyActivityStreamResponse = ModifyActivityStreamResponse'
  { -- | The status of the modification to the database activity stream.
    status :: Prelude.Maybe ActivityStreamStatus,
    -- | The status of the modification to the policy state of the database
    -- activity stream.
    policyStatus :: Prelude.Maybe ActivityStreamPolicyStatus,
    -- | Indicates whether engine-native audit fields are included in the
    -- database activity stream.
    engineNativeAuditFieldsIncluded :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Amazon Kinesis data stream to be used for the database
    -- activity stream.
    kinesisStreamName :: Prelude.Maybe Prelude.Text,
    -- | The mode of the database activity stream.
    mode :: Prelude.Maybe ActivityStreamMode,
    -- | The Amazon Web Services KMS key identifier for encryption of messages in
    -- the database activity stream.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyActivityStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'modifyActivityStreamResponse_status' - The status of the modification to the database activity stream.
--
-- 'policyStatus', 'modifyActivityStreamResponse_policyStatus' - The status of the modification to the policy state of the database
-- activity stream.
--
-- 'engineNativeAuditFieldsIncluded', 'modifyActivityStreamResponse_engineNativeAuditFieldsIncluded' - Indicates whether engine-native audit fields are included in the
-- database activity stream.
--
-- 'kinesisStreamName', 'modifyActivityStreamResponse_kinesisStreamName' - The name of the Amazon Kinesis data stream to be used for the database
-- activity stream.
--
-- 'mode', 'modifyActivityStreamResponse_mode' - The mode of the database activity stream.
--
-- 'kmsKeyId', 'modifyActivityStreamResponse_kmsKeyId' - The Amazon Web Services KMS key identifier for encryption of messages in
-- the database activity stream.
--
-- 'httpStatus', 'modifyActivityStreamResponse_httpStatus' - The response's http status code.
newModifyActivityStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyActivityStreamResponse
newModifyActivityStreamResponse pHttpStatus_ =
  ModifyActivityStreamResponse'
    { status =
        Prelude.Nothing,
      policyStatus = Prelude.Nothing,
      engineNativeAuditFieldsIncluded =
        Prelude.Nothing,
      kinesisStreamName = Prelude.Nothing,
      mode = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the modification to the database activity stream.
modifyActivityStreamResponse_status :: Lens.Lens' ModifyActivityStreamResponse (Prelude.Maybe ActivityStreamStatus)
modifyActivityStreamResponse_status = Lens.lens (\ModifyActivityStreamResponse' {status} -> status) (\s@ModifyActivityStreamResponse' {} a -> s {status = a} :: ModifyActivityStreamResponse)

-- | The status of the modification to the policy state of the database
-- activity stream.
modifyActivityStreamResponse_policyStatus :: Lens.Lens' ModifyActivityStreamResponse (Prelude.Maybe ActivityStreamPolicyStatus)
modifyActivityStreamResponse_policyStatus = Lens.lens (\ModifyActivityStreamResponse' {policyStatus} -> policyStatus) (\s@ModifyActivityStreamResponse' {} a -> s {policyStatus = a} :: ModifyActivityStreamResponse)

-- | Indicates whether engine-native audit fields are included in the
-- database activity stream.
modifyActivityStreamResponse_engineNativeAuditFieldsIncluded :: Lens.Lens' ModifyActivityStreamResponse (Prelude.Maybe Prelude.Bool)
modifyActivityStreamResponse_engineNativeAuditFieldsIncluded = Lens.lens (\ModifyActivityStreamResponse' {engineNativeAuditFieldsIncluded} -> engineNativeAuditFieldsIncluded) (\s@ModifyActivityStreamResponse' {} a -> s {engineNativeAuditFieldsIncluded = a} :: ModifyActivityStreamResponse)

-- | The name of the Amazon Kinesis data stream to be used for the database
-- activity stream.
modifyActivityStreamResponse_kinesisStreamName :: Lens.Lens' ModifyActivityStreamResponse (Prelude.Maybe Prelude.Text)
modifyActivityStreamResponse_kinesisStreamName = Lens.lens (\ModifyActivityStreamResponse' {kinesisStreamName} -> kinesisStreamName) (\s@ModifyActivityStreamResponse' {} a -> s {kinesisStreamName = a} :: ModifyActivityStreamResponse)

-- | The mode of the database activity stream.
modifyActivityStreamResponse_mode :: Lens.Lens' ModifyActivityStreamResponse (Prelude.Maybe ActivityStreamMode)
modifyActivityStreamResponse_mode = Lens.lens (\ModifyActivityStreamResponse' {mode} -> mode) (\s@ModifyActivityStreamResponse' {} a -> s {mode = a} :: ModifyActivityStreamResponse)

-- | The Amazon Web Services KMS key identifier for encryption of messages in
-- the database activity stream.
modifyActivityStreamResponse_kmsKeyId :: Lens.Lens' ModifyActivityStreamResponse (Prelude.Maybe Prelude.Text)
modifyActivityStreamResponse_kmsKeyId = Lens.lens (\ModifyActivityStreamResponse' {kmsKeyId} -> kmsKeyId) (\s@ModifyActivityStreamResponse' {} a -> s {kmsKeyId = a} :: ModifyActivityStreamResponse)

-- | The response's http status code.
modifyActivityStreamResponse_httpStatus :: Lens.Lens' ModifyActivityStreamResponse Prelude.Int
modifyActivityStreamResponse_httpStatus = Lens.lens (\ModifyActivityStreamResponse' {httpStatus} -> httpStatus) (\s@ModifyActivityStreamResponse' {} a -> s {httpStatus = a} :: ModifyActivityStreamResponse)

instance Prelude.NFData ModifyActivityStreamResponse where
  rnf ModifyActivityStreamResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf policyStatus
      `Prelude.seq` Prelude.rnf engineNativeAuditFieldsIncluded
      `Prelude.seq` Prelude.rnf kinesisStreamName
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf httpStatus
