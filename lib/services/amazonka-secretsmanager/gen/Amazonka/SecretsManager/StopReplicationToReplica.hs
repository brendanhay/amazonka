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
-- Module      : Amazonka.SecretsManager.StopReplicationToReplica
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the link between the replica secret and the primary secret and
-- promotes the replica to a primary secret in the replica Region.
--
-- You must call this operation from the Region in which you want to
-- promote the replica to a primary secret.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:StopReplicationToReplica@. For
-- more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.StopReplicationToReplica
  ( -- * Creating a Request
    StopReplicationToReplica (..),
    newStopReplicationToReplica,

    -- * Request Lenses
    stopReplicationToReplica_secretId,

    -- * Destructuring the Response
    StopReplicationToReplicaResponse (..),
    newStopReplicationToReplicaResponse,

    -- * Response Lenses
    stopReplicationToReplicaResponse_arn,
    stopReplicationToReplicaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newStopReplicationToReplica' smart constructor.
data StopReplicationToReplica = StopReplicationToReplica'
  { -- | The ARN of the primary secret.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopReplicationToReplica' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'stopReplicationToReplica_secretId' - The ARN of the primary secret.
newStopReplicationToReplica ::
  -- | 'secretId'
  Prelude.Text ->
  StopReplicationToReplica
newStopReplicationToReplica pSecretId_ =
  StopReplicationToReplica' {secretId = pSecretId_}

-- | The ARN of the primary secret.
stopReplicationToReplica_secretId :: Lens.Lens' StopReplicationToReplica Prelude.Text
stopReplicationToReplica_secretId = Lens.lens (\StopReplicationToReplica' {secretId} -> secretId) (\s@StopReplicationToReplica' {} a -> s {secretId = a} :: StopReplicationToReplica)

instance Core.AWSRequest StopReplicationToReplica where
  type
    AWSResponse StopReplicationToReplica =
      StopReplicationToReplicaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopReplicationToReplicaResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopReplicationToReplica where
  hashWithSalt _salt StopReplicationToReplica' {..} =
    _salt `Prelude.hashWithSalt` secretId

instance Prelude.NFData StopReplicationToReplica where
  rnf StopReplicationToReplica' {..} =
    Prelude.rnf secretId

instance Data.ToHeaders StopReplicationToReplica where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.StopReplicationToReplica" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopReplicationToReplica where
  toJSON StopReplicationToReplica' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Data..= secretId)]
      )

instance Data.ToPath StopReplicationToReplica where
  toPath = Prelude.const "/"

instance Data.ToQuery StopReplicationToReplica where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopReplicationToReplicaResponse' smart constructor.
data StopReplicationToReplicaResponse = StopReplicationToReplicaResponse'
  { -- | The ARN of the promoted secret. The ARN is the same as the original
    -- primary secret except the Region is changed.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopReplicationToReplicaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'stopReplicationToReplicaResponse_arn' - The ARN of the promoted secret. The ARN is the same as the original
-- primary secret except the Region is changed.
--
-- 'httpStatus', 'stopReplicationToReplicaResponse_httpStatus' - The response's http status code.
newStopReplicationToReplicaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopReplicationToReplicaResponse
newStopReplicationToReplicaResponse pHttpStatus_ =
  StopReplicationToReplicaResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the promoted secret. The ARN is the same as the original
-- primary secret except the Region is changed.
stopReplicationToReplicaResponse_arn :: Lens.Lens' StopReplicationToReplicaResponse (Prelude.Maybe Prelude.Text)
stopReplicationToReplicaResponse_arn = Lens.lens (\StopReplicationToReplicaResponse' {arn} -> arn) (\s@StopReplicationToReplicaResponse' {} a -> s {arn = a} :: StopReplicationToReplicaResponse)

-- | The response's http status code.
stopReplicationToReplicaResponse_httpStatus :: Lens.Lens' StopReplicationToReplicaResponse Prelude.Int
stopReplicationToReplicaResponse_httpStatus = Lens.lens (\StopReplicationToReplicaResponse' {httpStatus} -> httpStatus) (\s@StopReplicationToReplicaResponse' {} a -> s {httpStatus = a} :: StopReplicationToReplicaResponse)

instance
  Prelude.NFData
    StopReplicationToReplicaResponse
  where
  rnf StopReplicationToReplicaResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
