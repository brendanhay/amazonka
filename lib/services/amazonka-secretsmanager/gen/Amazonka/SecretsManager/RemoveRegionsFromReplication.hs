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
-- Module      : Amazonka.SecretsManager.RemoveRegionsFromReplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a secret that is replicated to other Regions, deletes the secret
-- replicas from the Regions you specify.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:RemoveRegionsFromReplication@.
-- For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.RemoveRegionsFromReplication
  ( -- * Creating a Request
    RemoveRegionsFromReplication (..),
    newRemoveRegionsFromReplication,

    -- * Request Lenses
    removeRegionsFromReplication_secretId,
    removeRegionsFromReplication_removeReplicaRegions,

    -- * Destructuring the Response
    RemoveRegionsFromReplicationResponse (..),
    newRemoveRegionsFromReplicationResponse,

    -- * Response Lenses
    removeRegionsFromReplicationResponse_replicationStatus,
    removeRegionsFromReplicationResponse_arn,
    removeRegionsFromReplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newRemoveRegionsFromReplication' smart constructor.
data RemoveRegionsFromReplication = RemoveRegionsFromReplication'
  { -- | The ARN or name of the secret.
    secretId :: Prelude.Text,
    -- | The Regions of the replicas to remove.
    removeReplicaRegions :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveRegionsFromReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'removeRegionsFromReplication_secretId' - The ARN or name of the secret.
--
-- 'removeReplicaRegions', 'removeRegionsFromReplication_removeReplicaRegions' - The Regions of the replicas to remove.
newRemoveRegionsFromReplication ::
  -- | 'secretId'
  Prelude.Text ->
  -- | 'removeReplicaRegions'
  Prelude.NonEmpty Prelude.Text ->
  RemoveRegionsFromReplication
newRemoveRegionsFromReplication
  pSecretId_
  pRemoveReplicaRegions_ =
    RemoveRegionsFromReplication'
      { secretId =
          pSecretId_,
        removeReplicaRegions =
          Lens.coerced Lens.# pRemoveReplicaRegions_
      }

-- | The ARN or name of the secret.
removeRegionsFromReplication_secretId :: Lens.Lens' RemoveRegionsFromReplication Prelude.Text
removeRegionsFromReplication_secretId = Lens.lens (\RemoveRegionsFromReplication' {secretId} -> secretId) (\s@RemoveRegionsFromReplication' {} a -> s {secretId = a} :: RemoveRegionsFromReplication)

-- | The Regions of the replicas to remove.
removeRegionsFromReplication_removeReplicaRegions :: Lens.Lens' RemoveRegionsFromReplication (Prelude.NonEmpty Prelude.Text)
removeRegionsFromReplication_removeReplicaRegions = Lens.lens (\RemoveRegionsFromReplication' {removeReplicaRegions} -> removeReplicaRegions) (\s@RemoveRegionsFromReplication' {} a -> s {removeReplicaRegions = a} :: RemoveRegionsFromReplication) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveRegionsFromReplication where
  type
    AWSResponse RemoveRegionsFromReplication =
      RemoveRegionsFromReplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveRegionsFromReplicationResponse'
            Prelude.<$> ( x Core..?> "ReplicationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RemoveRegionsFromReplication
  where
  hashWithSalt _salt RemoveRegionsFromReplication' {..} =
    _salt `Prelude.hashWithSalt` secretId
      `Prelude.hashWithSalt` removeReplicaRegions

instance Prelude.NFData RemoveRegionsFromReplication where
  rnf RemoveRegionsFromReplication' {..} =
    Prelude.rnf secretId
      `Prelude.seq` Prelude.rnf removeReplicaRegions

instance Core.ToHeaders RemoveRegionsFromReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.RemoveRegionsFromReplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveRegionsFromReplication where
  toJSON RemoveRegionsFromReplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SecretId" Core..= secretId),
            Prelude.Just
              ( "RemoveReplicaRegions"
                  Core..= removeReplicaRegions
              )
          ]
      )

instance Core.ToPath RemoveRegionsFromReplication where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveRegionsFromReplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveRegionsFromReplicationResponse' smart constructor.
data RemoveRegionsFromReplicationResponse = RemoveRegionsFromReplicationResponse'
  { -- | The status of replicas for this secret after you remove Regions.
    replicationStatus :: Prelude.Maybe [ReplicationStatusType],
    -- | The ARN of the primary secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveRegionsFromReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationStatus', 'removeRegionsFromReplicationResponse_replicationStatus' - The status of replicas for this secret after you remove Regions.
--
-- 'arn', 'removeRegionsFromReplicationResponse_arn' - The ARN of the primary secret.
--
-- 'httpStatus', 'removeRegionsFromReplicationResponse_httpStatus' - The response's http status code.
newRemoveRegionsFromReplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveRegionsFromReplicationResponse
newRemoveRegionsFromReplicationResponse pHttpStatus_ =
  RemoveRegionsFromReplicationResponse'
    { replicationStatus =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of replicas for this secret after you remove Regions.
removeRegionsFromReplicationResponse_replicationStatus :: Lens.Lens' RemoveRegionsFromReplicationResponse (Prelude.Maybe [ReplicationStatusType])
removeRegionsFromReplicationResponse_replicationStatus = Lens.lens (\RemoveRegionsFromReplicationResponse' {replicationStatus} -> replicationStatus) (\s@RemoveRegionsFromReplicationResponse' {} a -> s {replicationStatus = a} :: RemoveRegionsFromReplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the primary secret.
removeRegionsFromReplicationResponse_arn :: Lens.Lens' RemoveRegionsFromReplicationResponse (Prelude.Maybe Prelude.Text)
removeRegionsFromReplicationResponse_arn = Lens.lens (\RemoveRegionsFromReplicationResponse' {arn} -> arn) (\s@RemoveRegionsFromReplicationResponse' {} a -> s {arn = a} :: RemoveRegionsFromReplicationResponse)

-- | The response's http status code.
removeRegionsFromReplicationResponse_httpStatus :: Lens.Lens' RemoveRegionsFromReplicationResponse Prelude.Int
removeRegionsFromReplicationResponse_httpStatus = Lens.lens (\RemoveRegionsFromReplicationResponse' {httpStatus} -> httpStatus) (\s@RemoveRegionsFromReplicationResponse' {} a -> s {httpStatus = a} :: RemoveRegionsFromReplicationResponse)

instance
  Prelude.NFData
    RemoveRegionsFromReplicationResponse
  where
  rnf RemoveRegionsFromReplicationResponse' {..} =
    Prelude.rnf replicationStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
