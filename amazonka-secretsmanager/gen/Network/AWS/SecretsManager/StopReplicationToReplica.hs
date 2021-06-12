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
-- Module      : Network.AWS.SecretsManager.StopReplicationToReplica
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the secret from replication and promotes the secret to a
-- regional secret in the replica Region.
module Network.AWS.SecretsManager.StopReplicationToReplica
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newStopReplicationToReplica' smart constructor.
data StopReplicationToReplica = StopReplicationToReplica'
  { -- | Response to @StopReplicationToReplica@ of a secret, based on the
    -- @SecretId@.
    secretId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopReplicationToReplica' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'stopReplicationToReplica_secretId' - Response to @StopReplicationToReplica@ of a secret, based on the
-- @SecretId@.
newStopReplicationToReplica ::
  -- | 'secretId'
  Core.Text ->
  StopReplicationToReplica
newStopReplicationToReplica pSecretId_ =
  StopReplicationToReplica' {secretId = pSecretId_}

-- | Response to @StopReplicationToReplica@ of a secret, based on the
-- @SecretId@.
stopReplicationToReplica_secretId :: Lens.Lens' StopReplicationToReplica Core.Text
stopReplicationToReplica_secretId = Lens.lens (\StopReplicationToReplica' {secretId} -> secretId) (\s@StopReplicationToReplica' {} a -> s {secretId = a} :: StopReplicationToReplica)

instance Core.AWSRequest StopReplicationToReplica where
  type
    AWSResponse StopReplicationToReplica =
      StopReplicationToReplicaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopReplicationToReplicaResponse'
            Core.<$> (x Core..?> "ARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopReplicationToReplica

instance Core.NFData StopReplicationToReplica

instance Core.ToHeaders StopReplicationToReplica where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.StopReplicationToReplica" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopReplicationToReplica where
  toJSON StopReplicationToReplica' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SecretId" Core..= secretId)]
      )

instance Core.ToPath StopReplicationToReplica where
  toPath = Core.const "/"

instance Core.ToQuery StopReplicationToReplica where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopReplicationToReplicaResponse' smart constructor.
data StopReplicationToReplicaResponse = StopReplicationToReplicaResponse'
  { -- | Response @StopReplicationToReplica@ of a secret, based on the @ARN,@.
    arn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopReplicationToReplicaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'stopReplicationToReplicaResponse_arn' - Response @StopReplicationToReplica@ of a secret, based on the @ARN,@.
--
-- 'httpStatus', 'stopReplicationToReplicaResponse_httpStatus' - The response's http status code.
newStopReplicationToReplicaResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopReplicationToReplicaResponse
newStopReplicationToReplicaResponse pHttpStatus_ =
  StopReplicationToReplicaResponse'
    { arn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Response @StopReplicationToReplica@ of a secret, based on the @ARN,@.
stopReplicationToReplicaResponse_arn :: Lens.Lens' StopReplicationToReplicaResponse (Core.Maybe Core.Text)
stopReplicationToReplicaResponse_arn = Lens.lens (\StopReplicationToReplicaResponse' {arn} -> arn) (\s@StopReplicationToReplicaResponse' {} a -> s {arn = a} :: StopReplicationToReplicaResponse)

-- | The response's http status code.
stopReplicationToReplicaResponse_httpStatus :: Lens.Lens' StopReplicationToReplicaResponse Core.Int
stopReplicationToReplicaResponse_httpStatus = Lens.lens (\StopReplicationToReplicaResponse' {httpStatus} -> httpStatus) (\s@StopReplicationToReplicaResponse' {} a -> s {httpStatus = a} :: StopReplicationToReplicaResponse)

instance Core.NFData StopReplicationToReplicaResponse
