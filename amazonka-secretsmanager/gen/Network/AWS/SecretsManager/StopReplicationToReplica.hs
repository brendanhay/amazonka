{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newStopReplicationToReplica' smart constructor.
data StopReplicationToReplica = StopReplicationToReplica'
  { -- | Response to @StopReplicationToReplica@ of a secret, based on the
    -- @SecretId@.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StopReplicationToReplica
newStopReplicationToReplica pSecretId_ =
  StopReplicationToReplica' {secretId = pSecretId_}

-- | Response to @StopReplicationToReplica@ of a secret, based on the
-- @SecretId@.
stopReplicationToReplica_secretId :: Lens.Lens' StopReplicationToReplica Prelude.Text
stopReplicationToReplica_secretId = Lens.lens (\StopReplicationToReplica' {secretId} -> secretId) (\s@StopReplicationToReplica' {} a -> s {secretId = a} :: StopReplicationToReplica)

instance Prelude.AWSRequest StopReplicationToReplica where
  type
    Rs StopReplicationToReplica =
      StopReplicationToReplicaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopReplicationToReplicaResponse'
            Prelude.<$> (x Prelude..?> "ARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopReplicationToReplica

instance Prelude.NFData StopReplicationToReplica

instance Prelude.ToHeaders StopReplicationToReplica where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "secretsmanager.StopReplicationToReplica" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopReplicationToReplica where
  toJSON StopReplicationToReplica' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Prelude..= secretId)]
      )

instance Prelude.ToPath StopReplicationToReplica where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopReplicationToReplica where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopReplicationToReplicaResponse' smart constructor.
data StopReplicationToReplicaResponse = StopReplicationToReplicaResponse'
  { -- | Response @StopReplicationToReplica@ of a secret, based on the @ARN,@.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StopReplicationToReplicaResponse
newStopReplicationToReplicaResponse pHttpStatus_ =
  StopReplicationToReplicaResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Response @StopReplicationToReplica@ of a secret, based on the @ARN,@.
stopReplicationToReplicaResponse_arn :: Lens.Lens' StopReplicationToReplicaResponse (Prelude.Maybe Prelude.Text)
stopReplicationToReplicaResponse_arn = Lens.lens (\StopReplicationToReplicaResponse' {arn} -> arn) (\s@StopReplicationToReplicaResponse' {} a -> s {arn = a} :: StopReplicationToReplicaResponse)

-- | The response's http status code.
stopReplicationToReplicaResponse_httpStatus :: Lens.Lens' StopReplicationToReplicaResponse Prelude.Int
stopReplicationToReplicaResponse_httpStatus = Lens.lens (\StopReplicationToReplicaResponse' {httpStatus} -> httpStatus) (\s@StopReplicationToReplicaResponse' {} a -> s {httpStatus = a} :: StopReplicationToReplicaResponse)

instance
  Prelude.NFData
    StopReplicationToReplicaResponse
