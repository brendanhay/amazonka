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
-- Module      : Amazonka.EMR.GetClusterSessionCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides Temporary, basic HTTP credentials that are associated with a
-- given runtime IAM role and used by a cluster with fine-grained access
-- control activated. You can use these credentials to connect to cluster
-- endpoints that support username-based and password-based authentication.
module Amazonka.EMR.GetClusterSessionCredentials
  ( -- * Creating a Request
    GetClusterSessionCredentials (..),
    newGetClusterSessionCredentials,

    -- * Request Lenses
    getClusterSessionCredentials_clusterId,
    getClusterSessionCredentials_executionRoleArn,

    -- * Destructuring the Response
    GetClusterSessionCredentialsResponse (..),
    newGetClusterSessionCredentialsResponse,

    -- * Response Lenses
    getClusterSessionCredentialsResponse_credentials,
    getClusterSessionCredentialsResponse_expiresAt,
    getClusterSessionCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetClusterSessionCredentials' smart constructor.
data GetClusterSessionCredentials = GetClusterSessionCredentials'
  { -- | The unique identifier of the cluster.
    clusterId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the runtime role for interactive
    -- workload submission on the cluster. The runtime role can be a
    -- cross-account IAM role. The runtime role ARN is a combination of account
    -- ID, role name, and role type using the following format:
    -- @arn:partition:service:region:account:resource@.
    executionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterSessionCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'getClusterSessionCredentials_clusterId' - The unique identifier of the cluster.
--
-- 'executionRoleArn', 'getClusterSessionCredentials_executionRoleArn' - The Amazon Resource Name (ARN) of the runtime role for interactive
-- workload submission on the cluster. The runtime role can be a
-- cross-account IAM role. The runtime role ARN is a combination of account
-- ID, role name, and role type using the following format:
-- @arn:partition:service:region:account:resource@.
newGetClusterSessionCredentials ::
  -- | 'clusterId'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  GetClusterSessionCredentials
newGetClusterSessionCredentials
  pClusterId_
  pExecutionRoleArn_ =
    GetClusterSessionCredentials'
      { clusterId =
          pClusterId_,
        executionRoleArn = pExecutionRoleArn_
      }

-- | The unique identifier of the cluster.
getClusterSessionCredentials_clusterId :: Lens.Lens' GetClusterSessionCredentials Prelude.Text
getClusterSessionCredentials_clusterId = Lens.lens (\GetClusterSessionCredentials' {clusterId} -> clusterId) (\s@GetClusterSessionCredentials' {} a -> s {clusterId = a} :: GetClusterSessionCredentials)

-- | The Amazon Resource Name (ARN) of the runtime role for interactive
-- workload submission on the cluster. The runtime role can be a
-- cross-account IAM role. The runtime role ARN is a combination of account
-- ID, role name, and role type using the following format:
-- @arn:partition:service:region:account:resource@.
getClusterSessionCredentials_executionRoleArn :: Lens.Lens' GetClusterSessionCredentials Prelude.Text
getClusterSessionCredentials_executionRoleArn = Lens.lens (\GetClusterSessionCredentials' {executionRoleArn} -> executionRoleArn) (\s@GetClusterSessionCredentials' {} a -> s {executionRoleArn = a} :: GetClusterSessionCredentials)

instance Core.AWSRequest GetClusterSessionCredentials where
  type
    AWSResponse GetClusterSessionCredentials =
      GetClusterSessionCredentialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetClusterSessionCredentialsResponse'
            Prelude.<$> (x Data..?> "Credentials")
            Prelude.<*> (x Data..?> "ExpiresAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetClusterSessionCredentials
  where
  hashWithSalt _salt GetClusterSessionCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` executionRoleArn

instance Prelude.NFData GetClusterSessionCredentials where
  rnf GetClusterSessionCredentials' {..} =
    Prelude.rnf clusterId `Prelude.seq`
      Prelude.rnf executionRoleArn

instance Data.ToHeaders GetClusterSessionCredentials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.GetClusterSessionCredentials" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetClusterSessionCredentials where
  toJSON GetClusterSessionCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Data..= clusterId),
            Prelude.Just
              ("ExecutionRoleArn" Data..= executionRoleArn)
          ]
      )

instance Data.ToPath GetClusterSessionCredentials where
  toPath = Prelude.const "/"

instance Data.ToQuery GetClusterSessionCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetClusterSessionCredentialsResponse' smart constructor.
data GetClusterSessionCredentialsResponse = GetClusterSessionCredentialsResponse'
  { -- | The credentials that you can use to connect to cluster endpoints that
    -- support username-based and password-based authentication.
    credentials :: Prelude.Maybe Credentials,
    -- | The time when the credentials that are returned by the
    -- @GetClusterSessionCredentials@ API expire.
    expiresAt :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterSessionCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'getClusterSessionCredentialsResponse_credentials' - The credentials that you can use to connect to cluster endpoints that
-- support username-based and password-based authentication.
--
-- 'expiresAt', 'getClusterSessionCredentialsResponse_expiresAt' - The time when the credentials that are returned by the
-- @GetClusterSessionCredentials@ API expire.
--
-- 'httpStatus', 'getClusterSessionCredentialsResponse_httpStatus' - The response's http status code.
newGetClusterSessionCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetClusterSessionCredentialsResponse
newGetClusterSessionCredentialsResponse pHttpStatus_ =
  GetClusterSessionCredentialsResponse'
    { credentials =
        Prelude.Nothing,
      expiresAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The credentials that you can use to connect to cluster endpoints that
-- support username-based and password-based authentication.
getClusterSessionCredentialsResponse_credentials :: Lens.Lens' GetClusterSessionCredentialsResponse (Prelude.Maybe Credentials)
getClusterSessionCredentialsResponse_credentials = Lens.lens (\GetClusterSessionCredentialsResponse' {credentials} -> credentials) (\s@GetClusterSessionCredentialsResponse' {} a -> s {credentials = a} :: GetClusterSessionCredentialsResponse)

-- | The time when the credentials that are returned by the
-- @GetClusterSessionCredentials@ API expire.
getClusterSessionCredentialsResponse_expiresAt :: Lens.Lens' GetClusterSessionCredentialsResponse (Prelude.Maybe Prelude.UTCTime)
getClusterSessionCredentialsResponse_expiresAt = Lens.lens (\GetClusterSessionCredentialsResponse' {expiresAt} -> expiresAt) (\s@GetClusterSessionCredentialsResponse' {} a -> s {expiresAt = a} :: GetClusterSessionCredentialsResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getClusterSessionCredentialsResponse_httpStatus :: Lens.Lens' GetClusterSessionCredentialsResponse Prelude.Int
getClusterSessionCredentialsResponse_httpStatus = Lens.lens (\GetClusterSessionCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetClusterSessionCredentialsResponse' {} a -> s {httpStatus = a} :: GetClusterSessionCredentialsResponse)

instance
  Prelude.NFData
    GetClusterSessionCredentialsResponse
  where
  rnf GetClusterSessionCredentialsResponse' {..} =
    Prelude.rnf credentials `Prelude.seq`
      Prelude.rnf expiresAt `Prelude.seq`
        Prelude.rnf httpStatus
