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
-- Module      : Amazonka.Kafka.GetClusterPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the MSK cluster policy specified by the Amazon Resource Name (ARN)
-- in the request.
module Amazonka.Kafka.GetClusterPolicy
  ( -- * Creating a Request
    GetClusterPolicy (..),
    newGetClusterPolicy,

    -- * Request Lenses
    getClusterPolicy_clusterArn,

    -- * Destructuring the Response
    GetClusterPolicyResponse (..),
    newGetClusterPolicyResponse,

    -- * Response Lenses
    getClusterPolicyResponse_currentVersion,
    getClusterPolicyResponse_policy,
    getClusterPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetClusterPolicy' smart constructor.
data GetClusterPolicy = GetClusterPolicy'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'getClusterPolicy_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
newGetClusterPolicy ::
  -- | 'clusterArn'
  Prelude.Text ->
  GetClusterPolicy
newGetClusterPolicy pClusterArn_ =
  GetClusterPolicy' {clusterArn = pClusterArn_}

-- | The Amazon Resource Name (ARN) of the cluster.
getClusterPolicy_clusterArn :: Lens.Lens' GetClusterPolicy Prelude.Text
getClusterPolicy_clusterArn = Lens.lens (\GetClusterPolicy' {clusterArn} -> clusterArn) (\s@GetClusterPolicy' {} a -> s {clusterArn = a} :: GetClusterPolicy)

instance Core.AWSRequest GetClusterPolicy where
  type
    AWSResponse GetClusterPolicy =
      GetClusterPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetClusterPolicyResponse'
            Prelude.<$> (x Data..?> "currentVersion")
            Prelude.<*> (x Data..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetClusterPolicy where
  hashWithSalt _salt GetClusterPolicy' {..} =
    _salt `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData GetClusterPolicy where
  rnf GetClusterPolicy' {..} = Prelude.rnf clusterArn

instance Data.ToHeaders GetClusterPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetClusterPolicy where
  toPath GetClusterPolicy' {..} =
    Prelude.mconcat
      ["/v1/clusters/", Data.toBS clusterArn, "/policy"]

instance Data.ToQuery GetClusterPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetClusterPolicyResponse' smart constructor.
data GetClusterPolicyResponse = GetClusterPolicyResponse'
  { -- | The version of cluster policy.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The cluster policy.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'getClusterPolicyResponse_currentVersion' - The version of cluster policy.
--
-- 'policy', 'getClusterPolicyResponse_policy' - The cluster policy.
--
-- 'httpStatus', 'getClusterPolicyResponse_httpStatus' - The response's http status code.
newGetClusterPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetClusterPolicyResponse
newGetClusterPolicyResponse pHttpStatus_ =
  GetClusterPolicyResponse'
    { currentVersion =
        Prelude.Nothing,
      policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of cluster policy.
getClusterPolicyResponse_currentVersion :: Lens.Lens' GetClusterPolicyResponse (Prelude.Maybe Prelude.Text)
getClusterPolicyResponse_currentVersion = Lens.lens (\GetClusterPolicyResponse' {currentVersion} -> currentVersion) (\s@GetClusterPolicyResponse' {} a -> s {currentVersion = a} :: GetClusterPolicyResponse)

-- | The cluster policy.
getClusterPolicyResponse_policy :: Lens.Lens' GetClusterPolicyResponse (Prelude.Maybe Prelude.Text)
getClusterPolicyResponse_policy = Lens.lens (\GetClusterPolicyResponse' {policy} -> policy) (\s@GetClusterPolicyResponse' {} a -> s {policy = a} :: GetClusterPolicyResponse)

-- | The response's http status code.
getClusterPolicyResponse_httpStatus :: Lens.Lens' GetClusterPolicyResponse Prelude.Int
getClusterPolicyResponse_httpStatus = Lens.lens (\GetClusterPolicyResponse' {httpStatus} -> httpStatus) (\s@GetClusterPolicyResponse' {} a -> s {httpStatus = a} :: GetClusterPolicyResponse)

instance Prelude.NFData GetClusterPolicyResponse where
  rnf GetClusterPolicyResponse' {..} =
    Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
