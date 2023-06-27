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
-- Module      : Amazonka.Kafka.PutClusterPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the MSK cluster policy specified by the cluster
-- Amazon Resource Name (ARN) in the request.
module Amazonka.Kafka.PutClusterPolicy
  ( -- * Creating a Request
    PutClusterPolicy (..),
    newPutClusterPolicy,

    -- * Request Lenses
    putClusterPolicy_currentVersion,
    putClusterPolicy_clusterArn,
    putClusterPolicy_policy,

    -- * Destructuring the Response
    PutClusterPolicyResponse (..),
    newPutClusterPolicyResponse,

    -- * Response Lenses
    putClusterPolicyResponse_currentVersion,
    putClusterPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutClusterPolicy' smart constructor.
data PutClusterPolicy = PutClusterPolicy'
  { -- | The policy version.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Text,
    -- | The policy.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutClusterPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'putClusterPolicy_currentVersion' - The policy version.
--
-- 'clusterArn', 'putClusterPolicy_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'policy', 'putClusterPolicy_policy' - The policy.
newPutClusterPolicy ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutClusterPolicy
newPutClusterPolicy pClusterArn_ pPolicy_ =
  PutClusterPolicy'
    { currentVersion = Prelude.Nothing,
      clusterArn = pClusterArn_,
      policy = pPolicy_
    }

-- | The policy version.
putClusterPolicy_currentVersion :: Lens.Lens' PutClusterPolicy (Prelude.Maybe Prelude.Text)
putClusterPolicy_currentVersion = Lens.lens (\PutClusterPolicy' {currentVersion} -> currentVersion) (\s@PutClusterPolicy' {} a -> s {currentVersion = a} :: PutClusterPolicy)

-- | The Amazon Resource Name (ARN) of the cluster.
putClusterPolicy_clusterArn :: Lens.Lens' PutClusterPolicy Prelude.Text
putClusterPolicy_clusterArn = Lens.lens (\PutClusterPolicy' {clusterArn} -> clusterArn) (\s@PutClusterPolicy' {} a -> s {clusterArn = a} :: PutClusterPolicy)

-- | The policy.
putClusterPolicy_policy :: Lens.Lens' PutClusterPolicy Prelude.Text
putClusterPolicy_policy = Lens.lens (\PutClusterPolicy' {policy} -> policy) (\s@PutClusterPolicy' {} a -> s {policy = a} :: PutClusterPolicy)

instance Core.AWSRequest PutClusterPolicy where
  type
    AWSResponse PutClusterPolicy =
      PutClusterPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutClusterPolicyResponse'
            Prelude.<$> (x Data..?> "currentVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutClusterPolicy where
  hashWithSalt _salt PutClusterPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutClusterPolicy where
  rnf PutClusterPolicy' {..} =
    Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf policy

instance Data.ToHeaders PutClusterPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutClusterPolicy where
  toJSON PutClusterPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("currentVersion" Data..=)
              Prelude.<$> currentVersion,
            Prelude.Just ("policy" Data..= policy)
          ]
      )

instance Data.ToPath PutClusterPolicy where
  toPath PutClusterPolicy' {..} =
    Prelude.mconcat
      ["/v1/clusters/", Data.toBS clusterArn, "/policy"]

instance Data.ToQuery PutClusterPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutClusterPolicyResponse' smart constructor.
data PutClusterPolicyResponse = PutClusterPolicyResponse'
  { -- | The policy version.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutClusterPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'putClusterPolicyResponse_currentVersion' - The policy version.
--
-- 'httpStatus', 'putClusterPolicyResponse_httpStatus' - The response's http status code.
newPutClusterPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutClusterPolicyResponse
newPutClusterPolicyResponse pHttpStatus_ =
  PutClusterPolicyResponse'
    { currentVersion =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy version.
putClusterPolicyResponse_currentVersion :: Lens.Lens' PutClusterPolicyResponse (Prelude.Maybe Prelude.Text)
putClusterPolicyResponse_currentVersion = Lens.lens (\PutClusterPolicyResponse' {currentVersion} -> currentVersion) (\s@PutClusterPolicyResponse' {} a -> s {currentVersion = a} :: PutClusterPolicyResponse)

-- | The response's http status code.
putClusterPolicyResponse_httpStatus :: Lens.Lens' PutClusterPolicyResponse Prelude.Int
putClusterPolicyResponse_httpStatus = Lens.lens (\PutClusterPolicyResponse' {httpStatus} -> httpStatus) (\s@PutClusterPolicyResponse' {} a -> s {httpStatus = a} :: PutClusterPolicyResponse)

instance Prelude.NFData PutClusterPolicyResponse where
  rnf PutClusterPolicyResponse' {..} =
    Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf httpStatus
