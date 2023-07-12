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
-- Module      : Amazonka.EMR.GetAutoTerminationPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the auto-termination policy for an Amazon EMR cluster.
module Amazonka.EMR.GetAutoTerminationPolicy
  ( -- * Creating a Request
    GetAutoTerminationPolicy (..),
    newGetAutoTerminationPolicy,

    -- * Request Lenses
    getAutoTerminationPolicy_clusterId,

    -- * Destructuring the Response
    GetAutoTerminationPolicyResponse (..),
    newGetAutoTerminationPolicyResponse,

    -- * Response Lenses
    getAutoTerminationPolicyResponse_autoTerminationPolicy,
    getAutoTerminationPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAutoTerminationPolicy' smart constructor.
data GetAutoTerminationPolicy = GetAutoTerminationPolicy'
  { -- | Specifies the ID of the Amazon EMR cluster for which the
    -- auto-termination policy will be fetched.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutoTerminationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'getAutoTerminationPolicy_clusterId' - Specifies the ID of the Amazon EMR cluster for which the
-- auto-termination policy will be fetched.
newGetAutoTerminationPolicy ::
  -- | 'clusterId'
  Prelude.Text ->
  GetAutoTerminationPolicy
newGetAutoTerminationPolicy pClusterId_ =
  GetAutoTerminationPolicy' {clusterId = pClusterId_}

-- | Specifies the ID of the Amazon EMR cluster for which the
-- auto-termination policy will be fetched.
getAutoTerminationPolicy_clusterId :: Lens.Lens' GetAutoTerminationPolicy Prelude.Text
getAutoTerminationPolicy_clusterId = Lens.lens (\GetAutoTerminationPolicy' {clusterId} -> clusterId) (\s@GetAutoTerminationPolicy' {} a -> s {clusterId = a} :: GetAutoTerminationPolicy)

instance Core.AWSRequest GetAutoTerminationPolicy where
  type
    AWSResponse GetAutoTerminationPolicy =
      GetAutoTerminationPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutoTerminationPolicyResponse'
            Prelude.<$> (x Data..?> "AutoTerminationPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAutoTerminationPolicy where
  hashWithSalt _salt GetAutoTerminationPolicy' {..} =
    _salt `Prelude.hashWithSalt` clusterId

instance Prelude.NFData GetAutoTerminationPolicy where
  rnf GetAutoTerminationPolicy' {..} =
    Prelude.rnf clusterId

instance Data.ToHeaders GetAutoTerminationPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.GetAutoTerminationPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAutoTerminationPolicy where
  toJSON GetAutoTerminationPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Data..= clusterId)]
      )

instance Data.ToPath GetAutoTerminationPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAutoTerminationPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAutoTerminationPolicyResponse' smart constructor.
data GetAutoTerminationPolicyResponse = GetAutoTerminationPolicyResponse'
  { -- | Specifies the auto-termination policy that is attached to an Amazon EMR
    -- cluster.
    autoTerminationPolicy :: Prelude.Maybe AutoTerminationPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutoTerminationPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoTerminationPolicy', 'getAutoTerminationPolicyResponse_autoTerminationPolicy' - Specifies the auto-termination policy that is attached to an Amazon EMR
-- cluster.
--
-- 'httpStatus', 'getAutoTerminationPolicyResponse_httpStatus' - The response's http status code.
newGetAutoTerminationPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAutoTerminationPolicyResponse
newGetAutoTerminationPolicyResponse pHttpStatus_ =
  GetAutoTerminationPolicyResponse'
    { autoTerminationPolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the auto-termination policy that is attached to an Amazon EMR
-- cluster.
getAutoTerminationPolicyResponse_autoTerminationPolicy :: Lens.Lens' GetAutoTerminationPolicyResponse (Prelude.Maybe AutoTerminationPolicy)
getAutoTerminationPolicyResponse_autoTerminationPolicy = Lens.lens (\GetAutoTerminationPolicyResponse' {autoTerminationPolicy} -> autoTerminationPolicy) (\s@GetAutoTerminationPolicyResponse' {} a -> s {autoTerminationPolicy = a} :: GetAutoTerminationPolicyResponse)

-- | The response's http status code.
getAutoTerminationPolicyResponse_httpStatus :: Lens.Lens' GetAutoTerminationPolicyResponse Prelude.Int
getAutoTerminationPolicyResponse_httpStatus = Lens.lens (\GetAutoTerminationPolicyResponse' {httpStatus} -> httpStatus) (\s@GetAutoTerminationPolicyResponse' {} a -> s {httpStatus = a} :: GetAutoTerminationPolicyResponse)

instance
  Prelude.NFData
    GetAutoTerminationPolicyResponse
  where
  rnf GetAutoTerminationPolicyResponse' {..} =
    Prelude.rnf autoTerminationPolicy
      `Prelude.seq` Prelude.rnf httpStatus
