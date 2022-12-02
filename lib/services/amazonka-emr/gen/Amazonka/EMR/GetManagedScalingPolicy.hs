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
-- Module      : Amazonka.EMR.GetManagedScalingPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the attached managed scaling policy for an Amazon EMR cluster.
module Amazonka.EMR.GetManagedScalingPolicy
  ( -- * Creating a Request
    GetManagedScalingPolicy (..),
    newGetManagedScalingPolicy,

    -- * Request Lenses
    getManagedScalingPolicy_clusterId,

    -- * Destructuring the Response
    GetManagedScalingPolicyResponse (..),
    newGetManagedScalingPolicyResponse,

    -- * Response Lenses
    getManagedScalingPolicyResponse_managedScalingPolicy,
    getManagedScalingPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetManagedScalingPolicy' smart constructor.
data GetManagedScalingPolicy = GetManagedScalingPolicy'
  { -- | Specifies the ID of the cluster for which the managed scaling policy
    -- will be fetched.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'getManagedScalingPolicy_clusterId' - Specifies the ID of the cluster for which the managed scaling policy
-- will be fetched.
newGetManagedScalingPolicy ::
  -- | 'clusterId'
  Prelude.Text ->
  GetManagedScalingPolicy
newGetManagedScalingPolicy pClusterId_ =
  GetManagedScalingPolicy' {clusterId = pClusterId_}

-- | Specifies the ID of the cluster for which the managed scaling policy
-- will be fetched.
getManagedScalingPolicy_clusterId :: Lens.Lens' GetManagedScalingPolicy Prelude.Text
getManagedScalingPolicy_clusterId = Lens.lens (\GetManagedScalingPolicy' {clusterId} -> clusterId) (\s@GetManagedScalingPolicy' {} a -> s {clusterId = a} :: GetManagedScalingPolicy)

instance Core.AWSRequest GetManagedScalingPolicy where
  type
    AWSResponse GetManagedScalingPolicy =
      GetManagedScalingPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetManagedScalingPolicyResponse'
            Prelude.<$> (x Data..?> "ManagedScalingPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetManagedScalingPolicy where
  hashWithSalt _salt GetManagedScalingPolicy' {..} =
    _salt `Prelude.hashWithSalt` clusterId

instance Prelude.NFData GetManagedScalingPolicy where
  rnf GetManagedScalingPolicy' {..} =
    Prelude.rnf clusterId

instance Data.ToHeaders GetManagedScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.GetManagedScalingPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetManagedScalingPolicy where
  toJSON GetManagedScalingPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Data..= clusterId)]
      )

instance Data.ToPath GetManagedScalingPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetManagedScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetManagedScalingPolicyResponse' smart constructor.
data GetManagedScalingPolicyResponse = GetManagedScalingPolicyResponse'
  { -- | Specifies the managed scaling policy that is attached to an Amazon EMR
    -- cluster.
    managedScalingPolicy :: Prelude.Maybe ManagedScalingPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetManagedScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedScalingPolicy', 'getManagedScalingPolicyResponse_managedScalingPolicy' - Specifies the managed scaling policy that is attached to an Amazon EMR
-- cluster.
--
-- 'httpStatus', 'getManagedScalingPolicyResponse_httpStatus' - The response's http status code.
newGetManagedScalingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetManagedScalingPolicyResponse
newGetManagedScalingPolicyResponse pHttpStatus_ =
  GetManagedScalingPolicyResponse'
    { managedScalingPolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the managed scaling policy that is attached to an Amazon EMR
-- cluster.
getManagedScalingPolicyResponse_managedScalingPolicy :: Lens.Lens' GetManagedScalingPolicyResponse (Prelude.Maybe ManagedScalingPolicy)
getManagedScalingPolicyResponse_managedScalingPolicy = Lens.lens (\GetManagedScalingPolicyResponse' {managedScalingPolicy} -> managedScalingPolicy) (\s@GetManagedScalingPolicyResponse' {} a -> s {managedScalingPolicy = a} :: GetManagedScalingPolicyResponse)

-- | The response's http status code.
getManagedScalingPolicyResponse_httpStatus :: Lens.Lens' GetManagedScalingPolicyResponse Prelude.Int
getManagedScalingPolicyResponse_httpStatus = Lens.lens (\GetManagedScalingPolicyResponse' {httpStatus} -> httpStatus) (\s@GetManagedScalingPolicyResponse' {} a -> s {httpStatus = a} :: GetManagedScalingPolicyResponse)

instance
  Prelude.NFData
    GetManagedScalingPolicyResponse
  where
  rnf GetManagedScalingPolicyResponse' {..} =
    Prelude.rnf managedScalingPolicy
      `Prelude.seq` Prelude.rnf httpStatus
