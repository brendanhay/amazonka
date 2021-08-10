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
-- Module      : Network.AWS.MediaStore.GetMetricPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metric policy for the specified container.
module Network.AWS.MediaStore.GetMetricPolicy
  ( -- * Creating a Request
    GetMetricPolicy (..),
    newGetMetricPolicy,

    -- * Request Lenses
    getMetricPolicy_containerName,

    -- * Destructuring the Response
    GetMetricPolicyResponse (..),
    newGetMetricPolicyResponse,

    -- * Response Lenses
    getMetricPolicyResponse_httpStatus,
    getMetricPolicyResponse_metricPolicy,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMetricPolicy' smart constructor.
data GetMetricPolicy = GetMetricPolicy'
  { -- | The name of the container that is associated with the metric policy.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'getMetricPolicy_containerName' - The name of the container that is associated with the metric policy.
newGetMetricPolicy ::
  -- | 'containerName'
  Prelude.Text ->
  GetMetricPolicy
newGetMetricPolicy pContainerName_ =
  GetMetricPolicy' {containerName = pContainerName_}

-- | The name of the container that is associated with the metric policy.
getMetricPolicy_containerName :: Lens.Lens' GetMetricPolicy Prelude.Text
getMetricPolicy_containerName = Lens.lens (\GetMetricPolicy' {containerName} -> containerName) (\s@GetMetricPolicy' {} a -> s {containerName = a} :: GetMetricPolicy)

instance Core.AWSRequest GetMetricPolicy where
  type
    AWSResponse GetMetricPolicy =
      GetMetricPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMetricPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "MetricPolicy")
      )

instance Prelude.Hashable GetMetricPolicy

instance Prelude.NFData GetMetricPolicy

instance Core.ToHeaders GetMetricPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.GetMetricPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMetricPolicy where
  toJSON GetMetricPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Core..= containerName)
          ]
      )

instance Core.ToPath GetMetricPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery GetMetricPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMetricPolicyResponse' smart constructor.
data GetMetricPolicyResponse = GetMetricPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The metric policy that is associated with the specific container.
    metricPolicy :: MetricPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMetricPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getMetricPolicyResponse_httpStatus' - The response's http status code.
--
-- 'metricPolicy', 'getMetricPolicyResponse_metricPolicy' - The metric policy that is associated with the specific container.
newGetMetricPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'metricPolicy'
  MetricPolicy ->
  GetMetricPolicyResponse
newGetMetricPolicyResponse
  pHttpStatus_
  pMetricPolicy_ =
    GetMetricPolicyResponse'
      { httpStatus = pHttpStatus_,
        metricPolicy = pMetricPolicy_
      }

-- | The response's http status code.
getMetricPolicyResponse_httpStatus :: Lens.Lens' GetMetricPolicyResponse Prelude.Int
getMetricPolicyResponse_httpStatus = Lens.lens (\GetMetricPolicyResponse' {httpStatus} -> httpStatus) (\s@GetMetricPolicyResponse' {} a -> s {httpStatus = a} :: GetMetricPolicyResponse)

-- | The metric policy that is associated with the specific container.
getMetricPolicyResponse_metricPolicy :: Lens.Lens' GetMetricPolicyResponse MetricPolicy
getMetricPolicyResponse_metricPolicy = Lens.lens (\GetMetricPolicyResponse' {metricPolicy} -> metricPolicy) (\s@GetMetricPolicyResponse' {} a -> s {metricPolicy = a} :: GetMetricPolicyResponse)

instance Prelude.NFData GetMetricPolicyResponse
