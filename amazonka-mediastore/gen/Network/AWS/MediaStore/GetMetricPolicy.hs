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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMetricPolicy' smart constructor.
data GetMetricPolicy = GetMetricPolicy'
  { -- | The name of the container that is associated with the metric policy.
    containerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetMetricPolicy
newGetMetricPolicy pContainerName_ =
  GetMetricPolicy' {containerName = pContainerName_}

-- | The name of the container that is associated with the metric policy.
getMetricPolicy_containerName :: Lens.Lens' GetMetricPolicy Core.Text
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
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "MetricPolicy")
      )

instance Core.Hashable GetMetricPolicy

instance Core.NFData GetMetricPolicy

instance Core.ToHeaders GetMetricPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.GetMetricPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMetricPolicy where
  toJSON GetMetricPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.ToPath GetMetricPolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetMetricPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMetricPolicyResponse' smart constructor.
data GetMetricPolicyResponse = GetMetricPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The metric policy that is associated with the specific container.
    metricPolicy :: MetricPolicy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
getMetricPolicyResponse_httpStatus :: Lens.Lens' GetMetricPolicyResponse Core.Int
getMetricPolicyResponse_httpStatus = Lens.lens (\GetMetricPolicyResponse' {httpStatus} -> httpStatus) (\s@GetMetricPolicyResponse' {} a -> s {httpStatus = a} :: GetMetricPolicyResponse)

-- | The metric policy that is associated with the specific container.
getMetricPolicyResponse_metricPolicy :: Lens.Lens' GetMetricPolicyResponse MetricPolicy
getMetricPolicyResponse_metricPolicy = Lens.lens (\GetMetricPolicyResponse' {metricPolicy} -> metricPolicy) (\s@GetMetricPolicyResponse' {} a -> s {metricPolicy = a} :: GetMetricPolicyResponse)

instance Core.NFData GetMetricPolicyResponse
