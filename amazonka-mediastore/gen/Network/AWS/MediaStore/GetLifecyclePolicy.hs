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
-- Module      : Network.AWS.MediaStore.GetLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the object lifecycle policy that is assigned to a container.
module Network.AWS.MediaStore.GetLifecyclePolicy
  ( -- * Creating a Request
    GetLifecyclePolicy (..),
    newGetLifecyclePolicy,

    -- * Request Lenses
    getLifecyclePolicy_containerName,

    -- * Destructuring the Response
    GetLifecyclePolicyResponse (..),
    newGetLifecyclePolicyResponse,

    -- * Response Lenses
    getLifecyclePolicyResponse_httpStatus,
    getLifecyclePolicyResponse_lifecyclePolicy,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The name of the container that the object lifecycle policy is assigned
    -- to.
    containerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'getLifecyclePolicy_containerName' - The name of the container that the object lifecycle policy is assigned
-- to.
newGetLifecyclePolicy ::
  -- | 'containerName'
  Core.Text ->
  GetLifecyclePolicy
newGetLifecyclePolicy pContainerName_ =
  GetLifecyclePolicy'
    { containerName =
        pContainerName_
    }

-- | The name of the container that the object lifecycle policy is assigned
-- to.
getLifecyclePolicy_containerName :: Lens.Lens' GetLifecyclePolicy Core.Text
getLifecyclePolicy_containerName = Lens.lens (\GetLifecyclePolicy' {containerName} -> containerName) (\s@GetLifecyclePolicy' {} a -> s {containerName = a} :: GetLifecyclePolicy)

instance Core.AWSRequest GetLifecyclePolicy where
  type
    AWSResponse GetLifecyclePolicy =
      GetLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "LifecyclePolicy")
      )

instance Core.Hashable GetLifecyclePolicy

instance Core.NFData GetLifecyclePolicy

instance Core.ToHeaders GetLifecyclePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.GetLifecyclePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.ToPath GetLifecyclePolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetLifecyclePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The object lifecycle policy that is assigned to the container.
    lifecyclePolicy :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getLifecyclePolicyResponse_httpStatus' - The response's http status code.
--
-- 'lifecyclePolicy', 'getLifecyclePolicyResponse_lifecyclePolicy' - The object lifecycle policy that is assigned to the container.
newGetLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'lifecyclePolicy'
  Core.Text ->
  GetLifecyclePolicyResponse
newGetLifecyclePolicyResponse
  pHttpStatus_
  pLifecyclePolicy_ =
    GetLifecyclePolicyResponse'
      { httpStatus =
          pHttpStatus_,
        lifecyclePolicy = pLifecyclePolicy_
      }

-- | The response's http status code.
getLifecyclePolicyResponse_httpStatus :: Lens.Lens' GetLifecyclePolicyResponse Core.Int
getLifecyclePolicyResponse_httpStatus = Lens.lens (\GetLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@GetLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: GetLifecyclePolicyResponse)

-- | The object lifecycle policy that is assigned to the container.
getLifecyclePolicyResponse_lifecyclePolicy :: Lens.Lens' GetLifecyclePolicyResponse Core.Text
getLifecyclePolicyResponse_lifecyclePolicy = Lens.lens (\GetLifecyclePolicyResponse' {lifecyclePolicy} -> lifecyclePolicy) (\s@GetLifecyclePolicyResponse' {} a -> s {lifecyclePolicy = a} :: GetLifecyclePolicyResponse)

instance Core.NFData GetLifecyclePolicyResponse
