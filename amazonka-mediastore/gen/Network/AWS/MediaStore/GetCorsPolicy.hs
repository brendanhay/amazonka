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
-- Module      : Network.AWS.MediaStore.GetCorsPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cross-origin resource sharing (CORS) configuration
-- information that is set for the container.
--
-- To use this operation, you must have permission to perform the
-- @MediaStore:GetCorsPolicy@ action. By default, the container owner has
-- this permission and can grant it to others.
module Network.AWS.MediaStore.GetCorsPolicy
  ( -- * Creating a Request
    GetCorsPolicy (..),
    newGetCorsPolicy,

    -- * Request Lenses
    getCorsPolicy_containerName,

    -- * Destructuring the Response
    GetCorsPolicyResponse (..),
    newGetCorsPolicyResponse,

    -- * Response Lenses
    getCorsPolicyResponse_httpStatus,
    getCorsPolicyResponse_corsPolicy,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCorsPolicy' smart constructor.
data GetCorsPolicy = GetCorsPolicy'
  { -- | The name of the container that the policy is assigned to.
    containerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCorsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'getCorsPolicy_containerName' - The name of the container that the policy is assigned to.
newGetCorsPolicy ::
  -- | 'containerName'
  Core.Text ->
  GetCorsPolicy
newGetCorsPolicy pContainerName_ =
  GetCorsPolicy' {containerName = pContainerName_}

-- | The name of the container that the policy is assigned to.
getCorsPolicy_containerName :: Lens.Lens' GetCorsPolicy Core.Text
getCorsPolicy_containerName = Lens.lens (\GetCorsPolicy' {containerName} -> containerName) (\s@GetCorsPolicy' {} a -> s {containerName = a} :: GetCorsPolicy)

instance Core.AWSRequest GetCorsPolicy where
  type
    AWSResponse GetCorsPolicy =
      GetCorsPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCorsPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "CorsPolicy")
      )

instance Core.Hashable GetCorsPolicy

instance Core.NFData GetCorsPolicy

instance Core.ToHeaders GetCorsPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.GetCorsPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetCorsPolicy where
  toJSON GetCorsPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ContainerName" Core..= containerName)]
      )

instance Core.ToPath GetCorsPolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetCorsPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCorsPolicyResponse' smart constructor.
data GetCorsPolicyResponse = GetCorsPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The CORS policy assigned to the container.
    corsPolicy :: Core.NonEmpty CorsRule
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCorsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCorsPolicyResponse_httpStatus' - The response's http status code.
--
-- 'corsPolicy', 'getCorsPolicyResponse_corsPolicy' - The CORS policy assigned to the container.
newGetCorsPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'corsPolicy'
  Core.NonEmpty CorsRule ->
  GetCorsPolicyResponse
newGetCorsPolicyResponse pHttpStatus_ pCorsPolicy_ =
  GetCorsPolicyResponse'
    { httpStatus = pHttpStatus_,
      corsPolicy = Lens._Coerce Lens.# pCorsPolicy_
    }

-- | The response's http status code.
getCorsPolicyResponse_httpStatus :: Lens.Lens' GetCorsPolicyResponse Core.Int
getCorsPolicyResponse_httpStatus = Lens.lens (\GetCorsPolicyResponse' {httpStatus} -> httpStatus) (\s@GetCorsPolicyResponse' {} a -> s {httpStatus = a} :: GetCorsPolicyResponse)

-- | The CORS policy assigned to the container.
getCorsPolicyResponse_corsPolicy :: Lens.Lens' GetCorsPolicyResponse (Core.NonEmpty CorsRule)
getCorsPolicyResponse_corsPolicy = Lens.lens (\GetCorsPolicyResponse' {corsPolicy} -> corsPolicy) (\s@GetCorsPolicyResponse' {} a -> s {corsPolicy = a} :: GetCorsPolicyResponse) Core.. Lens._Coerce

instance Core.NFData GetCorsPolicyResponse
