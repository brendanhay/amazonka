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
-- Module      : Network.AWS.ECR.GetRegistryPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the permissions policy for a registry.
module Network.AWS.ECR.GetRegistryPolicy
  ( -- * Creating a Request
    GetRegistryPolicy (..),
    newGetRegistryPolicy,

    -- * Destructuring the Response
    GetRegistryPolicyResponse (..),
    newGetRegistryPolicyResponse,

    -- * Response Lenses
    getRegistryPolicyResponse_registryId,
    getRegistryPolicyResponse_policyText,
    getRegistryPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRegistryPolicy' smart constructor.
data GetRegistryPolicy = GetRegistryPolicy'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRegistryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetRegistryPolicy ::
  GetRegistryPolicy
newGetRegistryPolicy = GetRegistryPolicy'

instance Core.AWSRequest GetRegistryPolicy where
  type
    AWSResponse GetRegistryPolicy =
      GetRegistryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegistryPolicyResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "policyText")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRegistryPolicy

instance Core.NFData GetRegistryPolicy

instance Core.ToHeaders GetRegistryPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.GetRegistryPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRegistryPolicy where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetRegistryPolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetRegistryPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRegistryPolicyResponse' smart constructor.
data GetRegistryPolicyResponse = GetRegistryPolicyResponse'
  { -- | The ID of the registry.
    registryId :: Core.Maybe Core.Text,
    -- | The JSON text of the permissions policy for a registry.
    policyText :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRegistryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'getRegistryPolicyResponse_registryId' - The ID of the registry.
--
-- 'policyText', 'getRegistryPolicyResponse_policyText' - The JSON text of the permissions policy for a registry.
--
-- 'httpStatus', 'getRegistryPolicyResponse_httpStatus' - The response's http status code.
newGetRegistryPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRegistryPolicyResponse
newGetRegistryPolicyResponse pHttpStatus_ =
  GetRegistryPolicyResponse'
    { registryId =
        Core.Nothing,
      policyText = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the registry.
getRegistryPolicyResponse_registryId :: Lens.Lens' GetRegistryPolicyResponse (Core.Maybe Core.Text)
getRegistryPolicyResponse_registryId = Lens.lens (\GetRegistryPolicyResponse' {registryId} -> registryId) (\s@GetRegistryPolicyResponse' {} a -> s {registryId = a} :: GetRegistryPolicyResponse)

-- | The JSON text of the permissions policy for a registry.
getRegistryPolicyResponse_policyText :: Lens.Lens' GetRegistryPolicyResponse (Core.Maybe Core.Text)
getRegistryPolicyResponse_policyText = Lens.lens (\GetRegistryPolicyResponse' {policyText} -> policyText) (\s@GetRegistryPolicyResponse' {} a -> s {policyText = a} :: GetRegistryPolicyResponse)

-- | The response's http status code.
getRegistryPolicyResponse_httpStatus :: Lens.Lens' GetRegistryPolicyResponse Core.Int
getRegistryPolicyResponse_httpStatus = Lens.lens (\GetRegistryPolicyResponse' {httpStatus} -> httpStatus) (\s@GetRegistryPolicyResponse' {} a -> s {httpStatus = a} :: GetRegistryPolicyResponse)

instance Core.NFData GetRegistryPolicyResponse
