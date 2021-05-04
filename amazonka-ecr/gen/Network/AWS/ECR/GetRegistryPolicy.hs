{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRegistryPolicy' smart constructor.
data GetRegistryPolicy = GetRegistryPolicy'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetRegistryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetRegistryPolicy ::
  GetRegistryPolicy
newGetRegistryPolicy = GetRegistryPolicy'

instance Prelude.AWSRequest GetRegistryPolicy where
  type Rs GetRegistryPolicy = GetRegistryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegistryPolicyResponse'
            Prelude.<$> (x Prelude..?> "registryId")
            Prelude.<*> (x Prelude..?> "policyText")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRegistryPolicy

instance Prelude.NFData GetRegistryPolicy

instance Prelude.ToHeaders GetRegistryPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.GetRegistryPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetRegistryPolicy where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath GetRegistryPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetRegistryPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegistryPolicyResponse' smart constructor.
data GetRegistryPolicyResponse = GetRegistryPolicyResponse'
  { -- | The ID of the registry.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The JSON text of the permissions policy for a registry.
    policyText :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetRegistryPolicyResponse
newGetRegistryPolicyResponse pHttpStatus_ =
  GetRegistryPolicyResponse'
    { registryId =
        Prelude.Nothing,
      policyText = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the registry.
getRegistryPolicyResponse_registryId :: Lens.Lens' GetRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
getRegistryPolicyResponse_registryId = Lens.lens (\GetRegistryPolicyResponse' {registryId} -> registryId) (\s@GetRegistryPolicyResponse' {} a -> s {registryId = a} :: GetRegistryPolicyResponse)

-- | The JSON text of the permissions policy for a registry.
getRegistryPolicyResponse_policyText :: Lens.Lens' GetRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
getRegistryPolicyResponse_policyText = Lens.lens (\GetRegistryPolicyResponse' {policyText} -> policyText) (\s@GetRegistryPolicyResponse' {} a -> s {policyText = a} :: GetRegistryPolicyResponse)

-- | The response's http status code.
getRegistryPolicyResponse_httpStatus :: Lens.Lens' GetRegistryPolicyResponse Prelude.Int
getRegistryPolicyResponse_httpStatus = Lens.lens (\GetRegistryPolicyResponse' {httpStatus} -> httpStatus) (\s@GetRegistryPolicyResponse' {} a -> s {httpStatus = a} :: GetRegistryPolicyResponse)

instance Prelude.NFData GetRegistryPolicyResponse
