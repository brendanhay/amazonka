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
-- Module      : Amazonka.ECR.GetRegistryPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the permissions policy for a registry.
module Amazonka.ECR.GetRegistryPolicy
  ( -- * Creating a Request
    GetRegistryPolicy (..),
    newGetRegistryPolicy,

    -- * Destructuring the Response
    GetRegistryPolicyResponse (..),
    newGetRegistryPolicyResponse,

    -- * Response Lenses
    getRegistryPolicyResponse_policyText,
    getRegistryPolicyResponse_registryId,
    getRegistryPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRegistryPolicy' smart constructor.
data GetRegistryPolicy = GetRegistryPolicy'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRegistryPolicyResponse'
            Prelude.<$> (x Data..?> "policyText")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRegistryPolicy where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetRegistryPolicy where
  rnf _ = ()

instance Data.ToHeaders GetRegistryPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.GetRegistryPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRegistryPolicy where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetRegistryPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRegistryPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRegistryPolicyResponse' smart constructor.
data GetRegistryPolicyResponse = GetRegistryPolicyResponse'
  { -- | The JSON text of the permissions policy for a registry.
    policyText :: Prelude.Maybe Prelude.Text,
    -- | The ID of the registry.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRegistryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyText', 'getRegistryPolicyResponse_policyText' - The JSON text of the permissions policy for a registry.
--
-- 'registryId', 'getRegistryPolicyResponse_registryId' - The ID of the registry.
--
-- 'httpStatus', 'getRegistryPolicyResponse_httpStatus' - The response's http status code.
newGetRegistryPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRegistryPolicyResponse
newGetRegistryPolicyResponse pHttpStatus_ =
  GetRegistryPolicyResponse'
    { policyText =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The JSON text of the permissions policy for a registry.
getRegistryPolicyResponse_policyText :: Lens.Lens' GetRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
getRegistryPolicyResponse_policyText = Lens.lens (\GetRegistryPolicyResponse' {policyText} -> policyText) (\s@GetRegistryPolicyResponse' {} a -> s {policyText = a} :: GetRegistryPolicyResponse)

-- | The ID of the registry.
getRegistryPolicyResponse_registryId :: Lens.Lens' GetRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
getRegistryPolicyResponse_registryId = Lens.lens (\GetRegistryPolicyResponse' {registryId} -> registryId) (\s@GetRegistryPolicyResponse' {} a -> s {registryId = a} :: GetRegistryPolicyResponse)

-- | The response's http status code.
getRegistryPolicyResponse_httpStatus :: Lens.Lens' GetRegistryPolicyResponse Prelude.Int
getRegistryPolicyResponse_httpStatus = Lens.lens (\GetRegistryPolicyResponse' {httpStatus} -> httpStatus) (\s@GetRegistryPolicyResponse' {} a -> s {httpStatus = a} :: GetRegistryPolicyResponse)

instance Prelude.NFData GetRegistryPolicyResponse where
  rnf GetRegistryPolicyResponse' {..} =
    Prelude.rnf policyText `Prelude.seq`
      Prelude.rnf registryId `Prelude.seq`
        Prelude.rnf httpStatus
