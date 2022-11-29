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
-- Module      : Amazonka.Schemas.GetResourcePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource-based policy attached to a given registry.
module Amazonka.Schemas.GetResourcePolicy
  ( -- * Creating a Request
    GetResourcePolicy (..),
    newGetResourcePolicy,

    -- * Request Lenses
    getResourcePolicy_registryName,

    -- * Destructuring the Response
    GetResourcePolicyResponse (..),
    newGetResourcePolicyResponse,

    -- * Response Lenses
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_revisionId,
    getResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newGetResourcePolicy' smart constructor.
data GetResourcePolicy = GetResourcePolicy'
  { -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'getResourcePolicy_registryName' - The name of the registry.
newGetResourcePolicy ::
  GetResourcePolicy
newGetResourcePolicy =
  GetResourcePolicy' {registryName = Prelude.Nothing}

-- | The name of the registry.
getResourcePolicy_registryName :: Lens.Lens' GetResourcePolicy (Prelude.Maybe Prelude.Text)
getResourcePolicy_registryName = Lens.lens (\GetResourcePolicy' {registryName} -> registryName) (\s@GetResourcePolicy' {} a -> s {registryName = a} :: GetResourcePolicy)

instance Core.AWSRequest GetResourcePolicy where
  type
    AWSResponse GetResourcePolicy =
      GetResourcePolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Prelude.<$> (x Core..?> "Policy")
            Prelude.<*> (x Core..?> "RevisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePolicy where
  hashWithSalt _salt GetResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` registryName

instance Prelude.NFData GetResourcePolicy where
  rnf GetResourcePolicy' {..} = Prelude.rnf registryName

instance Core.ToHeaders GetResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetResourcePolicy where
  toPath = Prelude.const "/v1/policy"

instance Core.ToQuery GetResourcePolicy where
  toQuery GetResourcePolicy' {..} =
    Prelude.mconcat
      ["registryName" Core.=: registryName]

-- | /See:/ 'newGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The resource-based policy.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The revision ID.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getResourcePolicyResponse_policy' - The resource-based policy.
--
-- 'revisionId', 'getResourcePolicyResponse_revisionId' - The revision ID.
--
-- 'httpStatus', 'getResourcePolicyResponse_httpStatus' - The response's http status code.
newGetResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePolicyResponse
newGetResourcePolicyResponse pHttpStatus_ =
  GetResourcePolicyResponse'
    { policy =
        Prelude.Nothing,
      revisionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource-based policy.
getResourcePolicyResponse_policy :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_policy = Lens.lens (\GetResourcePolicyResponse' {policy} -> policy) (\s@GetResourcePolicyResponse' {} a -> s {policy = a} :: GetResourcePolicyResponse)

-- | The revision ID.
getResourcePolicyResponse_revisionId :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_revisionId = Lens.lens (\GetResourcePolicyResponse' {revisionId} -> revisionId) (\s@GetResourcePolicyResponse' {} a -> s {revisionId = a} :: GetResourcePolicyResponse)

-- | The response's http status code.
getResourcePolicyResponse_httpStatus :: Lens.Lens' GetResourcePolicyResponse Prelude.Int
getResourcePolicyResponse_httpStatus = Lens.lens (\GetResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@GetResourcePolicyResponse' {} a -> s {httpStatus = a} :: GetResourcePolicyResponse)

instance Prelude.NFData GetResourcePolicyResponse where
  rnf GetResourcePolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf httpStatus
