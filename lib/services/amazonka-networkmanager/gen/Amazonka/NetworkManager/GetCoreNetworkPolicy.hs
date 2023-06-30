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
-- Module      : Amazonka.NetworkManager.GetCoreNetworkPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about a core network policy. You can get details about
-- your current live policy or any previous policy version.
module Amazonka.NetworkManager.GetCoreNetworkPolicy
  ( -- * Creating a Request
    GetCoreNetworkPolicy (..),
    newGetCoreNetworkPolicy,

    -- * Request Lenses
    getCoreNetworkPolicy_alias,
    getCoreNetworkPolicy_policyVersionId,
    getCoreNetworkPolicy_coreNetworkId,

    -- * Destructuring the Response
    GetCoreNetworkPolicyResponse (..),
    newGetCoreNetworkPolicyResponse,

    -- * Response Lenses
    getCoreNetworkPolicyResponse_coreNetworkPolicy,
    getCoreNetworkPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCoreNetworkPolicy' smart constructor.
data GetCoreNetworkPolicy = GetCoreNetworkPolicy'
  { -- | The alias of a core network policy
    alias :: Prelude.Maybe CoreNetworkPolicyAlias,
    -- | The ID of a core network policy version.
    policyVersionId :: Prelude.Maybe Prelude.Int,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreNetworkPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'getCoreNetworkPolicy_alias' - The alias of a core network policy
--
-- 'policyVersionId', 'getCoreNetworkPolicy_policyVersionId' - The ID of a core network policy version.
--
-- 'coreNetworkId', 'getCoreNetworkPolicy_coreNetworkId' - The ID of a core network.
newGetCoreNetworkPolicy ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  GetCoreNetworkPolicy
newGetCoreNetworkPolicy pCoreNetworkId_ =
  GetCoreNetworkPolicy'
    { alias = Prelude.Nothing,
      policyVersionId = Prelude.Nothing,
      coreNetworkId = pCoreNetworkId_
    }

-- | The alias of a core network policy
getCoreNetworkPolicy_alias :: Lens.Lens' GetCoreNetworkPolicy (Prelude.Maybe CoreNetworkPolicyAlias)
getCoreNetworkPolicy_alias = Lens.lens (\GetCoreNetworkPolicy' {alias} -> alias) (\s@GetCoreNetworkPolicy' {} a -> s {alias = a} :: GetCoreNetworkPolicy)

-- | The ID of a core network policy version.
getCoreNetworkPolicy_policyVersionId :: Lens.Lens' GetCoreNetworkPolicy (Prelude.Maybe Prelude.Int)
getCoreNetworkPolicy_policyVersionId = Lens.lens (\GetCoreNetworkPolicy' {policyVersionId} -> policyVersionId) (\s@GetCoreNetworkPolicy' {} a -> s {policyVersionId = a} :: GetCoreNetworkPolicy)

-- | The ID of a core network.
getCoreNetworkPolicy_coreNetworkId :: Lens.Lens' GetCoreNetworkPolicy Prelude.Text
getCoreNetworkPolicy_coreNetworkId = Lens.lens (\GetCoreNetworkPolicy' {coreNetworkId} -> coreNetworkId) (\s@GetCoreNetworkPolicy' {} a -> s {coreNetworkId = a} :: GetCoreNetworkPolicy)

instance Core.AWSRequest GetCoreNetworkPolicy where
  type
    AWSResponse GetCoreNetworkPolicy =
      GetCoreNetworkPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreNetworkPolicyResponse'
            Prelude.<$> (x Data..?> "CoreNetworkPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCoreNetworkPolicy where
  hashWithSalt _salt GetCoreNetworkPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` policyVersionId
      `Prelude.hashWithSalt` coreNetworkId

instance Prelude.NFData GetCoreNetworkPolicy where
  rnf GetCoreNetworkPolicy' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf policyVersionId
      `Prelude.seq` Prelude.rnf coreNetworkId

instance Data.ToHeaders GetCoreNetworkPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCoreNetworkPolicy where
  toPath GetCoreNetworkPolicy' {..} =
    Prelude.mconcat
      [ "/core-networks/",
        Data.toBS coreNetworkId,
        "/core-network-policy"
      ]

instance Data.ToQuery GetCoreNetworkPolicy where
  toQuery GetCoreNetworkPolicy' {..} =
    Prelude.mconcat
      [ "alias" Data.=: alias,
        "policyVersionId" Data.=: policyVersionId
      ]

-- | /See:/ 'newGetCoreNetworkPolicyResponse' smart constructor.
data GetCoreNetworkPolicyResponse = GetCoreNetworkPolicyResponse'
  { -- | The details about a core network policy.
    coreNetworkPolicy :: Prelude.Maybe CoreNetworkPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCoreNetworkPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkPolicy', 'getCoreNetworkPolicyResponse_coreNetworkPolicy' - The details about a core network policy.
--
-- 'httpStatus', 'getCoreNetworkPolicyResponse_httpStatus' - The response's http status code.
newGetCoreNetworkPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCoreNetworkPolicyResponse
newGetCoreNetworkPolicyResponse pHttpStatus_ =
  GetCoreNetworkPolicyResponse'
    { coreNetworkPolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details about a core network policy.
getCoreNetworkPolicyResponse_coreNetworkPolicy :: Lens.Lens' GetCoreNetworkPolicyResponse (Prelude.Maybe CoreNetworkPolicy)
getCoreNetworkPolicyResponse_coreNetworkPolicy = Lens.lens (\GetCoreNetworkPolicyResponse' {coreNetworkPolicy} -> coreNetworkPolicy) (\s@GetCoreNetworkPolicyResponse' {} a -> s {coreNetworkPolicy = a} :: GetCoreNetworkPolicyResponse)

-- | The response's http status code.
getCoreNetworkPolicyResponse_httpStatus :: Lens.Lens' GetCoreNetworkPolicyResponse Prelude.Int
getCoreNetworkPolicyResponse_httpStatus = Lens.lens (\GetCoreNetworkPolicyResponse' {httpStatus} -> httpStatus) (\s@GetCoreNetworkPolicyResponse' {} a -> s {httpStatus = a} :: GetCoreNetworkPolicyResponse)

instance Prelude.NFData GetCoreNetworkPolicyResponse where
  rnf GetCoreNetworkPolicyResponse' {..} =
    Prelude.rnf coreNetworkPolicy
      `Prelude.seq` Prelude.rnf httpStatus
