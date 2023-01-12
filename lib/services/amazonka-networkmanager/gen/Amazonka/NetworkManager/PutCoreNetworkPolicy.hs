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
-- Module      : Amazonka.NetworkManager.PutCoreNetworkPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, immutable version of a core network policy. A subsequent
-- change set is created showing the differences between the LIVE policy
-- and the submitted policy.
module Amazonka.NetworkManager.PutCoreNetworkPolicy
  ( -- * Creating a Request
    PutCoreNetworkPolicy (..),
    newPutCoreNetworkPolicy,

    -- * Request Lenses
    putCoreNetworkPolicy_clientToken,
    putCoreNetworkPolicy_description,
    putCoreNetworkPolicy_latestVersionId,
    putCoreNetworkPolicy_coreNetworkId,
    putCoreNetworkPolicy_policyDocument,

    -- * Destructuring the Response
    PutCoreNetworkPolicyResponse (..),
    newPutCoreNetworkPolicyResponse,

    -- * Response Lenses
    putCoreNetworkPolicyResponse_coreNetworkPolicy,
    putCoreNetworkPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutCoreNetworkPolicy' smart constructor.
data PutCoreNetworkPolicy = PutCoreNetworkPolicy'
  { -- | The client token associated with the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | a core network policy description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network policy.
    latestVersionId :: Prelude.Maybe Prelude.Int,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Text,
    -- | The policy document.
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutCoreNetworkPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'putCoreNetworkPolicy_clientToken' - The client token associated with the request.
--
-- 'description', 'putCoreNetworkPolicy_description' - a core network policy description.
--
-- 'latestVersionId', 'putCoreNetworkPolicy_latestVersionId' - The ID of a core network policy.
--
-- 'coreNetworkId', 'putCoreNetworkPolicy_coreNetworkId' - The ID of a core network.
--
-- 'policyDocument', 'putCoreNetworkPolicy_policyDocument' - The policy document.
newPutCoreNetworkPolicy ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  PutCoreNetworkPolicy
newPutCoreNetworkPolicy
  pCoreNetworkId_
  pPolicyDocument_ =
    PutCoreNetworkPolicy'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        latestVersionId = Prelude.Nothing,
        coreNetworkId = pCoreNetworkId_,
        policyDocument = pPolicyDocument_
      }

-- | The client token associated with the request.
putCoreNetworkPolicy_clientToken :: Lens.Lens' PutCoreNetworkPolicy (Prelude.Maybe Prelude.Text)
putCoreNetworkPolicy_clientToken = Lens.lens (\PutCoreNetworkPolicy' {clientToken} -> clientToken) (\s@PutCoreNetworkPolicy' {} a -> s {clientToken = a} :: PutCoreNetworkPolicy)

-- | a core network policy description.
putCoreNetworkPolicy_description :: Lens.Lens' PutCoreNetworkPolicy (Prelude.Maybe Prelude.Text)
putCoreNetworkPolicy_description = Lens.lens (\PutCoreNetworkPolicy' {description} -> description) (\s@PutCoreNetworkPolicy' {} a -> s {description = a} :: PutCoreNetworkPolicy)

-- | The ID of a core network policy.
putCoreNetworkPolicy_latestVersionId :: Lens.Lens' PutCoreNetworkPolicy (Prelude.Maybe Prelude.Int)
putCoreNetworkPolicy_latestVersionId = Lens.lens (\PutCoreNetworkPolicy' {latestVersionId} -> latestVersionId) (\s@PutCoreNetworkPolicy' {} a -> s {latestVersionId = a} :: PutCoreNetworkPolicy)

-- | The ID of a core network.
putCoreNetworkPolicy_coreNetworkId :: Lens.Lens' PutCoreNetworkPolicy Prelude.Text
putCoreNetworkPolicy_coreNetworkId = Lens.lens (\PutCoreNetworkPolicy' {coreNetworkId} -> coreNetworkId) (\s@PutCoreNetworkPolicy' {} a -> s {coreNetworkId = a} :: PutCoreNetworkPolicy)

-- | The policy document.
putCoreNetworkPolicy_policyDocument :: Lens.Lens' PutCoreNetworkPolicy Prelude.Text
putCoreNetworkPolicy_policyDocument = Lens.lens (\PutCoreNetworkPolicy' {policyDocument} -> policyDocument) (\s@PutCoreNetworkPolicy' {} a -> s {policyDocument = a} :: PutCoreNetworkPolicy)

instance Core.AWSRequest PutCoreNetworkPolicy where
  type
    AWSResponse PutCoreNetworkPolicy =
      PutCoreNetworkPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutCoreNetworkPolicyResponse'
            Prelude.<$> (x Data..?> "CoreNetworkPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutCoreNetworkPolicy where
  hashWithSalt _salt PutCoreNetworkPolicy' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` latestVersionId
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData PutCoreNetworkPolicy where
  rnf PutCoreNetworkPolicy' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf latestVersionId
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf policyDocument

instance Data.ToHeaders PutCoreNetworkPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutCoreNetworkPolicy where
  toJSON PutCoreNetworkPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("LatestVersionId" Data..=)
              Prelude.<$> latestVersionId,
            Prelude.Just
              ("PolicyDocument" Data..= policyDocument)
          ]
      )

instance Data.ToPath PutCoreNetworkPolicy where
  toPath PutCoreNetworkPolicy' {..} =
    Prelude.mconcat
      [ "/core-networks/",
        Data.toBS coreNetworkId,
        "/core-network-policy"
      ]

instance Data.ToQuery PutCoreNetworkPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutCoreNetworkPolicyResponse' smart constructor.
data PutCoreNetworkPolicyResponse = PutCoreNetworkPolicyResponse'
  { -- | Describes the changed core network policy.
    coreNetworkPolicy :: Prelude.Maybe CoreNetworkPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutCoreNetworkPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkPolicy', 'putCoreNetworkPolicyResponse_coreNetworkPolicy' - Describes the changed core network policy.
--
-- 'httpStatus', 'putCoreNetworkPolicyResponse_httpStatus' - The response's http status code.
newPutCoreNetworkPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutCoreNetworkPolicyResponse
newPutCoreNetworkPolicyResponse pHttpStatus_ =
  PutCoreNetworkPolicyResponse'
    { coreNetworkPolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes the changed core network policy.
putCoreNetworkPolicyResponse_coreNetworkPolicy :: Lens.Lens' PutCoreNetworkPolicyResponse (Prelude.Maybe CoreNetworkPolicy)
putCoreNetworkPolicyResponse_coreNetworkPolicy = Lens.lens (\PutCoreNetworkPolicyResponse' {coreNetworkPolicy} -> coreNetworkPolicy) (\s@PutCoreNetworkPolicyResponse' {} a -> s {coreNetworkPolicy = a} :: PutCoreNetworkPolicyResponse)

-- | The response's http status code.
putCoreNetworkPolicyResponse_httpStatus :: Lens.Lens' PutCoreNetworkPolicyResponse Prelude.Int
putCoreNetworkPolicyResponse_httpStatus = Lens.lens (\PutCoreNetworkPolicyResponse' {httpStatus} -> httpStatus) (\s@PutCoreNetworkPolicyResponse' {} a -> s {httpStatus = a} :: PutCoreNetworkPolicyResponse)

instance Prelude.NFData PutCoreNetworkPolicyResponse where
  rnf PutCoreNetworkPolicyResponse' {..} =
    Prelude.rnf coreNetworkPolicy
      `Prelude.seq` Prelude.rnf httpStatus
