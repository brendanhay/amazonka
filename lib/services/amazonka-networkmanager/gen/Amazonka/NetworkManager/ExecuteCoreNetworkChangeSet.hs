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
-- Module      : Amazonka.NetworkManager.ExecuteCoreNetworkChangeSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes a change set on your core network. Deploys changes globally
-- based on the policy submitted..
module Amazonka.NetworkManager.ExecuteCoreNetworkChangeSet
  ( -- * Creating a Request
    ExecuteCoreNetworkChangeSet (..),
    newExecuteCoreNetworkChangeSet,

    -- * Request Lenses
    executeCoreNetworkChangeSet_coreNetworkId,
    executeCoreNetworkChangeSet_policyVersionId,

    -- * Destructuring the Response
    ExecuteCoreNetworkChangeSetResponse (..),
    newExecuteCoreNetworkChangeSetResponse,

    -- * Response Lenses
    executeCoreNetworkChangeSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExecuteCoreNetworkChangeSet' smart constructor.
data ExecuteCoreNetworkChangeSet = ExecuteCoreNetworkChangeSet'
  { -- | The ID of a core network.
    coreNetworkId :: Prelude.Text,
    -- | The ID of the policy version.
    policyVersionId :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteCoreNetworkChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkId', 'executeCoreNetworkChangeSet_coreNetworkId' - The ID of a core network.
--
-- 'policyVersionId', 'executeCoreNetworkChangeSet_policyVersionId' - The ID of the policy version.
newExecuteCoreNetworkChangeSet ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'policyVersionId'
  Prelude.Int ->
  ExecuteCoreNetworkChangeSet
newExecuteCoreNetworkChangeSet
  pCoreNetworkId_
  pPolicyVersionId_ =
    ExecuteCoreNetworkChangeSet'
      { coreNetworkId =
          pCoreNetworkId_,
        policyVersionId = pPolicyVersionId_
      }

-- | The ID of a core network.
executeCoreNetworkChangeSet_coreNetworkId :: Lens.Lens' ExecuteCoreNetworkChangeSet Prelude.Text
executeCoreNetworkChangeSet_coreNetworkId = Lens.lens (\ExecuteCoreNetworkChangeSet' {coreNetworkId} -> coreNetworkId) (\s@ExecuteCoreNetworkChangeSet' {} a -> s {coreNetworkId = a} :: ExecuteCoreNetworkChangeSet)

-- | The ID of the policy version.
executeCoreNetworkChangeSet_policyVersionId :: Lens.Lens' ExecuteCoreNetworkChangeSet Prelude.Int
executeCoreNetworkChangeSet_policyVersionId = Lens.lens (\ExecuteCoreNetworkChangeSet' {policyVersionId} -> policyVersionId) (\s@ExecuteCoreNetworkChangeSet' {} a -> s {policyVersionId = a} :: ExecuteCoreNetworkChangeSet)

instance Core.AWSRequest ExecuteCoreNetworkChangeSet where
  type
    AWSResponse ExecuteCoreNetworkChangeSet =
      ExecuteCoreNetworkChangeSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ExecuteCoreNetworkChangeSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExecuteCoreNetworkChangeSet where
  hashWithSalt _salt ExecuteCoreNetworkChangeSet' {..} =
    _salt `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` policyVersionId

instance Prelude.NFData ExecuteCoreNetworkChangeSet where
  rnf ExecuteCoreNetworkChangeSet' {..} =
    Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf policyVersionId

instance Core.ToHeaders ExecuteCoreNetworkChangeSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ExecuteCoreNetworkChangeSet where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath ExecuteCoreNetworkChangeSet where
  toPath ExecuteCoreNetworkChangeSet' {..} =
    Prelude.mconcat
      [ "/core-networks/",
        Core.toBS coreNetworkId,
        "/core-network-change-sets/",
        Core.toBS policyVersionId,
        "/execute"
      ]

instance Core.ToQuery ExecuteCoreNetworkChangeSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteCoreNetworkChangeSetResponse' smart constructor.
data ExecuteCoreNetworkChangeSetResponse = ExecuteCoreNetworkChangeSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteCoreNetworkChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'executeCoreNetworkChangeSetResponse_httpStatus' - The response's http status code.
newExecuteCoreNetworkChangeSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExecuteCoreNetworkChangeSetResponse
newExecuteCoreNetworkChangeSetResponse pHttpStatus_ =
  ExecuteCoreNetworkChangeSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
executeCoreNetworkChangeSetResponse_httpStatus :: Lens.Lens' ExecuteCoreNetworkChangeSetResponse Prelude.Int
executeCoreNetworkChangeSetResponse_httpStatus = Lens.lens (\ExecuteCoreNetworkChangeSetResponse' {httpStatus} -> httpStatus) (\s@ExecuteCoreNetworkChangeSetResponse' {} a -> s {httpStatus = a} :: ExecuteCoreNetworkChangeSetResponse)

instance
  Prelude.NFData
    ExecuteCoreNetworkChangeSetResponse
  where
  rnf ExecuteCoreNetworkChangeSetResponse' {..} =
    Prelude.rnf httpStatus
