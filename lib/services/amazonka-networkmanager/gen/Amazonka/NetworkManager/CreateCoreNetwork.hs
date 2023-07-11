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
-- Module      : Amazonka.NetworkManager.CreateCoreNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a core network as part of your global network, and optionally,
-- with a core network policy.
module Amazonka.NetworkManager.CreateCoreNetwork
  ( -- * Creating a Request
    CreateCoreNetwork (..),
    newCreateCoreNetwork,

    -- * Request Lenses
    createCoreNetwork_clientToken,
    createCoreNetwork_description,
    createCoreNetwork_policyDocument,
    createCoreNetwork_tags,
    createCoreNetwork_globalNetworkId,

    -- * Destructuring the Response
    CreateCoreNetworkResponse (..),
    newCreateCoreNetworkResponse,

    -- * Response Lenses
    createCoreNetworkResponse_coreNetwork,
    createCoreNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCoreNetwork' smart constructor.
data CreateCoreNetwork = CreateCoreNetwork'
  { -- | The client token associated with a core network request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of a core network.
    description :: Prelude.Maybe Prelude.Text,
    -- | The policy document for creating a core network.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | Key-value tags associated with a core network request.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the global network that a core network will be a part of.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCoreNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createCoreNetwork_clientToken' - The client token associated with a core network request.
--
-- 'description', 'createCoreNetwork_description' - The description of a core network.
--
-- 'policyDocument', 'createCoreNetwork_policyDocument' - The policy document for creating a core network.
--
-- 'tags', 'createCoreNetwork_tags' - Key-value tags associated with a core network request.
--
-- 'globalNetworkId', 'createCoreNetwork_globalNetworkId' - The ID of the global network that a core network will be a part of.
newCreateCoreNetwork ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  CreateCoreNetwork
newCreateCoreNetwork pGlobalNetworkId_ =
  CreateCoreNetwork'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      tags = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | The client token associated with a core network request.
createCoreNetwork_clientToken :: Lens.Lens' CreateCoreNetwork (Prelude.Maybe Prelude.Text)
createCoreNetwork_clientToken = Lens.lens (\CreateCoreNetwork' {clientToken} -> clientToken) (\s@CreateCoreNetwork' {} a -> s {clientToken = a} :: CreateCoreNetwork)

-- | The description of a core network.
createCoreNetwork_description :: Lens.Lens' CreateCoreNetwork (Prelude.Maybe Prelude.Text)
createCoreNetwork_description = Lens.lens (\CreateCoreNetwork' {description} -> description) (\s@CreateCoreNetwork' {} a -> s {description = a} :: CreateCoreNetwork)

-- | The policy document for creating a core network.
createCoreNetwork_policyDocument :: Lens.Lens' CreateCoreNetwork (Prelude.Maybe Prelude.Text)
createCoreNetwork_policyDocument = Lens.lens (\CreateCoreNetwork' {policyDocument} -> policyDocument) (\s@CreateCoreNetwork' {} a -> s {policyDocument = a} :: CreateCoreNetwork)

-- | Key-value tags associated with a core network request.
createCoreNetwork_tags :: Lens.Lens' CreateCoreNetwork (Prelude.Maybe [Tag])
createCoreNetwork_tags = Lens.lens (\CreateCoreNetwork' {tags} -> tags) (\s@CreateCoreNetwork' {} a -> s {tags = a} :: CreateCoreNetwork) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the global network that a core network will be a part of.
createCoreNetwork_globalNetworkId :: Lens.Lens' CreateCoreNetwork Prelude.Text
createCoreNetwork_globalNetworkId = Lens.lens (\CreateCoreNetwork' {globalNetworkId} -> globalNetworkId) (\s@CreateCoreNetwork' {} a -> s {globalNetworkId = a} :: CreateCoreNetwork)

instance Core.AWSRequest CreateCoreNetwork where
  type
    AWSResponse CreateCoreNetwork =
      CreateCoreNetworkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCoreNetworkResponse'
            Prelude.<$> (x Data..?> "CoreNetwork")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCoreNetwork where
  hashWithSalt _salt CreateCoreNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policyDocument
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData CreateCoreNetwork where
  rnf CreateCoreNetwork' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf policyDocument
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf globalNetworkId

instance Data.ToHeaders CreateCoreNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCoreNetwork where
  toJSON CreateCoreNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("PolicyDocument" Data..=)
              Prelude.<$> policyDocument,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("GlobalNetworkId" Data..= globalNetworkId)
          ]
      )

instance Data.ToPath CreateCoreNetwork where
  toPath = Prelude.const "/core-networks"

instance Data.ToQuery CreateCoreNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCoreNetworkResponse' smart constructor.
data CreateCoreNetworkResponse = CreateCoreNetworkResponse'
  { -- | Returns details about a core network.
    coreNetwork :: Prelude.Maybe CoreNetwork,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCoreNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetwork', 'createCoreNetworkResponse_coreNetwork' - Returns details about a core network.
--
-- 'httpStatus', 'createCoreNetworkResponse_httpStatus' - The response's http status code.
newCreateCoreNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCoreNetworkResponse
newCreateCoreNetworkResponse pHttpStatus_ =
  CreateCoreNetworkResponse'
    { coreNetwork =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns details about a core network.
createCoreNetworkResponse_coreNetwork :: Lens.Lens' CreateCoreNetworkResponse (Prelude.Maybe CoreNetwork)
createCoreNetworkResponse_coreNetwork = Lens.lens (\CreateCoreNetworkResponse' {coreNetwork} -> coreNetwork) (\s@CreateCoreNetworkResponse' {} a -> s {coreNetwork = a} :: CreateCoreNetworkResponse)

-- | The response's http status code.
createCoreNetworkResponse_httpStatus :: Lens.Lens' CreateCoreNetworkResponse Prelude.Int
createCoreNetworkResponse_httpStatus = Lens.lens (\CreateCoreNetworkResponse' {httpStatus} -> httpStatus) (\s@CreateCoreNetworkResponse' {} a -> s {httpStatus = a} :: CreateCoreNetworkResponse)

instance Prelude.NFData CreateCoreNetworkResponse where
  rnf CreateCoreNetworkResponse' {..} =
    Prelude.rnf coreNetwork
      `Prelude.seq` Prelude.rnf httpStatus
