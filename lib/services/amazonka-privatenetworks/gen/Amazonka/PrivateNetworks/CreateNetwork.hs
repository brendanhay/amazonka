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
-- Module      : Amazonka.PrivateNetworks.CreateNetwork
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network.
module Amazonka.PrivateNetworks.CreateNetwork
  ( -- * Creating a Request
    CreateNetwork (..),
    newCreateNetwork,

    -- * Request Lenses
    createNetwork_tags,
    createNetwork_clientToken,
    createNetwork_description,
    createNetwork_networkName,

    -- * Destructuring the Response
    CreateNetworkResponse (..),
    newCreateNetworkResponse,

    -- * Response Lenses
    createNetworkResponse_tags,
    createNetworkResponse_httpStatus,
    createNetworkResponse_network,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNetwork' smart constructor.
data CreateNetwork = CreateNetwork'
  { -- | The tags to apply to the network.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the network.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the network. You can\'t change the name after you create the
    -- network.
    networkName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createNetwork_tags' - The tags to apply to the network.
--
-- 'clientToken', 'createNetwork_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'description', 'createNetwork_description' - The description of the network.
--
-- 'networkName', 'createNetwork_networkName' - The name of the network. You can\'t change the name after you create the
-- network.
newCreateNetwork ::
  -- | 'networkName'
  Prelude.Text ->
  CreateNetwork
newCreateNetwork pNetworkName_ =
  CreateNetwork'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      networkName = pNetworkName_
    }

-- | The tags to apply to the network.
createNetwork_tags :: Lens.Lens' CreateNetwork (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNetwork_tags = Lens.lens (\CreateNetwork' {tags} -> tags) (\s@CreateNetwork' {} a -> s {tags = a} :: CreateNetwork) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
createNetwork_clientToken :: Lens.Lens' CreateNetwork (Prelude.Maybe Prelude.Text)
createNetwork_clientToken = Lens.lens (\CreateNetwork' {clientToken} -> clientToken) (\s@CreateNetwork' {} a -> s {clientToken = a} :: CreateNetwork)

-- | The description of the network.
createNetwork_description :: Lens.Lens' CreateNetwork (Prelude.Maybe Prelude.Text)
createNetwork_description = Lens.lens (\CreateNetwork' {description} -> description) (\s@CreateNetwork' {} a -> s {description = a} :: CreateNetwork)

-- | The name of the network. You can\'t change the name after you create the
-- network.
createNetwork_networkName :: Lens.Lens' CreateNetwork Prelude.Text
createNetwork_networkName = Lens.lens (\CreateNetwork' {networkName} -> networkName) (\s@CreateNetwork' {} a -> s {networkName = a} :: CreateNetwork)

instance Core.AWSRequest CreateNetwork where
  type
    AWSResponse CreateNetwork =
      CreateNetworkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNetworkResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "network")
      )

instance Prelude.Hashable CreateNetwork where
  hashWithSalt _salt CreateNetwork' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` networkName

instance Prelude.NFData CreateNetwork where
  rnf CreateNetwork' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf networkName

instance Core.ToHeaders CreateNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateNetwork where
  toJSON CreateNetwork' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("networkName" Core..= networkName)
          ]
      )

instance Core.ToPath CreateNetwork where
  toPath = Prelude.const "/v1/networks"

instance Core.ToQuery CreateNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNetworkResponse' smart constructor.
data CreateNetworkResponse = CreateNetworkResponse'
  { -- | The network tags.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the network.
    network :: Network
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createNetworkResponse_tags' - The network tags.
--
-- 'httpStatus', 'createNetworkResponse_httpStatus' - The response's http status code.
--
-- 'network', 'createNetworkResponse_network' - Information about the network.
newCreateNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'network'
  Network ->
  CreateNetworkResponse
newCreateNetworkResponse pHttpStatus_ pNetwork_ =
  CreateNetworkResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      network = pNetwork_
    }

-- | The network tags.
createNetworkResponse_tags :: Lens.Lens' CreateNetworkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNetworkResponse_tags = Lens.lens (\CreateNetworkResponse' {tags} -> tags) (\s@CreateNetworkResponse' {} a -> s {tags = a} :: CreateNetworkResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
createNetworkResponse_httpStatus :: Lens.Lens' CreateNetworkResponse Prelude.Int
createNetworkResponse_httpStatus = Lens.lens (\CreateNetworkResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkResponse' {} a -> s {httpStatus = a} :: CreateNetworkResponse)

-- | Information about the network.
createNetworkResponse_network :: Lens.Lens' CreateNetworkResponse Network
createNetworkResponse_network = Lens.lens (\CreateNetworkResponse' {network} -> network) (\s@CreateNetworkResponse' {} a -> s {network = a} :: CreateNetworkResponse)

instance Prelude.NFData CreateNetworkResponse where
  rnf CreateNetworkResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf network
