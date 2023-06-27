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
-- Module      : Amazonka.VPCLattice.CreateServiceNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a service network. A service network is a logical boundary for a
-- collection of services. You can associate services and VPCs with a
-- service network.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/service-networks.html Service networks>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.CreateServiceNetwork
  ( -- * Creating a Request
    CreateServiceNetwork (..),
    newCreateServiceNetwork,

    -- * Request Lenses
    createServiceNetwork_authType,
    createServiceNetwork_clientToken,
    createServiceNetwork_tags,
    createServiceNetwork_name,

    -- * Destructuring the Response
    CreateServiceNetworkResponse (..),
    newCreateServiceNetworkResponse,

    -- * Response Lenses
    createServiceNetworkResponse_arn,
    createServiceNetworkResponse_authType,
    createServiceNetworkResponse_id,
    createServiceNetworkResponse_name,
    createServiceNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newCreateServiceNetwork' smart constructor.
data CreateServiceNetwork = CreateServiceNetwork'
  { -- | The type of IAM policy.
    --
    -- -   @NONE@: The resource does not use an IAM policy. This is the
    --     default.
    --
    -- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
    --     auth is enabled and an auth policy is required.
    authType :: Prelude.Maybe AuthType,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you retry a request that completed
    -- successfully using the same client token and parameters, the retry
    -- succeeds without performing any actions. If the parameters aren\'t
    -- identical, the retry fails.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags for the service network.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the service network. The name must be unique to the account.
    -- The valid characters are a-z, 0-9, and hyphens (-). You can\'t use a
    -- hyphen as the first or last character, or immediately after another
    -- hyphen.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authType', 'createServiceNetwork_authType' - The type of IAM policy.
--
-- -   @NONE@: The resource does not use an IAM policy. This is the
--     default.
--
-- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
--     auth is enabled and an auth policy is required.
--
-- 'clientToken', 'createServiceNetwork_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
--
-- 'tags', 'createServiceNetwork_tags' - The tags for the service network.
--
-- 'name', 'createServiceNetwork_name' - The name of the service network. The name must be unique to the account.
-- The valid characters are a-z, 0-9, and hyphens (-). You can\'t use a
-- hyphen as the first or last character, or immediately after another
-- hyphen.
newCreateServiceNetwork ::
  -- | 'name'
  Prelude.Text ->
  CreateServiceNetwork
newCreateServiceNetwork pName_ =
  CreateServiceNetwork'
    { authType = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | The type of IAM policy.
--
-- -   @NONE@: The resource does not use an IAM policy. This is the
--     default.
--
-- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
--     auth is enabled and an auth policy is required.
createServiceNetwork_authType :: Lens.Lens' CreateServiceNetwork (Prelude.Maybe AuthType)
createServiceNetwork_authType = Lens.lens (\CreateServiceNetwork' {authType} -> authType) (\s@CreateServiceNetwork' {} a -> s {authType = a} :: CreateServiceNetwork)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
createServiceNetwork_clientToken :: Lens.Lens' CreateServiceNetwork (Prelude.Maybe Prelude.Text)
createServiceNetwork_clientToken = Lens.lens (\CreateServiceNetwork' {clientToken} -> clientToken) (\s@CreateServiceNetwork' {} a -> s {clientToken = a} :: CreateServiceNetwork)

-- | The tags for the service network.
createServiceNetwork_tags :: Lens.Lens' CreateServiceNetwork (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createServiceNetwork_tags = Lens.lens (\CreateServiceNetwork' {tags} -> tags) (\s@CreateServiceNetwork' {} a -> s {tags = a} :: CreateServiceNetwork) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service network. The name must be unique to the account.
-- The valid characters are a-z, 0-9, and hyphens (-). You can\'t use a
-- hyphen as the first or last character, or immediately after another
-- hyphen.
createServiceNetwork_name :: Lens.Lens' CreateServiceNetwork Prelude.Text
createServiceNetwork_name = Lens.lens (\CreateServiceNetwork' {name} -> name) (\s@CreateServiceNetwork' {} a -> s {name = a} :: CreateServiceNetwork)

instance Core.AWSRequest CreateServiceNetwork where
  type
    AWSResponse CreateServiceNetwork =
      CreateServiceNetworkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceNetworkResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authType")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateServiceNetwork where
  hashWithSalt _salt CreateServiceNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateServiceNetwork where
  rnf CreateServiceNetwork' {..} =
    Prelude.rnf authType
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateServiceNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateServiceNetwork where
  toJSON CreateServiceNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authType" Data..=) Prelude.<$> authType,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateServiceNetwork where
  toPath = Prelude.const "/servicenetworks"

instance Data.ToQuery CreateServiceNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceNetworkResponse' smart constructor.
data CreateServiceNetworkResponse = CreateServiceNetworkResponse'
  { -- | The Amazon Resource Name (ARN) of the service network.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of IAM policy.
    authType :: Prelude.Maybe AuthType,
    -- | The ID of the service network.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the service network.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createServiceNetworkResponse_arn' - The Amazon Resource Name (ARN) of the service network.
--
-- 'authType', 'createServiceNetworkResponse_authType' - The type of IAM policy.
--
-- 'id', 'createServiceNetworkResponse_id' - The ID of the service network.
--
-- 'name', 'createServiceNetworkResponse_name' - The name of the service network.
--
-- 'httpStatus', 'createServiceNetworkResponse_httpStatus' - The response's http status code.
newCreateServiceNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceNetworkResponse
newCreateServiceNetworkResponse pHttpStatus_ =
  CreateServiceNetworkResponse'
    { arn =
        Prelude.Nothing,
      authType = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the service network.
createServiceNetworkResponse_arn :: Lens.Lens' CreateServiceNetworkResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkResponse_arn = Lens.lens (\CreateServiceNetworkResponse' {arn} -> arn) (\s@CreateServiceNetworkResponse' {} a -> s {arn = a} :: CreateServiceNetworkResponse)

-- | The type of IAM policy.
createServiceNetworkResponse_authType :: Lens.Lens' CreateServiceNetworkResponse (Prelude.Maybe AuthType)
createServiceNetworkResponse_authType = Lens.lens (\CreateServiceNetworkResponse' {authType} -> authType) (\s@CreateServiceNetworkResponse' {} a -> s {authType = a} :: CreateServiceNetworkResponse)

-- | The ID of the service network.
createServiceNetworkResponse_id :: Lens.Lens' CreateServiceNetworkResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkResponse_id = Lens.lens (\CreateServiceNetworkResponse' {id} -> id) (\s@CreateServiceNetworkResponse' {} a -> s {id = a} :: CreateServiceNetworkResponse)

-- | The name of the service network.
createServiceNetworkResponse_name :: Lens.Lens' CreateServiceNetworkResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkResponse_name = Lens.lens (\CreateServiceNetworkResponse' {name} -> name) (\s@CreateServiceNetworkResponse' {} a -> s {name = a} :: CreateServiceNetworkResponse)

-- | The response's http status code.
createServiceNetworkResponse_httpStatus :: Lens.Lens' CreateServiceNetworkResponse Prelude.Int
createServiceNetworkResponse_httpStatus = Lens.lens (\CreateServiceNetworkResponse' {httpStatus} -> httpStatus) (\s@CreateServiceNetworkResponse' {} a -> s {httpStatus = a} :: CreateServiceNetworkResponse)

instance Prelude.NFData CreateServiceNetworkResponse where
  rnf CreateServiceNetworkResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
