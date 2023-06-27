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
-- Module      : Amazonka.VPCLattice.UpdateServiceNetwork
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified service network.
module Amazonka.VPCLattice.UpdateServiceNetwork
  ( -- * Creating a Request
    UpdateServiceNetwork (..),
    newUpdateServiceNetwork,

    -- * Request Lenses
    updateServiceNetwork_authType,
    updateServiceNetwork_serviceNetworkIdentifier,

    -- * Destructuring the Response
    UpdateServiceNetworkResponse (..),
    newUpdateServiceNetworkResponse,

    -- * Response Lenses
    updateServiceNetworkResponse_arn,
    updateServiceNetworkResponse_authType,
    updateServiceNetworkResponse_id,
    updateServiceNetworkResponse_name,
    updateServiceNetworkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newUpdateServiceNetwork' smart constructor.
data UpdateServiceNetwork = UpdateServiceNetwork'
  { -- | The type of IAM policy.
    --
    -- -   @NONE@: The resource does not use an IAM policy. This is the
    --     default.
    --
    -- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
    --     auth is enabled and an auth policy is required.
    authType :: AuthType,
    -- | The ID or Amazon Resource Name (ARN) of the service network.
    serviceNetworkIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authType', 'updateServiceNetwork_authType' - The type of IAM policy.
--
-- -   @NONE@: The resource does not use an IAM policy. This is the
--     default.
--
-- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
--     auth is enabled and an auth policy is required.
--
-- 'serviceNetworkIdentifier', 'updateServiceNetwork_serviceNetworkIdentifier' - The ID or Amazon Resource Name (ARN) of the service network.
newUpdateServiceNetwork ::
  -- | 'authType'
  AuthType ->
  -- | 'serviceNetworkIdentifier'
  Prelude.Text ->
  UpdateServiceNetwork
newUpdateServiceNetwork
  pAuthType_
  pServiceNetworkIdentifier_ =
    UpdateServiceNetwork'
      { authType = pAuthType_,
        serviceNetworkIdentifier =
          pServiceNetworkIdentifier_
      }

-- | The type of IAM policy.
--
-- -   @NONE@: The resource does not use an IAM policy. This is the
--     default.
--
-- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
--     auth is enabled and an auth policy is required.
updateServiceNetwork_authType :: Lens.Lens' UpdateServiceNetwork AuthType
updateServiceNetwork_authType = Lens.lens (\UpdateServiceNetwork' {authType} -> authType) (\s@UpdateServiceNetwork' {} a -> s {authType = a} :: UpdateServiceNetwork)

-- | The ID or Amazon Resource Name (ARN) of the service network.
updateServiceNetwork_serviceNetworkIdentifier :: Lens.Lens' UpdateServiceNetwork Prelude.Text
updateServiceNetwork_serviceNetworkIdentifier = Lens.lens (\UpdateServiceNetwork' {serviceNetworkIdentifier} -> serviceNetworkIdentifier) (\s@UpdateServiceNetwork' {} a -> s {serviceNetworkIdentifier = a} :: UpdateServiceNetwork)

instance Core.AWSRequest UpdateServiceNetwork where
  type
    AWSResponse UpdateServiceNetwork =
      UpdateServiceNetworkResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceNetworkResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authType")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServiceNetwork where
  hashWithSalt _salt UpdateServiceNetwork' {..} =
    _salt
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` serviceNetworkIdentifier

instance Prelude.NFData UpdateServiceNetwork where
  rnf UpdateServiceNetwork' {..} =
    Prelude.rnf authType
      `Prelude.seq` Prelude.rnf serviceNetworkIdentifier

instance Data.ToHeaders UpdateServiceNetwork where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateServiceNetwork where
  toJSON UpdateServiceNetwork' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("authType" Data..= authType)]
      )

instance Data.ToPath UpdateServiceNetwork where
  toPath UpdateServiceNetwork' {..} =
    Prelude.mconcat
      [ "/servicenetworks/",
        Data.toBS serviceNetworkIdentifier
      ]

instance Data.ToQuery UpdateServiceNetwork where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceNetworkResponse' smart constructor.
data UpdateServiceNetworkResponse = UpdateServiceNetworkResponse'
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
-- Create a value of 'UpdateServiceNetworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateServiceNetworkResponse_arn' - The Amazon Resource Name (ARN) of the service network.
--
-- 'authType', 'updateServiceNetworkResponse_authType' - The type of IAM policy.
--
-- 'id', 'updateServiceNetworkResponse_id' - The ID of the service network.
--
-- 'name', 'updateServiceNetworkResponse_name' - The name of the service network.
--
-- 'httpStatus', 'updateServiceNetworkResponse_httpStatus' - The response's http status code.
newUpdateServiceNetworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServiceNetworkResponse
newUpdateServiceNetworkResponse pHttpStatus_ =
  UpdateServiceNetworkResponse'
    { arn =
        Prelude.Nothing,
      authType = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the service network.
updateServiceNetworkResponse_arn :: Lens.Lens' UpdateServiceNetworkResponse (Prelude.Maybe Prelude.Text)
updateServiceNetworkResponse_arn = Lens.lens (\UpdateServiceNetworkResponse' {arn} -> arn) (\s@UpdateServiceNetworkResponse' {} a -> s {arn = a} :: UpdateServiceNetworkResponse)

-- | The type of IAM policy.
updateServiceNetworkResponse_authType :: Lens.Lens' UpdateServiceNetworkResponse (Prelude.Maybe AuthType)
updateServiceNetworkResponse_authType = Lens.lens (\UpdateServiceNetworkResponse' {authType} -> authType) (\s@UpdateServiceNetworkResponse' {} a -> s {authType = a} :: UpdateServiceNetworkResponse)

-- | The ID of the service network.
updateServiceNetworkResponse_id :: Lens.Lens' UpdateServiceNetworkResponse (Prelude.Maybe Prelude.Text)
updateServiceNetworkResponse_id = Lens.lens (\UpdateServiceNetworkResponse' {id} -> id) (\s@UpdateServiceNetworkResponse' {} a -> s {id = a} :: UpdateServiceNetworkResponse)

-- | The name of the service network.
updateServiceNetworkResponse_name :: Lens.Lens' UpdateServiceNetworkResponse (Prelude.Maybe Prelude.Text)
updateServiceNetworkResponse_name = Lens.lens (\UpdateServiceNetworkResponse' {name} -> name) (\s@UpdateServiceNetworkResponse' {} a -> s {name = a} :: UpdateServiceNetworkResponse)

-- | The response's http status code.
updateServiceNetworkResponse_httpStatus :: Lens.Lens' UpdateServiceNetworkResponse Prelude.Int
updateServiceNetworkResponse_httpStatus = Lens.lens (\UpdateServiceNetworkResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceNetworkResponse' {} a -> s {httpStatus = a} :: UpdateServiceNetworkResponse)

instance Prelude.NFData UpdateServiceNetworkResponse where
  rnf UpdateServiceNetworkResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
