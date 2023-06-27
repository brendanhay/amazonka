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
-- Module      : Amazonka.VPCLattice.UpdateService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified service.
module Amazonka.VPCLattice.UpdateService
  ( -- * Creating a Request
    UpdateService (..),
    newUpdateService,

    -- * Request Lenses
    updateService_authType,
    updateService_certificateArn,
    updateService_serviceIdentifier,

    -- * Destructuring the Response
    UpdateServiceResponse (..),
    newUpdateServiceResponse,

    -- * Response Lenses
    updateServiceResponse_arn,
    updateServiceResponse_authType,
    updateServiceResponse_certificateArn,
    updateServiceResponse_customDomainName,
    updateServiceResponse_id,
    updateServiceResponse_name,
    updateServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newUpdateService' smart constructor.
data UpdateService = UpdateService'
  { -- | The type of IAM policy.
    --
    -- -   @NONE@: The resource does not use an IAM policy. This is the
    --     default.
    --
    -- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
    --     auth is enabled and an auth policy is required.
    authType :: Prelude.Maybe AuthType,
    -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authType', 'updateService_authType' - The type of IAM policy.
--
-- -   @NONE@: The resource does not use an IAM policy. This is the
--     default.
--
-- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
--     auth is enabled and an auth policy is required.
--
-- 'certificateArn', 'updateService_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'serviceIdentifier', 'updateService_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newUpdateService ::
  -- | 'serviceIdentifier'
  Prelude.Text ->
  UpdateService
newUpdateService pServiceIdentifier_ =
  UpdateService'
    { authType = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      serviceIdentifier = pServiceIdentifier_
    }

-- | The type of IAM policy.
--
-- -   @NONE@: The resource does not use an IAM policy. This is the
--     default.
--
-- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
--     auth is enabled and an auth policy is required.
updateService_authType :: Lens.Lens' UpdateService (Prelude.Maybe AuthType)
updateService_authType = Lens.lens (\UpdateService' {authType} -> authType) (\s@UpdateService' {} a -> s {authType = a} :: UpdateService)

-- | The Amazon Resource Name (ARN) of the certificate.
updateService_certificateArn :: Lens.Lens' UpdateService (Prelude.Maybe Prelude.Text)
updateService_certificateArn = Lens.lens (\UpdateService' {certificateArn} -> certificateArn) (\s@UpdateService' {} a -> s {certificateArn = a} :: UpdateService)

-- | The ID or Amazon Resource Name (ARN) of the service.
updateService_serviceIdentifier :: Lens.Lens' UpdateService Prelude.Text
updateService_serviceIdentifier = Lens.lens (\UpdateService' {serviceIdentifier} -> serviceIdentifier) (\s@UpdateService' {} a -> s {serviceIdentifier = a} :: UpdateService)

instance Core.AWSRequest UpdateService where
  type
    AWSResponse UpdateService =
      UpdateServiceResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authType")
            Prelude.<*> (x Data..?> "certificateArn")
            Prelude.<*> (x Data..?> "customDomainName")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateService where
  hashWithSalt _salt UpdateService' {..} =
    _salt
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData UpdateService where
  rnf UpdateService' {..} =
    Prelude.rnf authType
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders UpdateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateService where
  toJSON UpdateService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authType" Data..=) Prelude.<$> authType,
            ("certificateArn" Data..=)
              Prelude.<$> certificateArn
          ]
      )

instance Data.ToPath UpdateService where
  toPath UpdateService' {..} =
    Prelude.mconcat
      ["/services/", Data.toBS serviceIdentifier]

instance Data.ToQuery UpdateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceResponse' smart constructor.
data UpdateServiceResponse = UpdateServiceResponse'
  { -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of IAM policy.
    authType :: Prelude.Maybe AuthType,
    -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name of the service.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateServiceResponse_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'authType', 'updateServiceResponse_authType' - The type of IAM policy.
--
-- 'certificateArn', 'updateServiceResponse_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'customDomainName', 'updateServiceResponse_customDomainName' - The custom domain name of the service.
--
-- 'id', 'updateServiceResponse_id' - The ID of the service.
--
-- 'name', 'updateServiceResponse_name' - The name of the service.
--
-- 'httpStatus', 'updateServiceResponse_httpStatus' - The response's http status code.
newUpdateServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateServiceResponse
newUpdateServiceResponse pHttpStatus_ =
  UpdateServiceResponse'
    { arn = Prelude.Nothing,
      authType = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the service.
updateServiceResponse_arn :: Lens.Lens' UpdateServiceResponse (Prelude.Maybe Prelude.Text)
updateServiceResponse_arn = Lens.lens (\UpdateServiceResponse' {arn} -> arn) (\s@UpdateServiceResponse' {} a -> s {arn = a} :: UpdateServiceResponse)

-- | The type of IAM policy.
updateServiceResponse_authType :: Lens.Lens' UpdateServiceResponse (Prelude.Maybe AuthType)
updateServiceResponse_authType = Lens.lens (\UpdateServiceResponse' {authType} -> authType) (\s@UpdateServiceResponse' {} a -> s {authType = a} :: UpdateServiceResponse)

-- | The Amazon Resource Name (ARN) of the certificate.
updateServiceResponse_certificateArn :: Lens.Lens' UpdateServiceResponse (Prelude.Maybe Prelude.Text)
updateServiceResponse_certificateArn = Lens.lens (\UpdateServiceResponse' {certificateArn} -> certificateArn) (\s@UpdateServiceResponse' {} a -> s {certificateArn = a} :: UpdateServiceResponse)

-- | The custom domain name of the service.
updateServiceResponse_customDomainName :: Lens.Lens' UpdateServiceResponse (Prelude.Maybe Prelude.Text)
updateServiceResponse_customDomainName = Lens.lens (\UpdateServiceResponse' {customDomainName} -> customDomainName) (\s@UpdateServiceResponse' {} a -> s {customDomainName = a} :: UpdateServiceResponse)

-- | The ID of the service.
updateServiceResponse_id :: Lens.Lens' UpdateServiceResponse (Prelude.Maybe Prelude.Text)
updateServiceResponse_id = Lens.lens (\UpdateServiceResponse' {id} -> id) (\s@UpdateServiceResponse' {} a -> s {id = a} :: UpdateServiceResponse)

-- | The name of the service.
updateServiceResponse_name :: Lens.Lens' UpdateServiceResponse (Prelude.Maybe Prelude.Text)
updateServiceResponse_name = Lens.lens (\UpdateServiceResponse' {name} -> name) (\s@UpdateServiceResponse' {} a -> s {name = a} :: UpdateServiceResponse)

-- | The response's http status code.
updateServiceResponse_httpStatus :: Lens.Lens' UpdateServiceResponse Prelude.Int
updateServiceResponse_httpStatus = Lens.lens (\UpdateServiceResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceResponse' {} a -> s {httpStatus = a} :: UpdateServiceResponse)

instance Prelude.NFData UpdateServiceResponse where
  rnf UpdateServiceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
