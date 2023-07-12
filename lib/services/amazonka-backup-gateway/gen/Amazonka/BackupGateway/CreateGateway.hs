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
-- Module      : Amazonka.BackupGateway.CreateGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a backup gateway. After you create a gateway, you can associate
-- it with a server using the @AssociateGatewayToServer@ operation.
module Amazonka.BackupGateway.CreateGateway
  ( -- * Creating a Request
    CreateGateway (..),
    newCreateGateway,

    -- * Request Lenses
    createGateway_tags,
    createGateway_activationKey,
    createGateway_gatewayDisplayName,
    createGateway_gatewayType,

    -- * Destructuring the Response
    CreateGatewayResponse (..),
    newCreateGatewayResponse,

    -- * Response Lenses
    createGatewayResponse_gatewayArn,
    createGatewayResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGateway' smart constructor.
data CreateGateway = CreateGateway'
  { -- | A list of up to 50 tags to assign to the gateway. Each tag is a
    -- key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | The activation key of the created gateway.
    activationKey :: Prelude.Text,
    -- | The display name of the created gateway.
    gatewayDisplayName :: Prelude.Text,
    -- | The type of created gateway.
    gatewayType :: GatewayType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createGateway_tags' - A list of up to 50 tags to assign to the gateway. Each tag is a
-- key-value pair.
--
-- 'activationKey', 'createGateway_activationKey' - The activation key of the created gateway.
--
-- 'gatewayDisplayName', 'createGateway_gatewayDisplayName' - The display name of the created gateway.
--
-- 'gatewayType', 'createGateway_gatewayType' - The type of created gateway.
newCreateGateway ::
  -- | 'activationKey'
  Prelude.Text ->
  -- | 'gatewayDisplayName'
  Prelude.Text ->
  -- | 'gatewayType'
  GatewayType ->
  CreateGateway
newCreateGateway
  pActivationKey_
  pGatewayDisplayName_
  pGatewayType_ =
    CreateGateway'
      { tags = Prelude.Nothing,
        activationKey = pActivationKey_,
        gatewayDisplayName = pGatewayDisplayName_,
        gatewayType = pGatewayType_
      }

-- | A list of up to 50 tags to assign to the gateway. Each tag is a
-- key-value pair.
createGateway_tags :: Lens.Lens' CreateGateway (Prelude.Maybe [Tag])
createGateway_tags = Lens.lens (\CreateGateway' {tags} -> tags) (\s@CreateGateway' {} a -> s {tags = a} :: CreateGateway) Prelude.. Lens.mapping Lens.coerced

-- | The activation key of the created gateway.
createGateway_activationKey :: Lens.Lens' CreateGateway Prelude.Text
createGateway_activationKey = Lens.lens (\CreateGateway' {activationKey} -> activationKey) (\s@CreateGateway' {} a -> s {activationKey = a} :: CreateGateway)

-- | The display name of the created gateway.
createGateway_gatewayDisplayName :: Lens.Lens' CreateGateway Prelude.Text
createGateway_gatewayDisplayName = Lens.lens (\CreateGateway' {gatewayDisplayName} -> gatewayDisplayName) (\s@CreateGateway' {} a -> s {gatewayDisplayName = a} :: CreateGateway)

-- | The type of created gateway.
createGateway_gatewayType :: Lens.Lens' CreateGateway GatewayType
createGateway_gatewayType = Lens.lens (\CreateGateway' {gatewayType} -> gatewayType) (\s@CreateGateway' {} a -> s {gatewayType = a} :: CreateGateway)

instance Core.AWSRequest CreateGateway where
  type
    AWSResponse CreateGateway =
      CreateGatewayResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGatewayResponse'
            Prelude.<$> (x Data..?> "GatewayArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGateway where
  hashWithSalt _salt CreateGateway' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` activationKey
      `Prelude.hashWithSalt` gatewayDisplayName
      `Prelude.hashWithSalt` gatewayType

instance Prelude.NFData CreateGateway where
  rnf CreateGateway' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf activationKey
      `Prelude.seq` Prelude.rnf gatewayDisplayName
      `Prelude.seq` Prelude.rnf gatewayType

instance Data.ToHeaders CreateGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.CreateGateway" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGateway where
  toJSON CreateGateway' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ActivationKey" Data..= activationKey),
            Prelude.Just
              ("GatewayDisplayName" Data..= gatewayDisplayName),
            Prelude.Just ("GatewayType" Data..= gatewayType)
          ]
      )

instance Data.ToPath CreateGateway where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGateway where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGatewayResponse' smart constructor.
data CreateGatewayResponse = CreateGatewayResponse'
  { -- | The Amazon Resource Name (ARN) of the gateway you create.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'createGatewayResponse_gatewayArn' - The Amazon Resource Name (ARN) of the gateway you create.
--
-- 'httpStatus', 'createGatewayResponse_httpStatus' - The response's http status code.
newCreateGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGatewayResponse
newCreateGatewayResponse pHttpStatus_ =
  CreateGatewayResponse'
    { gatewayArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the gateway you create.
createGatewayResponse_gatewayArn :: Lens.Lens' CreateGatewayResponse (Prelude.Maybe Prelude.Text)
createGatewayResponse_gatewayArn = Lens.lens (\CreateGatewayResponse' {gatewayArn} -> gatewayArn) (\s@CreateGatewayResponse' {} a -> s {gatewayArn = a} :: CreateGatewayResponse)

-- | The response's http status code.
createGatewayResponse_httpStatus :: Lens.Lens' CreateGatewayResponse Prelude.Int
createGatewayResponse_httpStatus = Lens.lens (\CreateGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateGatewayResponse' {} a -> s {httpStatus = a} :: CreateGatewayResponse)

instance Prelude.NFData CreateGatewayResponse where
  rnf CreateGatewayResponse' {..} =
    Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf httpStatus
