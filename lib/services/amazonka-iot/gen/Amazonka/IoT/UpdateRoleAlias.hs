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
-- Module      : Amazonka.IoT.UpdateRoleAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a role alias.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateRoleAlias>
-- action.
module Amazonka.IoT.UpdateRoleAlias
  ( -- * Creating a Request
    UpdateRoleAlias (..),
    newUpdateRoleAlias,

    -- * Request Lenses
    updateRoleAlias_roleArn,
    updateRoleAlias_credentialDurationSeconds,
    updateRoleAlias_roleAlias,

    -- * Destructuring the Response
    UpdateRoleAliasResponse (..),
    newUpdateRoleAliasResponse,

    -- * Response Lenses
    updateRoleAliasResponse_roleAlias,
    updateRoleAliasResponse_roleAliasArn,
    updateRoleAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRoleAlias' smart constructor.
data UpdateRoleAlias = UpdateRoleAlias'
  { -- | The role ARN.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds the credential will be valid.
    --
    -- This value must be less than or equal to the maximum session duration of
    -- the IAM role that the role alias references.
    credentialDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The role alias to update.
    roleAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoleAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateRoleAlias_roleArn' - The role ARN.
--
-- 'credentialDurationSeconds', 'updateRoleAlias_credentialDurationSeconds' - The number of seconds the credential will be valid.
--
-- This value must be less than or equal to the maximum session duration of
-- the IAM role that the role alias references.
--
-- 'roleAlias', 'updateRoleAlias_roleAlias' - The role alias to update.
newUpdateRoleAlias ::
  -- | 'roleAlias'
  Prelude.Text ->
  UpdateRoleAlias
newUpdateRoleAlias pRoleAlias_ =
  UpdateRoleAlias'
    { roleArn = Prelude.Nothing,
      credentialDurationSeconds = Prelude.Nothing,
      roleAlias = pRoleAlias_
    }

-- | The role ARN.
updateRoleAlias_roleArn :: Lens.Lens' UpdateRoleAlias (Prelude.Maybe Prelude.Text)
updateRoleAlias_roleArn = Lens.lens (\UpdateRoleAlias' {roleArn} -> roleArn) (\s@UpdateRoleAlias' {} a -> s {roleArn = a} :: UpdateRoleAlias)

-- | The number of seconds the credential will be valid.
--
-- This value must be less than or equal to the maximum session duration of
-- the IAM role that the role alias references.
updateRoleAlias_credentialDurationSeconds :: Lens.Lens' UpdateRoleAlias (Prelude.Maybe Prelude.Natural)
updateRoleAlias_credentialDurationSeconds = Lens.lens (\UpdateRoleAlias' {credentialDurationSeconds} -> credentialDurationSeconds) (\s@UpdateRoleAlias' {} a -> s {credentialDurationSeconds = a} :: UpdateRoleAlias)

-- | The role alias to update.
updateRoleAlias_roleAlias :: Lens.Lens' UpdateRoleAlias Prelude.Text
updateRoleAlias_roleAlias = Lens.lens (\UpdateRoleAlias' {roleAlias} -> roleAlias) (\s@UpdateRoleAlias' {} a -> s {roleAlias = a} :: UpdateRoleAlias)

instance Core.AWSRequest UpdateRoleAlias where
  type
    AWSResponse UpdateRoleAlias =
      UpdateRoleAliasResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRoleAliasResponse'
            Prelude.<$> (x Data..?> "roleAlias")
            Prelude.<*> (x Data..?> "roleAliasArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoleAlias where
  hashWithSalt _salt UpdateRoleAlias' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` credentialDurationSeconds
      `Prelude.hashWithSalt` roleAlias

instance Prelude.NFData UpdateRoleAlias where
  rnf UpdateRoleAlias' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf credentialDurationSeconds
      `Prelude.seq` Prelude.rnf roleAlias

instance Data.ToHeaders UpdateRoleAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateRoleAlias where
  toJSON UpdateRoleAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("roleArn" Data..=) Prelude.<$> roleArn,
            ("credentialDurationSeconds" Data..=)
              Prelude.<$> credentialDurationSeconds
          ]
      )

instance Data.ToPath UpdateRoleAlias where
  toPath UpdateRoleAlias' {..} =
    Prelude.mconcat
      ["/role-aliases/", Data.toBS roleAlias]

instance Data.ToQuery UpdateRoleAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoleAliasResponse' smart constructor.
data UpdateRoleAliasResponse = UpdateRoleAliasResponse'
  { -- | The role alias.
    roleAlias :: Prelude.Maybe Prelude.Text,
    -- | The role alias ARN.
    roleAliasArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoleAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleAlias', 'updateRoleAliasResponse_roleAlias' - The role alias.
--
-- 'roleAliasArn', 'updateRoleAliasResponse_roleAliasArn' - The role alias ARN.
--
-- 'httpStatus', 'updateRoleAliasResponse_httpStatus' - The response's http status code.
newUpdateRoleAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoleAliasResponse
newUpdateRoleAliasResponse pHttpStatus_ =
  UpdateRoleAliasResponse'
    { roleAlias =
        Prelude.Nothing,
      roleAliasArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The role alias.
updateRoleAliasResponse_roleAlias :: Lens.Lens' UpdateRoleAliasResponse (Prelude.Maybe Prelude.Text)
updateRoleAliasResponse_roleAlias = Lens.lens (\UpdateRoleAliasResponse' {roleAlias} -> roleAlias) (\s@UpdateRoleAliasResponse' {} a -> s {roleAlias = a} :: UpdateRoleAliasResponse)

-- | The role alias ARN.
updateRoleAliasResponse_roleAliasArn :: Lens.Lens' UpdateRoleAliasResponse (Prelude.Maybe Prelude.Text)
updateRoleAliasResponse_roleAliasArn = Lens.lens (\UpdateRoleAliasResponse' {roleAliasArn} -> roleAliasArn) (\s@UpdateRoleAliasResponse' {} a -> s {roleAliasArn = a} :: UpdateRoleAliasResponse)

-- | The response's http status code.
updateRoleAliasResponse_httpStatus :: Lens.Lens' UpdateRoleAliasResponse Prelude.Int
updateRoleAliasResponse_httpStatus = Lens.lens (\UpdateRoleAliasResponse' {httpStatus} -> httpStatus) (\s@UpdateRoleAliasResponse' {} a -> s {httpStatus = a} :: UpdateRoleAliasResponse)

instance Prelude.NFData UpdateRoleAliasResponse where
  rnf UpdateRoleAliasResponse' {..} =
    Prelude.rnf roleAlias
      `Prelude.seq` Prelude.rnf roleAliasArn
      `Prelude.seq` Prelude.rnf httpStatus
