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
-- Module      : Network.AWS.IoT.UpdateRoleAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a role alias.
module Network.AWS.IoT.UpdateRoleAlias
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
    updateRoleAliasResponse_roleAliasArn,
    updateRoleAliasResponse_roleAlias,
    updateRoleAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRoleAlias' smart constructor.
data UpdateRoleAlias = UpdateRoleAlias'
  { -- | The role ARN.
    roleArn :: Core.Maybe Core.Text,
    -- | The number of seconds the credential will be valid.
    credentialDurationSeconds :: Core.Maybe Core.Natural,
    -- | The role alias to update.
    roleAlias :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'roleAlias', 'updateRoleAlias_roleAlias' - The role alias to update.
newUpdateRoleAlias ::
  -- | 'roleAlias'
  Core.Text ->
  UpdateRoleAlias
newUpdateRoleAlias pRoleAlias_ =
  UpdateRoleAlias'
    { roleArn = Core.Nothing,
      credentialDurationSeconds = Core.Nothing,
      roleAlias = pRoleAlias_
    }

-- | The role ARN.
updateRoleAlias_roleArn :: Lens.Lens' UpdateRoleAlias (Core.Maybe Core.Text)
updateRoleAlias_roleArn = Lens.lens (\UpdateRoleAlias' {roleArn} -> roleArn) (\s@UpdateRoleAlias' {} a -> s {roleArn = a} :: UpdateRoleAlias)

-- | The number of seconds the credential will be valid.
updateRoleAlias_credentialDurationSeconds :: Lens.Lens' UpdateRoleAlias (Core.Maybe Core.Natural)
updateRoleAlias_credentialDurationSeconds = Lens.lens (\UpdateRoleAlias' {credentialDurationSeconds} -> credentialDurationSeconds) (\s@UpdateRoleAlias' {} a -> s {credentialDurationSeconds = a} :: UpdateRoleAlias)

-- | The role alias to update.
updateRoleAlias_roleAlias :: Lens.Lens' UpdateRoleAlias Core.Text
updateRoleAlias_roleAlias = Lens.lens (\UpdateRoleAlias' {roleAlias} -> roleAlias) (\s@UpdateRoleAlias' {} a -> s {roleAlias = a} :: UpdateRoleAlias)

instance Core.AWSRequest UpdateRoleAlias where
  type
    AWSResponse UpdateRoleAlias =
      UpdateRoleAliasResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRoleAliasResponse'
            Core.<$> (x Core..?> "roleAliasArn")
            Core.<*> (x Core..?> "roleAlias")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateRoleAlias

instance Core.NFData UpdateRoleAlias

instance Core.ToHeaders UpdateRoleAlias where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateRoleAlias where
  toJSON UpdateRoleAlias' {..} =
    Core.object
      ( Core.catMaybes
          [ ("roleArn" Core..=) Core.<$> roleArn,
            ("credentialDurationSeconds" Core..=)
              Core.<$> credentialDurationSeconds
          ]
      )

instance Core.ToPath UpdateRoleAlias where
  toPath UpdateRoleAlias' {..} =
    Core.mconcat
      ["/role-aliases/", Core.toBS roleAlias]

instance Core.ToQuery UpdateRoleAlias where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateRoleAliasResponse' smart constructor.
data UpdateRoleAliasResponse = UpdateRoleAliasResponse'
  { -- | The role alias ARN.
    roleAliasArn :: Core.Maybe Core.Text,
    -- | The role alias.
    roleAlias :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRoleAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleAliasArn', 'updateRoleAliasResponse_roleAliasArn' - The role alias ARN.
--
-- 'roleAlias', 'updateRoleAliasResponse_roleAlias' - The role alias.
--
-- 'httpStatus', 'updateRoleAliasResponse_httpStatus' - The response's http status code.
newUpdateRoleAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateRoleAliasResponse
newUpdateRoleAliasResponse pHttpStatus_ =
  UpdateRoleAliasResponse'
    { roleAliasArn =
        Core.Nothing,
      roleAlias = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The role alias ARN.
updateRoleAliasResponse_roleAliasArn :: Lens.Lens' UpdateRoleAliasResponse (Core.Maybe Core.Text)
updateRoleAliasResponse_roleAliasArn = Lens.lens (\UpdateRoleAliasResponse' {roleAliasArn} -> roleAliasArn) (\s@UpdateRoleAliasResponse' {} a -> s {roleAliasArn = a} :: UpdateRoleAliasResponse)

-- | The role alias.
updateRoleAliasResponse_roleAlias :: Lens.Lens' UpdateRoleAliasResponse (Core.Maybe Core.Text)
updateRoleAliasResponse_roleAlias = Lens.lens (\UpdateRoleAliasResponse' {roleAlias} -> roleAlias) (\s@UpdateRoleAliasResponse' {} a -> s {roleAlias = a} :: UpdateRoleAliasResponse)

-- | The response's http status code.
updateRoleAliasResponse_httpStatus :: Lens.Lens' UpdateRoleAliasResponse Core.Int
updateRoleAliasResponse_httpStatus = Lens.lens (\UpdateRoleAliasResponse' {httpStatus} -> httpStatus) (\s@UpdateRoleAliasResponse' {} a -> s {httpStatus = a} :: UpdateRoleAliasResponse)

instance Core.NFData UpdateRoleAliasResponse
