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
-- Module      : Amazonka.WorkSpaces.ModifySelfservicePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the self-service WorkSpace management capabilities for your
-- users. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/enable-user-self-service-workspace-management.html Enable Self-Service WorkSpace Management Capabilities for Your Users>.
module Amazonka.WorkSpaces.ModifySelfservicePermissions
  ( -- * Creating a Request
    ModifySelfservicePermissions (..),
    newModifySelfservicePermissions,

    -- * Request Lenses
    modifySelfservicePermissions_resourceId,
    modifySelfservicePermissions_selfservicePermissions,

    -- * Destructuring the Response
    ModifySelfservicePermissionsResponse (..),
    newModifySelfservicePermissionsResponse,

    -- * Response Lenses
    modifySelfservicePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newModifySelfservicePermissions' smart constructor.
data ModifySelfservicePermissions = ModifySelfservicePermissions'
  { -- | The identifier of the directory.
    resourceId :: Prelude.Text,
    -- | The permissions to enable or disable self-service capabilities.
    selfservicePermissions :: SelfservicePermissions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySelfservicePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'modifySelfservicePermissions_resourceId' - The identifier of the directory.
--
-- 'selfservicePermissions', 'modifySelfservicePermissions_selfservicePermissions' - The permissions to enable or disable self-service capabilities.
newModifySelfservicePermissions ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'selfservicePermissions'
  SelfservicePermissions ->
  ModifySelfservicePermissions
newModifySelfservicePermissions
  pResourceId_
  pSelfservicePermissions_ =
    ModifySelfservicePermissions'
      { resourceId =
          pResourceId_,
        selfservicePermissions =
          pSelfservicePermissions_
      }

-- | The identifier of the directory.
modifySelfservicePermissions_resourceId :: Lens.Lens' ModifySelfservicePermissions Prelude.Text
modifySelfservicePermissions_resourceId = Lens.lens (\ModifySelfservicePermissions' {resourceId} -> resourceId) (\s@ModifySelfservicePermissions' {} a -> s {resourceId = a} :: ModifySelfservicePermissions)

-- | The permissions to enable or disable self-service capabilities.
modifySelfservicePermissions_selfservicePermissions :: Lens.Lens' ModifySelfservicePermissions SelfservicePermissions
modifySelfservicePermissions_selfservicePermissions = Lens.lens (\ModifySelfservicePermissions' {selfservicePermissions} -> selfservicePermissions) (\s@ModifySelfservicePermissions' {} a -> s {selfservicePermissions = a} :: ModifySelfservicePermissions)

instance Core.AWSRequest ModifySelfservicePermissions where
  type
    AWSResponse ModifySelfservicePermissions =
      ModifySelfservicePermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifySelfservicePermissionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifySelfservicePermissions
  where
  hashWithSalt _salt ModifySelfservicePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` selfservicePermissions

instance Prelude.NFData ModifySelfservicePermissions where
  rnf ModifySelfservicePermissions' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf selfservicePermissions

instance Data.ToHeaders ModifySelfservicePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.ModifySelfservicePermissions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifySelfservicePermissions where
  toJSON ModifySelfservicePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just
              ( "SelfservicePermissions"
                  Data..= selfservicePermissions
              )
          ]
      )

instance Data.ToPath ModifySelfservicePermissions where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifySelfservicePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifySelfservicePermissionsResponse' smart constructor.
data ModifySelfservicePermissionsResponse = ModifySelfservicePermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySelfservicePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifySelfservicePermissionsResponse_httpStatus' - The response's http status code.
newModifySelfservicePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifySelfservicePermissionsResponse
newModifySelfservicePermissionsResponse pHttpStatus_ =
  ModifySelfservicePermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifySelfservicePermissionsResponse_httpStatus :: Lens.Lens' ModifySelfservicePermissionsResponse Prelude.Int
modifySelfservicePermissionsResponse_httpStatus = Lens.lens (\ModifySelfservicePermissionsResponse' {httpStatus} -> httpStatus) (\s@ModifySelfservicePermissionsResponse' {} a -> s {httpStatus = a} :: ModifySelfservicePermissionsResponse)

instance
  Prelude.NFData
    ModifySelfservicePermissionsResponse
  where
  rnf ModifySelfservicePermissionsResponse' {..} =
    Prelude.rnf httpStatus
