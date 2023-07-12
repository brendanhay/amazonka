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
-- Module      : Amazonka.Grafana.DisassociateLicense
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the Grafana Enterprise license from a workspace.
module Amazonka.Grafana.DisassociateLicense
  ( -- * Creating a Request
    DisassociateLicense (..),
    newDisassociateLicense,

    -- * Request Lenses
    disassociateLicense_licenseType,
    disassociateLicense_workspaceId,

    -- * Destructuring the Response
    DisassociateLicenseResponse (..),
    newDisassociateLicenseResponse,

    -- * Response Lenses
    disassociateLicenseResponse_httpStatus,
    disassociateLicenseResponse_workspace,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateLicense' smart constructor.
data DisassociateLicense = DisassociateLicense'
  { -- | The type of license to remove from the workspace.
    licenseType :: LicenseType,
    -- | The ID of the workspace to remove the Grafana Enterprise license from.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLicense' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseType', 'disassociateLicense_licenseType' - The type of license to remove from the workspace.
--
-- 'workspaceId', 'disassociateLicense_workspaceId' - The ID of the workspace to remove the Grafana Enterprise license from.
newDisassociateLicense ::
  -- | 'licenseType'
  LicenseType ->
  -- | 'workspaceId'
  Prelude.Text ->
  DisassociateLicense
newDisassociateLicense pLicenseType_ pWorkspaceId_ =
  DisassociateLicense'
    { licenseType = pLicenseType_,
      workspaceId = pWorkspaceId_
    }

-- | The type of license to remove from the workspace.
disassociateLicense_licenseType :: Lens.Lens' DisassociateLicense LicenseType
disassociateLicense_licenseType = Lens.lens (\DisassociateLicense' {licenseType} -> licenseType) (\s@DisassociateLicense' {} a -> s {licenseType = a} :: DisassociateLicense)

-- | The ID of the workspace to remove the Grafana Enterprise license from.
disassociateLicense_workspaceId :: Lens.Lens' DisassociateLicense Prelude.Text
disassociateLicense_workspaceId = Lens.lens (\DisassociateLicense' {workspaceId} -> workspaceId) (\s@DisassociateLicense' {} a -> s {workspaceId = a} :: DisassociateLicense)

instance Core.AWSRequest DisassociateLicense where
  type
    AWSResponse DisassociateLicense =
      DisassociateLicenseResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateLicenseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workspace")
      )

instance Prelude.Hashable DisassociateLicense where
  hashWithSalt _salt DisassociateLicense' {..} =
    _salt
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData DisassociateLicense where
  rnf DisassociateLicense' {..} =
    Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders DisassociateLicense where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateLicense where
  toPath DisassociateLicense' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/licenses/",
        Data.toBS licenseType
      ]

instance Data.ToQuery DisassociateLicense where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateLicenseResponse' smart constructor.
data DisassociateLicenseResponse = DisassociateLicenseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing information about the workspace.
    workspace :: WorkspaceDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateLicenseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateLicenseResponse_httpStatus' - The response's http status code.
--
-- 'workspace', 'disassociateLicenseResponse_workspace' - A structure containing information about the workspace.
newDisassociateLicenseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workspace'
  WorkspaceDescription ->
  DisassociateLicenseResponse
newDisassociateLicenseResponse
  pHttpStatus_
  pWorkspace_ =
    DisassociateLicenseResponse'
      { httpStatus =
          pHttpStatus_,
        workspace = pWorkspace_
      }

-- | The response's http status code.
disassociateLicenseResponse_httpStatus :: Lens.Lens' DisassociateLicenseResponse Prelude.Int
disassociateLicenseResponse_httpStatus = Lens.lens (\DisassociateLicenseResponse' {httpStatus} -> httpStatus) (\s@DisassociateLicenseResponse' {} a -> s {httpStatus = a} :: DisassociateLicenseResponse)

-- | A structure containing information about the workspace.
disassociateLicenseResponse_workspace :: Lens.Lens' DisassociateLicenseResponse WorkspaceDescription
disassociateLicenseResponse_workspace = Lens.lens (\DisassociateLicenseResponse' {workspace} -> workspace) (\s@DisassociateLicenseResponse' {} a -> s {workspace = a} :: DisassociateLicenseResponse)

instance Prelude.NFData DisassociateLicenseResponse where
  rnf DisassociateLicenseResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workspace
