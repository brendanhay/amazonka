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
-- Module      : Amazonka.Grafana.AssociateLicense
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns a Grafana Enterprise license to a workspace. Upgrading to
-- Grafana Enterprise incurs additional fees. For more information, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/upgrade-to-Grafana-Enterprise.html Upgrade a workspace to Grafana Enterprise>.
module Amazonka.Grafana.AssociateLicense
  ( -- * Creating a Request
    AssociateLicense (..),
    newAssociateLicense,

    -- * Request Lenses
    associateLicense_licenseType,
    associateLicense_workspaceId,

    -- * Destructuring the Response
    AssociateLicenseResponse (..),
    newAssociateLicenseResponse,

    -- * Response Lenses
    associateLicenseResponse_httpStatus,
    associateLicenseResponse_workspace,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateLicense' smart constructor.
data AssociateLicense = AssociateLicense'
  { -- | The type of license to associate with the workspace.
    licenseType :: LicenseType,
    -- | The ID of the workspace to associate the license with.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateLicense' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseType', 'associateLicense_licenseType' - The type of license to associate with the workspace.
--
-- 'workspaceId', 'associateLicense_workspaceId' - The ID of the workspace to associate the license with.
newAssociateLicense ::
  -- | 'licenseType'
  LicenseType ->
  -- | 'workspaceId'
  Prelude.Text ->
  AssociateLicense
newAssociateLicense pLicenseType_ pWorkspaceId_ =
  AssociateLicense'
    { licenseType = pLicenseType_,
      workspaceId = pWorkspaceId_
    }

-- | The type of license to associate with the workspace.
associateLicense_licenseType :: Lens.Lens' AssociateLicense LicenseType
associateLicense_licenseType = Lens.lens (\AssociateLicense' {licenseType} -> licenseType) (\s@AssociateLicense' {} a -> s {licenseType = a} :: AssociateLicense)

-- | The ID of the workspace to associate the license with.
associateLicense_workspaceId :: Lens.Lens' AssociateLicense Prelude.Text
associateLicense_workspaceId = Lens.lens (\AssociateLicense' {workspaceId} -> workspaceId) (\s@AssociateLicense' {} a -> s {workspaceId = a} :: AssociateLicense)

instance Core.AWSRequest AssociateLicense where
  type
    AWSResponse AssociateLicense =
      AssociateLicenseResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateLicenseResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workspace")
      )

instance Prelude.Hashable AssociateLicense where
  hashWithSalt _salt AssociateLicense' {..} =
    _salt
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData AssociateLicense where
  rnf AssociateLicense' {..} =
    Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders AssociateLicense where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateLicense where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AssociateLicense where
  toPath AssociateLicense' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/licenses/",
        Data.toBS licenseType
      ]

instance Data.ToQuery AssociateLicense where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateLicenseResponse' smart constructor.
data AssociateLicenseResponse = AssociateLicenseResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing data about the workspace.
    workspace :: WorkspaceDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateLicenseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateLicenseResponse_httpStatus' - The response's http status code.
--
-- 'workspace', 'associateLicenseResponse_workspace' - A structure containing data about the workspace.
newAssociateLicenseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workspace'
  WorkspaceDescription ->
  AssociateLicenseResponse
newAssociateLicenseResponse pHttpStatus_ pWorkspace_ =
  AssociateLicenseResponse'
    { httpStatus =
        pHttpStatus_,
      workspace = pWorkspace_
    }

-- | The response's http status code.
associateLicenseResponse_httpStatus :: Lens.Lens' AssociateLicenseResponse Prelude.Int
associateLicenseResponse_httpStatus = Lens.lens (\AssociateLicenseResponse' {httpStatus} -> httpStatus) (\s@AssociateLicenseResponse' {} a -> s {httpStatus = a} :: AssociateLicenseResponse)

-- | A structure containing data about the workspace.
associateLicenseResponse_workspace :: Lens.Lens' AssociateLicenseResponse WorkspaceDescription
associateLicenseResponse_workspace = Lens.lens (\AssociateLicenseResponse' {workspace} -> workspace) (\s@AssociateLicenseResponse' {} a -> s {workspace = a} :: AssociateLicenseResponse)

instance Prelude.NFData AssociateLicenseResponse where
  rnf AssociateLicenseResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workspace
