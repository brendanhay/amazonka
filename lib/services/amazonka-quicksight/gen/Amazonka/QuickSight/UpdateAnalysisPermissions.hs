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
-- Module      : Amazonka.QuickSight.UpdateAnalysisPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the read and write permissions for an analysis.
module Amazonka.QuickSight.UpdateAnalysisPermissions
  ( -- * Creating a Request
    UpdateAnalysisPermissions (..),
    newUpdateAnalysisPermissions,

    -- * Request Lenses
    updateAnalysisPermissions_revokePermissions,
    updateAnalysisPermissions_grantPermissions,
    updateAnalysisPermissions_awsAccountId,
    updateAnalysisPermissions_analysisId,

    -- * Destructuring the Response
    UpdateAnalysisPermissionsResponse (..),
    newUpdateAnalysisPermissionsResponse,

    -- * Response Lenses
    updateAnalysisPermissionsResponse_requestId,
    updateAnalysisPermissionsResponse_analysisId,
    updateAnalysisPermissionsResponse_analysisArn,
    updateAnalysisPermissionsResponse_permissions,
    updateAnalysisPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAnalysisPermissions' smart constructor.
data UpdateAnalysisPermissions = UpdateAnalysisPermissions'
  { -- | A structure that describes the permissions to remove and the principal
    -- to remove them from.
    revokePermissions :: Prelude.Maybe [ResourcePermission],
    -- | A structure that describes the permissions to add and the principal to
    -- add them to.
    grantPermissions :: Prelude.Maybe [ResourcePermission],
    -- | The ID of the Amazon Web Services account that contains the analysis
    -- whose permissions you\'re updating. You must be using the Amazon Web
    -- Services account that the analysis is in.
    awsAccountId :: Prelude.Text,
    -- | The ID of the analysis whose permissions you\'re updating. The ID is
    -- part of the analysis URL.
    analysisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnalysisPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revokePermissions', 'updateAnalysisPermissions_revokePermissions' - A structure that describes the permissions to remove and the principal
-- to remove them from.
--
-- 'grantPermissions', 'updateAnalysisPermissions_grantPermissions' - A structure that describes the permissions to add and the principal to
-- add them to.
--
-- 'awsAccountId', 'updateAnalysisPermissions_awsAccountId' - The ID of the Amazon Web Services account that contains the analysis
-- whose permissions you\'re updating. You must be using the Amazon Web
-- Services account that the analysis is in.
--
-- 'analysisId', 'updateAnalysisPermissions_analysisId' - The ID of the analysis whose permissions you\'re updating. The ID is
-- part of the analysis URL.
newUpdateAnalysisPermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'analysisId'
  Prelude.Text ->
  UpdateAnalysisPermissions
newUpdateAnalysisPermissions
  pAwsAccountId_
  pAnalysisId_ =
    UpdateAnalysisPermissions'
      { revokePermissions =
          Prelude.Nothing,
        grantPermissions = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        analysisId = pAnalysisId_
      }

-- | A structure that describes the permissions to remove and the principal
-- to remove them from.
updateAnalysisPermissions_revokePermissions :: Lens.Lens' UpdateAnalysisPermissions (Prelude.Maybe [ResourcePermission])
updateAnalysisPermissions_revokePermissions = Lens.lens (\UpdateAnalysisPermissions' {revokePermissions} -> revokePermissions) (\s@UpdateAnalysisPermissions' {} a -> s {revokePermissions = a} :: UpdateAnalysisPermissions) Prelude.. Lens.mapping Lens.coerced

-- | A structure that describes the permissions to add and the principal to
-- add them to.
updateAnalysisPermissions_grantPermissions :: Lens.Lens' UpdateAnalysisPermissions (Prelude.Maybe [ResourcePermission])
updateAnalysisPermissions_grantPermissions = Lens.lens (\UpdateAnalysisPermissions' {grantPermissions} -> grantPermissions) (\s@UpdateAnalysisPermissions' {} a -> s {grantPermissions = a} :: UpdateAnalysisPermissions) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that contains the analysis
-- whose permissions you\'re updating. You must be using the Amazon Web
-- Services account that the analysis is in.
updateAnalysisPermissions_awsAccountId :: Lens.Lens' UpdateAnalysisPermissions Prelude.Text
updateAnalysisPermissions_awsAccountId = Lens.lens (\UpdateAnalysisPermissions' {awsAccountId} -> awsAccountId) (\s@UpdateAnalysisPermissions' {} a -> s {awsAccountId = a} :: UpdateAnalysisPermissions)

-- | The ID of the analysis whose permissions you\'re updating. The ID is
-- part of the analysis URL.
updateAnalysisPermissions_analysisId :: Lens.Lens' UpdateAnalysisPermissions Prelude.Text
updateAnalysisPermissions_analysisId = Lens.lens (\UpdateAnalysisPermissions' {analysisId} -> analysisId) (\s@UpdateAnalysisPermissions' {} a -> s {analysisId = a} :: UpdateAnalysisPermissions)

instance Core.AWSRequest UpdateAnalysisPermissions where
  type
    AWSResponse UpdateAnalysisPermissions =
      UpdateAnalysisPermissionsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnalysisPermissionsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "AnalysisId")
            Prelude.<*> (x Core..?> "AnalysisArn")
            Prelude.<*> (x Core..?> "Permissions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAnalysisPermissions where
  hashWithSalt _salt UpdateAnalysisPermissions' {..} =
    _salt `Prelude.hashWithSalt` revokePermissions
      `Prelude.hashWithSalt` grantPermissions
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` analysisId

instance Prelude.NFData UpdateAnalysisPermissions where
  rnf UpdateAnalysisPermissions' {..} =
    Prelude.rnf revokePermissions
      `Prelude.seq` Prelude.rnf grantPermissions
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf analysisId

instance Core.ToHeaders UpdateAnalysisPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAnalysisPermissions where
  toJSON UpdateAnalysisPermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RevokePermissions" Core..=)
              Prelude.<$> revokePermissions,
            ("GrantPermissions" Core..=)
              Prelude.<$> grantPermissions
          ]
      )

instance Core.ToPath UpdateAnalysisPermissions where
  toPath UpdateAnalysisPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/analyses/",
        Core.toBS analysisId,
        "/permissions"
      ]

instance Core.ToQuery UpdateAnalysisPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAnalysisPermissionsResponse' smart constructor.
data UpdateAnalysisPermissionsResponse = UpdateAnalysisPermissionsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the analysis that you updated permissions for.
    analysisId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the analysis that you updated.
    analysisArn :: Prelude.Maybe Prelude.Text,
    -- | A structure that describes the principals and the resource-level
    -- permissions on an analysis.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnalysisPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateAnalysisPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'analysisId', 'updateAnalysisPermissionsResponse_analysisId' - The ID of the analysis that you updated permissions for.
--
-- 'analysisArn', 'updateAnalysisPermissionsResponse_analysisArn' - The Amazon Resource Name (ARN) of the analysis that you updated.
--
-- 'permissions', 'updateAnalysisPermissionsResponse_permissions' - A structure that describes the principals and the resource-level
-- permissions on an analysis.
--
-- 'status', 'updateAnalysisPermissionsResponse_status' - The HTTP status of the request.
newUpdateAnalysisPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateAnalysisPermissionsResponse
newUpdateAnalysisPermissionsResponse pStatus_ =
  UpdateAnalysisPermissionsResponse'
    { requestId =
        Prelude.Nothing,
      analysisId = Prelude.Nothing,
      analysisArn = Prelude.Nothing,
      permissions = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
updateAnalysisPermissionsResponse_requestId :: Lens.Lens' UpdateAnalysisPermissionsResponse (Prelude.Maybe Prelude.Text)
updateAnalysisPermissionsResponse_requestId = Lens.lens (\UpdateAnalysisPermissionsResponse' {requestId} -> requestId) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {requestId = a} :: UpdateAnalysisPermissionsResponse)

-- | The ID of the analysis that you updated permissions for.
updateAnalysisPermissionsResponse_analysisId :: Lens.Lens' UpdateAnalysisPermissionsResponse (Prelude.Maybe Prelude.Text)
updateAnalysisPermissionsResponse_analysisId = Lens.lens (\UpdateAnalysisPermissionsResponse' {analysisId} -> analysisId) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {analysisId = a} :: UpdateAnalysisPermissionsResponse)

-- | The Amazon Resource Name (ARN) of the analysis that you updated.
updateAnalysisPermissionsResponse_analysisArn :: Lens.Lens' UpdateAnalysisPermissionsResponse (Prelude.Maybe Prelude.Text)
updateAnalysisPermissionsResponse_analysisArn = Lens.lens (\UpdateAnalysisPermissionsResponse' {analysisArn} -> analysisArn) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {analysisArn = a} :: UpdateAnalysisPermissionsResponse)

-- | A structure that describes the principals and the resource-level
-- permissions on an analysis.
updateAnalysisPermissionsResponse_permissions :: Lens.Lens' UpdateAnalysisPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateAnalysisPermissionsResponse_permissions = Lens.lens (\UpdateAnalysisPermissionsResponse' {permissions} -> permissions) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {permissions = a} :: UpdateAnalysisPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
updateAnalysisPermissionsResponse_status :: Lens.Lens' UpdateAnalysisPermissionsResponse Prelude.Int
updateAnalysisPermissionsResponse_status = Lens.lens (\UpdateAnalysisPermissionsResponse' {status} -> status) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {status = a} :: UpdateAnalysisPermissionsResponse)

instance
  Prelude.NFData
    UpdateAnalysisPermissionsResponse
  where
  rnf UpdateAnalysisPermissionsResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf analysisId
      `Prelude.seq` Prelude.rnf analysisArn
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf status
