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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    updateAnalysisPermissions_grantPermissions,
    updateAnalysisPermissions_revokePermissions,
    updateAnalysisPermissions_awsAccountId,
    updateAnalysisPermissions_analysisId,

    -- * Destructuring the Response
    UpdateAnalysisPermissionsResponse (..),
    newUpdateAnalysisPermissionsResponse,

    -- * Response Lenses
    updateAnalysisPermissionsResponse_analysisArn,
    updateAnalysisPermissionsResponse_analysisId,
    updateAnalysisPermissionsResponse_permissions,
    updateAnalysisPermissionsResponse_requestId,
    updateAnalysisPermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAnalysisPermissions' smart constructor.
data UpdateAnalysisPermissions = UpdateAnalysisPermissions'
  { -- | A structure that describes the permissions to add and the principal to
    -- add them to.
    grantPermissions :: Prelude.Maybe [ResourcePermission],
    -- | A structure that describes the permissions to remove and the principal
    -- to remove them from.
    revokePermissions :: Prelude.Maybe [ResourcePermission],
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
-- 'grantPermissions', 'updateAnalysisPermissions_grantPermissions' - A structure that describes the permissions to add and the principal to
-- add them to.
--
-- 'revokePermissions', 'updateAnalysisPermissions_revokePermissions' - A structure that describes the permissions to remove and the principal
-- to remove them from.
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
      { grantPermissions =
          Prelude.Nothing,
        revokePermissions = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        analysisId = pAnalysisId_
      }

-- | A structure that describes the permissions to add and the principal to
-- add them to.
updateAnalysisPermissions_grantPermissions :: Lens.Lens' UpdateAnalysisPermissions (Prelude.Maybe [ResourcePermission])
updateAnalysisPermissions_grantPermissions = Lens.lens (\UpdateAnalysisPermissions' {grantPermissions} -> grantPermissions) (\s@UpdateAnalysisPermissions' {} a -> s {grantPermissions = a} :: UpdateAnalysisPermissions) Prelude.. Lens.mapping Lens.coerced

-- | A structure that describes the permissions to remove and the principal
-- to remove them from.
updateAnalysisPermissions_revokePermissions :: Lens.Lens' UpdateAnalysisPermissions (Prelude.Maybe [ResourcePermission])
updateAnalysisPermissions_revokePermissions = Lens.lens (\UpdateAnalysisPermissions' {revokePermissions} -> revokePermissions) (\s@UpdateAnalysisPermissions' {} a -> s {revokePermissions = a} :: UpdateAnalysisPermissions) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnalysisPermissionsResponse'
            Prelude.<$> (x Data..?> "AnalysisArn")
            Prelude.<*> (x Data..?> "AnalysisId")
            Prelude.<*> (x Data..?> "Permissions")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAnalysisPermissions where
  hashWithSalt _salt UpdateAnalysisPermissions' {..} =
    _salt `Prelude.hashWithSalt` grantPermissions
      `Prelude.hashWithSalt` revokePermissions
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` analysisId

instance Prelude.NFData UpdateAnalysisPermissions where
  rnf UpdateAnalysisPermissions' {..} =
    Prelude.rnf grantPermissions
      `Prelude.seq` Prelude.rnf revokePermissions
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf analysisId

instance Data.ToHeaders UpdateAnalysisPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAnalysisPermissions where
  toJSON UpdateAnalysisPermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantPermissions" Data..=)
              Prelude.<$> grantPermissions,
            ("RevokePermissions" Data..=)
              Prelude.<$> revokePermissions
          ]
      )

instance Data.ToPath UpdateAnalysisPermissions where
  toPath UpdateAnalysisPermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/analyses/",
        Data.toBS analysisId,
        "/permissions"
      ]

instance Data.ToQuery UpdateAnalysisPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAnalysisPermissionsResponse' smart constructor.
data UpdateAnalysisPermissionsResponse = UpdateAnalysisPermissionsResponse'
  { -- | The Amazon Resource Name (ARN) of the analysis that you updated.
    analysisArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the analysis that you updated permissions for.
    analysisId :: Prelude.Maybe Prelude.Text,
    -- | A structure that describes the principals and the resource-level
    -- permissions on an analysis.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
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
-- 'analysisArn', 'updateAnalysisPermissionsResponse_analysisArn' - The Amazon Resource Name (ARN) of the analysis that you updated.
--
-- 'analysisId', 'updateAnalysisPermissionsResponse_analysisId' - The ID of the analysis that you updated permissions for.
--
-- 'permissions', 'updateAnalysisPermissionsResponse_permissions' - A structure that describes the principals and the resource-level
-- permissions on an analysis.
--
-- 'requestId', 'updateAnalysisPermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'updateAnalysisPermissionsResponse_status' - The HTTP status of the request.
newUpdateAnalysisPermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateAnalysisPermissionsResponse
newUpdateAnalysisPermissionsResponse pStatus_ =
  UpdateAnalysisPermissionsResponse'
    { analysisArn =
        Prelude.Nothing,
      analysisId = Prelude.Nothing,
      permissions = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the analysis that you updated.
updateAnalysisPermissionsResponse_analysisArn :: Lens.Lens' UpdateAnalysisPermissionsResponse (Prelude.Maybe Prelude.Text)
updateAnalysisPermissionsResponse_analysisArn = Lens.lens (\UpdateAnalysisPermissionsResponse' {analysisArn} -> analysisArn) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {analysisArn = a} :: UpdateAnalysisPermissionsResponse)

-- | The ID of the analysis that you updated permissions for.
updateAnalysisPermissionsResponse_analysisId :: Lens.Lens' UpdateAnalysisPermissionsResponse (Prelude.Maybe Prelude.Text)
updateAnalysisPermissionsResponse_analysisId = Lens.lens (\UpdateAnalysisPermissionsResponse' {analysisId} -> analysisId) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {analysisId = a} :: UpdateAnalysisPermissionsResponse)

-- | A structure that describes the principals and the resource-level
-- permissions on an analysis.
updateAnalysisPermissionsResponse_permissions :: Lens.Lens' UpdateAnalysisPermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateAnalysisPermissionsResponse_permissions = Lens.lens (\UpdateAnalysisPermissionsResponse' {permissions} -> permissions) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {permissions = a} :: UpdateAnalysisPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
updateAnalysisPermissionsResponse_requestId :: Lens.Lens' UpdateAnalysisPermissionsResponse (Prelude.Maybe Prelude.Text)
updateAnalysisPermissionsResponse_requestId = Lens.lens (\UpdateAnalysisPermissionsResponse' {requestId} -> requestId) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {requestId = a} :: UpdateAnalysisPermissionsResponse)

-- | The HTTP status of the request.
updateAnalysisPermissionsResponse_status :: Lens.Lens' UpdateAnalysisPermissionsResponse Prelude.Int
updateAnalysisPermissionsResponse_status = Lens.lens (\UpdateAnalysisPermissionsResponse' {status} -> status) (\s@UpdateAnalysisPermissionsResponse' {} a -> s {status = a} :: UpdateAnalysisPermissionsResponse)

instance
  Prelude.NFData
    UpdateAnalysisPermissionsResponse
  where
  rnf UpdateAnalysisPermissionsResponse' {..} =
    Prelude.rnf analysisArn
      `Prelude.seq` Prelude.rnf analysisId
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
