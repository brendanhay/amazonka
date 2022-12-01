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
-- Module      : Amazonka.QuickSight.UpdateTemplatePermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the resource permissions for a template.
module Amazonka.QuickSight.UpdateTemplatePermissions
  ( -- * Creating a Request
    UpdateTemplatePermissions (..),
    newUpdateTemplatePermissions,

    -- * Request Lenses
    updateTemplatePermissions_grantPermissions,
    updateTemplatePermissions_revokePermissions,
    updateTemplatePermissions_awsAccountId,
    updateTemplatePermissions_templateId,

    -- * Destructuring the Response
    UpdateTemplatePermissionsResponse (..),
    newUpdateTemplatePermissionsResponse,

    -- * Response Lenses
    updateTemplatePermissionsResponse_requestId,
    updateTemplatePermissionsResponse_permissions,
    updateTemplatePermissionsResponse_templateId,
    updateTemplatePermissionsResponse_templateArn,
    updateTemplatePermissionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTemplatePermissions' smart constructor.
data UpdateTemplatePermissions = UpdateTemplatePermissions'
  { -- | A list of resource permissions to be granted on the template.
    grantPermissions :: Prelude.Maybe [ResourcePermission],
    -- | A list of resource permissions to be revoked from the template.
    revokePermissions :: Prelude.Maybe [ResourcePermission],
    -- | The ID of the Amazon Web Services account that contains the template.
    awsAccountId :: Prelude.Text,
    -- | The ID for the template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplatePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantPermissions', 'updateTemplatePermissions_grantPermissions' - A list of resource permissions to be granted on the template.
--
-- 'revokePermissions', 'updateTemplatePermissions_revokePermissions' - A list of resource permissions to be revoked from the template.
--
-- 'awsAccountId', 'updateTemplatePermissions_awsAccountId' - The ID of the Amazon Web Services account that contains the template.
--
-- 'templateId', 'updateTemplatePermissions_templateId' - The ID for the template.
newUpdateTemplatePermissions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  UpdateTemplatePermissions
newUpdateTemplatePermissions
  pAwsAccountId_
  pTemplateId_ =
    UpdateTemplatePermissions'
      { grantPermissions =
          Prelude.Nothing,
        revokePermissions = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        templateId = pTemplateId_
      }

-- | A list of resource permissions to be granted on the template.
updateTemplatePermissions_grantPermissions :: Lens.Lens' UpdateTemplatePermissions (Prelude.Maybe [ResourcePermission])
updateTemplatePermissions_grantPermissions = Lens.lens (\UpdateTemplatePermissions' {grantPermissions} -> grantPermissions) (\s@UpdateTemplatePermissions' {} a -> s {grantPermissions = a} :: UpdateTemplatePermissions) Prelude.. Lens.mapping Lens.coerced

-- | A list of resource permissions to be revoked from the template.
updateTemplatePermissions_revokePermissions :: Lens.Lens' UpdateTemplatePermissions (Prelude.Maybe [ResourcePermission])
updateTemplatePermissions_revokePermissions = Lens.lens (\UpdateTemplatePermissions' {revokePermissions} -> revokePermissions) (\s@UpdateTemplatePermissions' {} a -> s {revokePermissions = a} :: UpdateTemplatePermissions) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that contains the template.
updateTemplatePermissions_awsAccountId :: Lens.Lens' UpdateTemplatePermissions Prelude.Text
updateTemplatePermissions_awsAccountId = Lens.lens (\UpdateTemplatePermissions' {awsAccountId} -> awsAccountId) (\s@UpdateTemplatePermissions' {} a -> s {awsAccountId = a} :: UpdateTemplatePermissions)

-- | The ID for the template.
updateTemplatePermissions_templateId :: Lens.Lens' UpdateTemplatePermissions Prelude.Text
updateTemplatePermissions_templateId = Lens.lens (\UpdateTemplatePermissions' {templateId} -> templateId) (\s@UpdateTemplatePermissions' {} a -> s {templateId = a} :: UpdateTemplatePermissions)

instance Core.AWSRequest UpdateTemplatePermissions where
  type
    AWSResponse UpdateTemplatePermissions =
      UpdateTemplatePermissionsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTemplatePermissionsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Permissions")
            Prelude.<*> (x Core..?> "TemplateId")
            Prelude.<*> (x Core..?> "TemplateArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTemplatePermissions where
  hashWithSalt _salt UpdateTemplatePermissions' {..} =
    _salt `Prelude.hashWithSalt` grantPermissions
      `Prelude.hashWithSalt` revokePermissions
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData UpdateTemplatePermissions where
  rnf UpdateTemplatePermissions' {..} =
    Prelude.rnf grantPermissions
      `Prelude.seq` Prelude.rnf revokePermissions
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf templateId

instance Core.ToHeaders UpdateTemplatePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateTemplatePermissions where
  toJSON UpdateTemplatePermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GrantPermissions" Core..=)
              Prelude.<$> grantPermissions,
            ("RevokePermissions" Core..=)
              Prelude.<$> revokePermissions
          ]
      )

instance Core.ToPath UpdateTemplatePermissions where
  toPath UpdateTemplatePermissions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/templates/",
        Core.toBS templateId,
        "/permissions"
      ]

instance Core.ToQuery UpdateTemplatePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTemplatePermissionsResponse' smart constructor.
data UpdateTemplatePermissionsResponse = UpdateTemplatePermissionsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A list of resource permissions to be set on the template.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The ID for the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplatePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'updateTemplatePermissionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'permissions', 'updateTemplatePermissionsResponse_permissions' - A list of resource permissions to be set on the template.
--
-- 'templateId', 'updateTemplatePermissionsResponse_templateId' - The ID for the template.
--
-- 'templateArn', 'updateTemplatePermissionsResponse_templateArn' - The Amazon Resource Name (ARN) of the template.
--
-- 'status', 'updateTemplatePermissionsResponse_status' - The HTTP status of the request.
newUpdateTemplatePermissionsResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateTemplatePermissionsResponse
newUpdateTemplatePermissionsResponse pStatus_ =
  UpdateTemplatePermissionsResponse'
    { requestId =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      templateId = Prelude.Nothing,
      templateArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
updateTemplatePermissionsResponse_requestId :: Lens.Lens' UpdateTemplatePermissionsResponse (Prelude.Maybe Prelude.Text)
updateTemplatePermissionsResponse_requestId = Lens.lens (\UpdateTemplatePermissionsResponse' {requestId} -> requestId) (\s@UpdateTemplatePermissionsResponse' {} a -> s {requestId = a} :: UpdateTemplatePermissionsResponse)

-- | A list of resource permissions to be set on the template.
updateTemplatePermissionsResponse_permissions :: Lens.Lens' UpdateTemplatePermissionsResponse (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
updateTemplatePermissionsResponse_permissions = Lens.lens (\UpdateTemplatePermissionsResponse' {permissions} -> permissions) (\s@UpdateTemplatePermissionsResponse' {} a -> s {permissions = a} :: UpdateTemplatePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID for the template.
updateTemplatePermissionsResponse_templateId :: Lens.Lens' UpdateTemplatePermissionsResponse (Prelude.Maybe Prelude.Text)
updateTemplatePermissionsResponse_templateId = Lens.lens (\UpdateTemplatePermissionsResponse' {templateId} -> templateId) (\s@UpdateTemplatePermissionsResponse' {} a -> s {templateId = a} :: UpdateTemplatePermissionsResponse)

-- | The Amazon Resource Name (ARN) of the template.
updateTemplatePermissionsResponse_templateArn :: Lens.Lens' UpdateTemplatePermissionsResponse (Prelude.Maybe Prelude.Text)
updateTemplatePermissionsResponse_templateArn = Lens.lens (\UpdateTemplatePermissionsResponse' {templateArn} -> templateArn) (\s@UpdateTemplatePermissionsResponse' {} a -> s {templateArn = a} :: UpdateTemplatePermissionsResponse)

-- | The HTTP status of the request.
updateTemplatePermissionsResponse_status :: Lens.Lens' UpdateTemplatePermissionsResponse Prelude.Int
updateTemplatePermissionsResponse_status = Lens.lens (\UpdateTemplatePermissionsResponse' {status} -> status) (\s@UpdateTemplatePermissionsResponse' {} a -> s {status = a} :: UpdateTemplatePermissionsResponse)

instance
  Prelude.NFData
    UpdateTemplatePermissionsResponse
  where
  rnf UpdateTemplatePermissionsResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf status
