{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lambda.AddLayerVersionPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds permissions to the resource-based policy of a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
-- Use this action to grant layer usage permission to other accounts. You
-- can grant permission to a single account, all AWS accounts, or all
-- accounts in an organization.
--
-- To revoke permission, call RemoveLayerVersionPermission with the
-- statement ID that you specified when you added it.
module Network.AWS.Lambda.AddLayerVersionPermission
  ( -- * Creating a Request
    AddLayerVersionPermission (..),
    newAddLayerVersionPermission,

    -- * Request Lenses
    addLayerVersionPermission_revisionId,
    addLayerVersionPermission_organizationId,
    addLayerVersionPermission_layerName,
    addLayerVersionPermission_versionNumber,
    addLayerVersionPermission_statementId,
    addLayerVersionPermission_action,
    addLayerVersionPermission_principal,

    -- * Destructuring the Response
    AddLayerVersionPermissionResponse (..),
    newAddLayerVersionPermissionResponse,

    -- * Response Lenses
    addLayerVersionPermissionResponse_revisionId,
    addLayerVersionPermissionResponse_statement,
    addLayerVersionPermissionResponse_httpStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddLayerVersionPermission' smart constructor.
data AddLayerVersionPermission = AddLayerVersionPermission'
  { -- | Only update the policy if the revision ID matches the ID specified. Use
    -- this option to avoid modifying a policy that has changed since you last
    -- read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | With the principal set to @*@, grant permission to all accounts in the
    -- specified organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Prelude.Text,
    -- | The version number.
    versionNumber :: Prelude.Integer,
    -- | An identifier that distinguishes the policy from others on the same
    -- layer version.
    statementId :: Prelude.Text,
    -- | The API action that grants access to the layer. For example,
    -- @lambda:GetLayerVersion@.
    action :: Prelude.Text,
    -- | An account ID, or @*@ to grant permission to all AWS accounts.
    principal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddLayerVersionPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'addLayerVersionPermission_revisionId' - Only update the policy if the revision ID matches the ID specified. Use
-- this option to avoid modifying a policy that has changed since you last
-- read it.
--
-- 'organizationId', 'addLayerVersionPermission_organizationId' - With the principal set to @*@, grant permission to all accounts in the
-- specified organization.
--
-- 'layerName', 'addLayerVersionPermission_layerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- 'versionNumber', 'addLayerVersionPermission_versionNumber' - The version number.
--
-- 'statementId', 'addLayerVersionPermission_statementId' - An identifier that distinguishes the policy from others on the same
-- layer version.
--
-- 'action', 'addLayerVersionPermission_action' - The API action that grants access to the layer. For example,
-- @lambda:GetLayerVersion@.
--
-- 'principal', 'addLayerVersionPermission_principal' - An account ID, or @*@ to grant permission to all AWS accounts.
newAddLayerVersionPermission ::
  -- | 'layerName'
  Prelude.Text ->
  -- | 'versionNumber'
  Prelude.Integer ->
  -- | 'statementId'
  Prelude.Text ->
  -- | 'action'
  Prelude.Text ->
  -- | 'principal'
  Prelude.Text ->
  AddLayerVersionPermission
newAddLayerVersionPermission
  pLayerName_
  pVersionNumber_
  pStatementId_
  pAction_
  pPrincipal_ =
    AddLayerVersionPermission'
      { revisionId =
          Prelude.Nothing,
        organizationId = Prelude.Nothing,
        layerName = pLayerName_,
        versionNumber = pVersionNumber_,
        statementId = pStatementId_,
        action = pAction_,
        principal = pPrincipal_
      }

-- | Only update the policy if the revision ID matches the ID specified. Use
-- this option to avoid modifying a policy that has changed since you last
-- read it.
addLayerVersionPermission_revisionId :: Lens.Lens' AddLayerVersionPermission (Prelude.Maybe Prelude.Text)
addLayerVersionPermission_revisionId = Lens.lens (\AddLayerVersionPermission' {revisionId} -> revisionId) (\s@AddLayerVersionPermission' {} a -> s {revisionId = a} :: AddLayerVersionPermission)

-- | With the principal set to @*@, grant permission to all accounts in the
-- specified organization.
addLayerVersionPermission_organizationId :: Lens.Lens' AddLayerVersionPermission (Prelude.Maybe Prelude.Text)
addLayerVersionPermission_organizationId = Lens.lens (\AddLayerVersionPermission' {organizationId} -> organizationId) (\s@AddLayerVersionPermission' {} a -> s {organizationId = a} :: AddLayerVersionPermission)

-- | The name or Amazon Resource Name (ARN) of the layer.
addLayerVersionPermission_layerName :: Lens.Lens' AddLayerVersionPermission Prelude.Text
addLayerVersionPermission_layerName = Lens.lens (\AddLayerVersionPermission' {layerName} -> layerName) (\s@AddLayerVersionPermission' {} a -> s {layerName = a} :: AddLayerVersionPermission)

-- | The version number.
addLayerVersionPermission_versionNumber :: Lens.Lens' AddLayerVersionPermission Prelude.Integer
addLayerVersionPermission_versionNumber = Lens.lens (\AddLayerVersionPermission' {versionNumber} -> versionNumber) (\s@AddLayerVersionPermission' {} a -> s {versionNumber = a} :: AddLayerVersionPermission)

-- | An identifier that distinguishes the policy from others on the same
-- layer version.
addLayerVersionPermission_statementId :: Lens.Lens' AddLayerVersionPermission Prelude.Text
addLayerVersionPermission_statementId = Lens.lens (\AddLayerVersionPermission' {statementId} -> statementId) (\s@AddLayerVersionPermission' {} a -> s {statementId = a} :: AddLayerVersionPermission)

-- | The API action that grants access to the layer. For example,
-- @lambda:GetLayerVersion@.
addLayerVersionPermission_action :: Lens.Lens' AddLayerVersionPermission Prelude.Text
addLayerVersionPermission_action = Lens.lens (\AddLayerVersionPermission' {action} -> action) (\s@AddLayerVersionPermission' {} a -> s {action = a} :: AddLayerVersionPermission)

-- | An account ID, or @*@ to grant permission to all AWS accounts.
addLayerVersionPermission_principal :: Lens.Lens' AddLayerVersionPermission Prelude.Text
addLayerVersionPermission_principal = Lens.lens (\AddLayerVersionPermission' {principal} -> principal) (\s@AddLayerVersionPermission' {} a -> s {principal = a} :: AddLayerVersionPermission)

instance Prelude.AWSRequest AddLayerVersionPermission where
  type
    Rs AddLayerVersionPermission =
      AddLayerVersionPermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddLayerVersionPermissionResponse'
            Prelude.<$> (x Prelude..?> "RevisionId")
            Prelude.<*> (x Prelude..?> "Statement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddLayerVersionPermission

instance Prelude.NFData AddLayerVersionPermission

instance Prelude.ToHeaders AddLayerVersionPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON AddLayerVersionPermission where
  toJSON AddLayerVersionPermission' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OrganizationId" Prelude..=)
              Prelude.<$> organizationId,
            Prelude.Just ("StatementId" Prelude..= statementId),
            Prelude.Just ("Action" Prelude..= action),
            Prelude.Just ("Principal" Prelude..= principal)
          ]
      )

instance Prelude.ToPath AddLayerVersionPermission where
  toPath AddLayerVersionPermission' {..} =
    Prelude.mconcat
      [ "/2018-10-31/layers/",
        Prelude.toBS layerName,
        "/versions/",
        Prelude.toBS versionNumber,
        "/policy"
      ]

instance Prelude.ToQuery AddLayerVersionPermission where
  toQuery AddLayerVersionPermission' {..} =
    Prelude.mconcat
      ["RevisionId" Prelude.=: revisionId]

-- | /See:/ 'newAddLayerVersionPermissionResponse' smart constructor.
data AddLayerVersionPermissionResponse = AddLayerVersionPermissionResponse'
  { -- | A unique identifier for the current revision of the policy.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The permission statement.
    statement :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddLayerVersionPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'addLayerVersionPermissionResponse_revisionId' - A unique identifier for the current revision of the policy.
--
-- 'statement', 'addLayerVersionPermissionResponse_statement' - The permission statement.
--
-- 'httpStatus', 'addLayerVersionPermissionResponse_httpStatus' - The response's http status code.
newAddLayerVersionPermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddLayerVersionPermissionResponse
newAddLayerVersionPermissionResponse pHttpStatus_ =
  AddLayerVersionPermissionResponse'
    { revisionId =
        Prelude.Nothing,
      statement = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for the current revision of the policy.
addLayerVersionPermissionResponse_revisionId :: Lens.Lens' AddLayerVersionPermissionResponse (Prelude.Maybe Prelude.Text)
addLayerVersionPermissionResponse_revisionId = Lens.lens (\AddLayerVersionPermissionResponse' {revisionId} -> revisionId) (\s@AddLayerVersionPermissionResponse' {} a -> s {revisionId = a} :: AddLayerVersionPermissionResponse)

-- | The permission statement.
addLayerVersionPermissionResponse_statement :: Lens.Lens' AddLayerVersionPermissionResponse (Prelude.Maybe Prelude.Text)
addLayerVersionPermissionResponse_statement = Lens.lens (\AddLayerVersionPermissionResponse' {statement} -> statement) (\s@AddLayerVersionPermissionResponse' {} a -> s {statement = a} :: AddLayerVersionPermissionResponse)

-- | The response's http status code.
addLayerVersionPermissionResponse_httpStatus :: Lens.Lens' AddLayerVersionPermissionResponse Prelude.Int
addLayerVersionPermissionResponse_httpStatus = Lens.lens (\AddLayerVersionPermissionResponse' {httpStatus} -> httpStatus) (\s@AddLayerVersionPermissionResponse' {} a -> s {httpStatus = a} :: AddLayerVersionPermissionResponse)

instance
  Prelude.NFData
    AddLayerVersionPermissionResponse
