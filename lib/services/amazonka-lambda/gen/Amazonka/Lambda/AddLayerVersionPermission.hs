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
-- Module      : Amazonka.Lambda.AddLayerVersionPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds permissions to the resource-based policy of a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>.
-- Use this action to grant layer usage permission to other accounts. You
-- can grant permission to a single account, all accounts in an
-- organization, or all Amazon Web Services accounts.
--
-- To revoke permission, call RemoveLayerVersionPermission with the
-- statement ID that you specified when you added it.
module Amazonka.Lambda.AddLayerVersionPermission
  ( -- * Creating a Request
    AddLayerVersionPermission (..),
    newAddLayerVersionPermission,

    -- * Request Lenses
    addLayerVersionPermission_organizationId,
    addLayerVersionPermission_revisionId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddLayerVersionPermission' smart constructor.
data AddLayerVersionPermission = AddLayerVersionPermission'
  { -- | With the principal set to @*@, grant permission to all accounts in the
    -- specified organization.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | Only update the policy if the revision ID matches the ID specified. Use
    -- this option to avoid modifying a policy that has changed since you last
    -- read it.
    revisionId :: Prelude.Maybe Prelude.Text,
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
    -- | An account ID, or @*@ to grant layer usage permission to all accounts in
    -- an organization, or all Amazon Web Services accounts (if
    -- @organizationId@ is not specified). For the last case, make sure that
    -- you really do want all Amazon Web Services accounts to have usage
    -- permission to this layer.
    principal :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddLayerVersionPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'addLayerVersionPermission_organizationId' - With the principal set to @*@, grant permission to all accounts in the
-- specified organization.
--
-- 'revisionId', 'addLayerVersionPermission_revisionId' - Only update the policy if the revision ID matches the ID specified. Use
-- this option to avoid modifying a policy that has changed since you last
-- read it.
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
-- 'principal', 'addLayerVersionPermission_principal' - An account ID, or @*@ to grant layer usage permission to all accounts in
-- an organization, or all Amazon Web Services accounts (if
-- @organizationId@ is not specified). For the last case, make sure that
-- you really do want all Amazon Web Services accounts to have usage
-- permission to this layer.
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
      { organizationId =
          Prelude.Nothing,
        revisionId = Prelude.Nothing,
        layerName = pLayerName_,
        versionNumber = pVersionNumber_,
        statementId = pStatementId_,
        action = pAction_,
        principal = pPrincipal_
      }

-- | With the principal set to @*@, grant permission to all accounts in the
-- specified organization.
addLayerVersionPermission_organizationId :: Lens.Lens' AddLayerVersionPermission (Prelude.Maybe Prelude.Text)
addLayerVersionPermission_organizationId = Lens.lens (\AddLayerVersionPermission' {organizationId} -> organizationId) (\s@AddLayerVersionPermission' {} a -> s {organizationId = a} :: AddLayerVersionPermission)

-- | Only update the policy if the revision ID matches the ID specified. Use
-- this option to avoid modifying a policy that has changed since you last
-- read it.
addLayerVersionPermission_revisionId :: Lens.Lens' AddLayerVersionPermission (Prelude.Maybe Prelude.Text)
addLayerVersionPermission_revisionId = Lens.lens (\AddLayerVersionPermission' {revisionId} -> revisionId) (\s@AddLayerVersionPermission' {} a -> s {revisionId = a} :: AddLayerVersionPermission)

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

-- | An account ID, or @*@ to grant layer usage permission to all accounts in
-- an organization, or all Amazon Web Services accounts (if
-- @organizationId@ is not specified). For the last case, make sure that
-- you really do want all Amazon Web Services accounts to have usage
-- permission to this layer.
addLayerVersionPermission_principal :: Lens.Lens' AddLayerVersionPermission Prelude.Text
addLayerVersionPermission_principal = Lens.lens (\AddLayerVersionPermission' {principal} -> principal) (\s@AddLayerVersionPermission' {} a -> s {principal = a} :: AddLayerVersionPermission)

instance Core.AWSRequest AddLayerVersionPermission where
  type
    AWSResponse AddLayerVersionPermission =
      AddLayerVersionPermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddLayerVersionPermissionResponse'
            Prelude.<$> (x Data..?> "RevisionId")
            Prelude.<*> (x Data..?> "Statement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddLayerVersionPermission where
  hashWithSalt _salt AddLayerVersionPermission' {..} =
    _salt `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` layerName
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` statementId
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` principal

instance Prelude.NFData AddLayerVersionPermission where
  rnf AddLayerVersionPermission' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf layerName
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf statementId
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf principal

instance Data.ToHeaders AddLayerVersionPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AddLayerVersionPermission where
  toJSON AddLayerVersionPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OrganizationId" Data..=)
              Prelude.<$> organizationId,
            Prelude.Just ("StatementId" Data..= statementId),
            Prelude.Just ("Action" Data..= action),
            Prelude.Just ("Principal" Data..= principal)
          ]
      )

instance Data.ToPath AddLayerVersionPermission where
  toPath AddLayerVersionPermission' {..} =
    Prelude.mconcat
      [ "/2018-10-31/layers/",
        Data.toBS layerName,
        "/versions/",
        Data.toBS versionNumber,
        "/policy"
      ]

instance Data.ToQuery AddLayerVersionPermission where
  toQuery AddLayerVersionPermission' {..} =
    Prelude.mconcat ["RevisionId" Data.=: revisionId]

-- | /See:/ 'newAddLayerVersionPermissionResponse' smart constructor.
data AddLayerVersionPermissionResponse = AddLayerVersionPermissionResponse'
  { -- | A unique identifier for the current revision of the policy.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The permission statement.
    statement :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf AddLayerVersionPermissionResponse' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf statement
      `Prelude.seq` Prelude.rnf httpStatus
