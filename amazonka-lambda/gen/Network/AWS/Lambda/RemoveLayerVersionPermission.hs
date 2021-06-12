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
-- Module      : Network.AWS.Lambda.RemoveLayerVersionPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a statement from the permissions policy for a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
-- For more information, see AddLayerVersionPermission.
module Network.AWS.Lambda.RemoveLayerVersionPermission
  ( -- * Creating a Request
    RemoveLayerVersionPermission (..),
    newRemoveLayerVersionPermission,

    -- * Request Lenses
    removeLayerVersionPermission_revisionId,
    removeLayerVersionPermission_layerName,
    removeLayerVersionPermission_versionNumber,
    removeLayerVersionPermission_statementId,

    -- * Destructuring the Response
    RemoveLayerVersionPermissionResponse (..),
    newRemoveLayerVersionPermissionResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveLayerVersionPermission' smart constructor.
data RemoveLayerVersionPermission = RemoveLayerVersionPermission'
  { -- | Only update the policy if the revision ID matches the ID specified. Use
    -- this option to avoid modifying a policy that has changed since you last
    -- read it.
    revisionId :: Core.Maybe Core.Text,
    -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Core.Text,
    -- | The version number.
    versionNumber :: Core.Integer,
    -- | The identifier that was specified when the statement was added.
    statementId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveLayerVersionPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'removeLayerVersionPermission_revisionId' - Only update the policy if the revision ID matches the ID specified. Use
-- this option to avoid modifying a policy that has changed since you last
-- read it.
--
-- 'layerName', 'removeLayerVersionPermission_layerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- 'versionNumber', 'removeLayerVersionPermission_versionNumber' - The version number.
--
-- 'statementId', 'removeLayerVersionPermission_statementId' - The identifier that was specified when the statement was added.
newRemoveLayerVersionPermission ::
  -- | 'layerName'
  Core.Text ->
  -- | 'versionNumber'
  Core.Integer ->
  -- | 'statementId'
  Core.Text ->
  RemoveLayerVersionPermission
newRemoveLayerVersionPermission
  pLayerName_
  pVersionNumber_
  pStatementId_ =
    RemoveLayerVersionPermission'
      { revisionId =
          Core.Nothing,
        layerName = pLayerName_,
        versionNumber = pVersionNumber_,
        statementId = pStatementId_
      }

-- | Only update the policy if the revision ID matches the ID specified. Use
-- this option to avoid modifying a policy that has changed since you last
-- read it.
removeLayerVersionPermission_revisionId :: Lens.Lens' RemoveLayerVersionPermission (Core.Maybe Core.Text)
removeLayerVersionPermission_revisionId = Lens.lens (\RemoveLayerVersionPermission' {revisionId} -> revisionId) (\s@RemoveLayerVersionPermission' {} a -> s {revisionId = a} :: RemoveLayerVersionPermission)

-- | The name or Amazon Resource Name (ARN) of the layer.
removeLayerVersionPermission_layerName :: Lens.Lens' RemoveLayerVersionPermission Core.Text
removeLayerVersionPermission_layerName = Lens.lens (\RemoveLayerVersionPermission' {layerName} -> layerName) (\s@RemoveLayerVersionPermission' {} a -> s {layerName = a} :: RemoveLayerVersionPermission)

-- | The version number.
removeLayerVersionPermission_versionNumber :: Lens.Lens' RemoveLayerVersionPermission Core.Integer
removeLayerVersionPermission_versionNumber = Lens.lens (\RemoveLayerVersionPermission' {versionNumber} -> versionNumber) (\s@RemoveLayerVersionPermission' {} a -> s {versionNumber = a} :: RemoveLayerVersionPermission)

-- | The identifier that was specified when the statement was added.
removeLayerVersionPermission_statementId :: Lens.Lens' RemoveLayerVersionPermission Core.Text
removeLayerVersionPermission_statementId = Lens.lens (\RemoveLayerVersionPermission' {statementId} -> statementId) (\s@RemoveLayerVersionPermission' {} a -> s {statementId = a} :: RemoveLayerVersionPermission)

instance Core.AWSRequest RemoveLayerVersionPermission where
  type
    AWSResponse RemoveLayerVersionPermission =
      RemoveLayerVersionPermissionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      RemoveLayerVersionPermissionResponse'

instance Core.Hashable RemoveLayerVersionPermission

instance Core.NFData RemoveLayerVersionPermission

instance Core.ToHeaders RemoveLayerVersionPermission where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RemoveLayerVersionPermission where
  toPath RemoveLayerVersionPermission' {..} =
    Core.mconcat
      [ "/2018-10-31/layers/",
        Core.toBS layerName,
        "/versions/",
        Core.toBS versionNumber,
        "/policy/",
        Core.toBS statementId
      ]

instance Core.ToQuery RemoveLayerVersionPermission where
  toQuery RemoveLayerVersionPermission' {..} =
    Core.mconcat ["RevisionId" Core.=: revisionId]

-- | /See:/ 'newRemoveLayerVersionPermissionResponse' smart constructor.
data RemoveLayerVersionPermissionResponse = RemoveLayerVersionPermissionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveLayerVersionPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveLayerVersionPermissionResponse ::
  RemoveLayerVersionPermissionResponse
newRemoveLayerVersionPermissionResponse =
  RemoveLayerVersionPermissionResponse'

instance
  Core.NFData
    RemoveLayerVersionPermissionResponse
