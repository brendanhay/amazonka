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
-- Module      : Amazonka.Lambda.RemoveLayerVersionPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a statement from the permissions policy for a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>.
-- For more information, see AddLayerVersionPermission.
module Amazonka.Lambda.RemoveLayerVersionPermission
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveLayerVersionPermission' smart constructor.
data RemoveLayerVersionPermission = RemoveLayerVersionPermission'
  { -- | Only update the policy if the revision ID matches the ID specified. Use
    -- this option to avoid modifying a policy that has changed since you last
    -- read it.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Prelude.Text,
    -- | The version number.
    versionNumber :: Prelude.Integer,
    -- | The identifier that was specified when the statement was added.
    statementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'versionNumber'
  Prelude.Integer ->
  -- | 'statementId'
  Prelude.Text ->
  RemoveLayerVersionPermission
newRemoveLayerVersionPermission
  pLayerName_
  pVersionNumber_
  pStatementId_ =
    RemoveLayerVersionPermission'
      { revisionId =
          Prelude.Nothing,
        layerName = pLayerName_,
        versionNumber = pVersionNumber_,
        statementId = pStatementId_
      }

-- | Only update the policy if the revision ID matches the ID specified. Use
-- this option to avoid modifying a policy that has changed since you last
-- read it.
removeLayerVersionPermission_revisionId :: Lens.Lens' RemoveLayerVersionPermission (Prelude.Maybe Prelude.Text)
removeLayerVersionPermission_revisionId = Lens.lens (\RemoveLayerVersionPermission' {revisionId} -> revisionId) (\s@RemoveLayerVersionPermission' {} a -> s {revisionId = a} :: RemoveLayerVersionPermission)

-- | The name or Amazon Resource Name (ARN) of the layer.
removeLayerVersionPermission_layerName :: Lens.Lens' RemoveLayerVersionPermission Prelude.Text
removeLayerVersionPermission_layerName = Lens.lens (\RemoveLayerVersionPermission' {layerName} -> layerName) (\s@RemoveLayerVersionPermission' {} a -> s {layerName = a} :: RemoveLayerVersionPermission)

-- | The version number.
removeLayerVersionPermission_versionNumber :: Lens.Lens' RemoveLayerVersionPermission Prelude.Integer
removeLayerVersionPermission_versionNumber = Lens.lens (\RemoveLayerVersionPermission' {versionNumber} -> versionNumber) (\s@RemoveLayerVersionPermission' {} a -> s {versionNumber = a} :: RemoveLayerVersionPermission)

-- | The identifier that was specified when the statement was added.
removeLayerVersionPermission_statementId :: Lens.Lens' RemoveLayerVersionPermission Prelude.Text
removeLayerVersionPermission_statementId = Lens.lens (\RemoveLayerVersionPermission' {statementId} -> statementId) (\s@RemoveLayerVersionPermission' {} a -> s {statementId = a} :: RemoveLayerVersionPermission)

instance Core.AWSRequest RemoveLayerVersionPermission where
  type
    AWSResponse RemoveLayerVersionPermission =
      RemoveLayerVersionPermissionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      RemoveLayerVersionPermissionResponse'

instance
  Prelude.Hashable
    RemoveLayerVersionPermission
  where
  hashWithSalt _salt RemoveLayerVersionPermission' {..} =
    _salt
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` layerName
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` statementId

instance Prelude.NFData RemoveLayerVersionPermission where
  rnf RemoveLayerVersionPermission' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf layerName
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf statementId

instance Data.ToHeaders RemoveLayerVersionPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RemoveLayerVersionPermission where
  toPath RemoveLayerVersionPermission' {..} =
    Prelude.mconcat
      [ "/2018-10-31/layers/",
        Data.toBS layerName,
        "/versions/",
        Data.toBS versionNumber,
        "/policy/",
        Data.toBS statementId
      ]

instance Data.ToQuery RemoveLayerVersionPermission where
  toQuery RemoveLayerVersionPermission' {..} =
    Prelude.mconcat ["RevisionId" Data.=: revisionId]

-- | /See:/ 'newRemoveLayerVersionPermissionResponse' smart constructor.
data RemoveLayerVersionPermissionResponse = RemoveLayerVersionPermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveLayerVersionPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveLayerVersionPermissionResponse ::
  RemoveLayerVersionPermissionResponse
newRemoveLayerVersionPermissionResponse =
  RemoveLayerVersionPermissionResponse'

instance
  Prelude.NFData
    RemoveLayerVersionPermissionResponse
  where
  rnf _ = ()
