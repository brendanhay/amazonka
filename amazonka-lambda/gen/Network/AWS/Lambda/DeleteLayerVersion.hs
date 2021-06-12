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
-- Module      : Network.AWS.Lambda.DeleteLayerVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>.
-- Deleted versions can no longer be viewed or added to functions. To avoid
-- breaking functions, a copy of the version remains in Lambda until no
-- functions refer to it.
module Network.AWS.Lambda.DeleteLayerVersion
  ( -- * Creating a Request
    DeleteLayerVersion (..),
    newDeleteLayerVersion,

    -- * Request Lenses
    deleteLayerVersion_layerName,
    deleteLayerVersion_versionNumber,

    -- * Destructuring the Response
    DeleteLayerVersionResponse (..),
    newDeleteLayerVersionResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLayerVersion' smart constructor.
data DeleteLayerVersion = DeleteLayerVersion'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Core.Text,
    -- | The version number.
    versionNumber :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLayerVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerName', 'deleteLayerVersion_layerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- 'versionNumber', 'deleteLayerVersion_versionNumber' - The version number.
newDeleteLayerVersion ::
  -- | 'layerName'
  Core.Text ->
  -- | 'versionNumber'
  Core.Integer ->
  DeleteLayerVersion
newDeleteLayerVersion pLayerName_ pVersionNumber_ =
  DeleteLayerVersion'
    { layerName = pLayerName_,
      versionNumber = pVersionNumber_
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
deleteLayerVersion_layerName :: Lens.Lens' DeleteLayerVersion Core.Text
deleteLayerVersion_layerName = Lens.lens (\DeleteLayerVersion' {layerName} -> layerName) (\s@DeleteLayerVersion' {} a -> s {layerName = a} :: DeleteLayerVersion)

-- | The version number.
deleteLayerVersion_versionNumber :: Lens.Lens' DeleteLayerVersion Core.Integer
deleteLayerVersion_versionNumber = Lens.lens (\DeleteLayerVersion' {versionNumber} -> versionNumber) (\s@DeleteLayerVersion' {} a -> s {versionNumber = a} :: DeleteLayerVersion)

instance Core.AWSRequest DeleteLayerVersion where
  type
    AWSResponse DeleteLayerVersion =
      DeleteLayerVersionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteLayerVersionResponse'

instance Core.Hashable DeleteLayerVersion

instance Core.NFData DeleteLayerVersion

instance Core.ToHeaders DeleteLayerVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteLayerVersion where
  toPath DeleteLayerVersion' {..} =
    Core.mconcat
      [ "/2018-10-31/layers/",
        Core.toBS layerName,
        "/versions/",
        Core.toBS versionNumber
      ]

instance Core.ToQuery DeleteLayerVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteLayerVersionResponse' smart constructor.
data DeleteLayerVersionResponse = DeleteLayerVersionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLayerVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLayerVersionResponse ::
  DeleteLayerVersionResponse
newDeleteLayerVersionResponse =
  DeleteLayerVersionResponse'

instance Core.NFData DeleteLayerVersionResponse
