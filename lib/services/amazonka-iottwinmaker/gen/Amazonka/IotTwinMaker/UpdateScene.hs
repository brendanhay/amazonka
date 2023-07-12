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
-- Module      : Amazonka.IotTwinMaker.UpdateScene
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a scene.
module Amazonka.IotTwinMaker.UpdateScene
  ( -- * Creating a Request
    UpdateScene (..),
    newUpdateScene,

    -- * Request Lenses
    updateScene_capabilities,
    updateScene_contentLocation,
    updateScene_description,
    updateScene_workspaceId,
    updateScene_sceneId,

    -- * Destructuring the Response
    UpdateSceneResponse (..),
    newUpdateSceneResponse,

    -- * Response Lenses
    updateSceneResponse_httpStatus,
    updateSceneResponse_updateDateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateScene' smart constructor.
data UpdateScene = UpdateScene'
  { -- | A list of capabilities that the scene uses to render.
    capabilities :: Prelude.Maybe [Prelude.Text],
    -- | The relative path that specifies the location of the content definition
    -- file.
    contentLocation :: Prelude.Maybe Prelude.Text,
    -- | The description of this scene.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace that contains the scene.
    workspaceId :: Prelude.Text,
    -- | The ID of the scene.
    sceneId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateScene' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilities', 'updateScene_capabilities' - A list of capabilities that the scene uses to render.
--
-- 'contentLocation', 'updateScene_contentLocation' - The relative path that specifies the location of the content definition
-- file.
--
-- 'description', 'updateScene_description' - The description of this scene.
--
-- 'workspaceId', 'updateScene_workspaceId' - The ID of the workspace that contains the scene.
--
-- 'sceneId', 'updateScene_sceneId' - The ID of the scene.
newUpdateScene ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'sceneId'
  Prelude.Text ->
  UpdateScene
newUpdateScene pWorkspaceId_ pSceneId_ =
  UpdateScene'
    { capabilities = Prelude.Nothing,
      contentLocation = Prelude.Nothing,
      description = Prelude.Nothing,
      workspaceId = pWorkspaceId_,
      sceneId = pSceneId_
    }

-- | A list of capabilities that the scene uses to render.
updateScene_capabilities :: Lens.Lens' UpdateScene (Prelude.Maybe [Prelude.Text])
updateScene_capabilities = Lens.lens (\UpdateScene' {capabilities} -> capabilities) (\s@UpdateScene' {} a -> s {capabilities = a} :: UpdateScene) Prelude.. Lens.mapping Lens.coerced

-- | The relative path that specifies the location of the content definition
-- file.
updateScene_contentLocation :: Lens.Lens' UpdateScene (Prelude.Maybe Prelude.Text)
updateScene_contentLocation = Lens.lens (\UpdateScene' {contentLocation} -> contentLocation) (\s@UpdateScene' {} a -> s {contentLocation = a} :: UpdateScene)

-- | The description of this scene.
updateScene_description :: Lens.Lens' UpdateScene (Prelude.Maybe Prelude.Text)
updateScene_description = Lens.lens (\UpdateScene' {description} -> description) (\s@UpdateScene' {} a -> s {description = a} :: UpdateScene)

-- | The ID of the workspace that contains the scene.
updateScene_workspaceId :: Lens.Lens' UpdateScene Prelude.Text
updateScene_workspaceId = Lens.lens (\UpdateScene' {workspaceId} -> workspaceId) (\s@UpdateScene' {} a -> s {workspaceId = a} :: UpdateScene)

-- | The ID of the scene.
updateScene_sceneId :: Lens.Lens' UpdateScene Prelude.Text
updateScene_sceneId = Lens.lens (\UpdateScene' {sceneId} -> sceneId) (\s@UpdateScene' {} a -> s {sceneId = a} :: UpdateScene)

instance Core.AWSRequest UpdateScene where
  type AWSResponse UpdateScene = UpdateSceneResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSceneResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "updateDateTime")
      )

instance Prelude.Hashable UpdateScene where
  hashWithSalt _salt UpdateScene' {..} =
    _salt
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` contentLocation
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` sceneId

instance Prelude.NFData UpdateScene where
  rnf UpdateScene' {..} =
    Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf contentLocation
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf sceneId

instance Data.ToHeaders UpdateScene where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateScene where
  toJSON UpdateScene' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("capabilities" Data..=) Prelude.<$> capabilities,
            ("contentLocation" Data..=)
              Prelude.<$> contentLocation,
            ("description" Data..=) Prelude.<$> description
          ]
      )

instance Data.ToPath UpdateScene where
  toPath UpdateScene' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/scenes/",
        Data.toBS sceneId
      ]

instance Data.ToQuery UpdateScene where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSceneResponse' smart constructor.
data UpdateSceneResponse = UpdateSceneResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date and time when the scene was last updated.
    updateDateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSceneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSceneResponse_httpStatus' - The response's http status code.
--
-- 'updateDateTime', 'updateSceneResponse_updateDateTime' - The date and time when the scene was last updated.
newUpdateSceneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  UpdateSceneResponse
newUpdateSceneResponse pHttpStatus_ pUpdateDateTime_ =
  UpdateSceneResponse'
    { httpStatus = pHttpStatus_,
      updateDateTime = Data._Time Lens.# pUpdateDateTime_
    }

-- | The response's http status code.
updateSceneResponse_httpStatus :: Lens.Lens' UpdateSceneResponse Prelude.Int
updateSceneResponse_httpStatus = Lens.lens (\UpdateSceneResponse' {httpStatus} -> httpStatus) (\s@UpdateSceneResponse' {} a -> s {httpStatus = a} :: UpdateSceneResponse)

-- | The date and time when the scene was last updated.
updateSceneResponse_updateDateTime :: Lens.Lens' UpdateSceneResponse Prelude.UTCTime
updateSceneResponse_updateDateTime = Lens.lens (\UpdateSceneResponse' {updateDateTime} -> updateDateTime) (\s@UpdateSceneResponse' {} a -> s {updateDateTime = a} :: UpdateSceneResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateSceneResponse where
  rnf UpdateSceneResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf updateDateTime
