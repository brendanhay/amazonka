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
-- Module      : Amazonka.IotTwinMaker.GetScene
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a scene.
module Amazonka.IotTwinMaker.GetScene
  ( -- * Creating a Request
    GetScene (..),
    newGetScene,

    -- * Request Lenses
    getScene_sceneId,
    getScene_workspaceId,

    -- * Destructuring the Response
    GetSceneResponse (..),
    newGetSceneResponse,

    -- * Response Lenses
    getSceneResponse_description,
    getSceneResponse_capabilities,
    getSceneResponse_httpStatus,
    getSceneResponse_arn,
    getSceneResponse_contentLocation,
    getSceneResponse_creationDateTime,
    getSceneResponse_sceneId,
    getSceneResponse_updateDateTime,
    getSceneResponse_workspaceId,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetScene' smart constructor.
data GetScene = GetScene'
  { -- | The ID of the scene.
    sceneId :: Prelude.Text,
    -- | The ID of the workspace that contains the scene.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetScene' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sceneId', 'getScene_sceneId' - The ID of the scene.
--
-- 'workspaceId', 'getScene_workspaceId' - The ID of the workspace that contains the scene.
newGetScene ::
  -- | 'sceneId'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  GetScene
newGetScene pSceneId_ pWorkspaceId_ =
  GetScene'
    { sceneId = pSceneId_,
      workspaceId = pWorkspaceId_
    }

-- | The ID of the scene.
getScene_sceneId :: Lens.Lens' GetScene Prelude.Text
getScene_sceneId = Lens.lens (\GetScene' {sceneId} -> sceneId) (\s@GetScene' {} a -> s {sceneId = a} :: GetScene)

-- | The ID of the workspace that contains the scene.
getScene_workspaceId :: Lens.Lens' GetScene Prelude.Text
getScene_workspaceId = Lens.lens (\GetScene' {workspaceId} -> workspaceId) (\s@GetScene' {} a -> s {workspaceId = a} :: GetScene)

instance Core.AWSRequest GetScene where
  type AWSResponse GetScene = GetSceneResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSceneResponse'
            Prelude.<$> (x Core..?> "description")
            Prelude.<*> (x Core..?> "capabilities" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "contentLocation")
            Prelude.<*> (x Core..:> "creationDateTime")
            Prelude.<*> (x Core..:> "sceneId")
            Prelude.<*> (x Core..:> "updateDateTime")
            Prelude.<*> (x Core..:> "workspaceId")
      )

instance Prelude.Hashable GetScene where
  hashWithSalt _salt GetScene' {..} =
    _salt `Prelude.hashWithSalt` sceneId
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData GetScene where
  rnf GetScene' {..} =
    Prelude.rnf sceneId
      `Prelude.seq` Prelude.rnf workspaceId

instance Core.ToHeaders GetScene where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetScene where
  toPath GetScene' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Core.toBS workspaceId,
        "/scenes/",
        Core.toBS sceneId
      ]

instance Core.ToQuery GetScene where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSceneResponse' smart constructor.
data GetSceneResponse = GetSceneResponse'
  { -- | The description of the scene.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of capabilities that the scene uses to render.
    capabilities :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the scene.
    arn :: Prelude.Text,
    -- | The relative path that specifies the location of the content definition
    -- file.
    contentLocation :: Prelude.Text,
    -- | The date and time when the scene was created.
    creationDateTime :: Core.POSIX,
    -- | The ID of the scene.
    sceneId :: Prelude.Text,
    -- | The date and time when the scene was last updated.
    updateDateTime :: Core.POSIX,
    -- | The ID of the workspace that contains the scene.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSceneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getSceneResponse_description' - The description of the scene.
--
-- 'capabilities', 'getSceneResponse_capabilities' - A list of capabilities that the scene uses to render.
--
-- 'httpStatus', 'getSceneResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getSceneResponse_arn' - The ARN of the scene.
--
-- 'contentLocation', 'getSceneResponse_contentLocation' - The relative path that specifies the location of the content definition
-- file.
--
-- 'creationDateTime', 'getSceneResponse_creationDateTime' - The date and time when the scene was created.
--
-- 'sceneId', 'getSceneResponse_sceneId' - The ID of the scene.
--
-- 'updateDateTime', 'getSceneResponse_updateDateTime' - The date and time when the scene was last updated.
--
-- 'workspaceId', 'getSceneResponse_workspaceId' - The ID of the workspace that contains the scene.
newGetSceneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'contentLocation'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'sceneId'
  Prelude.Text ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  -- | 'workspaceId'
  Prelude.Text ->
  GetSceneResponse
newGetSceneResponse
  pHttpStatus_
  pArn_
  pContentLocation_
  pCreationDateTime_
  pSceneId_
  pUpdateDateTime_
  pWorkspaceId_ =
    GetSceneResponse'
      { description = Prelude.Nothing,
        capabilities = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        contentLocation = pContentLocation_,
        creationDateTime =
          Core._Time Lens.# pCreationDateTime_,
        sceneId = pSceneId_,
        updateDateTime = Core._Time Lens.# pUpdateDateTime_,
        workspaceId = pWorkspaceId_
      }

-- | The description of the scene.
getSceneResponse_description :: Lens.Lens' GetSceneResponse (Prelude.Maybe Prelude.Text)
getSceneResponse_description = Lens.lens (\GetSceneResponse' {description} -> description) (\s@GetSceneResponse' {} a -> s {description = a} :: GetSceneResponse)

-- | A list of capabilities that the scene uses to render.
getSceneResponse_capabilities :: Lens.Lens' GetSceneResponse (Prelude.Maybe [Prelude.Text])
getSceneResponse_capabilities = Lens.lens (\GetSceneResponse' {capabilities} -> capabilities) (\s@GetSceneResponse' {} a -> s {capabilities = a} :: GetSceneResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSceneResponse_httpStatus :: Lens.Lens' GetSceneResponse Prelude.Int
getSceneResponse_httpStatus = Lens.lens (\GetSceneResponse' {httpStatus} -> httpStatus) (\s@GetSceneResponse' {} a -> s {httpStatus = a} :: GetSceneResponse)

-- | The ARN of the scene.
getSceneResponse_arn :: Lens.Lens' GetSceneResponse Prelude.Text
getSceneResponse_arn = Lens.lens (\GetSceneResponse' {arn} -> arn) (\s@GetSceneResponse' {} a -> s {arn = a} :: GetSceneResponse)

-- | The relative path that specifies the location of the content definition
-- file.
getSceneResponse_contentLocation :: Lens.Lens' GetSceneResponse Prelude.Text
getSceneResponse_contentLocation = Lens.lens (\GetSceneResponse' {contentLocation} -> contentLocation) (\s@GetSceneResponse' {} a -> s {contentLocation = a} :: GetSceneResponse)

-- | The date and time when the scene was created.
getSceneResponse_creationDateTime :: Lens.Lens' GetSceneResponse Prelude.UTCTime
getSceneResponse_creationDateTime = Lens.lens (\GetSceneResponse' {creationDateTime} -> creationDateTime) (\s@GetSceneResponse' {} a -> s {creationDateTime = a} :: GetSceneResponse) Prelude.. Core._Time

-- | The ID of the scene.
getSceneResponse_sceneId :: Lens.Lens' GetSceneResponse Prelude.Text
getSceneResponse_sceneId = Lens.lens (\GetSceneResponse' {sceneId} -> sceneId) (\s@GetSceneResponse' {} a -> s {sceneId = a} :: GetSceneResponse)

-- | The date and time when the scene was last updated.
getSceneResponse_updateDateTime :: Lens.Lens' GetSceneResponse Prelude.UTCTime
getSceneResponse_updateDateTime = Lens.lens (\GetSceneResponse' {updateDateTime} -> updateDateTime) (\s@GetSceneResponse' {} a -> s {updateDateTime = a} :: GetSceneResponse) Prelude.. Core._Time

-- | The ID of the workspace that contains the scene.
getSceneResponse_workspaceId :: Lens.Lens' GetSceneResponse Prelude.Text
getSceneResponse_workspaceId = Lens.lens (\GetSceneResponse' {workspaceId} -> workspaceId) (\s@GetSceneResponse' {} a -> s {workspaceId = a} :: GetSceneResponse)

instance Prelude.NFData GetSceneResponse where
  rnf GetSceneResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf contentLocation
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf sceneId
      `Prelude.seq` Prelude.rnf updateDateTime
      `Prelude.seq` Prelude.rnf workspaceId
