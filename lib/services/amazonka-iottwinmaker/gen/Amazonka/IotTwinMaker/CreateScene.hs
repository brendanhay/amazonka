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
-- Module      : Amazonka.IotTwinMaker.CreateScene
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a scene.
module Amazonka.IotTwinMaker.CreateScene
  ( -- * Creating a Request
    CreateScene (..),
    newCreateScene,

    -- * Request Lenses
    createScene_capabilities,
    createScene_description,
    createScene_tags,
    createScene_workspaceId,
    createScene_sceneId,
    createScene_contentLocation,

    -- * Destructuring the Response
    CreateSceneResponse (..),
    newCreateSceneResponse,

    -- * Response Lenses
    createSceneResponse_httpStatus,
    createSceneResponse_arn,
    createSceneResponse_creationDateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateScene' smart constructor.
data CreateScene = CreateScene'
  { -- | A list of capabilities that the scene uses to render itself.
    capabilities :: Prelude.Maybe [Prelude.Text],
    -- | The description for this scene.
    description :: Prelude.Maybe Prelude.Text,
    -- | Metadata that you can use to manage the scene.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the workspace that contains the scene.
    workspaceId :: Prelude.Text,
    -- | The ID of the scene.
    sceneId :: Prelude.Text,
    -- | The relative path that specifies the location of the content definition
    -- file.
    contentLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateScene' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilities', 'createScene_capabilities' - A list of capabilities that the scene uses to render itself.
--
-- 'description', 'createScene_description' - The description for this scene.
--
-- 'tags', 'createScene_tags' - Metadata that you can use to manage the scene.
--
-- 'workspaceId', 'createScene_workspaceId' - The ID of the workspace that contains the scene.
--
-- 'sceneId', 'createScene_sceneId' - The ID of the scene.
--
-- 'contentLocation', 'createScene_contentLocation' - The relative path that specifies the location of the content definition
-- file.
newCreateScene ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'sceneId'
  Prelude.Text ->
  -- | 'contentLocation'
  Prelude.Text ->
  CreateScene
newCreateScene
  pWorkspaceId_
  pSceneId_
  pContentLocation_ =
    CreateScene'
      { capabilities = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        workspaceId = pWorkspaceId_,
        sceneId = pSceneId_,
        contentLocation = pContentLocation_
      }

-- | A list of capabilities that the scene uses to render itself.
createScene_capabilities :: Lens.Lens' CreateScene (Prelude.Maybe [Prelude.Text])
createScene_capabilities = Lens.lens (\CreateScene' {capabilities} -> capabilities) (\s@CreateScene' {} a -> s {capabilities = a} :: CreateScene) Prelude.. Lens.mapping Lens.coerced

-- | The description for this scene.
createScene_description :: Lens.Lens' CreateScene (Prelude.Maybe Prelude.Text)
createScene_description = Lens.lens (\CreateScene' {description} -> description) (\s@CreateScene' {} a -> s {description = a} :: CreateScene)

-- | Metadata that you can use to manage the scene.
createScene_tags :: Lens.Lens' CreateScene (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createScene_tags = Lens.lens (\CreateScene' {tags} -> tags) (\s@CreateScene' {} a -> s {tags = a} :: CreateScene) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the workspace that contains the scene.
createScene_workspaceId :: Lens.Lens' CreateScene Prelude.Text
createScene_workspaceId = Lens.lens (\CreateScene' {workspaceId} -> workspaceId) (\s@CreateScene' {} a -> s {workspaceId = a} :: CreateScene)

-- | The ID of the scene.
createScene_sceneId :: Lens.Lens' CreateScene Prelude.Text
createScene_sceneId = Lens.lens (\CreateScene' {sceneId} -> sceneId) (\s@CreateScene' {} a -> s {sceneId = a} :: CreateScene)

-- | The relative path that specifies the location of the content definition
-- file.
createScene_contentLocation :: Lens.Lens' CreateScene Prelude.Text
createScene_contentLocation = Lens.lens (\CreateScene' {contentLocation} -> contentLocation) (\s@CreateScene' {} a -> s {contentLocation = a} :: CreateScene)

instance Core.AWSRequest CreateScene where
  type AWSResponse CreateScene = CreateSceneResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSceneResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationDateTime")
      )

instance Prelude.Hashable CreateScene where
  hashWithSalt _salt CreateScene' {..} =
    _salt
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` sceneId
      `Prelude.hashWithSalt` contentLocation

instance Prelude.NFData CreateScene where
  rnf CreateScene' {..} =
    Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf sceneId
      `Prelude.seq` Prelude.rnf contentLocation

instance Data.ToHeaders CreateScene where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateScene where
  toJSON CreateScene' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("capabilities" Data..=) Prelude.<$> capabilities,
            ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("sceneId" Data..= sceneId),
            Prelude.Just
              ("contentLocation" Data..= contentLocation)
          ]
      )

instance Data.ToPath CreateScene where
  toPath CreateScene' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId, "/scenes"]

instance Data.ToQuery CreateScene where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSceneResponse' smart constructor.
data CreateSceneResponse = CreateSceneResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the scene.
    arn :: Prelude.Text,
    -- | The date and time when the scene was created.
    creationDateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSceneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSceneResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createSceneResponse_arn' - The ARN of the scene.
--
-- 'creationDateTime', 'createSceneResponse_creationDateTime' - The date and time when the scene was created.
newCreateSceneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  CreateSceneResponse
newCreateSceneResponse
  pHttpStatus_
  pArn_
  pCreationDateTime_ =
    CreateSceneResponse'
      { httpStatus = pHttpStatus_,
        arn = pArn_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_
      }

-- | The response's http status code.
createSceneResponse_httpStatus :: Lens.Lens' CreateSceneResponse Prelude.Int
createSceneResponse_httpStatus = Lens.lens (\CreateSceneResponse' {httpStatus} -> httpStatus) (\s@CreateSceneResponse' {} a -> s {httpStatus = a} :: CreateSceneResponse)

-- | The ARN of the scene.
createSceneResponse_arn :: Lens.Lens' CreateSceneResponse Prelude.Text
createSceneResponse_arn = Lens.lens (\CreateSceneResponse' {arn} -> arn) (\s@CreateSceneResponse' {} a -> s {arn = a} :: CreateSceneResponse)

-- | The date and time when the scene was created.
createSceneResponse_creationDateTime :: Lens.Lens' CreateSceneResponse Prelude.UTCTime
createSceneResponse_creationDateTime = Lens.lens (\CreateSceneResponse' {creationDateTime} -> creationDateTime) (\s@CreateSceneResponse' {} a -> s {creationDateTime = a} :: CreateSceneResponse) Prelude.. Data._Time

instance Prelude.NFData CreateSceneResponse where
  rnf CreateSceneResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
