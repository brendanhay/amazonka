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
-- Module      : Amazonka.IotTwinMaker.CreateEntity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an entity.
module Amazonka.IotTwinMaker.CreateEntity
  ( -- * Creating a Request
    CreateEntity (..),
    newCreateEntity,

    -- * Request Lenses
    createEntity_components,
    createEntity_description,
    createEntity_entityId,
    createEntity_parentEntityId,
    createEntity_tags,
    createEntity_workspaceId,
    createEntity_entityName,

    -- * Destructuring the Response
    CreateEntityResponse (..),
    newCreateEntityResponse,

    -- * Response Lenses
    createEntityResponse_httpStatus,
    createEntityResponse_entityId,
    createEntityResponse_arn,
    createEntityResponse_creationDateTime,
    createEntityResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEntity' smart constructor.
data CreateEntity = CreateEntity'
  { -- | An object that maps strings to the components in the entity. Each string
    -- in the mapping must be unique to this object.
    components :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentRequest),
    -- | The description of the entity.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the entity.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the entity\'s parent entity.
    parentEntityId :: Prelude.Maybe Prelude.Text,
    -- | Metadata that you can use to manage the entity.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the workspace that contains the entity.
    workspaceId :: Prelude.Text,
    -- | The name of the entity.
    entityName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'components', 'createEntity_components' - An object that maps strings to the components in the entity. Each string
-- in the mapping must be unique to this object.
--
-- 'description', 'createEntity_description' - The description of the entity.
--
-- 'entityId', 'createEntity_entityId' - The ID of the entity.
--
-- 'parentEntityId', 'createEntity_parentEntityId' - The ID of the entity\'s parent entity.
--
-- 'tags', 'createEntity_tags' - Metadata that you can use to manage the entity.
--
-- 'workspaceId', 'createEntity_workspaceId' - The ID of the workspace that contains the entity.
--
-- 'entityName', 'createEntity_entityName' - The name of the entity.
newCreateEntity ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'entityName'
  Prelude.Text ->
  CreateEntity
newCreateEntity pWorkspaceId_ pEntityName_ =
  CreateEntity'
    { components = Prelude.Nothing,
      description = Prelude.Nothing,
      entityId = Prelude.Nothing,
      parentEntityId = Prelude.Nothing,
      tags = Prelude.Nothing,
      workspaceId = pWorkspaceId_,
      entityName = pEntityName_
    }

-- | An object that maps strings to the components in the entity. Each string
-- in the mapping must be unique to this object.
createEntity_components :: Lens.Lens' CreateEntity (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentRequest))
createEntity_components = Lens.lens (\CreateEntity' {components} -> components) (\s@CreateEntity' {} a -> s {components = a} :: CreateEntity) Prelude.. Lens.mapping Lens.coerced

-- | The description of the entity.
createEntity_description :: Lens.Lens' CreateEntity (Prelude.Maybe Prelude.Text)
createEntity_description = Lens.lens (\CreateEntity' {description} -> description) (\s@CreateEntity' {} a -> s {description = a} :: CreateEntity)

-- | The ID of the entity.
createEntity_entityId :: Lens.Lens' CreateEntity (Prelude.Maybe Prelude.Text)
createEntity_entityId = Lens.lens (\CreateEntity' {entityId} -> entityId) (\s@CreateEntity' {} a -> s {entityId = a} :: CreateEntity)

-- | The ID of the entity\'s parent entity.
createEntity_parentEntityId :: Lens.Lens' CreateEntity (Prelude.Maybe Prelude.Text)
createEntity_parentEntityId = Lens.lens (\CreateEntity' {parentEntityId} -> parentEntityId) (\s@CreateEntity' {} a -> s {parentEntityId = a} :: CreateEntity)

-- | Metadata that you can use to manage the entity.
createEntity_tags :: Lens.Lens' CreateEntity (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEntity_tags = Lens.lens (\CreateEntity' {tags} -> tags) (\s@CreateEntity' {} a -> s {tags = a} :: CreateEntity) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the workspace that contains the entity.
createEntity_workspaceId :: Lens.Lens' CreateEntity Prelude.Text
createEntity_workspaceId = Lens.lens (\CreateEntity' {workspaceId} -> workspaceId) (\s@CreateEntity' {} a -> s {workspaceId = a} :: CreateEntity)

-- | The name of the entity.
createEntity_entityName :: Lens.Lens' CreateEntity Prelude.Text
createEntity_entityName = Lens.lens (\CreateEntity' {entityName} -> entityName) (\s@CreateEntity' {} a -> s {entityName = a} :: CreateEntity)

instance Core.AWSRequest CreateEntity where
  type AWSResponse CreateEntity = CreateEntityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEntityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "entityId")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationDateTime")
            Prelude.<*> (x Data..:> "state")
      )

instance Prelude.Hashable CreateEntity where
  hashWithSalt _salt CreateEntity' {..} =
    _salt
      `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` parentEntityId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` entityName

instance Prelude.NFData CreateEntity where
  rnf CreateEntity' {..} =
    Prelude.rnf components
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf parentEntityId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf entityName

instance Data.ToHeaders CreateEntity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEntity where
  toJSON CreateEntity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("components" Data..=) Prelude.<$> components,
            ("description" Data..=) Prelude.<$> description,
            ("entityId" Data..=) Prelude.<$> entityId,
            ("parentEntityId" Data..=)
              Prelude.<$> parentEntityId,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("entityName" Data..= entityName)
          ]
      )

instance Data.ToPath CreateEntity where
  toPath CreateEntity' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId, "/entities"]

instance Data.ToQuery CreateEntity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEntityResponse' smart constructor.
data CreateEntityResponse = CreateEntityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the entity.
    entityId :: Prelude.Text,
    -- | The ARN of the entity.
    arn :: Prelude.Text,
    -- | The date and time when the entity was created.
    creationDateTime :: Data.POSIX,
    -- | The current state of the entity.
    state :: State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEntityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEntityResponse_httpStatus' - The response's http status code.
--
-- 'entityId', 'createEntityResponse_entityId' - The ID of the entity.
--
-- 'arn', 'createEntityResponse_arn' - The ARN of the entity.
--
-- 'creationDateTime', 'createEntityResponse_creationDateTime' - The date and time when the entity was created.
--
-- 'state', 'createEntityResponse_state' - The current state of the entity.
newCreateEntityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'state'
  State ->
  CreateEntityResponse
newCreateEntityResponse
  pHttpStatus_
  pEntityId_
  pArn_
  pCreationDateTime_
  pState_ =
    CreateEntityResponse'
      { httpStatus = pHttpStatus_,
        entityId = pEntityId_,
        arn = pArn_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_,
        state = pState_
      }

-- | The response's http status code.
createEntityResponse_httpStatus :: Lens.Lens' CreateEntityResponse Prelude.Int
createEntityResponse_httpStatus = Lens.lens (\CreateEntityResponse' {httpStatus} -> httpStatus) (\s@CreateEntityResponse' {} a -> s {httpStatus = a} :: CreateEntityResponse)

-- | The ID of the entity.
createEntityResponse_entityId :: Lens.Lens' CreateEntityResponse Prelude.Text
createEntityResponse_entityId = Lens.lens (\CreateEntityResponse' {entityId} -> entityId) (\s@CreateEntityResponse' {} a -> s {entityId = a} :: CreateEntityResponse)

-- | The ARN of the entity.
createEntityResponse_arn :: Lens.Lens' CreateEntityResponse Prelude.Text
createEntityResponse_arn = Lens.lens (\CreateEntityResponse' {arn} -> arn) (\s@CreateEntityResponse' {} a -> s {arn = a} :: CreateEntityResponse)

-- | The date and time when the entity was created.
createEntityResponse_creationDateTime :: Lens.Lens' CreateEntityResponse Prelude.UTCTime
createEntityResponse_creationDateTime = Lens.lens (\CreateEntityResponse' {creationDateTime} -> creationDateTime) (\s@CreateEntityResponse' {} a -> s {creationDateTime = a} :: CreateEntityResponse) Prelude.. Data._Time

-- | The current state of the entity.
createEntityResponse_state :: Lens.Lens' CreateEntityResponse State
createEntityResponse_state = Lens.lens (\CreateEntityResponse' {state} -> state) (\s@CreateEntityResponse' {} a -> s {state = a} :: CreateEntityResponse)

instance Prelude.NFData CreateEntityResponse where
  rnf CreateEntityResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf state
