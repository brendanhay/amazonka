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
-- Module      : Amazonka.IotTwinMaker.GetEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an entity.
module Amazonka.IotTwinMaker.GetEntity
  ( -- * Creating a Request
    GetEntity (..),
    newGetEntity,

    -- * Request Lenses
    getEntity_entityId,
    getEntity_workspaceId,

    -- * Destructuring the Response
    GetEntityResponse (..),
    newGetEntityResponse,

    -- * Response Lenses
    getEntityResponse_description,
    getEntityResponse_components,
    getEntityResponse_httpStatus,
    getEntityResponse_arn,
    getEntityResponse_creationDateTime,
    getEntityResponse_entityId,
    getEntityResponse_entityName,
    getEntityResponse_hasChildEntities,
    getEntityResponse_parentEntityId,
    getEntityResponse_status,
    getEntityResponse_updateDateTime,
    getEntityResponse_workspaceId,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEntity' smart constructor.
data GetEntity = GetEntity'
  { -- | The ID of the entity.
    entityId :: Prelude.Text,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'getEntity_entityId' - The ID of the entity.
--
-- 'workspaceId', 'getEntity_workspaceId' - The ID of the workspace.
newGetEntity ::
  -- | 'entityId'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  GetEntity
newGetEntity pEntityId_ pWorkspaceId_ =
  GetEntity'
    { entityId = pEntityId_,
      workspaceId = pWorkspaceId_
    }

-- | The ID of the entity.
getEntity_entityId :: Lens.Lens' GetEntity Prelude.Text
getEntity_entityId = Lens.lens (\GetEntity' {entityId} -> entityId) (\s@GetEntity' {} a -> s {entityId = a} :: GetEntity)

-- | The ID of the workspace.
getEntity_workspaceId :: Lens.Lens' GetEntity Prelude.Text
getEntity_workspaceId = Lens.lens (\GetEntity' {workspaceId} -> workspaceId) (\s@GetEntity' {} a -> s {workspaceId = a} :: GetEntity)

instance Core.AWSRequest GetEntity where
  type AWSResponse GetEntity = GetEntityResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEntityResponse'
            Prelude.<$> (x Core..?> "description")
            Prelude.<*> (x Core..?> "components" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "creationDateTime")
            Prelude.<*> (x Core..:> "entityId")
            Prelude.<*> (x Core..:> "entityName")
            Prelude.<*> (x Core..:> "hasChildEntities")
            Prelude.<*> (x Core..:> "parentEntityId")
            Prelude.<*> (x Core..:> "status")
            Prelude.<*> (x Core..:> "updateDateTime")
            Prelude.<*> (x Core..:> "workspaceId")
      )

instance Prelude.Hashable GetEntity where
  hashWithSalt _salt GetEntity' {..} =
    _salt `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData GetEntity where
  rnf GetEntity' {..} =
    Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf workspaceId

instance Core.ToHeaders GetEntity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetEntity where
  toPath GetEntity' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Core.toBS workspaceId,
        "/entities/",
        Core.toBS entityId
      ]

instance Core.ToQuery GetEntity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEntityResponse' smart constructor.
data GetEntityResponse = GetEntityResponse'
  { -- | The description of the entity.
    description :: Prelude.Maybe Prelude.Text,
    -- | An object that maps strings to the components in the entity. Each string
    -- in the mapping must be unique to this object.
    components :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentResponse),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the entity.
    arn :: Prelude.Text,
    -- | The date and time when the entity was created.
    creationDateTime :: Core.POSIX,
    -- | The ID of the entity.
    entityId :: Prelude.Text,
    -- | The name of the entity.
    entityName :: Prelude.Text,
    -- | A Boolean value that specifies whether the entity has associated child
    -- entities.
    hasChildEntities :: Prelude.Bool,
    -- | The ID of the parent entity for this entity.
    parentEntityId :: Prelude.Text,
    -- | The current status of the entity.
    status :: Status,
    -- | The date and time when the entity was last updated.
    updateDateTime :: Core.POSIX,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEntityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getEntityResponse_description' - The description of the entity.
--
-- 'components', 'getEntityResponse_components' - An object that maps strings to the components in the entity. Each string
-- in the mapping must be unique to this object.
--
-- 'httpStatus', 'getEntityResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getEntityResponse_arn' - The ARN of the entity.
--
-- 'creationDateTime', 'getEntityResponse_creationDateTime' - The date and time when the entity was created.
--
-- 'entityId', 'getEntityResponse_entityId' - The ID of the entity.
--
-- 'entityName', 'getEntityResponse_entityName' - The name of the entity.
--
-- 'hasChildEntities', 'getEntityResponse_hasChildEntities' - A Boolean value that specifies whether the entity has associated child
-- entities.
--
-- 'parentEntityId', 'getEntityResponse_parentEntityId' - The ID of the parent entity for this entity.
--
-- 'status', 'getEntityResponse_status' - The current status of the entity.
--
-- 'updateDateTime', 'getEntityResponse_updateDateTime' - The date and time when the entity was last updated.
--
-- 'workspaceId', 'getEntityResponse_workspaceId' - The ID of the workspace.
newGetEntityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'entityName'
  Prelude.Text ->
  -- | 'hasChildEntities'
  Prelude.Bool ->
  -- | 'parentEntityId'
  Prelude.Text ->
  -- | 'status'
  Status ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  -- | 'workspaceId'
  Prelude.Text ->
  GetEntityResponse
newGetEntityResponse
  pHttpStatus_
  pArn_
  pCreationDateTime_
  pEntityId_
  pEntityName_
  pHasChildEntities_
  pParentEntityId_
  pStatus_
  pUpdateDateTime_
  pWorkspaceId_ =
    GetEntityResponse'
      { description = Prelude.Nothing,
        components = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        creationDateTime =
          Core._Time Lens.# pCreationDateTime_,
        entityId = pEntityId_,
        entityName = pEntityName_,
        hasChildEntities = pHasChildEntities_,
        parentEntityId = pParentEntityId_,
        status = pStatus_,
        updateDateTime = Core._Time Lens.# pUpdateDateTime_,
        workspaceId = pWorkspaceId_
      }

-- | The description of the entity.
getEntityResponse_description :: Lens.Lens' GetEntityResponse (Prelude.Maybe Prelude.Text)
getEntityResponse_description = Lens.lens (\GetEntityResponse' {description} -> description) (\s@GetEntityResponse' {} a -> s {description = a} :: GetEntityResponse)

-- | An object that maps strings to the components in the entity. Each string
-- in the mapping must be unique to this object.
getEntityResponse_components :: Lens.Lens' GetEntityResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentResponse))
getEntityResponse_components = Lens.lens (\GetEntityResponse' {components} -> components) (\s@GetEntityResponse' {} a -> s {components = a} :: GetEntityResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEntityResponse_httpStatus :: Lens.Lens' GetEntityResponse Prelude.Int
getEntityResponse_httpStatus = Lens.lens (\GetEntityResponse' {httpStatus} -> httpStatus) (\s@GetEntityResponse' {} a -> s {httpStatus = a} :: GetEntityResponse)

-- | The ARN of the entity.
getEntityResponse_arn :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_arn = Lens.lens (\GetEntityResponse' {arn} -> arn) (\s@GetEntityResponse' {} a -> s {arn = a} :: GetEntityResponse)

-- | The date and time when the entity was created.
getEntityResponse_creationDateTime :: Lens.Lens' GetEntityResponse Prelude.UTCTime
getEntityResponse_creationDateTime = Lens.lens (\GetEntityResponse' {creationDateTime} -> creationDateTime) (\s@GetEntityResponse' {} a -> s {creationDateTime = a} :: GetEntityResponse) Prelude.. Core._Time

-- | The ID of the entity.
getEntityResponse_entityId :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_entityId = Lens.lens (\GetEntityResponse' {entityId} -> entityId) (\s@GetEntityResponse' {} a -> s {entityId = a} :: GetEntityResponse)

-- | The name of the entity.
getEntityResponse_entityName :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_entityName = Lens.lens (\GetEntityResponse' {entityName} -> entityName) (\s@GetEntityResponse' {} a -> s {entityName = a} :: GetEntityResponse)

-- | A Boolean value that specifies whether the entity has associated child
-- entities.
getEntityResponse_hasChildEntities :: Lens.Lens' GetEntityResponse Prelude.Bool
getEntityResponse_hasChildEntities = Lens.lens (\GetEntityResponse' {hasChildEntities} -> hasChildEntities) (\s@GetEntityResponse' {} a -> s {hasChildEntities = a} :: GetEntityResponse)

-- | The ID of the parent entity for this entity.
getEntityResponse_parentEntityId :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_parentEntityId = Lens.lens (\GetEntityResponse' {parentEntityId} -> parentEntityId) (\s@GetEntityResponse' {} a -> s {parentEntityId = a} :: GetEntityResponse)

-- | The current status of the entity.
getEntityResponse_status :: Lens.Lens' GetEntityResponse Status
getEntityResponse_status = Lens.lens (\GetEntityResponse' {status} -> status) (\s@GetEntityResponse' {} a -> s {status = a} :: GetEntityResponse)

-- | The date and time when the entity was last updated.
getEntityResponse_updateDateTime :: Lens.Lens' GetEntityResponse Prelude.UTCTime
getEntityResponse_updateDateTime = Lens.lens (\GetEntityResponse' {updateDateTime} -> updateDateTime) (\s@GetEntityResponse' {} a -> s {updateDateTime = a} :: GetEntityResponse) Prelude.. Core._Time

-- | The ID of the workspace.
getEntityResponse_workspaceId :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_workspaceId = Lens.lens (\GetEntityResponse' {workspaceId} -> workspaceId) (\s@GetEntityResponse' {} a -> s {workspaceId = a} :: GetEntityResponse)

instance Prelude.NFData GetEntityResponse where
  rnf GetEntityResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf components
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf entityName
      `Prelude.seq` Prelude.rnf hasChildEntities
      `Prelude.seq` Prelude.rnf parentEntityId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateDateTime
      `Prelude.seq` Prelude.rnf workspaceId
