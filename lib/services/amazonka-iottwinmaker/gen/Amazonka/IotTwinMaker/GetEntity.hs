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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an entity.
module Amazonka.IotTwinMaker.GetEntity
  ( -- * Creating a Request
    GetEntity (..),
    newGetEntity,

    -- * Request Lenses
    getEntity_workspaceId,
    getEntity_entityId,

    -- * Destructuring the Response
    GetEntityResponse (..),
    newGetEntityResponse,

    -- * Response Lenses
    getEntityResponse_components,
    getEntityResponse_description,
    getEntityResponse_syncSource,
    getEntityResponse_httpStatus,
    getEntityResponse_entityId,
    getEntityResponse_entityName,
    getEntityResponse_arn,
    getEntityResponse_status,
    getEntityResponse_workspaceId,
    getEntityResponse_parentEntityId,
    getEntityResponse_hasChildEntities,
    getEntityResponse_creationDateTime,
    getEntityResponse_updateDateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEntity' smart constructor.
data GetEntity = GetEntity'
  { -- | The ID of the workspace.
    workspaceId :: Prelude.Text,
    -- | The ID of the entity.
    entityId :: Prelude.Text
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
-- 'workspaceId', 'getEntity_workspaceId' - The ID of the workspace.
--
-- 'entityId', 'getEntity_entityId' - The ID of the entity.
newGetEntity ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  GetEntity
newGetEntity pWorkspaceId_ pEntityId_ =
  GetEntity'
    { workspaceId = pWorkspaceId_,
      entityId = pEntityId_
    }

-- | The ID of the workspace.
getEntity_workspaceId :: Lens.Lens' GetEntity Prelude.Text
getEntity_workspaceId = Lens.lens (\GetEntity' {workspaceId} -> workspaceId) (\s@GetEntity' {} a -> s {workspaceId = a} :: GetEntity)

-- | The ID of the entity.
getEntity_entityId :: Lens.Lens' GetEntity Prelude.Text
getEntity_entityId = Lens.lens (\GetEntity' {entityId} -> entityId) (\s@GetEntity' {} a -> s {entityId = a} :: GetEntity)

instance Core.AWSRequest GetEntity where
  type AWSResponse GetEntity = GetEntityResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEntityResponse'
            Prelude.<$> (x Data..?> "components" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "syncSource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "entityId")
            Prelude.<*> (x Data..:> "entityName")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "workspaceId")
            Prelude.<*> (x Data..:> "parentEntityId")
            Prelude.<*> (x Data..:> "hasChildEntities")
            Prelude.<*> (x Data..:> "creationDateTime")
            Prelude.<*> (x Data..:> "updateDateTime")
      )

instance Prelude.Hashable GetEntity where
  hashWithSalt _salt GetEntity' {..} =
    _salt
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` entityId

instance Prelude.NFData GetEntity where
  rnf GetEntity' {..} =
    Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf entityId

instance Data.ToHeaders GetEntity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEntity where
  toPath GetEntity' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/entities/",
        Data.toBS entityId
      ]

instance Data.ToQuery GetEntity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEntityResponse' smart constructor.
data GetEntityResponse = GetEntityResponse'
  { -- | An object that maps strings to the components in the entity. Each string
    -- in the mapping must be unique to this object.
    components :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentResponse),
    -- | The description of the entity.
    description :: Prelude.Maybe Prelude.Text,
    -- | The syncSource of the sync job, if this entity was created by a sync
    -- job.
    syncSource :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the entity.
    entityId :: Prelude.Text,
    -- | The name of the entity.
    entityName :: Prelude.Text,
    -- | The ARN of the entity.
    arn :: Prelude.Text,
    -- | The current status of the entity.
    status :: Status,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text,
    -- | The ID of the parent entity for this entity.
    parentEntityId :: Prelude.Text,
    -- | A Boolean value that specifies whether the entity has associated child
    -- entities.
    hasChildEntities :: Prelude.Bool,
    -- | The date and time when the entity was created.
    creationDateTime :: Data.POSIX,
    -- | The date and time when the entity was last updated.
    updateDateTime :: Data.POSIX
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
-- 'components', 'getEntityResponse_components' - An object that maps strings to the components in the entity. Each string
-- in the mapping must be unique to this object.
--
-- 'description', 'getEntityResponse_description' - The description of the entity.
--
-- 'syncSource', 'getEntityResponse_syncSource' - The syncSource of the sync job, if this entity was created by a sync
-- job.
--
-- 'httpStatus', 'getEntityResponse_httpStatus' - The response's http status code.
--
-- 'entityId', 'getEntityResponse_entityId' - The ID of the entity.
--
-- 'entityName', 'getEntityResponse_entityName' - The name of the entity.
--
-- 'arn', 'getEntityResponse_arn' - The ARN of the entity.
--
-- 'status', 'getEntityResponse_status' - The current status of the entity.
--
-- 'workspaceId', 'getEntityResponse_workspaceId' - The ID of the workspace.
--
-- 'parentEntityId', 'getEntityResponse_parentEntityId' - The ID of the parent entity for this entity.
--
-- 'hasChildEntities', 'getEntityResponse_hasChildEntities' - A Boolean value that specifies whether the entity has associated child
-- entities.
--
-- 'creationDateTime', 'getEntityResponse_creationDateTime' - The date and time when the entity was created.
--
-- 'updateDateTime', 'getEntityResponse_updateDateTime' - The date and time when the entity was last updated.
newGetEntityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'entityName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'status'
  Status ->
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'parentEntityId'
  Prelude.Text ->
  -- | 'hasChildEntities'
  Prelude.Bool ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  GetEntityResponse
newGetEntityResponse
  pHttpStatus_
  pEntityId_
  pEntityName_
  pArn_
  pStatus_
  pWorkspaceId_
  pParentEntityId_
  pHasChildEntities_
  pCreationDateTime_
  pUpdateDateTime_ =
    GetEntityResponse'
      { components = Prelude.Nothing,
        description = Prelude.Nothing,
        syncSource = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        entityId = pEntityId_,
        entityName = pEntityName_,
        arn = pArn_,
        status = pStatus_,
        workspaceId = pWorkspaceId_,
        parentEntityId = pParentEntityId_,
        hasChildEntities = pHasChildEntities_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_,
        updateDateTime = Data._Time Lens.# pUpdateDateTime_
      }

-- | An object that maps strings to the components in the entity. Each string
-- in the mapping must be unique to this object.
getEntityResponse_components :: Lens.Lens' GetEntityResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentResponse))
getEntityResponse_components = Lens.lens (\GetEntityResponse' {components} -> components) (\s@GetEntityResponse' {} a -> s {components = a} :: GetEntityResponse) Prelude.. Lens.mapping Lens.coerced

-- | The description of the entity.
getEntityResponse_description :: Lens.Lens' GetEntityResponse (Prelude.Maybe Prelude.Text)
getEntityResponse_description = Lens.lens (\GetEntityResponse' {description} -> description) (\s@GetEntityResponse' {} a -> s {description = a} :: GetEntityResponse)

-- | The syncSource of the sync job, if this entity was created by a sync
-- job.
getEntityResponse_syncSource :: Lens.Lens' GetEntityResponse (Prelude.Maybe Prelude.Text)
getEntityResponse_syncSource = Lens.lens (\GetEntityResponse' {syncSource} -> syncSource) (\s@GetEntityResponse' {} a -> s {syncSource = a} :: GetEntityResponse)

-- | The response's http status code.
getEntityResponse_httpStatus :: Lens.Lens' GetEntityResponse Prelude.Int
getEntityResponse_httpStatus = Lens.lens (\GetEntityResponse' {httpStatus} -> httpStatus) (\s@GetEntityResponse' {} a -> s {httpStatus = a} :: GetEntityResponse)

-- | The ID of the entity.
getEntityResponse_entityId :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_entityId = Lens.lens (\GetEntityResponse' {entityId} -> entityId) (\s@GetEntityResponse' {} a -> s {entityId = a} :: GetEntityResponse)

-- | The name of the entity.
getEntityResponse_entityName :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_entityName = Lens.lens (\GetEntityResponse' {entityName} -> entityName) (\s@GetEntityResponse' {} a -> s {entityName = a} :: GetEntityResponse)

-- | The ARN of the entity.
getEntityResponse_arn :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_arn = Lens.lens (\GetEntityResponse' {arn} -> arn) (\s@GetEntityResponse' {} a -> s {arn = a} :: GetEntityResponse)

-- | The current status of the entity.
getEntityResponse_status :: Lens.Lens' GetEntityResponse Status
getEntityResponse_status = Lens.lens (\GetEntityResponse' {status} -> status) (\s@GetEntityResponse' {} a -> s {status = a} :: GetEntityResponse)

-- | The ID of the workspace.
getEntityResponse_workspaceId :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_workspaceId = Lens.lens (\GetEntityResponse' {workspaceId} -> workspaceId) (\s@GetEntityResponse' {} a -> s {workspaceId = a} :: GetEntityResponse)

-- | The ID of the parent entity for this entity.
getEntityResponse_parentEntityId :: Lens.Lens' GetEntityResponse Prelude.Text
getEntityResponse_parentEntityId = Lens.lens (\GetEntityResponse' {parentEntityId} -> parentEntityId) (\s@GetEntityResponse' {} a -> s {parentEntityId = a} :: GetEntityResponse)

-- | A Boolean value that specifies whether the entity has associated child
-- entities.
getEntityResponse_hasChildEntities :: Lens.Lens' GetEntityResponse Prelude.Bool
getEntityResponse_hasChildEntities = Lens.lens (\GetEntityResponse' {hasChildEntities} -> hasChildEntities) (\s@GetEntityResponse' {} a -> s {hasChildEntities = a} :: GetEntityResponse)

-- | The date and time when the entity was created.
getEntityResponse_creationDateTime :: Lens.Lens' GetEntityResponse Prelude.UTCTime
getEntityResponse_creationDateTime = Lens.lens (\GetEntityResponse' {creationDateTime} -> creationDateTime) (\s@GetEntityResponse' {} a -> s {creationDateTime = a} :: GetEntityResponse) Prelude.. Data._Time

-- | The date and time when the entity was last updated.
getEntityResponse_updateDateTime :: Lens.Lens' GetEntityResponse Prelude.UTCTime
getEntityResponse_updateDateTime = Lens.lens (\GetEntityResponse' {updateDateTime} -> updateDateTime) (\s@GetEntityResponse' {} a -> s {updateDateTime = a} :: GetEntityResponse) Prelude.. Data._Time

instance Prelude.NFData GetEntityResponse where
  rnf GetEntityResponse' {..} =
    Prelude.rnf components
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf syncSource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf entityName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf parentEntityId
      `Prelude.seq` Prelude.rnf hasChildEntities
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf updateDateTime
