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
-- Module      : Amazonka.IotTwinMaker.GetComponentType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a component type.
module Amazonka.IotTwinMaker.GetComponentType
  ( -- * Creating a Request
    GetComponentType (..),
    newGetComponentType,

    -- * Request Lenses
    getComponentType_workspaceId,
    getComponentType_componentTypeId,

    -- * Destructuring the Response
    GetComponentTypeResponse (..),
    newGetComponentTypeResponse,

    -- * Response Lenses
    getComponentTypeResponse_componentTypeName,
    getComponentTypeResponse_description,
    getComponentTypeResponse_extendsFrom,
    getComponentTypeResponse_functions,
    getComponentTypeResponse_isAbstract,
    getComponentTypeResponse_isSchemaInitialized,
    getComponentTypeResponse_isSingleton,
    getComponentTypeResponse_propertyDefinitions,
    getComponentTypeResponse_propertyGroups,
    getComponentTypeResponse_status,
    getComponentTypeResponse_syncSource,
    getComponentTypeResponse_httpStatus,
    getComponentTypeResponse_workspaceId,
    getComponentTypeResponse_componentTypeId,
    getComponentTypeResponse_creationDateTime,
    getComponentTypeResponse_updateDateTime,
    getComponentTypeResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetComponentType' smart constructor.
data GetComponentType = GetComponentType'
  { -- | The ID of the workspace that contains the component type.
    workspaceId :: Prelude.Text,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponentType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'getComponentType_workspaceId' - The ID of the workspace that contains the component type.
--
-- 'componentTypeId', 'getComponentType_componentTypeId' - The ID of the component type.
newGetComponentType ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'componentTypeId'
  Prelude.Text ->
  GetComponentType
newGetComponentType pWorkspaceId_ pComponentTypeId_ =
  GetComponentType'
    { workspaceId = pWorkspaceId_,
      componentTypeId = pComponentTypeId_
    }

-- | The ID of the workspace that contains the component type.
getComponentType_workspaceId :: Lens.Lens' GetComponentType Prelude.Text
getComponentType_workspaceId = Lens.lens (\GetComponentType' {workspaceId} -> workspaceId) (\s@GetComponentType' {} a -> s {workspaceId = a} :: GetComponentType)

-- | The ID of the component type.
getComponentType_componentTypeId :: Lens.Lens' GetComponentType Prelude.Text
getComponentType_componentTypeId = Lens.lens (\GetComponentType' {componentTypeId} -> componentTypeId) (\s@GetComponentType' {} a -> s {componentTypeId = a} :: GetComponentType)

instance Core.AWSRequest GetComponentType where
  type
    AWSResponse GetComponentType =
      GetComponentTypeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComponentTypeResponse'
            Prelude.<$> (x Data..?> "componentTypeName")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "extendsFrom" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "functions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "isAbstract")
            Prelude.<*> (x Data..?> "isSchemaInitialized")
            Prelude.<*> (x Data..?> "isSingleton")
            Prelude.<*> ( x Data..?> "propertyDefinitions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "propertyGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "syncSource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workspaceId")
            Prelude.<*> (x Data..:> "componentTypeId")
            Prelude.<*> (x Data..:> "creationDateTime")
            Prelude.<*> (x Data..:> "updateDateTime")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable GetComponentType where
  hashWithSalt _salt GetComponentType' {..} =
    _salt `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` componentTypeId

instance Prelude.NFData GetComponentType where
  rnf GetComponentType' {..} =
    Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf componentTypeId

instance Data.ToHeaders GetComponentType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetComponentType where
  toPath GetComponentType' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/component-types/",
        Data.toBS componentTypeId
      ]

instance Data.ToQuery GetComponentType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetComponentTypeResponse' smart constructor.
data GetComponentTypeResponse = GetComponentTypeResponse'
  { -- | The component type name.
    componentTypeName :: Prelude.Maybe Prelude.Text,
    -- | The description of the component type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the parent component type that this component type extends.
    extendsFrom :: Prelude.Maybe [Prelude.Text],
    -- | An object that maps strings to the functions in the component type. Each
    -- string in the mapping must be unique to this object.
    functions :: Prelude.Maybe (Prelude.HashMap Prelude.Text FunctionResponse),
    -- | A Boolean value that specifies whether the component type is abstract.
    isAbstract :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that specifies whether the component type has a schema
    -- initializer and that the schema initializer has run.
    isSchemaInitialized :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that specifies whether an entity can have more than one
    -- component of this type.
    isSingleton :: Prelude.Maybe Prelude.Bool,
    -- | An object that maps strings to the property definitions in the component
    -- type. Each string in the mapping must be unique to this object.
    propertyDefinitions :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyDefinitionResponse),
    -- | The maximum number of results to return at one time. The default is 25.
    --
    -- Valid Range: Minimum value of 1. Maximum value of 250.
    propertyGroups :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyGroupResponse),
    -- | The current status of the component type.
    status :: Prelude.Maybe Status,
    -- | The syncSource of the sync job, if this entity was created by a sync
    -- job.
    syncSource :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the workspace that contains the component type.
    workspaceId :: Prelude.Text,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Text,
    -- | The date and time when the component type was created.
    creationDateTime :: Data.POSIX,
    -- | The date and time when the component was last updated.
    updateDateTime :: Data.POSIX,
    -- | The ARN of the component type.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComponentTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentTypeName', 'getComponentTypeResponse_componentTypeName' - The component type name.
--
-- 'description', 'getComponentTypeResponse_description' - The description of the component type.
--
-- 'extendsFrom', 'getComponentTypeResponse_extendsFrom' - The name of the parent component type that this component type extends.
--
-- 'functions', 'getComponentTypeResponse_functions' - An object that maps strings to the functions in the component type. Each
-- string in the mapping must be unique to this object.
--
-- 'isAbstract', 'getComponentTypeResponse_isAbstract' - A Boolean value that specifies whether the component type is abstract.
--
-- 'isSchemaInitialized', 'getComponentTypeResponse_isSchemaInitialized' - A Boolean value that specifies whether the component type has a schema
-- initializer and that the schema initializer has run.
--
-- 'isSingleton', 'getComponentTypeResponse_isSingleton' - A Boolean value that specifies whether an entity can have more than one
-- component of this type.
--
-- 'propertyDefinitions', 'getComponentTypeResponse_propertyDefinitions' - An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
--
-- 'propertyGroups', 'getComponentTypeResponse_propertyGroups' - The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
--
-- 'status', 'getComponentTypeResponse_status' - The current status of the component type.
--
-- 'syncSource', 'getComponentTypeResponse_syncSource' - The syncSource of the sync job, if this entity was created by a sync
-- job.
--
-- 'httpStatus', 'getComponentTypeResponse_httpStatus' - The response's http status code.
--
-- 'workspaceId', 'getComponentTypeResponse_workspaceId' - The ID of the workspace that contains the component type.
--
-- 'componentTypeId', 'getComponentTypeResponse_componentTypeId' - The ID of the component type.
--
-- 'creationDateTime', 'getComponentTypeResponse_creationDateTime' - The date and time when the component type was created.
--
-- 'updateDateTime', 'getComponentTypeResponse_updateDateTime' - The date and time when the component was last updated.
--
-- 'arn', 'getComponentTypeResponse_arn' - The ARN of the component type.
newGetComponentTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'componentTypeId'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  -- | 'arn'
  Prelude.Text ->
  GetComponentTypeResponse
newGetComponentTypeResponse
  pHttpStatus_
  pWorkspaceId_
  pComponentTypeId_
  pCreationDateTime_
  pUpdateDateTime_
  pArn_ =
    GetComponentTypeResponse'
      { componentTypeName =
          Prelude.Nothing,
        description = Prelude.Nothing,
        extendsFrom = Prelude.Nothing,
        functions = Prelude.Nothing,
        isAbstract = Prelude.Nothing,
        isSchemaInitialized = Prelude.Nothing,
        isSingleton = Prelude.Nothing,
        propertyDefinitions = Prelude.Nothing,
        propertyGroups = Prelude.Nothing,
        status = Prelude.Nothing,
        syncSource = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        workspaceId = pWorkspaceId_,
        componentTypeId = pComponentTypeId_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_,
        updateDateTime =
          Data._Time Lens.# pUpdateDateTime_,
        arn = pArn_
      }

-- | The component type name.
getComponentTypeResponse_componentTypeName :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe Prelude.Text)
getComponentTypeResponse_componentTypeName = Lens.lens (\GetComponentTypeResponse' {componentTypeName} -> componentTypeName) (\s@GetComponentTypeResponse' {} a -> s {componentTypeName = a} :: GetComponentTypeResponse)

-- | The description of the component type.
getComponentTypeResponse_description :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe Prelude.Text)
getComponentTypeResponse_description = Lens.lens (\GetComponentTypeResponse' {description} -> description) (\s@GetComponentTypeResponse' {} a -> s {description = a} :: GetComponentTypeResponse)

-- | The name of the parent component type that this component type extends.
getComponentTypeResponse_extendsFrom :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe [Prelude.Text])
getComponentTypeResponse_extendsFrom = Lens.lens (\GetComponentTypeResponse' {extendsFrom} -> extendsFrom) (\s@GetComponentTypeResponse' {} a -> s {extendsFrom = a} :: GetComponentTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | An object that maps strings to the functions in the component type. Each
-- string in the mapping must be unique to this object.
getComponentTypeResponse_functions :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text FunctionResponse))
getComponentTypeResponse_functions = Lens.lens (\GetComponentTypeResponse' {functions} -> functions) (\s@GetComponentTypeResponse' {} a -> s {functions = a} :: GetComponentTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value that specifies whether the component type is abstract.
getComponentTypeResponse_isAbstract :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe Prelude.Bool)
getComponentTypeResponse_isAbstract = Lens.lens (\GetComponentTypeResponse' {isAbstract} -> isAbstract) (\s@GetComponentTypeResponse' {} a -> s {isAbstract = a} :: GetComponentTypeResponse)

-- | A Boolean value that specifies whether the component type has a schema
-- initializer and that the schema initializer has run.
getComponentTypeResponse_isSchemaInitialized :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe Prelude.Bool)
getComponentTypeResponse_isSchemaInitialized = Lens.lens (\GetComponentTypeResponse' {isSchemaInitialized} -> isSchemaInitialized) (\s@GetComponentTypeResponse' {} a -> s {isSchemaInitialized = a} :: GetComponentTypeResponse)

-- | A Boolean value that specifies whether an entity can have more than one
-- component of this type.
getComponentTypeResponse_isSingleton :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe Prelude.Bool)
getComponentTypeResponse_isSingleton = Lens.lens (\GetComponentTypeResponse' {isSingleton} -> isSingleton) (\s@GetComponentTypeResponse' {} a -> s {isSingleton = a} :: GetComponentTypeResponse)

-- | An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
getComponentTypeResponse_propertyDefinitions :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyDefinitionResponse))
getComponentTypeResponse_propertyDefinitions = Lens.lens (\GetComponentTypeResponse' {propertyDefinitions} -> propertyDefinitions) (\s@GetComponentTypeResponse' {} a -> s {propertyDefinitions = a} :: GetComponentTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
getComponentTypeResponse_propertyGroups :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyGroupResponse))
getComponentTypeResponse_propertyGroups = Lens.lens (\GetComponentTypeResponse' {propertyGroups} -> propertyGroups) (\s@GetComponentTypeResponse' {} a -> s {propertyGroups = a} :: GetComponentTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the component type.
getComponentTypeResponse_status :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe Status)
getComponentTypeResponse_status = Lens.lens (\GetComponentTypeResponse' {status} -> status) (\s@GetComponentTypeResponse' {} a -> s {status = a} :: GetComponentTypeResponse)

-- | The syncSource of the sync job, if this entity was created by a sync
-- job.
getComponentTypeResponse_syncSource :: Lens.Lens' GetComponentTypeResponse (Prelude.Maybe Prelude.Text)
getComponentTypeResponse_syncSource = Lens.lens (\GetComponentTypeResponse' {syncSource} -> syncSource) (\s@GetComponentTypeResponse' {} a -> s {syncSource = a} :: GetComponentTypeResponse)

-- | The response's http status code.
getComponentTypeResponse_httpStatus :: Lens.Lens' GetComponentTypeResponse Prelude.Int
getComponentTypeResponse_httpStatus = Lens.lens (\GetComponentTypeResponse' {httpStatus} -> httpStatus) (\s@GetComponentTypeResponse' {} a -> s {httpStatus = a} :: GetComponentTypeResponse)

-- | The ID of the workspace that contains the component type.
getComponentTypeResponse_workspaceId :: Lens.Lens' GetComponentTypeResponse Prelude.Text
getComponentTypeResponse_workspaceId = Lens.lens (\GetComponentTypeResponse' {workspaceId} -> workspaceId) (\s@GetComponentTypeResponse' {} a -> s {workspaceId = a} :: GetComponentTypeResponse)

-- | The ID of the component type.
getComponentTypeResponse_componentTypeId :: Lens.Lens' GetComponentTypeResponse Prelude.Text
getComponentTypeResponse_componentTypeId = Lens.lens (\GetComponentTypeResponse' {componentTypeId} -> componentTypeId) (\s@GetComponentTypeResponse' {} a -> s {componentTypeId = a} :: GetComponentTypeResponse)

-- | The date and time when the component type was created.
getComponentTypeResponse_creationDateTime :: Lens.Lens' GetComponentTypeResponse Prelude.UTCTime
getComponentTypeResponse_creationDateTime = Lens.lens (\GetComponentTypeResponse' {creationDateTime} -> creationDateTime) (\s@GetComponentTypeResponse' {} a -> s {creationDateTime = a} :: GetComponentTypeResponse) Prelude.. Data._Time

-- | The date and time when the component was last updated.
getComponentTypeResponse_updateDateTime :: Lens.Lens' GetComponentTypeResponse Prelude.UTCTime
getComponentTypeResponse_updateDateTime = Lens.lens (\GetComponentTypeResponse' {updateDateTime} -> updateDateTime) (\s@GetComponentTypeResponse' {} a -> s {updateDateTime = a} :: GetComponentTypeResponse) Prelude.. Data._Time

-- | The ARN of the component type.
getComponentTypeResponse_arn :: Lens.Lens' GetComponentTypeResponse Prelude.Text
getComponentTypeResponse_arn = Lens.lens (\GetComponentTypeResponse' {arn} -> arn) (\s@GetComponentTypeResponse' {} a -> s {arn = a} :: GetComponentTypeResponse)

instance Prelude.NFData GetComponentTypeResponse where
  rnf GetComponentTypeResponse' {..} =
    Prelude.rnf componentTypeName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf extendsFrom
      `Prelude.seq` Prelude.rnf functions
      `Prelude.seq` Prelude.rnf isAbstract
      `Prelude.seq` Prelude.rnf isSchemaInitialized
      `Prelude.seq` Prelude.rnf isSingleton
      `Prelude.seq` Prelude.rnf propertyDefinitions
      `Prelude.seq` Prelude.rnf propertyGroups
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf syncSource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf componentTypeId
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf updateDateTime
      `Prelude.seq` Prelude.rnf arn
