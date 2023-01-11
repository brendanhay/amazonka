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
-- Module      : Amazonka.IotTwinMaker.CreateComponentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a component type.
module Amazonka.IotTwinMaker.CreateComponentType
  ( -- * Creating a Request
    CreateComponentType (..),
    newCreateComponentType,

    -- * Request Lenses
    createComponentType_componentTypeName,
    createComponentType_description,
    createComponentType_extendsFrom,
    createComponentType_functions,
    createComponentType_isSingleton,
    createComponentType_propertyDefinitions,
    createComponentType_propertyGroups,
    createComponentType_tags,
    createComponentType_workspaceId,
    createComponentType_componentTypeId,

    -- * Destructuring the Response
    CreateComponentTypeResponse (..),
    newCreateComponentTypeResponse,

    -- * Response Lenses
    createComponentTypeResponse_httpStatus,
    createComponentTypeResponse_arn,
    createComponentTypeResponse_creationDateTime,
    createComponentTypeResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateComponentType' smart constructor.
data CreateComponentType = CreateComponentType'
  { -- | A friendly name for the component type.
    componentTypeName :: Prelude.Maybe Prelude.Text,
    -- | The description of the component type.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the parent component type to extend.
    extendsFrom :: Prelude.Maybe [Prelude.Text],
    -- | An object that maps strings to the functions in the component type. Each
    -- string in the mapping must be unique to this object.
    functions :: Prelude.Maybe (Prelude.HashMap Prelude.Text FunctionRequest),
    -- | A Boolean value that specifies whether an entity can have more than one
    -- component of this type.
    isSingleton :: Prelude.Maybe Prelude.Bool,
    -- | An object that maps strings to the property definitions in the component
    -- type. Each string in the mapping must be unique to this object.
    propertyDefinitions :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyDefinitionRequest),
    propertyGroups :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyGroupRequest),
    -- | Metadata that you can use to manage the component type.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the workspace that contains the component type.
    workspaceId :: Prelude.Text,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponentType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentTypeName', 'createComponentType_componentTypeName' - A friendly name for the component type.
--
-- 'description', 'createComponentType_description' - The description of the component type.
--
-- 'extendsFrom', 'createComponentType_extendsFrom' - Specifies the parent component type to extend.
--
-- 'functions', 'createComponentType_functions' - An object that maps strings to the functions in the component type. Each
-- string in the mapping must be unique to this object.
--
-- 'isSingleton', 'createComponentType_isSingleton' - A Boolean value that specifies whether an entity can have more than one
-- component of this type.
--
-- 'propertyDefinitions', 'createComponentType_propertyDefinitions' - An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
--
-- 'propertyGroups', 'createComponentType_propertyGroups' -
--
-- 'tags', 'createComponentType_tags' - Metadata that you can use to manage the component type.
--
-- 'workspaceId', 'createComponentType_workspaceId' - The ID of the workspace that contains the component type.
--
-- 'componentTypeId', 'createComponentType_componentTypeId' - The ID of the component type.
newCreateComponentType ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'componentTypeId'
  Prelude.Text ->
  CreateComponentType
newCreateComponentType
  pWorkspaceId_
  pComponentTypeId_ =
    CreateComponentType'
      { componentTypeName =
          Prelude.Nothing,
        description = Prelude.Nothing,
        extendsFrom = Prelude.Nothing,
        functions = Prelude.Nothing,
        isSingleton = Prelude.Nothing,
        propertyDefinitions = Prelude.Nothing,
        propertyGroups = Prelude.Nothing,
        tags = Prelude.Nothing,
        workspaceId = pWorkspaceId_,
        componentTypeId = pComponentTypeId_
      }

-- | A friendly name for the component type.
createComponentType_componentTypeName :: Lens.Lens' CreateComponentType (Prelude.Maybe Prelude.Text)
createComponentType_componentTypeName = Lens.lens (\CreateComponentType' {componentTypeName} -> componentTypeName) (\s@CreateComponentType' {} a -> s {componentTypeName = a} :: CreateComponentType)

-- | The description of the component type.
createComponentType_description :: Lens.Lens' CreateComponentType (Prelude.Maybe Prelude.Text)
createComponentType_description = Lens.lens (\CreateComponentType' {description} -> description) (\s@CreateComponentType' {} a -> s {description = a} :: CreateComponentType)

-- | Specifies the parent component type to extend.
createComponentType_extendsFrom :: Lens.Lens' CreateComponentType (Prelude.Maybe [Prelude.Text])
createComponentType_extendsFrom = Lens.lens (\CreateComponentType' {extendsFrom} -> extendsFrom) (\s@CreateComponentType' {} a -> s {extendsFrom = a} :: CreateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | An object that maps strings to the functions in the component type. Each
-- string in the mapping must be unique to this object.
createComponentType_functions :: Lens.Lens' CreateComponentType (Prelude.Maybe (Prelude.HashMap Prelude.Text FunctionRequest))
createComponentType_functions = Lens.lens (\CreateComponentType' {functions} -> functions) (\s@CreateComponentType' {} a -> s {functions = a} :: CreateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value that specifies whether an entity can have more than one
-- component of this type.
createComponentType_isSingleton :: Lens.Lens' CreateComponentType (Prelude.Maybe Prelude.Bool)
createComponentType_isSingleton = Lens.lens (\CreateComponentType' {isSingleton} -> isSingleton) (\s@CreateComponentType' {} a -> s {isSingleton = a} :: CreateComponentType)

-- | An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
createComponentType_propertyDefinitions :: Lens.Lens' CreateComponentType (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyDefinitionRequest))
createComponentType_propertyDefinitions = Lens.lens (\CreateComponentType' {propertyDefinitions} -> propertyDefinitions) (\s@CreateComponentType' {} a -> s {propertyDefinitions = a} :: CreateComponentType) Prelude.. Lens.mapping Lens.coerced

-- |
createComponentType_propertyGroups :: Lens.Lens' CreateComponentType (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyGroupRequest))
createComponentType_propertyGroups = Lens.lens (\CreateComponentType' {propertyGroups} -> propertyGroups) (\s@CreateComponentType' {} a -> s {propertyGroups = a} :: CreateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | Metadata that you can use to manage the component type.
createComponentType_tags :: Lens.Lens' CreateComponentType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createComponentType_tags = Lens.lens (\CreateComponentType' {tags} -> tags) (\s@CreateComponentType' {} a -> s {tags = a} :: CreateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the workspace that contains the component type.
createComponentType_workspaceId :: Lens.Lens' CreateComponentType Prelude.Text
createComponentType_workspaceId = Lens.lens (\CreateComponentType' {workspaceId} -> workspaceId) (\s@CreateComponentType' {} a -> s {workspaceId = a} :: CreateComponentType)

-- | The ID of the component type.
createComponentType_componentTypeId :: Lens.Lens' CreateComponentType Prelude.Text
createComponentType_componentTypeId = Lens.lens (\CreateComponentType' {componentTypeId} -> componentTypeId) (\s@CreateComponentType' {} a -> s {componentTypeId = a} :: CreateComponentType)

instance Core.AWSRequest CreateComponentType where
  type
    AWSResponse CreateComponentType =
      CreateComponentTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateComponentTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationDateTime")
            Prelude.<*> (x Data..:> "state")
      )

instance Prelude.Hashable CreateComponentType where
  hashWithSalt _salt CreateComponentType' {..} =
    _salt `Prelude.hashWithSalt` componentTypeName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` extendsFrom
      `Prelude.hashWithSalt` functions
      `Prelude.hashWithSalt` isSingleton
      `Prelude.hashWithSalt` propertyDefinitions
      `Prelude.hashWithSalt` propertyGroups
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` componentTypeId

instance Prelude.NFData CreateComponentType where
  rnf CreateComponentType' {..} =
    Prelude.rnf componentTypeName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf extendsFrom
      `Prelude.seq` Prelude.rnf functions
      `Prelude.seq` Prelude.rnf isSingleton
      `Prelude.seq` Prelude.rnf propertyDefinitions
      `Prelude.seq` Prelude.rnf propertyGroups
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf componentTypeId

instance Data.ToHeaders CreateComponentType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateComponentType where
  toJSON CreateComponentType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("componentTypeName" Data..=)
              Prelude.<$> componentTypeName,
            ("description" Data..=) Prelude.<$> description,
            ("extendsFrom" Data..=) Prelude.<$> extendsFrom,
            ("functions" Data..=) Prelude.<$> functions,
            ("isSingleton" Data..=) Prelude.<$> isSingleton,
            ("propertyDefinitions" Data..=)
              Prelude.<$> propertyDefinitions,
            ("propertyGroups" Data..=)
              Prelude.<$> propertyGroups,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateComponentType where
  toPath CreateComponentType' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/component-types/",
        Data.toBS componentTypeId
      ]

instance Data.ToQuery CreateComponentType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateComponentTypeResponse' smart constructor.
data CreateComponentTypeResponse = CreateComponentTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the component type.
    arn :: Prelude.Text,
    -- | The date and time when the entity was created.
    creationDateTime :: Data.POSIX,
    -- | The current state of the component type.
    state :: State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateComponentTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createComponentTypeResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createComponentTypeResponse_arn' - The ARN of the component type.
--
-- 'creationDateTime', 'createComponentTypeResponse_creationDateTime' - The date and time when the entity was created.
--
-- 'state', 'createComponentTypeResponse_state' - The current state of the component type.
newCreateComponentTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'state'
  State ->
  CreateComponentTypeResponse
newCreateComponentTypeResponse
  pHttpStatus_
  pArn_
  pCreationDateTime_
  pState_ =
    CreateComponentTypeResponse'
      { httpStatus =
          pHttpStatus_,
        arn = pArn_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_,
        state = pState_
      }

-- | The response's http status code.
createComponentTypeResponse_httpStatus :: Lens.Lens' CreateComponentTypeResponse Prelude.Int
createComponentTypeResponse_httpStatus = Lens.lens (\CreateComponentTypeResponse' {httpStatus} -> httpStatus) (\s@CreateComponentTypeResponse' {} a -> s {httpStatus = a} :: CreateComponentTypeResponse)

-- | The ARN of the component type.
createComponentTypeResponse_arn :: Lens.Lens' CreateComponentTypeResponse Prelude.Text
createComponentTypeResponse_arn = Lens.lens (\CreateComponentTypeResponse' {arn} -> arn) (\s@CreateComponentTypeResponse' {} a -> s {arn = a} :: CreateComponentTypeResponse)

-- | The date and time when the entity was created.
createComponentTypeResponse_creationDateTime :: Lens.Lens' CreateComponentTypeResponse Prelude.UTCTime
createComponentTypeResponse_creationDateTime = Lens.lens (\CreateComponentTypeResponse' {creationDateTime} -> creationDateTime) (\s@CreateComponentTypeResponse' {} a -> s {creationDateTime = a} :: CreateComponentTypeResponse) Prelude.. Data._Time

-- | The current state of the component type.
createComponentTypeResponse_state :: Lens.Lens' CreateComponentTypeResponse State
createComponentTypeResponse_state = Lens.lens (\CreateComponentTypeResponse' {state} -> state) (\s@CreateComponentTypeResponse' {} a -> s {state = a} :: CreateComponentTypeResponse)

instance Prelude.NFData CreateComponentTypeResponse where
  rnf CreateComponentTypeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf state
