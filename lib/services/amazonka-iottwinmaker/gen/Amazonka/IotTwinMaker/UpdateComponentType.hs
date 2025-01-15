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
-- Module      : Amazonka.IotTwinMaker.UpdateComponentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information in a component type.
module Amazonka.IotTwinMaker.UpdateComponentType
  ( -- * Creating a Request
    UpdateComponentType (..),
    newUpdateComponentType,

    -- * Request Lenses
    updateComponentType_componentTypeName,
    updateComponentType_description,
    updateComponentType_extendsFrom,
    updateComponentType_functions,
    updateComponentType_isSingleton,
    updateComponentType_propertyDefinitions,
    updateComponentType_propertyGroups,
    updateComponentType_workspaceId,
    updateComponentType_componentTypeId,

    -- * Destructuring the Response
    UpdateComponentTypeResponse (..),
    newUpdateComponentTypeResponse,

    -- * Response Lenses
    updateComponentTypeResponse_httpStatus,
    updateComponentTypeResponse_workspaceId,
    updateComponentTypeResponse_arn,
    updateComponentTypeResponse_componentTypeId,
    updateComponentTypeResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateComponentType' smart constructor.
data UpdateComponentType = UpdateComponentType'
  { -- | The component type name.
    componentTypeName :: Prelude.Maybe Prelude.Text,
    -- | The description of the component type.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the component type that this component type extends.
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
    -- | The property groups
    propertyGroups :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyGroupRequest),
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComponentType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentTypeName', 'updateComponentType_componentTypeName' - The component type name.
--
-- 'description', 'updateComponentType_description' - The description of the component type.
--
-- 'extendsFrom', 'updateComponentType_extendsFrom' - Specifies the component type that this component type extends.
--
-- 'functions', 'updateComponentType_functions' - An object that maps strings to the functions in the component type. Each
-- string in the mapping must be unique to this object.
--
-- 'isSingleton', 'updateComponentType_isSingleton' - A Boolean value that specifies whether an entity can have more than one
-- component of this type.
--
-- 'propertyDefinitions', 'updateComponentType_propertyDefinitions' - An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
--
-- 'propertyGroups', 'updateComponentType_propertyGroups' - The property groups
--
-- 'workspaceId', 'updateComponentType_workspaceId' - The ID of the workspace.
--
-- 'componentTypeId', 'updateComponentType_componentTypeId' - The ID of the component type.
newUpdateComponentType ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'componentTypeId'
  Prelude.Text ->
  UpdateComponentType
newUpdateComponentType
  pWorkspaceId_
  pComponentTypeId_ =
    UpdateComponentType'
      { componentTypeName =
          Prelude.Nothing,
        description = Prelude.Nothing,
        extendsFrom = Prelude.Nothing,
        functions = Prelude.Nothing,
        isSingleton = Prelude.Nothing,
        propertyDefinitions = Prelude.Nothing,
        propertyGroups = Prelude.Nothing,
        workspaceId = pWorkspaceId_,
        componentTypeId = pComponentTypeId_
      }

-- | The component type name.
updateComponentType_componentTypeName :: Lens.Lens' UpdateComponentType (Prelude.Maybe Prelude.Text)
updateComponentType_componentTypeName = Lens.lens (\UpdateComponentType' {componentTypeName} -> componentTypeName) (\s@UpdateComponentType' {} a -> s {componentTypeName = a} :: UpdateComponentType)

-- | The description of the component type.
updateComponentType_description :: Lens.Lens' UpdateComponentType (Prelude.Maybe Prelude.Text)
updateComponentType_description = Lens.lens (\UpdateComponentType' {description} -> description) (\s@UpdateComponentType' {} a -> s {description = a} :: UpdateComponentType)

-- | Specifies the component type that this component type extends.
updateComponentType_extendsFrom :: Lens.Lens' UpdateComponentType (Prelude.Maybe [Prelude.Text])
updateComponentType_extendsFrom = Lens.lens (\UpdateComponentType' {extendsFrom} -> extendsFrom) (\s@UpdateComponentType' {} a -> s {extendsFrom = a} :: UpdateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | An object that maps strings to the functions in the component type. Each
-- string in the mapping must be unique to this object.
updateComponentType_functions :: Lens.Lens' UpdateComponentType (Prelude.Maybe (Prelude.HashMap Prelude.Text FunctionRequest))
updateComponentType_functions = Lens.lens (\UpdateComponentType' {functions} -> functions) (\s@UpdateComponentType' {} a -> s {functions = a} :: UpdateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | A Boolean value that specifies whether an entity can have more than one
-- component of this type.
updateComponentType_isSingleton :: Lens.Lens' UpdateComponentType (Prelude.Maybe Prelude.Bool)
updateComponentType_isSingleton = Lens.lens (\UpdateComponentType' {isSingleton} -> isSingleton) (\s@UpdateComponentType' {} a -> s {isSingleton = a} :: UpdateComponentType)

-- | An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
updateComponentType_propertyDefinitions :: Lens.Lens' UpdateComponentType (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyDefinitionRequest))
updateComponentType_propertyDefinitions = Lens.lens (\UpdateComponentType' {propertyDefinitions} -> propertyDefinitions) (\s@UpdateComponentType' {} a -> s {propertyDefinitions = a} :: UpdateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | The property groups
updateComponentType_propertyGroups :: Lens.Lens' UpdateComponentType (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyGroupRequest))
updateComponentType_propertyGroups = Lens.lens (\UpdateComponentType' {propertyGroups} -> propertyGroups) (\s@UpdateComponentType' {} a -> s {propertyGroups = a} :: UpdateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the workspace.
updateComponentType_workspaceId :: Lens.Lens' UpdateComponentType Prelude.Text
updateComponentType_workspaceId = Lens.lens (\UpdateComponentType' {workspaceId} -> workspaceId) (\s@UpdateComponentType' {} a -> s {workspaceId = a} :: UpdateComponentType)

-- | The ID of the component type.
updateComponentType_componentTypeId :: Lens.Lens' UpdateComponentType Prelude.Text
updateComponentType_componentTypeId = Lens.lens (\UpdateComponentType' {componentTypeId} -> componentTypeId) (\s@UpdateComponentType' {} a -> s {componentTypeId = a} :: UpdateComponentType)

instance Core.AWSRequest UpdateComponentType where
  type
    AWSResponse UpdateComponentType =
      UpdateComponentTypeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateComponentTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workspaceId")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "componentTypeId")
            Prelude.<*> (x Data..:> "state")
      )

instance Prelude.Hashable UpdateComponentType where
  hashWithSalt _salt UpdateComponentType' {..} =
    _salt
      `Prelude.hashWithSalt` componentTypeName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` extendsFrom
      `Prelude.hashWithSalt` functions
      `Prelude.hashWithSalt` isSingleton
      `Prelude.hashWithSalt` propertyDefinitions
      `Prelude.hashWithSalt` propertyGroups
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` componentTypeId

instance Prelude.NFData UpdateComponentType where
  rnf UpdateComponentType' {..} =
    Prelude.rnf componentTypeName `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf extendsFrom `Prelude.seq`
          Prelude.rnf functions `Prelude.seq`
            Prelude.rnf isSingleton `Prelude.seq`
              Prelude.rnf propertyDefinitions `Prelude.seq`
                Prelude.rnf propertyGroups `Prelude.seq`
                  Prelude.rnf workspaceId `Prelude.seq`
                    Prelude.rnf componentTypeId

instance Data.ToHeaders UpdateComponentType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateComponentType where
  toJSON UpdateComponentType' {..} =
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
              Prelude.<$> propertyGroups
          ]
      )

instance Data.ToPath UpdateComponentType where
  toPath UpdateComponentType' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/component-types/",
        Data.toBS componentTypeId
      ]

instance Data.ToQuery UpdateComponentType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateComponentTypeResponse' smart constructor.
data UpdateComponentTypeResponse = UpdateComponentTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the workspace that contains the component type.
    workspaceId :: Prelude.Text,
    -- | The ARN of the component type.
    arn :: Prelude.Text,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Text,
    -- | The current state of the component type.
    state :: State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateComponentTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateComponentTypeResponse_httpStatus' - The response's http status code.
--
-- 'workspaceId', 'updateComponentTypeResponse_workspaceId' - The ID of the workspace that contains the component type.
--
-- 'arn', 'updateComponentTypeResponse_arn' - The ARN of the component type.
--
-- 'componentTypeId', 'updateComponentTypeResponse_componentTypeId' - The ID of the component type.
--
-- 'state', 'updateComponentTypeResponse_state' - The current state of the component type.
newUpdateComponentTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'componentTypeId'
  Prelude.Text ->
  -- | 'state'
  State ->
  UpdateComponentTypeResponse
newUpdateComponentTypeResponse
  pHttpStatus_
  pWorkspaceId_
  pArn_
  pComponentTypeId_
  pState_ =
    UpdateComponentTypeResponse'
      { httpStatus =
          pHttpStatus_,
        workspaceId = pWorkspaceId_,
        arn = pArn_,
        componentTypeId = pComponentTypeId_,
        state = pState_
      }

-- | The response's http status code.
updateComponentTypeResponse_httpStatus :: Lens.Lens' UpdateComponentTypeResponse Prelude.Int
updateComponentTypeResponse_httpStatus = Lens.lens (\UpdateComponentTypeResponse' {httpStatus} -> httpStatus) (\s@UpdateComponentTypeResponse' {} a -> s {httpStatus = a} :: UpdateComponentTypeResponse)

-- | The ID of the workspace that contains the component type.
updateComponentTypeResponse_workspaceId :: Lens.Lens' UpdateComponentTypeResponse Prelude.Text
updateComponentTypeResponse_workspaceId = Lens.lens (\UpdateComponentTypeResponse' {workspaceId} -> workspaceId) (\s@UpdateComponentTypeResponse' {} a -> s {workspaceId = a} :: UpdateComponentTypeResponse)

-- | The ARN of the component type.
updateComponentTypeResponse_arn :: Lens.Lens' UpdateComponentTypeResponse Prelude.Text
updateComponentTypeResponse_arn = Lens.lens (\UpdateComponentTypeResponse' {arn} -> arn) (\s@UpdateComponentTypeResponse' {} a -> s {arn = a} :: UpdateComponentTypeResponse)

-- | The ID of the component type.
updateComponentTypeResponse_componentTypeId :: Lens.Lens' UpdateComponentTypeResponse Prelude.Text
updateComponentTypeResponse_componentTypeId = Lens.lens (\UpdateComponentTypeResponse' {componentTypeId} -> componentTypeId) (\s@UpdateComponentTypeResponse' {} a -> s {componentTypeId = a} :: UpdateComponentTypeResponse)

-- | The current state of the component type.
updateComponentTypeResponse_state :: Lens.Lens' UpdateComponentTypeResponse State
updateComponentTypeResponse_state = Lens.lens (\UpdateComponentTypeResponse' {state} -> state) (\s@UpdateComponentTypeResponse' {} a -> s {state = a} :: UpdateComponentTypeResponse)

instance Prelude.NFData UpdateComponentTypeResponse where
  rnf UpdateComponentTypeResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf workspaceId `Prelude.seq`
        Prelude.rnf arn `Prelude.seq`
          Prelude.rnf componentTypeId `Prelude.seq`
            Prelude.rnf state
