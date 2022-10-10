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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information in a component type.
module Amazonka.IotTwinMaker.UpdateComponentType
  ( -- * Creating a Request
    UpdateComponentType (..),
    newUpdateComponentType,

    -- * Request Lenses
    updateComponentType_functions,
    updateComponentType_propertyDefinitions,
    updateComponentType_description,
    updateComponentType_isSingleton,
    updateComponentType_extendsFrom,
    updateComponentType_componentTypeId,
    updateComponentType_workspaceId,

    -- * Destructuring the Response
    UpdateComponentTypeResponse (..),
    newUpdateComponentTypeResponse,

    -- * Response Lenses
    updateComponentTypeResponse_httpStatus,
    updateComponentTypeResponse_arn,
    updateComponentTypeResponse_componentTypeId,
    updateComponentTypeResponse_state,
    updateComponentTypeResponse_workspaceId,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateComponentType' smart constructor.
data UpdateComponentType = UpdateComponentType'
  { -- | An object that maps strings to the functions in the component type. Each
    -- string in the mapping must be unique to this object.
    functions :: Prelude.Maybe (Prelude.HashMap Prelude.Text FunctionRequest),
    -- | An object that maps strings to the property definitions in the component
    -- type. Each string in the mapping must be unique to this object.
    propertyDefinitions :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyDefinitionRequest),
    -- | The description of the component type.
    description :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that specifies whether an entity can have more than one
    -- component of this type.
    isSingleton :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the component type that this component type extends.
    extendsFrom :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the component type.
    componentTypeId :: Prelude.Text,
    -- | The ID of the workspace that contains the component type.
    workspaceId :: Prelude.Text
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
-- 'functions', 'updateComponentType_functions' - An object that maps strings to the functions in the component type. Each
-- string in the mapping must be unique to this object.
--
-- 'propertyDefinitions', 'updateComponentType_propertyDefinitions' - An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
--
-- 'description', 'updateComponentType_description' - The description of the component type.
--
-- 'isSingleton', 'updateComponentType_isSingleton' - A Boolean value that specifies whether an entity can have more than one
-- component of this type.
--
-- 'extendsFrom', 'updateComponentType_extendsFrom' - Specifies the component type that this component type extends.
--
-- 'componentTypeId', 'updateComponentType_componentTypeId' - The ID of the component type.
--
-- 'workspaceId', 'updateComponentType_workspaceId' - The ID of the workspace that contains the component type.
newUpdateComponentType ::
  -- | 'componentTypeId'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  UpdateComponentType
newUpdateComponentType
  pComponentTypeId_
  pWorkspaceId_ =
    UpdateComponentType'
      { functions = Prelude.Nothing,
        propertyDefinitions = Prelude.Nothing,
        description = Prelude.Nothing,
        isSingleton = Prelude.Nothing,
        extendsFrom = Prelude.Nothing,
        componentTypeId = pComponentTypeId_,
        workspaceId = pWorkspaceId_
      }

-- | An object that maps strings to the functions in the component type. Each
-- string in the mapping must be unique to this object.
updateComponentType_functions :: Lens.Lens' UpdateComponentType (Prelude.Maybe (Prelude.HashMap Prelude.Text FunctionRequest))
updateComponentType_functions = Lens.lens (\UpdateComponentType' {functions} -> functions) (\s@UpdateComponentType' {} a -> s {functions = a} :: UpdateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
updateComponentType_propertyDefinitions :: Lens.Lens' UpdateComponentType (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyDefinitionRequest))
updateComponentType_propertyDefinitions = Lens.lens (\UpdateComponentType' {propertyDefinitions} -> propertyDefinitions) (\s@UpdateComponentType' {} a -> s {propertyDefinitions = a} :: UpdateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | The description of the component type.
updateComponentType_description :: Lens.Lens' UpdateComponentType (Prelude.Maybe Prelude.Text)
updateComponentType_description = Lens.lens (\UpdateComponentType' {description} -> description) (\s@UpdateComponentType' {} a -> s {description = a} :: UpdateComponentType)

-- | A Boolean value that specifies whether an entity can have more than one
-- component of this type.
updateComponentType_isSingleton :: Lens.Lens' UpdateComponentType (Prelude.Maybe Prelude.Bool)
updateComponentType_isSingleton = Lens.lens (\UpdateComponentType' {isSingleton} -> isSingleton) (\s@UpdateComponentType' {} a -> s {isSingleton = a} :: UpdateComponentType)

-- | Specifies the component type that this component type extends.
updateComponentType_extendsFrom :: Lens.Lens' UpdateComponentType (Prelude.Maybe [Prelude.Text])
updateComponentType_extendsFrom = Lens.lens (\UpdateComponentType' {extendsFrom} -> extendsFrom) (\s@UpdateComponentType' {} a -> s {extendsFrom = a} :: UpdateComponentType) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the component type.
updateComponentType_componentTypeId :: Lens.Lens' UpdateComponentType Prelude.Text
updateComponentType_componentTypeId = Lens.lens (\UpdateComponentType' {componentTypeId} -> componentTypeId) (\s@UpdateComponentType' {} a -> s {componentTypeId = a} :: UpdateComponentType)

-- | The ID of the workspace that contains the component type.
updateComponentType_workspaceId :: Lens.Lens' UpdateComponentType Prelude.Text
updateComponentType_workspaceId = Lens.lens (\UpdateComponentType' {workspaceId} -> workspaceId) (\s@UpdateComponentType' {} a -> s {workspaceId = a} :: UpdateComponentType)

instance Core.AWSRequest UpdateComponentType where
  type
    AWSResponse UpdateComponentType =
      UpdateComponentTypeResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateComponentTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "componentTypeId")
            Prelude.<*> (x Core..:> "state")
            Prelude.<*> (x Core..:> "workspaceId")
      )

instance Prelude.Hashable UpdateComponentType where
  hashWithSalt _salt UpdateComponentType' {..} =
    _salt `Prelude.hashWithSalt` functions
      `Prelude.hashWithSalt` propertyDefinitions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isSingleton
      `Prelude.hashWithSalt` extendsFrom
      `Prelude.hashWithSalt` componentTypeId
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData UpdateComponentType where
  rnf UpdateComponentType' {..} =
    Prelude.rnf functions
      `Prelude.seq` Prelude.rnf propertyDefinitions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isSingleton
      `Prelude.seq` Prelude.rnf extendsFrom
      `Prelude.seq` Prelude.rnf componentTypeId
      `Prelude.seq` Prelude.rnf workspaceId

instance Core.ToHeaders UpdateComponentType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateComponentType where
  toJSON UpdateComponentType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("functions" Core..=) Prelude.<$> functions,
            ("propertyDefinitions" Core..=)
              Prelude.<$> propertyDefinitions,
            ("description" Core..=) Prelude.<$> description,
            ("isSingleton" Core..=) Prelude.<$> isSingleton,
            ("extendsFrom" Core..=) Prelude.<$> extendsFrom
          ]
      )

instance Core.ToPath UpdateComponentType where
  toPath UpdateComponentType' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Core.toBS workspaceId,
        "/component-types/",
        Core.toBS componentTypeId
      ]

instance Core.ToQuery UpdateComponentType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateComponentTypeResponse' smart constructor.
data UpdateComponentTypeResponse = UpdateComponentTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the component type.
    arn :: Prelude.Text,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Text,
    -- | The current state of the component type.
    state :: State,
    -- | The ID of the workspace that contains the component type.
    workspaceId :: Prelude.Text
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
-- 'arn', 'updateComponentTypeResponse_arn' - The ARN of the component type.
--
-- 'componentTypeId', 'updateComponentTypeResponse_componentTypeId' - The ID of the component type.
--
-- 'state', 'updateComponentTypeResponse_state' - The current state of the component type.
--
-- 'workspaceId', 'updateComponentTypeResponse_workspaceId' - The ID of the workspace that contains the component type.
newUpdateComponentTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'componentTypeId'
  Prelude.Text ->
  -- | 'state'
  State ->
  -- | 'workspaceId'
  Prelude.Text ->
  UpdateComponentTypeResponse
newUpdateComponentTypeResponse
  pHttpStatus_
  pArn_
  pComponentTypeId_
  pState_
  pWorkspaceId_ =
    UpdateComponentTypeResponse'
      { httpStatus =
          pHttpStatus_,
        arn = pArn_,
        componentTypeId = pComponentTypeId_,
        state = pState_,
        workspaceId = pWorkspaceId_
      }

-- | The response's http status code.
updateComponentTypeResponse_httpStatus :: Lens.Lens' UpdateComponentTypeResponse Prelude.Int
updateComponentTypeResponse_httpStatus = Lens.lens (\UpdateComponentTypeResponse' {httpStatus} -> httpStatus) (\s@UpdateComponentTypeResponse' {} a -> s {httpStatus = a} :: UpdateComponentTypeResponse)

-- | The ARN of the component type.
updateComponentTypeResponse_arn :: Lens.Lens' UpdateComponentTypeResponse Prelude.Text
updateComponentTypeResponse_arn = Lens.lens (\UpdateComponentTypeResponse' {arn} -> arn) (\s@UpdateComponentTypeResponse' {} a -> s {arn = a} :: UpdateComponentTypeResponse)

-- | The ID of the component type.
updateComponentTypeResponse_componentTypeId :: Lens.Lens' UpdateComponentTypeResponse Prelude.Text
updateComponentTypeResponse_componentTypeId = Lens.lens (\UpdateComponentTypeResponse' {componentTypeId} -> componentTypeId) (\s@UpdateComponentTypeResponse' {} a -> s {componentTypeId = a} :: UpdateComponentTypeResponse)

-- | The current state of the component type.
updateComponentTypeResponse_state :: Lens.Lens' UpdateComponentTypeResponse State
updateComponentTypeResponse_state = Lens.lens (\UpdateComponentTypeResponse' {state} -> state) (\s@UpdateComponentTypeResponse' {} a -> s {state = a} :: UpdateComponentTypeResponse)

-- | The ID of the workspace that contains the component type.
updateComponentTypeResponse_workspaceId :: Lens.Lens' UpdateComponentTypeResponse Prelude.Text
updateComponentTypeResponse_workspaceId = Lens.lens (\UpdateComponentTypeResponse' {workspaceId} -> workspaceId) (\s@UpdateComponentTypeResponse' {} a -> s {workspaceId = a} :: UpdateComponentTypeResponse)

instance Prelude.NFData UpdateComponentTypeResponse where
  rnf UpdateComponentTypeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf componentTypeId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf workspaceId
