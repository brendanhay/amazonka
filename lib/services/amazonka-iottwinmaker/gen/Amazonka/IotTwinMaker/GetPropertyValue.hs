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
-- Module      : Amazonka.IotTwinMaker.GetPropertyValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the property values for a component, component type, entity, or
-- workspace.
--
-- You must specify a value for either @componentName@, @componentTypeId@,
-- @entityId@, or @workspaceId@.
module Amazonka.IotTwinMaker.GetPropertyValue
  ( -- * Creating a Request
    GetPropertyValue (..),
    newGetPropertyValue,

    -- * Request Lenses
    getPropertyValue_entityId,
    getPropertyValue_componentName,
    getPropertyValue_componentTypeId,
    getPropertyValue_selectedProperties,
    getPropertyValue_workspaceId,

    -- * Destructuring the Response
    GetPropertyValueResponse (..),
    newGetPropertyValueResponse,

    -- * Response Lenses
    getPropertyValueResponse_httpStatus,
    getPropertyValueResponse_propertyValues,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPropertyValue' smart constructor.
data GetPropertyValue = GetPropertyValue'
  { -- | The ID of the entity whose property values the operation returns.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The name of the component whose property values the operation returns.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the component type whose property values the operation
    -- returns.
    componentTypeId :: Prelude.Maybe Prelude.Text,
    -- | The properties whose values the operation returns.
    selectedProperties :: Prelude.NonEmpty Prelude.Text,
    -- | The ID of the workspace whose values the operation returns.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPropertyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'getPropertyValue_entityId' - The ID of the entity whose property values the operation returns.
--
-- 'componentName', 'getPropertyValue_componentName' - The name of the component whose property values the operation returns.
--
-- 'componentTypeId', 'getPropertyValue_componentTypeId' - The ID of the component type whose property values the operation
-- returns.
--
-- 'selectedProperties', 'getPropertyValue_selectedProperties' - The properties whose values the operation returns.
--
-- 'workspaceId', 'getPropertyValue_workspaceId' - The ID of the workspace whose values the operation returns.
newGetPropertyValue ::
  -- | 'selectedProperties'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  GetPropertyValue
newGetPropertyValue
  pSelectedProperties_
  pWorkspaceId_ =
    GetPropertyValue'
      { entityId = Prelude.Nothing,
        componentName = Prelude.Nothing,
        componentTypeId = Prelude.Nothing,
        selectedProperties =
          Lens.coerced Lens.# pSelectedProperties_,
        workspaceId = pWorkspaceId_
      }

-- | The ID of the entity whose property values the operation returns.
getPropertyValue_entityId :: Lens.Lens' GetPropertyValue (Prelude.Maybe Prelude.Text)
getPropertyValue_entityId = Lens.lens (\GetPropertyValue' {entityId} -> entityId) (\s@GetPropertyValue' {} a -> s {entityId = a} :: GetPropertyValue)

-- | The name of the component whose property values the operation returns.
getPropertyValue_componentName :: Lens.Lens' GetPropertyValue (Prelude.Maybe Prelude.Text)
getPropertyValue_componentName = Lens.lens (\GetPropertyValue' {componentName} -> componentName) (\s@GetPropertyValue' {} a -> s {componentName = a} :: GetPropertyValue)

-- | The ID of the component type whose property values the operation
-- returns.
getPropertyValue_componentTypeId :: Lens.Lens' GetPropertyValue (Prelude.Maybe Prelude.Text)
getPropertyValue_componentTypeId = Lens.lens (\GetPropertyValue' {componentTypeId} -> componentTypeId) (\s@GetPropertyValue' {} a -> s {componentTypeId = a} :: GetPropertyValue)

-- | The properties whose values the operation returns.
getPropertyValue_selectedProperties :: Lens.Lens' GetPropertyValue (Prelude.NonEmpty Prelude.Text)
getPropertyValue_selectedProperties = Lens.lens (\GetPropertyValue' {selectedProperties} -> selectedProperties) (\s@GetPropertyValue' {} a -> s {selectedProperties = a} :: GetPropertyValue) Prelude.. Lens.coerced

-- | The ID of the workspace whose values the operation returns.
getPropertyValue_workspaceId :: Lens.Lens' GetPropertyValue Prelude.Text
getPropertyValue_workspaceId = Lens.lens (\GetPropertyValue' {workspaceId} -> workspaceId) (\s@GetPropertyValue' {} a -> s {workspaceId = a} :: GetPropertyValue)

instance Core.AWSRequest GetPropertyValue where
  type
    AWSResponse GetPropertyValue =
      GetPropertyValueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPropertyValueResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "propertyValues"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetPropertyValue where
  hashWithSalt _salt GetPropertyValue' {..} =
    _salt `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` componentTypeId
      `Prelude.hashWithSalt` selectedProperties
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData GetPropertyValue where
  rnf GetPropertyValue' {..} =
    Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf componentTypeId
      `Prelude.seq` Prelude.rnf selectedProperties
      `Prelude.seq` Prelude.rnf workspaceId

instance Core.ToHeaders GetPropertyValue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPropertyValue where
  toJSON GetPropertyValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("entityId" Core..=) Prelude.<$> entityId,
            ("componentName" Core..=) Prelude.<$> componentName,
            ("componentTypeId" Core..=)
              Prelude.<$> componentTypeId,
            Prelude.Just
              ("selectedProperties" Core..= selectedProperties)
          ]
      )

instance Core.ToPath GetPropertyValue where
  toPath GetPropertyValue' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Core.toBS workspaceId,
        "/entity-properties/value"
      ]

instance Core.ToQuery GetPropertyValue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPropertyValueResponse' smart constructor.
data GetPropertyValueResponse = GetPropertyValueResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that maps strings to the properties and latest property values
    -- in the response. Each string in the mapping must be unique to this
    -- object.
    propertyValues :: Prelude.HashMap Prelude.Text PropertyLatestValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPropertyValueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getPropertyValueResponse_httpStatus' - The response's http status code.
--
-- 'propertyValues', 'getPropertyValueResponse_propertyValues' - An object that maps strings to the properties and latest property values
-- in the response. Each string in the mapping must be unique to this
-- object.
newGetPropertyValueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPropertyValueResponse
newGetPropertyValueResponse pHttpStatus_ =
  GetPropertyValueResponse'
    { httpStatus =
        pHttpStatus_,
      propertyValues = Prelude.mempty
    }

-- | The response's http status code.
getPropertyValueResponse_httpStatus :: Lens.Lens' GetPropertyValueResponse Prelude.Int
getPropertyValueResponse_httpStatus = Lens.lens (\GetPropertyValueResponse' {httpStatus} -> httpStatus) (\s@GetPropertyValueResponse' {} a -> s {httpStatus = a} :: GetPropertyValueResponse)

-- | An object that maps strings to the properties and latest property values
-- in the response. Each string in the mapping must be unique to this
-- object.
getPropertyValueResponse_propertyValues :: Lens.Lens' GetPropertyValueResponse (Prelude.HashMap Prelude.Text PropertyLatestValue)
getPropertyValueResponse_propertyValues = Lens.lens (\GetPropertyValueResponse' {propertyValues} -> propertyValues) (\s@GetPropertyValueResponse' {} a -> s {propertyValues = a} :: GetPropertyValueResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetPropertyValueResponse where
  rnf GetPropertyValueResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf propertyValues
