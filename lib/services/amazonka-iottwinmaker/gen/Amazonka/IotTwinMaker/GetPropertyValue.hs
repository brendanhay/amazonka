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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    getPropertyValue_componentName,
    getPropertyValue_componentTypeId,
    getPropertyValue_entityId,
    getPropertyValue_maxResults,
    getPropertyValue_nextToken,
    getPropertyValue_propertyGroupName,
    getPropertyValue_tabularConditions,
    getPropertyValue_selectedProperties,
    getPropertyValue_workspaceId,

    -- * Destructuring the Response
    GetPropertyValueResponse (..),
    newGetPropertyValueResponse,

    -- * Response Lenses
    getPropertyValueResponse_nextToken,
    getPropertyValueResponse_propertyValues,
    getPropertyValueResponse_tabularPropertyValues,
    getPropertyValueResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPropertyValue' smart constructor.
data GetPropertyValue = GetPropertyValue'
  { -- | The name of the component whose property values the operation returns.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the component type whose property values the operation
    -- returns.
    componentTypeId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the entity whose property values the operation returns.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    --
    -- Valid Range: Minimum value of 1. Maximum value of 250.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The property group name.
    propertyGroupName :: Prelude.Maybe Prelude.Text,
    -- | The tabular conditions.
    tabularConditions :: Prelude.Maybe TabularConditions,
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
-- 'componentName', 'getPropertyValue_componentName' - The name of the component whose property values the operation returns.
--
-- 'componentTypeId', 'getPropertyValue_componentTypeId' - The ID of the component type whose property values the operation
-- returns.
--
-- 'entityId', 'getPropertyValue_entityId' - The ID of the entity whose property values the operation returns.
--
-- 'maxResults', 'getPropertyValue_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
--
-- 'nextToken', 'getPropertyValue_nextToken' - The string that specifies the next page of results.
--
-- 'propertyGroupName', 'getPropertyValue_propertyGroupName' - The property group name.
--
-- 'tabularConditions', 'getPropertyValue_tabularConditions' - The tabular conditions.
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
      { componentName = Prelude.Nothing,
        componentTypeId = Prelude.Nothing,
        entityId = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        propertyGroupName = Prelude.Nothing,
        tabularConditions = Prelude.Nothing,
        selectedProperties =
          Lens.coerced Lens.# pSelectedProperties_,
        workspaceId = pWorkspaceId_
      }

-- | The name of the component whose property values the operation returns.
getPropertyValue_componentName :: Lens.Lens' GetPropertyValue (Prelude.Maybe Prelude.Text)
getPropertyValue_componentName = Lens.lens (\GetPropertyValue' {componentName} -> componentName) (\s@GetPropertyValue' {} a -> s {componentName = a} :: GetPropertyValue)

-- | The ID of the component type whose property values the operation
-- returns.
getPropertyValue_componentTypeId :: Lens.Lens' GetPropertyValue (Prelude.Maybe Prelude.Text)
getPropertyValue_componentTypeId = Lens.lens (\GetPropertyValue' {componentTypeId} -> componentTypeId) (\s@GetPropertyValue' {} a -> s {componentTypeId = a} :: GetPropertyValue)

-- | The ID of the entity whose property values the operation returns.
getPropertyValue_entityId :: Lens.Lens' GetPropertyValue (Prelude.Maybe Prelude.Text)
getPropertyValue_entityId = Lens.lens (\GetPropertyValue' {entityId} -> entityId) (\s@GetPropertyValue' {} a -> s {entityId = a} :: GetPropertyValue)

-- | The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
getPropertyValue_maxResults :: Lens.Lens' GetPropertyValue (Prelude.Maybe Prelude.Natural)
getPropertyValue_maxResults = Lens.lens (\GetPropertyValue' {maxResults} -> maxResults) (\s@GetPropertyValue' {} a -> s {maxResults = a} :: GetPropertyValue)

-- | The string that specifies the next page of results.
getPropertyValue_nextToken :: Lens.Lens' GetPropertyValue (Prelude.Maybe Prelude.Text)
getPropertyValue_nextToken = Lens.lens (\GetPropertyValue' {nextToken} -> nextToken) (\s@GetPropertyValue' {} a -> s {nextToken = a} :: GetPropertyValue)

-- | The property group name.
getPropertyValue_propertyGroupName :: Lens.Lens' GetPropertyValue (Prelude.Maybe Prelude.Text)
getPropertyValue_propertyGroupName = Lens.lens (\GetPropertyValue' {propertyGroupName} -> propertyGroupName) (\s@GetPropertyValue' {} a -> s {propertyGroupName = a} :: GetPropertyValue)

-- | The tabular conditions.
getPropertyValue_tabularConditions :: Lens.Lens' GetPropertyValue (Prelude.Maybe TabularConditions)
getPropertyValue_tabularConditions = Lens.lens (\GetPropertyValue' {tabularConditions} -> tabularConditions) (\s@GetPropertyValue' {} a -> s {tabularConditions = a} :: GetPropertyValue)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPropertyValueResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "propertyValues" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "tabularPropertyValues"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPropertyValue where
  hashWithSalt _salt GetPropertyValue' {..} =
    _salt
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` componentTypeId
      `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` propertyGroupName
      `Prelude.hashWithSalt` tabularConditions
      `Prelude.hashWithSalt` selectedProperties
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData GetPropertyValue where
  rnf GetPropertyValue' {..} =
    Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf componentTypeId
      `Prelude.seq` Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf propertyGroupName
      `Prelude.seq` Prelude.rnf tabularConditions
      `Prelude.seq` Prelude.rnf selectedProperties
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders GetPropertyValue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPropertyValue where
  toJSON GetPropertyValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("componentName" Data..=) Prelude.<$> componentName,
            ("componentTypeId" Data..=)
              Prelude.<$> componentTypeId,
            ("entityId" Data..=) Prelude.<$> entityId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("propertyGroupName" Data..=)
              Prelude.<$> propertyGroupName,
            ("tabularConditions" Data..=)
              Prelude.<$> tabularConditions,
            Prelude.Just
              ("selectedProperties" Data..= selectedProperties)
          ]
      )

instance Data.ToPath GetPropertyValue where
  toPath GetPropertyValue' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/entity-properties/value"
      ]

instance Data.ToQuery GetPropertyValue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPropertyValueResponse' smart constructor.
data GetPropertyValueResponse = GetPropertyValueResponse'
  { -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object that maps strings to the properties and latest property values
    -- in the response. Each string in the mapping must be unique to this
    -- object.
    propertyValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyLatestValue),
    -- | A table of property values.
    tabularPropertyValues :: Prelude.Maybe [[Prelude.HashMap Prelude.Text DataValue]],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'getPropertyValueResponse_nextToken' - The string that specifies the next page of results.
--
-- 'propertyValues', 'getPropertyValueResponse_propertyValues' - An object that maps strings to the properties and latest property values
-- in the response. Each string in the mapping must be unique to this
-- object.
--
-- 'tabularPropertyValues', 'getPropertyValueResponse_tabularPropertyValues' - A table of property values.
--
-- 'httpStatus', 'getPropertyValueResponse_httpStatus' - The response's http status code.
newGetPropertyValueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPropertyValueResponse
newGetPropertyValueResponse pHttpStatus_ =
  GetPropertyValueResponse'
    { nextToken =
        Prelude.Nothing,
      propertyValues = Prelude.Nothing,
      tabularPropertyValues = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that specifies the next page of results.
getPropertyValueResponse_nextToken :: Lens.Lens' GetPropertyValueResponse (Prelude.Maybe Prelude.Text)
getPropertyValueResponse_nextToken = Lens.lens (\GetPropertyValueResponse' {nextToken} -> nextToken) (\s@GetPropertyValueResponse' {} a -> s {nextToken = a} :: GetPropertyValueResponse)

-- | An object that maps strings to the properties and latest property values
-- in the response. Each string in the mapping must be unique to this
-- object.
getPropertyValueResponse_propertyValues :: Lens.Lens' GetPropertyValueResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyLatestValue))
getPropertyValueResponse_propertyValues = Lens.lens (\GetPropertyValueResponse' {propertyValues} -> propertyValues) (\s@GetPropertyValueResponse' {} a -> s {propertyValues = a} :: GetPropertyValueResponse) Prelude.. Lens.mapping Lens.coerced

-- | A table of property values.
getPropertyValueResponse_tabularPropertyValues :: Lens.Lens' GetPropertyValueResponse (Prelude.Maybe [[Prelude.HashMap Prelude.Text DataValue]])
getPropertyValueResponse_tabularPropertyValues = Lens.lens (\GetPropertyValueResponse' {tabularPropertyValues} -> tabularPropertyValues) (\s@GetPropertyValueResponse' {} a -> s {tabularPropertyValues = a} :: GetPropertyValueResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getPropertyValueResponse_httpStatus :: Lens.Lens' GetPropertyValueResponse Prelude.Int
getPropertyValueResponse_httpStatus = Lens.lens (\GetPropertyValueResponse' {httpStatus} -> httpStatus) (\s@GetPropertyValueResponse' {} a -> s {httpStatus = a} :: GetPropertyValueResponse)

instance Prelude.NFData GetPropertyValueResponse where
  rnf GetPropertyValueResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf propertyValues
      `Prelude.seq` Prelude.rnf tabularPropertyValues
      `Prelude.seq` Prelude.rnf httpStatus
