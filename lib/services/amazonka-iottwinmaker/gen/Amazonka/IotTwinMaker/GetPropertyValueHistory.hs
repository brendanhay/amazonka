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
-- Module      : Amazonka.IotTwinMaker.GetPropertyValueHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the history of a time series property value
-- for a component, component type, entity, or workspace.
--
-- You must specify a value for @workspaceId@. For entity-specific queries,
-- specify values for @componentName@ and @entityId@. For cross-entity
-- quries, specify a value for @componentTypeId@.
module Amazonka.IotTwinMaker.GetPropertyValueHistory
  ( -- * Creating a Request
    GetPropertyValueHistory (..),
    newGetPropertyValueHistory,

    -- * Request Lenses
    getPropertyValueHistory_entityId,
    getPropertyValueHistory_nextToken,
    getPropertyValueHistory_componentName,
    getPropertyValueHistory_propertyFilters,
    getPropertyValueHistory_startDateTime,
    getPropertyValueHistory_endTime,
    getPropertyValueHistory_maxResults,
    getPropertyValueHistory_interpolation,
    getPropertyValueHistory_orderByTime,
    getPropertyValueHistory_startTime,
    getPropertyValueHistory_endDateTime,
    getPropertyValueHistory_componentTypeId,
    getPropertyValueHistory_workspaceId,
    getPropertyValueHistory_selectedProperties,

    -- * Destructuring the Response
    GetPropertyValueHistoryResponse (..),
    newGetPropertyValueHistoryResponse,

    -- * Response Lenses
    getPropertyValueHistoryResponse_nextToken,
    getPropertyValueHistoryResponse_httpStatus,
    getPropertyValueHistoryResponse_propertyValues,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPropertyValueHistory' smart constructor.
data GetPropertyValueHistory = GetPropertyValueHistory'
  { -- | The ID of the entity.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | A list of objects that filter the property value history request.
    propertyFilters :: Prelude.Maybe (Prelude.NonEmpty PropertyFilter),
    -- | The date and time of the earliest property value to return.
    startDateTime :: Prelude.Maybe Data.POSIX,
    -- | The ISO8601 DateTime of the latest property value to return.
    --
    -- For more information about the ISO8601 DateTime format, see the data
    -- type
    -- <https://docs.aws.amazon.com/iot-twinmaker/latest/apireference/API_PropertyValue.html PropertyValue>.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    --
    -- Valid Range: Minimum value of 1. Maximum value of 250.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An object that specifies the interpolation type and the interval over
    -- which to interpolate data.
    interpolation :: Prelude.Maybe InterpolationParameters,
    -- | The time direction to use in the result order.
    orderByTime :: Prelude.Maybe OrderByTime,
    -- | The ISO8601 DateTime of the earliest property value to return.
    --
    -- For more information about the ISO8601 DateTime format, see the data
    -- type
    -- <https://docs.aws.amazon.com/iot-twinmaker/latest/apireference/API_PropertyValue.html PropertyValue>.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The date and time of the latest property value to return.
    endDateTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text,
    -- | A list of properties whose value histories the request retrieves.
    selectedProperties :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPropertyValueHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityId', 'getPropertyValueHistory_entityId' - The ID of the entity.
--
-- 'nextToken', 'getPropertyValueHistory_nextToken' - The string that specifies the next page of results.
--
-- 'componentName', 'getPropertyValueHistory_componentName' - The name of the component.
--
-- 'propertyFilters', 'getPropertyValueHistory_propertyFilters' - A list of objects that filter the property value history request.
--
-- 'startDateTime', 'getPropertyValueHistory_startDateTime' - The date and time of the earliest property value to return.
--
-- 'endTime', 'getPropertyValueHistory_endTime' - The ISO8601 DateTime of the latest property value to return.
--
-- For more information about the ISO8601 DateTime format, see the data
-- type
-- <https://docs.aws.amazon.com/iot-twinmaker/latest/apireference/API_PropertyValue.html PropertyValue>.
--
-- 'maxResults', 'getPropertyValueHistory_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
--
-- 'interpolation', 'getPropertyValueHistory_interpolation' - An object that specifies the interpolation type and the interval over
-- which to interpolate data.
--
-- 'orderByTime', 'getPropertyValueHistory_orderByTime' - The time direction to use in the result order.
--
-- 'startTime', 'getPropertyValueHistory_startTime' - The ISO8601 DateTime of the earliest property value to return.
--
-- For more information about the ISO8601 DateTime format, see the data
-- type
-- <https://docs.aws.amazon.com/iot-twinmaker/latest/apireference/API_PropertyValue.html PropertyValue>.
--
-- 'endDateTime', 'getPropertyValueHistory_endDateTime' - The date and time of the latest property value to return.
--
-- 'componentTypeId', 'getPropertyValueHistory_componentTypeId' - The ID of the component type.
--
-- 'workspaceId', 'getPropertyValueHistory_workspaceId' - The ID of the workspace.
--
-- 'selectedProperties', 'getPropertyValueHistory_selectedProperties' - A list of properties whose value histories the request retrieves.
newGetPropertyValueHistory ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'selectedProperties'
  Prelude.NonEmpty Prelude.Text ->
  GetPropertyValueHistory
newGetPropertyValueHistory
  pWorkspaceId_
  pSelectedProperties_ =
    GetPropertyValueHistory'
      { entityId =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        componentName = Prelude.Nothing,
        propertyFilters = Prelude.Nothing,
        startDateTime = Prelude.Nothing,
        endTime = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        interpolation = Prelude.Nothing,
        orderByTime = Prelude.Nothing,
        startTime = Prelude.Nothing,
        endDateTime = Prelude.Nothing,
        componentTypeId = Prelude.Nothing,
        workspaceId = pWorkspaceId_,
        selectedProperties =
          Lens.coerced Lens.# pSelectedProperties_
      }

-- | The ID of the entity.
getPropertyValueHistory_entityId :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getPropertyValueHistory_entityId = Lens.lens (\GetPropertyValueHistory' {entityId} -> entityId) (\s@GetPropertyValueHistory' {} a -> s {entityId = a} :: GetPropertyValueHistory)

-- | The string that specifies the next page of results.
getPropertyValueHistory_nextToken :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getPropertyValueHistory_nextToken = Lens.lens (\GetPropertyValueHistory' {nextToken} -> nextToken) (\s@GetPropertyValueHistory' {} a -> s {nextToken = a} :: GetPropertyValueHistory)

-- | The name of the component.
getPropertyValueHistory_componentName :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getPropertyValueHistory_componentName = Lens.lens (\GetPropertyValueHistory' {componentName} -> componentName) (\s@GetPropertyValueHistory' {} a -> s {componentName = a} :: GetPropertyValueHistory)

-- | A list of objects that filter the property value history request.
getPropertyValueHistory_propertyFilters :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe (Prelude.NonEmpty PropertyFilter))
getPropertyValueHistory_propertyFilters = Lens.lens (\GetPropertyValueHistory' {propertyFilters} -> propertyFilters) (\s@GetPropertyValueHistory' {} a -> s {propertyFilters = a} :: GetPropertyValueHistory) Prelude.. Lens.mapping Lens.coerced

-- | The date and time of the earliest property value to return.
getPropertyValueHistory_startDateTime :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe Prelude.UTCTime)
getPropertyValueHistory_startDateTime = Lens.lens (\GetPropertyValueHistory' {startDateTime} -> startDateTime) (\s@GetPropertyValueHistory' {} a -> s {startDateTime = a} :: GetPropertyValueHistory) Prelude.. Lens.mapping Data._Time

-- | The ISO8601 DateTime of the latest property value to return.
--
-- For more information about the ISO8601 DateTime format, see the data
-- type
-- <https://docs.aws.amazon.com/iot-twinmaker/latest/apireference/API_PropertyValue.html PropertyValue>.
getPropertyValueHistory_endTime :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getPropertyValueHistory_endTime = Lens.lens (\GetPropertyValueHistory' {endTime} -> endTime) (\s@GetPropertyValueHistory' {} a -> s {endTime = a} :: GetPropertyValueHistory)

-- | The maximum number of results to return at one time. The default is 25.
--
-- Valid Range: Minimum value of 1. Maximum value of 250.
getPropertyValueHistory_maxResults :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe Prelude.Natural)
getPropertyValueHistory_maxResults = Lens.lens (\GetPropertyValueHistory' {maxResults} -> maxResults) (\s@GetPropertyValueHistory' {} a -> s {maxResults = a} :: GetPropertyValueHistory)

-- | An object that specifies the interpolation type and the interval over
-- which to interpolate data.
getPropertyValueHistory_interpolation :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe InterpolationParameters)
getPropertyValueHistory_interpolation = Lens.lens (\GetPropertyValueHistory' {interpolation} -> interpolation) (\s@GetPropertyValueHistory' {} a -> s {interpolation = a} :: GetPropertyValueHistory)

-- | The time direction to use in the result order.
getPropertyValueHistory_orderByTime :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe OrderByTime)
getPropertyValueHistory_orderByTime = Lens.lens (\GetPropertyValueHistory' {orderByTime} -> orderByTime) (\s@GetPropertyValueHistory' {} a -> s {orderByTime = a} :: GetPropertyValueHistory)

-- | The ISO8601 DateTime of the earliest property value to return.
--
-- For more information about the ISO8601 DateTime format, see the data
-- type
-- <https://docs.aws.amazon.com/iot-twinmaker/latest/apireference/API_PropertyValue.html PropertyValue>.
getPropertyValueHistory_startTime :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getPropertyValueHistory_startTime = Lens.lens (\GetPropertyValueHistory' {startTime} -> startTime) (\s@GetPropertyValueHistory' {} a -> s {startTime = a} :: GetPropertyValueHistory)

-- | The date and time of the latest property value to return.
getPropertyValueHistory_endDateTime :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe Prelude.UTCTime)
getPropertyValueHistory_endDateTime = Lens.lens (\GetPropertyValueHistory' {endDateTime} -> endDateTime) (\s@GetPropertyValueHistory' {} a -> s {endDateTime = a} :: GetPropertyValueHistory) Prelude.. Lens.mapping Data._Time

-- | The ID of the component type.
getPropertyValueHistory_componentTypeId :: Lens.Lens' GetPropertyValueHistory (Prelude.Maybe Prelude.Text)
getPropertyValueHistory_componentTypeId = Lens.lens (\GetPropertyValueHistory' {componentTypeId} -> componentTypeId) (\s@GetPropertyValueHistory' {} a -> s {componentTypeId = a} :: GetPropertyValueHistory)

-- | The ID of the workspace.
getPropertyValueHistory_workspaceId :: Lens.Lens' GetPropertyValueHistory Prelude.Text
getPropertyValueHistory_workspaceId = Lens.lens (\GetPropertyValueHistory' {workspaceId} -> workspaceId) (\s@GetPropertyValueHistory' {} a -> s {workspaceId = a} :: GetPropertyValueHistory)

-- | A list of properties whose value histories the request retrieves.
getPropertyValueHistory_selectedProperties :: Lens.Lens' GetPropertyValueHistory (Prelude.NonEmpty Prelude.Text)
getPropertyValueHistory_selectedProperties = Lens.lens (\GetPropertyValueHistory' {selectedProperties} -> selectedProperties) (\s@GetPropertyValueHistory' {} a -> s {selectedProperties = a} :: GetPropertyValueHistory) Prelude.. Lens.coerced

instance Core.AWSRequest GetPropertyValueHistory where
  type
    AWSResponse GetPropertyValueHistory =
      GetPropertyValueHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPropertyValueHistoryResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "propertyValues"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetPropertyValueHistory where
  hashWithSalt _salt GetPropertyValueHistory' {..} =
    _salt `Prelude.hashWithSalt` entityId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` propertyFilters
      `Prelude.hashWithSalt` startDateTime
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` interpolation
      `Prelude.hashWithSalt` orderByTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` componentTypeId
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` selectedProperties

instance Prelude.NFData GetPropertyValueHistory where
  rnf GetPropertyValueHistory' {..} =
    Prelude.rnf entityId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf propertyFilters
      `Prelude.seq` Prelude.rnf startDateTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf interpolation
      `Prelude.seq` Prelude.rnf orderByTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endDateTime
      `Prelude.seq` Prelude.rnf componentTypeId
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf selectedProperties

instance Data.ToHeaders GetPropertyValueHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPropertyValueHistory where
  toJSON GetPropertyValueHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("entityId" Data..=) Prelude.<$> entityId,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("componentName" Data..=) Prelude.<$> componentName,
            ("propertyFilters" Data..=)
              Prelude.<$> propertyFilters,
            ("startDateTime" Data..=) Prelude.<$> startDateTime,
            ("endTime" Data..=) Prelude.<$> endTime,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("interpolation" Data..=) Prelude.<$> interpolation,
            ("orderByTime" Data..=) Prelude.<$> orderByTime,
            ("startTime" Data..=) Prelude.<$> startTime,
            ("endDateTime" Data..=) Prelude.<$> endDateTime,
            ("componentTypeId" Data..=)
              Prelude.<$> componentTypeId,
            Prelude.Just
              ("selectedProperties" Data..= selectedProperties)
          ]
      )

instance Data.ToPath GetPropertyValueHistory where
  toPath GetPropertyValueHistory' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/entity-properties/history"
      ]

instance Data.ToQuery GetPropertyValueHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPropertyValueHistoryResponse' smart constructor.
data GetPropertyValueHistoryResponse = GetPropertyValueHistoryResponse'
  { -- | The string that specifies the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that maps strings to the property definitions in the component
    -- type. Each string in the mapping must be unique to this object.
    propertyValues :: [PropertyValueHistory]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPropertyValueHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getPropertyValueHistoryResponse_nextToken' - The string that specifies the next page of results.
--
-- 'httpStatus', 'getPropertyValueHistoryResponse_httpStatus' - The response's http status code.
--
-- 'propertyValues', 'getPropertyValueHistoryResponse_propertyValues' - An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
newGetPropertyValueHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPropertyValueHistoryResponse
newGetPropertyValueHistoryResponse pHttpStatus_ =
  GetPropertyValueHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      propertyValues = Prelude.mempty
    }

-- | The string that specifies the next page of results.
getPropertyValueHistoryResponse_nextToken :: Lens.Lens' GetPropertyValueHistoryResponse (Prelude.Maybe Prelude.Text)
getPropertyValueHistoryResponse_nextToken = Lens.lens (\GetPropertyValueHistoryResponse' {nextToken} -> nextToken) (\s@GetPropertyValueHistoryResponse' {} a -> s {nextToken = a} :: GetPropertyValueHistoryResponse)

-- | The response's http status code.
getPropertyValueHistoryResponse_httpStatus :: Lens.Lens' GetPropertyValueHistoryResponse Prelude.Int
getPropertyValueHistoryResponse_httpStatus = Lens.lens (\GetPropertyValueHistoryResponse' {httpStatus} -> httpStatus) (\s@GetPropertyValueHistoryResponse' {} a -> s {httpStatus = a} :: GetPropertyValueHistoryResponse)

-- | An object that maps strings to the property definitions in the component
-- type. Each string in the mapping must be unique to this object.
getPropertyValueHistoryResponse_propertyValues :: Lens.Lens' GetPropertyValueHistoryResponse [PropertyValueHistory]
getPropertyValueHistoryResponse_propertyValues = Lens.lens (\GetPropertyValueHistoryResponse' {propertyValues} -> propertyValues) (\s@GetPropertyValueHistoryResponse' {} a -> s {propertyValues = a} :: GetPropertyValueHistoryResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetPropertyValueHistoryResponse
  where
  rnf GetPropertyValueHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf propertyValues
