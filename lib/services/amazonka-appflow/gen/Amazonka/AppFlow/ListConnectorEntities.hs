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
-- Module      : Amazonka.AppFlow.ListConnectorEntities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of available connector entities supported by Amazon
-- AppFlow. For example, you can query Salesforce for /Account/ and
-- /Opportunity/ entities, or query ServiceNow for the /Incident/ entity.
module Amazonka.AppFlow.ListConnectorEntities
  ( -- * Creating a Request
    ListConnectorEntities (..),
    newListConnectorEntities,

    -- * Request Lenses
    listConnectorEntities_apiVersion,
    listConnectorEntities_connectorProfileName,
    listConnectorEntities_connectorType,
    listConnectorEntities_entitiesPath,
    listConnectorEntities_maxResults,
    listConnectorEntities_nextToken,

    -- * Destructuring the Response
    ListConnectorEntitiesResponse (..),
    newListConnectorEntitiesResponse,

    -- * Response Lenses
    listConnectorEntitiesResponse_nextToken,
    listConnectorEntitiesResponse_httpStatus,
    listConnectorEntitiesResponse_connectorEntityMap,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConnectorEntities' smart constructor.
data ListConnectorEntities = ListConnectorEntities'
  { -- | The version of the API that\'s used by the connector.
    apiVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector profile. The name is unique for each
    -- @ConnectorProfile@ in the Amazon Web Services account, and is used to
    -- query the downstream connector.
    connectorProfileName :: Prelude.Maybe Prelude.Text,
    -- | The type of connector, such as Salesforce, Amplitude, and so on.
    connectorType :: Prelude.Maybe ConnectorType,
    -- | This optional parameter is specific to connector implementation. Some
    -- connectors support multiple levels or categories of entities. You can
    -- find out the list of roots for such providers by sending a request
    -- without the @entitiesPath@ parameter. If the connector supports entities
    -- at different roots, this initial request returns the list of roots.
    -- Otherwise, this request returns all entities supported by the provider.
    entitiesPath :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items that the operation returns in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that was provided by your prior @ListConnectorEntities@
    -- operation if the response was too big for the page size. You specify
    -- this token to get the next page of results in paginated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectorEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiVersion', 'listConnectorEntities_apiVersion' - The version of the API that\'s used by the connector.
--
-- 'connectorProfileName', 'listConnectorEntities_connectorProfileName' - The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account, and is used to
-- query the downstream connector.
--
-- 'connectorType', 'listConnectorEntities_connectorType' - The type of connector, such as Salesforce, Amplitude, and so on.
--
-- 'entitiesPath', 'listConnectorEntities_entitiesPath' - This optional parameter is specific to connector implementation. Some
-- connectors support multiple levels or categories of entities. You can
-- find out the list of roots for such providers by sending a request
-- without the @entitiesPath@ parameter. If the connector supports entities
-- at different roots, this initial request returns the list of roots.
-- Otherwise, this request returns all entities supported by the provider.
--
-- 'maxResults', 'listConnectorEntities_maxResults' - The maximum number of items that the operation returns in the response.
--
-- 'nextToken', 'listConnectorEntities_nextToken' - A token that was provided by your prior @ListConnectorEntities@
-- operation if the response was too big for the page size. You specify
-- this token to get the next page of results in paginated response.
newListConnectorEntities ::
  ListConnectorEntities
newListConnectorEntities =
  ListConnectorEntities'
    { apiVersion =
        Prelude.Nothing,
      connectorProfileName = Prelude.Nothing,
      connectorType = Prelude.Nothing,
      entitiesPath = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The version of the API that\'s used by the connector.
listConnectorEntities_apiVersion :: Lens.Lens' ListConnectorEntities (Prelude.Maybe Prelude.Text)
listConnectorEntities_apiVersion = Lens.lens (\ListConnectorEntities' {apiVersion} -> apiVersion) (\s@ListConnectorEntities' {} a -> s {apiVersion = a} :: ListConnectorEntities)

-- | The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account, and is used to
-- query the downstream connector.
listConnectorEntities_connectorProfileName :: Lens.Lens' ListConnectorEntities (Prelude.Maybe Prelude.Text)
listConnectorEntities_connectorProfileName = Lens.lens (\ListConnectorEntities' {connectorProfileName} -> connectorProfileName) (\s@ListConnectorEntities' {} a -> s {connectorProfileName = a} :: ListConnectorEntities)

-- | The type of connector, such as Salesforce, Amplitude, and so on.
listConnectorEntities_connectorType :: Lens.Lens' ListConnectorEntities (Prelude.Maybe ConnectorType)
listConnectorEntities_connectorType = Lens.lens (\ListConnectorEntities' {connectorType} -> connectorType) (\s@ListConnectorEntities' {} a -> s {connectorType = a} :: ListConnectorEntities)

-- | This optional parameter is specific to connector implementation. Some
-- connectors support multiple levels or categories of entities. You can
-- find out the list of roots for such providers by sending a request
-- without the @entitiesPath@ parameter. If the connector supports entities
-- at different roots, this initial request returns the list of roots.
-- Otherwise, this request returns all entities supported by the provider.
listConnectorEntities_entitiesPath :: Lens.Lens' ListConnectorEntities (Prelude.Maybe Prelude.Text)
listConnectorEntities_entitiesPath = Lens.lens (\ListConnectorEntities' {entitiesPath} -> entitiesPath) (\s@ListConnectorEntities' {} a -> s {entitiesPath = a} :: ListConnectorEntities)

-- | The maximum number of items that the operation returns in the response.
listConnectorEntities_maxResults :: Lens.Lens' ListConnectorEntities (Prelude.Maybe Prelude.Natural)
listConnectorEntities_maxResults = Lens.lens (\ListConnectorEntities' {maxResults} -> maxResults) (\s@ListConnectorEntities' {} a -> s {maxResults = a} :: ListConnectorEntities)

-- | A token that was provided by your prior @ListConnectorEntities@
-- operation if the response was too big for the page size. You specify
-- this token to get the next page of results in paginated response.
listConnectorEntities_nextToken :: Lens.Lens' ListConnectorEntities (Prelude.Maybe Prelude.Text)
listConnectorEntities_nextToken = Lens.lens (\ListConnectorEntities' {nextToken} -> nextToken) (\s@ListConnectorEntities' {} a -> s {nextToken = a} :: ListConnectorEntities)

instance Core.AWSRequest ListConnectorEntities where
  type
    AWSResponse ListConnectorEntities =
      ListConnectorEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConnectorEntitiesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "connectorEntityMap"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListConnectorEntities where
  hashWithSalt _salt ListConnectorEntities' {..} =
    _salt
      `Prelude.hashWithSalt` apiVersion
      `Prelude.hashWithSalt` connectorProfileName
      `Prelude.hashWithSalt` connectorType
      `Prelude.hashWithSalt` entitiesPath
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListConnectorEntities where
  rnf ListConnectorEntities' {..} =
    Prelude.rnf apiVersion
      `Prelude.seq` Prelude.rnf connectorProfileName
      `Prelude.seq` Prelude.rnf connectorType
      `Prelude.seq` Prelude.rnf entitiesPath
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListConnectorEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListConnectorEntities where
  toJSON ListConnectorEntities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("apiVersion" Data..=) Prelude.<$> apiVersion,
            ("connectorProfileName" Data..=)
              Prelude.<$> connectorProfileName,
            ("connectorType" Data..=) Prelude.<$> connectorType,
            ("entitiesPath" Data..=) Prelude.<$> entitiesPath,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListConnectorEntities where
  toPath = Prelude.const "/list-connector-entities"

instance Data.ToQuery ListConnectorEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConnectorEntitiesResponse' smart constructor.
data ListConnectorEntitiesResponse = ListConnectorEntitiesResponse'
  { -- | A token that you specify in your next @ListConnectorEntities@ operation
    -- to get the next page of results in paginated response. The
    -- @ListConnectorEntities@ operation provides this token if the response is
    -- too big for the page size.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The response of @ListConnectorEntities@ lists entities grouped by
    -- category. This map\'s key represents the group name, and its value
    -- contains the list of entities belonging to that group.
    connectorEntityMap :: Prelude.HashMap Prelude.Text [ConnectorEntity]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConnectorEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConnectorEntitiesResponse_nextToken' - A token that you specify in your next @ListConnectorEntities@ operation
-- to get the next page of results in paginated response. The
-- @ListConnectorEntities@ operation provides this token if the response is
-- too big for the page size.
--
-- 'httpStatus', 'listConnectorEntitiesResponse_httpStatus' - The response's http status code.
--
-- 'connectorEntityMap', 'listConnectorEntitiesResponse_connectorEntityMap' - The response of @ListConnectorEntities@ lists entities grouped by
-- category. This map\'s key represents the group name, and its value
-- contains the list of entities belonging to that group.
newListConnectorEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConnectorEntitiesResponse
newListConnectorEntitiesResponse pHttpStatus_ =
  ListConnectorEntitiesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      connectorEntityMap = Prelude.mempty
    }

-- | A token that you specify in your next @ListConnectorEntities@ operation
-- to get the next page of results in paginated response. The
-- @ListConnectorEntities@ operation provides this token if the response is
-- too big for the page size.
listConnectorEntitiesResponse_nextToken :: Lens.Lens' ListConnectorEntitiesResponse (Prelude.Maybe Prelude.Text)
listConnectorEntitiesResponse_nextToken = Lens.lens (\ListConnectorEntitiesResponse' {nextToken} -> nextToken) (\s@ListConnectorEntitiesResponse' {} a -> s {nextToken = a} :: ListConnectorEntitiesResponse)

-- | The response's http status code.
listConnectorEntitiesResponse_httpStatus :: Lens.Lens' ListConnectorEntitiesResponse Prelude.Int
listConnectorEntitiesResponse_httpStatus = Lens.lens (\ListConnectorEntitiesResponse' {httpStatus} -> httpStatus) (\s@ListConnectorEntitiesResponse' {} a -> s {httpStatus = a} :: ListConnectorEntitiesResponse)

-- | The response of @ListConnectorEntities@ lists entities grouped by
-- category. This map\'s key represents the group name, and its value
-- contains the list of entities belonging to that group.
listConnectorEntitiesResponse_connectorEntityMap :: Lens.Lens' ListConnectorEntitiesResponse (Prelude.HashMap Prelude.Text [ConnectorEntity])
listConnectorEntitiesResponse_connectorEntityMap = Lens.lens (\ListConnectorEntitiesResponse' {connectorEntityMap} -> connectorEntityMap) (\s@ListConnectorEntitiesResponse' {} a -> s {connectorEntityMap = a} :: ListConnectorEntitiesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListConnectorEntitiesResponse where
  rnf ListConnectorEntitiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf connectorEntityMap
