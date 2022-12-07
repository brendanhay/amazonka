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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    listConnectorEntities_entitiesPath,
    listConnectorEntities_apiVersion,
    listConnectorEntities_connectorProfileName,
    listConnectorEntities_connectorType,

    -- * Destructuring the Response
    ListConnectorEntitiesResponse (..),
    newListConnectorEntitiesResponse,

    -- * Response Lenses
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
  { -- | This optional parameter is specific to connector implementation. Some
    -- connectors support multiple levels or categories of entities. You can
    -- find out the list of roots for such providers by sending a request
    -- without the @entitiesPath@ parameter. If the connector supports entities
    -- at different roots, this initial request returns the list of roots.
    -- Otherwise, this request returns all entities supported by the provider.
    entitiesPath :: Prelude.Maybe Prelude.Text,
    -- | The version of the API that\'s used by the connector.
    apiVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector profile. The name is unique for each
    -- @ConnectorProfile@ in the Amazon Web Services account, and is used to
    -- query the downstream connector.
    connectorProfileName :: Prelude.Maybe Prelude.Text,
    -- | The type of connector, such as Salesforce, Amplitude, and so on.
    connectorType :: Prelude.Maybe ConnectorType
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
-- 'entitiesPath', 'listConnectorEntities_entitiesPath' - This optional parameter is specific to connector implementation. Some
-- connectors support multiple levels or categories of entities. You can
-- find out the list of roots for such providers by sending a request
-- without the @entitiesPath@ parameter. If the connector supports entities
-- at different roots, this initial request returns the list of roots.
-- Otherwise, this request returns all entities supported by the provider.
--
-- 'apiVersion', 'listConnectorEntities_apiVersion' - The version of the API that\'s used by the connector.
--
-- 'connectorProfileName', 'listConnectorEntities_connectorProfileName' - The name of the connector profile. The name is unique for each
-- @ConnectorProfile@ in the Amazon Web Services account, and is used to
-- query the downstream connector.
--
-- 'connectorType', 'listConnectorEntities_connectorType' - The type of connector, such as Salesforce, Amplitude, and so on.
newListConnectorEntities ::
  ListConnectorEntities
newListConnectorEntities =
  ListConnectorEntities'
    { entitiesPath =
        Prelude.Nothing,
      apiVersion = Prelude.Nothing,
      connectorProfileName = Prelude.Nothing,
      connectorType = Prelude.Nothing
    }

-- | This optional parameter is specific to connector implementation. Some
-- connectors support multiple levels or categories of entities. You can
-- find out the list of roots for such providers by sending a request
-- without the @entitiesPath@ parameter. If the connector supports entities
-- at different roots, this initial request returns the list of roots.
-- Otherwise, this request returns all entities supported by the provider.
listConnectorEntities_entitiesPath :: Lens.Lens' ListConnectorEntities (Prelude.Maybe Prelude.Text)
listConnectorEntities_entitiesPath = Lens.lens (\ListConnectorEntities' {entitiesPath} -> entitiesPath) (\s@ListConnectorEntities' {} a -> s {entitiesPath = a} :: ListConnectorEntities)

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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "connectorEntityMap"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListConnectorEntities where
  hashWithSalt _salt ListConnectorEntities' {..} =
    _salt `Prelude.hashWithSalt` entitiesPath
      `Prelude.hashWithSalt` apiVersion
      `Prelude.hashWithSalt` connectorProfileName
      `Prelude.hashWithSalt` connectorType

instance Prelude.NFData ListConnectorEntities where
  rnf ListConnectorEntities' {..} =
    Prelude.rnf entitiesPath
      `Prelude.seq` Prelude.rnf apiVersion
      `Prelude.seq` Prelude.rnf connectorProfileName
      `Prelude.seq` Prelude.rnf connectorType

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
          [ ("entitiesPath" Data..=) Prelude.<$> entitiesPath,
            ("apiVersion" Data..=) Prelude.<$> apiVersion,
            ("connectorProfileName" Data..=)
              Prelude.<$> connectorProfileName,
            ("connectorType" Data..=) Prelude.<$> connectorType
          ]
      )

instance Data.ToPath ListConnectorEntities where
  toPath = Prelude.const "/list-connector-entities"

instance Data.ToQuery ListConnectorEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConnectorEntitiesResponse' smart constructor.
data ListConnectorEntitiesResponse = ListConnectorEntitiesResponse'
  { -- | The response's http status code.
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
    { httpStatus =
        pHttpStatus_,
      connectorEntityMap = Prelude.mempty
    }

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
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf connectorEntityMap
