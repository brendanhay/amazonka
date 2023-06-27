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
-- Module      : Amazonka.AppFlow.ResetConnectorMetadataCache
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets metadata about your connector entities that Amazon AppFlow stored
-- in its cache. Use this action when you want Amazon AppFlow to return the
-- latest information about the data that you have in a source application.
--
-- Amazon AppFlow returns metadata about your entities when you use the
-- ListConnectorEntities or DescribeConnectorEntities actions. Following
-- these actions, Amazon AppFlow caches the metadata to reduce the number
-- of API requests that it must send to the source application. Amazon
-- AppFlow automatically resets the cache once every hour, but you can use
-- this action when you want to get the latest metadata right away.
module Amazonka.AppFlow.ResetConnectorMetadataCache
  ( -- * Creating a Request
    ResetConnectorMetadataCache (..),
    newResetConnectorMetadataCache,

    -- * Request Lenses
    resetConnectorMetadataCache_apiVersion,
    resetConnectorMetadataCache_connectorEntityName,
    resetConnectorMetadataCache_connectorProfileName,
    resetConnectorMetadataCache_connectorType,
    resetConnectorMetadataCache_entitiesPath,

    -- * Destructuring the Response
    ResetConnectorMetadataCacheResponse (..),
    newResetConnectorMetadataCacheResponse,

    -- * Response Lenses
    resetConnectorMetadataCacheResponse_httpStatus,
  )
where

import Amazonka.AppFlow.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetConnectorMetadataCache' smart constructor.
data ResetConnectorMetadataCache = ResetConnectorMetadataCache'
  { -- | The API version that you specified in the connector profile that you’re
    -- resetting cached metadata for. You must use this parameter only if the
    -- connector supports multiple API versions or if the connector type is
    -- CustomConnector.
    --
    -- To look up how many versions a connector supports, use the
    -- DescribeConnectors action. In the response, find the value that Amazon
    -- AppFlow returns for the connectorVersion parameter.
    --
    -- To look up the connector type, use the DescribeConnectorProfiles action.
    -- In the response, find the value that Amazon AppFlow returns for the
    -- connectorType parameter.
    --
    -- To look up the API version that you specified in a connector profile,
    -- use the DescribeConnectorProfiles action.
    apiVersion :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter if you want to reset cached metadata about the
    -- details for an individual entity.
    --
    -- If you don\'t include this parameter in your request, Amazon AppFlow
    -- only resets cached metadata about entity names, not entity details.
    connectorEntityName :: Prelude.Maybe Prelude.Text,
    -- | The name of the connector profile that you want to reset cached metadata
    -- for.
    --
    -- You can omit this parameter if you\'re resetting the cache for any of
    -- the following connectors: Amazon Connect, Amazon EventBridge, Amazon
    -- Lookout for Metrics, Amazon S3, or Upsolver. If you\'re resetting the
    -- cache for any other connector, you must include this parameter in your
    -- request.
    connectorProfileName :: Prelude.Maybe Prelude.Text,
    -- | The type of connector to reset cached metadata for.
    --
    -- You must include this parameter in your request if you\'re resetting the
    -- cache for any of the following connectors: Amazon Connect, Amazon
    -- EventBridge, Amazon Lookout for Metrics, Amazon S3, or Upsolver. If
    -- you\'re resetting the cache for any other connector, you can omit this
    -- parameter from your request.
    connectorType :: Prelude.Maybe ConnectorType,
    -- | Use this parameter only if you’re resetting the cached metadata about a
    -- nested entity. Only some connectors support nested entities. A nested
    -- entity is one that has another entity as a parent. To use this
    -- parameter, specify the name of the parent entity.
    --
    -- To look up the parent-child relationship of entities, you can send a
    -- ListConnectorEntities request that omits the entitiesPath parameter.
    -- Amazon AppFlow will return a list of top-level entities. For each one,
    -- it indicates whether the entity has nested entities. Then, in a
    -- subsequent ListConnectorEntities request, you can specify a parent
    -- entity name for the entitiesPath parameter. Amazon AppFlow will return a
    -- list of the child entities for that parent.
    entitiesPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetConnectorMetadataCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiVersion', 'resetConnectorMetadataCache_apiVersion' - The API version that you specified in the connector profile that you’re
-- resetting cached metadata for. You must use this parameter only if the
-- connector supports multiple API versions or if the connector type is
-- CustomConnector.
--
-- To look up how many versions a connector supports, use the
-- DescribeConnectors action. In the response, find the value that Amazon
-- AppFlow returns for the connectorVersion parameter.
--
-- To look up the connector type, use the DescribeConnectorProfiles action.
-- In the response, find the value that Amazon AppFlow returns for the
-- connectorType parameter.
--
-- To look up the API version that you specified in a connector profile,
-- use the DescribeConnectorProfiles action.
--
-- 'connectorEntityName', 'resetConnectorMetadataCache_connectorEntityName' - Use this parameter if you want to reset cached metadata about the
-- details for an individual entity.
--
-- If you don\'t include this parameter in your request, Amazon AppFlow
-- only resets cached metadata about entity names, not entity details.
--
-- 'connectorProfileName', 'resetConnectorMetadataCache_connectorProfileName' - The name of the connector profile that you want to reset cached metadata
-- for.
--
-- You can omit this parameter if you\'re resetting the cache for any of
-- the following connectors: Amazon Connect, Amazon EventBridge, Amazon
-- Lookout for Metrics, Amazon S3, or Upsolver. If you\'re resetting the
-- cache for any other connector, you must include this parameter in your
-- request.
--
-- 'connectorType', 'resetConnectorMetadataCache_connectorType' - The type of connector to reset cached metadata for.
--
-- You must include this parameter in your request if you\'re resetting the
-- cache for any of the following connectors: Amazon Connect, Amazon
-- EventBridge, Amazon Lookout for Metrics, Amazon S3, or Upsolver. If
-- you\'re resetting the cache for any other connector, you can omit this
-- parameter from your request.
--
-- 'entitiesPath', 'resetConnectorMetadataCache_entitiesPath' - Use this parameter only if you’re resetting the cached metadata about a
-- nested entity. Only some connectors support nested entities. A nested
-- entity is one that has another entity as a parent. To use this
-- parameter, specify the name of the parent entity.
--
-- To look up the parent-child relationship of entities, you can send a
-- ListConnectorEntities request that omits the entitiesPath parameter.
-- Amazon AppFlow will return a list of top-level entities. For each one,
-- it indicates whether the entity has nested entities. Then, in a
-- subsequent ListConnectorEntities request, you can specify a parent
-- entity name for the entitiesPath parameter. Amazon AppFlow will return a
-- list of the child entities for that parent.
newResetConnectorMetadataCache ::
  ResetConnectorMetadataCache
newResetConnectorMetadataCache =
  ResetConnectorMetadataCache'
    { apiVersion =
        Prelude.Nothing,
      connectorEntityName = Prelude.Nothing,
      connectorProfileName = Prelude.Nothing,
      connectorType = Prelude.Nothing,
      entitiesPath = Prelude.Nothing
    }

-- | The API version that you specified in the connector profile that you’re
-- resetting cached metadata for. You must use this parameter only if the
-- connector supports multiple API versions or if the connector type is
-- CustomConnector.
--
-- To look up how many versions a connector supports, use the
-- DescribeConnectors action. In the response, find the value that Amazon
-- AppFlow returns for the connectorVersion parameter.
--
-- To look up the connector type, use the DescribeConnectorProfiles action.
-- In the response, find the value that Amazon AppFlow returns for the
-- connectorType parameter.
--
-- To look up the API version that you specified in a connector profile,
-- use the DescribeConnectorProfiles action.
resetConnectorMetadataCache_apiVersion :: Lens.Lens' ResetConnectorMetadataCache (Prelude.Maybe Prelude.Text)
resetConnectorMetadataCache_apiVersion = Lens.lens (\ResetConnectorMetadataCache' {apiVersion} -> apiVersion) (\s@ResetConnectorMetadataCache' {} a -> s {apiVersion = a} :: ResetConnectorMetadataCache)

-- | Use this parameter if you want to reset cached metadata about the
-- details for an individual entity.
--
-- If you don\'t include this parameter in your request, Amazon AppFlow
-- only resets cached metadata about entity names, not entity details.
resetConnectorMetadataCache_connectorEntityName :: Lens.Lens' ResetConnectorMetadataCache (Prelude.Maybe Prelude.Text)
resetConnectorMetadataCache_connectorEntityName = Lens.lens (\ResetConnectorMetadataCache' {connectorEntityName} -> connectorEntityName) (\s@ResetConnectorMetadataCache' {} a -> s {connectorEntityName = a} :: ResetConnectorMetadataCache)

-- | The name of the connector profile that you want to reset cached metadata
-- for.
--
-- You can omit this parameter if you\'re resetting the cache for any of
-- the following connectors: Amazon Connect, Amazon EventBridge, Amazon
-- Lookout for Metrics, Amazon S3, or Upsolver. If you\'re resetting the
-- cache for any other connector, you must include this parameter in your
-- request.
resetConnectorMetadataCache_connectorProfileName :: Lens.Lens' ResetConnectorMetadataCache (Prelude.Maybe Prelude.Text)
resetConnectorMetadataCache_connectorProfileName = Lens.lens (\ResetConnectorMetadataCache' {connectorProfileName} -> connectorProfileName) (\s@ResetConnectorMetadataCache' {} a -> s {connectorProfileName = a} :: ResetConnectorMetadataCache)

-- | The type of connector to reset cached metadata for.
--
-- You must include this parameter in your request if you\'re resetting the
-- cache for any of the following connectors: Amazon Connect, Amazon
-- EventBridge, Amazon Lookout for Metrics, Amazon S3, or Upsolver. If
-- you\'re resetting the cache for any other connector, you can omit this
-- parameter from your request.
resetConnectorMetadataCache_connectorType :: Lens.Lens' ResetConnectorMetadataCache (Prelude.Maybe ConnectorType)
resetConnectorMetadataCache_connectorType = Lens.lens (\ResetConnectorMetadataCache' {connectorType} -> connectorType) (\s@ResetConnectorMetadataCache' {} a -> s {connectorType = a} :: ResetConnectorMetadataCache)

-- | Use this parameter only if you’re resetting the cached metadata about a
-- nested entity. Only some connectors support nested entities. A nested
-- entity is one that has another entity as a parent. To use this
-- parameter, specify the name of the parent entity.
--
-- To look up the parent-child relationship of entities, you can send a
-- ListConnectorEntities request that omits the entitiesPath parameter.
-- Amazon AppFlow will return a list of top-level entities. For each one,
-- it indicates whether the entity has nested entities. Then, in a
-- subsequent ListConnectorEntities request, you can specify a parent
-- entity name for the entitiesPath parameter. Amazon AppFlow will return a
-- list of the child entities for that parent.
resetConnectorMetadataCache_entitiesPath :: Lens.Lens' ResetConnectorMetadataCache (Prelude.Maybe Prelude.Text)
resetConnectorMetadataCache_entitiesPath = Lens.lens (\ResetConnectorMetadataCache' {entitiesPath} -> entitiesPath) (\s@ResetConnectorMetadataCache' {} a -> s {entitiesPath = a} :: ResetConnectorMetadataCache)

instance Core.AWSRequest ResetConnectorMetadataCache where
  type
    AWSResponse ResetConnectorMetadataCache =
      ResetConnectorMetadataCacheResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResetConnectorMetadataCacheResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetConnectorMetadataCache where
  hashWithSalt _salt ResetConnectorMetadataCache' {..} =
    _salt
      `Prelude.hashWithSalt` apiVersion
      `Prelude.hashWithSalt` connectorEntityName
      `Prelude.hashWithSalt` connectorProfileName
      `Prelude.hashWithSalt` connectorType
      `Prelude.hashWithSalt` entitiesPath

instance Prelude.NFData ResetConnectorMetadataCache where
  rnf ResetConnectorMetadataCache' {..} =
    Prelude.rnf apiVersion
      `Prelude.seq` Prelude.rnf connectorEntityName
      `Prelude.seq` Prelude.rnf connectorProfileName
      `Prelude.seq` Prelude.rnf connectorType
      `Prelude.seq` Prelude.rnf entitiesPath

instance Data.ToHeaders ResetConnectorMetadataCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResetConnectorMetadataCache where
  toJSON ResetConnectorMetadataCache' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("apiVersion" Data..=) Prelude.<$> apiVersion,
            ("connectorEntityName" Data..=)
              Prelude.<$> connectorEntityName,
            ("connectorProfileName" Data..=)
              Prelude.<$> connectorProfileName,
            ("connectorType" Data..=) Prelude.<$> connectorType,
            ("entitiesPath" Data..=) Prelude.<$> entitiesPath
          ]
      )

instance Data.ToPath ResetConnectorMetadataCache where
  toPath =
    Prelude.const "/reset-connector-metadata-cache"

instance Data.ToQuery ResetConnectorMetadataCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetConnectorMetadataCacheResponse' smart constructor.
data ResetConnectorMetadataCacheResponse = ResetConnectorMetadataCacheResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetConnectorMetadataCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'resetConnectorMetadataCacheResponse_httpStatus' - The response's http status code.
newResetConnectorMetadataCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetConnectorMetadataCacheResponse
newResetConnectorMetadataCacheResponse pHttpStatus_ =
  ResetConnectorMetadataCacheResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
resetConnectorMetadataCacheResponse_httpStatus :: Lens.Lens' ResetConnectorMetadataCacheResponse Prelude.Int
resetConnectorMetadataCacheResponse_httpStatus = Lens.lens (\ResetConnectorMetadataCacheResponse' {httpStatus} -> httpStatus) (\s@ResetConnectorMetadataCacheResponse' {} a -> s {httpStatus = a} :: ResetConnectorMetadataCacheResponse)

instance
  Prelude.NFData
    ResetConnectorMetadataCacheResponse
  where
  rnf ResetConnectorMetadataCacheResponse' {..} =
    Prelude.rnf httpStatus
