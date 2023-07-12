{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.AwsApiGatewayV2ApiDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiGatewayV2ApiDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCorsConfiguration

-- | Contains information about a version 2 API in Amazon API Gateway.
--
-- /See:/ 'newAwsApiGatewayV2ApiDetails' smart constructor.
data AwsApiGatewayV2ApiDetails = AwsApiGatewayV2ApiDetails'
  { -- | The URI of the API.
    --
    -- Uses the format
    -- @ @/@\<api-id>@/@.execute-api.@/@\<region>@/@.amazonaws.com@
    --
    -- The stage name is typically appended to the URI to form a complete path
    -- to a deployed API stage.
    apiEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the API.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | An API key selection expression. Supported only for WebSocket APIs.
    apiKeySelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | A cross-origin resource sharing (CORS) configuration. Supported only for
    -- HTTP APIs.
    corsConfiguration :: Prelude.Maybe AwsCorsConfiguration,
    -- | Indicates when the API was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | A description of the API.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the API.
    name :: Prelude.Maybe Prelude.Text,
    -- | The API protocol for the API.
    --
    -- Valid values: @WEBSOCKET@ | @HTTP@
    protocolType :: Prelude.Maybe Prelude.Text,
    -- | The route selection expression for the API.
    --
    -- For HTTP APIs, must be @${request.method} ${request.path}@. This is the
    -- default value for HTTP APIs.
    --
    -- For WebSocket APIs, there is no default value.
    routeSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The version identifier for the API.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiGatewayV2ApiDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiEndpoint', 'awsApiGatewayV2ApiDetails_apiEndpoint' - The URI of the API.
--
-- Uses the format
-- @ @/@\<api-id>@/@.execute-api.@/@\<region>@/@.amazonaws.com@
--
-- The stage name is typically appended to the URI to form a complete path
-- to a deployed API stage.
--
-- 'apiId', 'awsApiGatewayV2ApiDetails_apiId' - The identifier of the API.
--
-- 'apiKeySelectionExpression', 'awsApiGatewayV2ApiDetails_apiKeySelectionExpression' - An API key selection expression. Supported only for WebSocket APIs.
--
-- 'corsConfiguration', 'awsApiGatewayV2ApiDetails_corsConfiguration' - A cross-origin resource sharing (CORS) configuration. Supported only for
-- HTTP APIs.
--
-- 'createdDate', 'awsApiGatewayV2ApiDetails_createdDate' - Indicates when the API was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'description', 'awsApiGatewayV2ApiDetails_description' - A description of the API.
--
-- 'name', 'awsApiGatewayV2ApiDetails_name' - The name of the API.
--
-- 'protocolType', 'awsApiGatewayV2ApiDetails_protocolType' - The API protocol for the API.
--
-- Valid values: @WEBSOCKET@ | @HTTP@
--
-- 'routeSelectionExpression', 'awsApiGatewayV2ApiDetails_routeSelectionExpression' - The route selection expression for the API.
--
-- For HTTP APIs, must be @${request.method} ${request.path}@. This is the
-- default value for HTTP APIs.
--
-- For WebSocket APIs, there is no default value.
--
-- 'version', 'awsApiGatewayV2ApiDetails_version' - The version identifier for the API.
newAwsApiGatewayV2ApiDetails ::
  AwsApiGatewayV2ApiDetails
newAwsApiGatewayV2ApiDetails =
  AwsApiGatewayV2ApiDetails'
    { apiEndpoint =
        Prelude.Nothing,
      apiId = Prelude.Nothing,
      apiKeySelectionExpression = Prelude.Nothing,
      corsConfiguration = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      protocolType = Prelude.Nothing,
      routeSelectionExpression = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The URI of the API.
--
-- Uses the format
-- @ @/@\<api-id>@/@.execute-api.@/@\<region>@/@.amazonaws.com@
--
-- The stage name is typically appended to the URI to form a complete path
-- to a deployed API stage.
awsApiGatewayV2ApiDetails_apiEndpoint :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2ApiDetails_apiEndpoint = Lens.lens (\AwsApiGatewayV2ApiDetails' {apiEndpoint} -> apiEndpoint) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {apiEndpoint = a} :: AwsApiGatewayV2ApiDetails)

-- | The identifier of the API.
awsApiGatewayV2ApiDetails_apiId :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2ApiDetails_apiId = Lens.lens (\AwsApiGatewayV2ApiDetails' {apiId} -> apiId) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {apiId = a} :: AwsApiGatewayV2ApiDetails)

-- | An API key selection expression. Supported only for WebSocket APIs.
awsApiGatewayV2ApiDetails_apiKeySelectionExpression :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2ApiDetails_apiKeySelectionExpression = Lens.lens (\AwsApiGatewayV2ApiDetails' {apiKeySelectionExpression} -> apiKeySelectionExpression) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {apiKeySelectionExpression = a} :: AwsApiGatewayV2ApiDetails)

-- | A cross-origin resource sharing (CORS) configuration. Supported only for
-- HTTP APIs.
awsApiGatewayV2ApiDetails_corsConfiguration :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe AwsCorsConfiguration)
awsApiGatewayV2ApiDetails_corsConfiguration = Lens.lens (\AwsApiGatewayV2ApiDetails' {corsConfiguration} -> corsConfiguration) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {corsConfiguration = a} :: AwsApiGatewayV2ApiDetails)

-- | Indicates when the API was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsApiGatewayV2ApiDetails_createdDate :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2ApiDetails_createdDate = Lens.lens (\AwsApiGatewayV2ApiDetails' {createdDate} -> createdDate) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {createdDate = a} :: AwsApiGatewayV2ApiDetails)

-- | A description of the API.
awsApiGatewayV2ApiDetails_description :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2ApiDetails_description = Lens.lens (\AwsApiGatewayV2ApiDetails' {description} -> description) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {description = a} :: AwsApiGatewayV2ApiDetails)

-- | The name of the API.
awsApiGatewayV2ApiDetails_name :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2ApiDetails_name = Lens.lens (\AwsApiGatewayV2ApiDetails' {name} -> name) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {name = a} :: AwsApiGatewayV2ApiDetails)

-- | The API protocol for the API.
--
-- Valid values: @WEBSOCKET@ | @HTTP@
awsApiGatewayV2ApiDetails_protocolType :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2ApiDetails_protocolType = Lens.lens (\AwsApiGatewayV2ApiDetails' {protocolType} -> protocolType) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {protocolType = a} :: AwsApiGatewayV2ApiDetails)

-- | The route selection expression for the API.
--
-- For HTTP APIs, must be @${request.method} ${request.path}@. This is the
-- default value for HTTP APIs.
--
-- For WebSocket APIs, there is no default value.
awsApiGatewayV2ApiDetails_routeSelectionExpression :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2ApiDetails_routeSelectionExpression = Lens.lens (\AwsApiGatewayV2ApiDetails' {routeSelectionExpression} -> routeSelectionExpression) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {routeSelectionExpression = a} :: AwsApiGatewayV2ApiDetails)

-- | The version identifier for the API.
awsApiGatewayV2ApiDetails_version :: Lens.Lens' AwsApiGatewayV2ApiDetails (Prelude.Maybe Prelude.Text)
awsApiGatewayV2ApiDetails_version = Lens.lens (\AwsApiGatewayV2ApiDetails' {version} -> version) (\s@AwsApiGatewayV2ApiDetails' {} a -> s {version = a} :: AwsApiGatewayV2ApiDetails)

instance Data.FromJSON AwsApiGatewayV2ApiDetails where
  parseJSON =
    Data.withObject
      "AwsApiGatewayV2ApiDetails"
      ( \x ->
          AwsApiGatewayV2ApiDetails'
            Prelude.<$> (x Data..:? "ApiEndpoint")
            Prelude.<*> (x Data..:? "ApiId")
            Prelude.<*> (x Data..:? "ApiKeySelectionExpression")
            Prelude.<*> (x Data..:? "CorsConfiguration")
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ProtocolType")
            Prelude.<*> (x Data..:? "RouteSelectionExpression")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable AwsApiGatewayV2ApiDetails where
  hashWithSalt _salt AwsApiGatewayV2ApiDetails' {..} =
    _salt
      `Prelude.hashWithSalt` apiEndpoint
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` apiKeySelectionExpression
      `Prelude.hashWithSalt` corsConfiguration
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` protocolType
      `Prelude.hashWithSalt` routeSelectionExpression
      `Prelude.hashWithSalt` version

instance Prelude.NFData AwsApiGatewayV2ApiDetails where
  rnf AwsApiGatewayV2ApiDetails' {..} =
    Prelude.rnf apiEndpoint
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf apiKeySelectionExpression
      `Prelude.seq` Prelude.rnf corsConfiguration
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf protocolType
      `Prelude.seq` Prelude.rnf routeSelectionExpression
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON AwsApiGatewayV2ApiDetails where
  toJSON AwsApiGatewayV2ApiDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApiEndpoint" Data..=) Prelude.<$> apiEndpoint,
            ("ApiId" Data..=) Prelude.<$> apiId,
            ("ApiKeySelectionExpression" Data..=)
              Prelude.<$> apiKeySelectionExpression,
            ("CorsConfiguration" Data..=)
              Prelude.<$> corsConfiguration,
            ("CreatedDate" Data..=) Prelude.<$> createdDate,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("ProtocolType" Data..=) Prelude.<$> protocolType,
            ("RouteSelectionExpression" Data..=)
              Prelude.<$> routeSelectionExpression,
            ("Version" Data..=) Prelude.<$> version
          ]
      )
