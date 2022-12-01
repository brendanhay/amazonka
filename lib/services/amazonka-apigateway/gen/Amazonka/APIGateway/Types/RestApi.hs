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
-- Module      : Amazonka.APIGateway.Types.RestApi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.RestApi where

import Amazonka.APIGateway.Types.ApiKeySourceType
import Amazonka.APIGateway.Types.EndpointConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a REST API.
--
-- /See:/ 'newRestApi' smart constructor.
data RestApi = RestApi'
  { -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A stringified JSON policy document that applies to this RestApi
    -- regardless of the caller and Method configuration.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The API\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The API\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The API\'s identifier. This identifier is unique across all of your APIs
    -- in API Gateway.
    id :: Prelude.Maybe Prelude.Text,
    -- | The list of binary media types supported by the RestApi. By default, the
    -- RestApi supports only UTF-8-encoded text payloads.
    binaryMediaTypes :: Prelude.Maybe [Prelude.Text],
    -- | The warning messages reported when @failonwarnings@ is turned on during
    -- API import.
    warnings :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether clients can invoke your API by using the default
    -- @execute-api@ endpoint. By default, clients can invoke your API with the
    -- default @https:\/\/{api_id}.execute-api.{region}.amazonaws.com@
    -- endpoint. To require that clients use a custom domain name to invoke
    -- your API, disable the default endpoint.
    disableExecuteApiEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | The endpoint configuration of this RestApi showing the endpoint types of
    -- the API.
    endpointConfiguration :: Prelude.Maybe EndpointConfiguration,
    -- | The timestamp when the API was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The source of the API key for metering requests according to a usage
    -- plan. Valid values are: >@HEADER@ to read the API key from the
    -- @X-API-Key@ header of a request. @AUTHORIZER@ to read the API key from
    -- the @UsageIdentifierKey@ from a custom authorizer.
    apiKeySource :: Prelude.Maybe ApiKeySourceType,
    -- | A nullable integer that is used to enable compression (with non-negative
    -- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
    -- (with a null value) on an API. When compression is enabled, compression
    -- or decompression is not applied on the payload if the payload size is
    -- smaller than this value. Setting it to zero allows compression for any
    -- payload size.
    minimumCompressionSize :: Prelude.Maybe Prelude.Int,
    -- | A version identifier for the API.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'restApi_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'policy', 'restApi_policy' - A stringified JSON policy document that applies to this RestApi
-- regardless of the caller and Method configuration.
--
-- 'name', 'restApi_name' - The API\'s name.
--
-- 'description', 'restApi_description' - The API\'s description.
--
-- 'id', 'restApi_id' - The API\'s identifier. This identifier is unique across all of your APIs
-- in API Gateway.
--
-- 'binaryMediaTypes', 'restApi_binaryMediaTypes' - The list of binary media types supported by the RestApi. By default, the
-- RestApi supports only UTF-8-encoded text payloads.
--
-- 'warnings', 'restApi_warnings' - The warning messages reported when @failonwarnings@ is turned on during
-- API import.
--
-- 'disableExecuteApiEndpoint', 'restApi_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- @execute-api@ endpoint. By default, clients can invoke your API with the
-- default @https:\/\/{api_id}.execute-api.{region}.amazonaws.com@
-- endpoint. To require that clients use a custom domain name to invoke
-- your API, disable the default endpoint.
--
-- 'endpointConfiguration', 'restApi_endpointConfiguration' - The endpoint configuration of this RestApi showing the endpoint types of
-- the API.
--
-- 'createdDate', 'restApi_createdDate' - The timestamp when the API was created.
--
-- 'apiKeySource', 'restApi_apiKeySource' - The source of the API key for metering requests according to a usage
-- plan. Valid values are: >@HEADER@ to read the API key from the
-- @X-API-Key@ header of a request. @AUTHORIZER@ to read the API key from
-- the @UsageIdentifierKey@ from a custom authorizer.
--
-- 'minimumCompressionSize', 'restApi_minimumCompressionSize' - A nullable integer that is used to enable compression (with non-negative
-- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
-- (with a null value) on an API. When compression is enabled, compression
-- or decompression is not applied on the payload if the payload size is
-- smaller than this value. Setting it to zero allows compression for any
-- payload size.
--
-- 'version', 'restApi_version' - A version identifier for the API.
newRestApi ::
  RestApi
newRestApi =
  RestApi'
    { tags = Prelude.Nothing,
      policy = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      binaryMediaTypes = Prelude.Nothing,
      warnings = Prelude.Nothing,
      disableExecuteApiEndpoint = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      apiKeySource = Prelude.Nothing,
      minimumCompressionSize = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The collection of tags. Each tag element is associated with a given
-- resource.
restApi_tags :: Lens.Lens' RestApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
restApi_tags = Lens.lens (\RestApi' {tags} -> tags) (\s@RestApi' {} a -> s {tags = a} :: RestApi) Prelude.. Lens.mapping Lens.coerced

-- | A stringified JSON policy document that applies to this RestApi
-- regardless of the caller and Method configuration.
restApi_policy :: Lens.Lens' RestApi (Prelude.Maybe Prelude.Text)
restApi_policy = Lens.lens (\RestApi' {policy} -> policy) (\s@RestApi' {} a -> s {policy = a} :: RestApi)

-- | The API\'s name.
restApi_name :: Lens.Lens' RestApi (Prelude.Maybe Prelude.Text)
restApi_name = Lens.lens (\RestApi' {name} -> name) (\s@RestApi' {} a -> s {name = a} :: RestApi)

-- | The API\'s description.
restApi_description :: Lens.Lens' RestApi (Prelude.Maybe Prelude.Text)
restApi_description = Lens.lens (\RestApi' {description} -> description) (\s@RestApi' {} a -> s {description = a} :: RestApi)

-- | The API\'s identifier. This identifier is unique across all of your APIs
-- in API Gateway.
restApi_id :: Lens.Lens' RestApi (Prelude.Maybe Prelude.Text)
restApi_id = Lens.lens (\RestApi' {id} -> id) (\s@RestApi' {} a -> s {id = a} :: RestApi)

-- | The list of binary media types supported by the RestApi. By default, the
-- RestApi supports only UTF-8-encoded text payloads.
restApi_binaryMediaTypes :: Lens.Lens' RestApi (Prelude.Maybe [Prelude.Text])
restApi_binaryMediaTypes = Lens.lens (\RestApi' {binaryMediaTypes} -> binaryMediaTypes) (\s@RestApi' {} a -> s {binaryMediaTypes = a} :: RestApi) Prelude.. Lens.mapping Lens.coerced

-- | The warning messages reported when @failonwarnings@ is turned on during
-- API import.
restApi_warnings :: Lens.Lens' RestApi (Prelude.Maybe [Prelude.Text])
restApi_warnings = Lens.lens (\RestApi' {warnings} -> warnings) (\s@RestApi' {} a -> s {warnings = a} :: RestApi) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether clients can invoke your API by using the default
-- @execute-api@ endpoint. By default, clients can invoke your API with the
-- default @https:\/\/{api_id}.execute-api.{region}.amazonaws.com@
-- endpoint. To require that clients use a custom domain name to invoke
-- your API, disable the default endpoint.
restApi_disableExecuteApiEndpoint :: Lens.Lens' RestApi (Prelude.Maybe Prelude.Bool)
restApi_disableExecuteApiEndpoint = Lens.lens (\RestApi' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@RestApi' {} a -> s {disableExecuteApiEndpoint = a} :: RestApi)

-- | The endpoint configuration of this RestApi showing the endpoint types of
-- the API.
restApi_endpointConfiguration :: Lens.Lens' RestApi (Prelude.Maybe EndpointConfiguration)
restApi_endpointConfiguration = Lens.lens (\RestApi' {endpointConfiguration} -> endpointConfiguration) (\s@RestApi' {} a -> s {endpointConfiguration = a} :: RestApi)

-- | The timestamp when the API was created.
restApi_createdDate :: Lens.Lens' RestApi (Prelude.Maybe Prelude.UTCTime)
restApi_createdDate = Lens.lens (\RestApi' {createdDate} -> createdDate) (\s@RestApi' {} a -> s {createdDate = a} :: RestApi) Prelude.. Lens.mapping Core._Time

-- | The source of the API key for metering requests according to a usage
-- plan. Valid values are: >@HEADER@ to read the API key from the
-- @X-API-Key@ header of a request. @AUTHORIZER@ to read the API key from
-- the @UsageIdentifierKey@ from a custom authorizer.
restApi_apiKeySource :: Lens.Lens' RestApi (Prelude.Maybe ApiKeySourceType)
restApi_apiKeySource = Lens.lens (\RestApi' {apiKeySource} -> apiKeySource) (\s@RestApi' {} a -> s {apiKeySource = a} :: RestApi)

-- | A nullable integer that is used to enable compression (with non-negative
-- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
-- (with a null value) on an API. When compression is enabled, compression
-- or decompression is not applied on the payload if the payload size is
-- smaller than this value. Setting it to zero allows compression for any
-- payload size.
restApi_minimumCompressionSize :: Lens.Lens' RestApi (Prelude.Maybe Prelude.Int)
restApi_minimumCompressionSize = Lens.lens (\RestApi' {minimumCompressionSize} -> minimumCompressionSize) (\s@RestApi' {} a -> s {minimumCompressionSize = a} :: RestApi)

-- | A version identifier for the API.
restApi_version :: Lens.Lens' RestApi (Prelude.Maybe Prelude.Text)
restApi_version = Lens.lens (\RestApi' {version} -> version) (\s@RestApi' {} a -> s {version = a} :: RestApi)

instance Core.FromJSON RestApi where
  parseJSON =
    Core.withObject
      "RestApi"
      ( \x ->
          RestApi'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "policy")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> ( x Core..:? "binaryMediaTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "warnings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "disableExecuteApiEndpoint")
            Prelude.<*> (x Core..:? "endpointConfiguration")
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "apiKeySource")
            Prelude.<*> (x Core..:? "minimumCompressionSize")
            Prelude.<*> (x Core..:? "version")
      )

instance Prelude.Hashable RestApi where
  hashWithSalt _salt RestApi' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` binaryMediaTypes
      `Prelude.hashWithSalt` warnings
      `Prelude.hashWithSalt` disableExecuteApiEndpoint
      `Prelude.hashWithSalt` endpointConfiguration
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` apiKeySource
      `Prelude.hashWithSalt` minimumCompressionSize
      `Prelude.hashWithSalt` version

instance Prelude.NFData RestApi where
  rnf RestApi' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf binaryMediaTypes
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf disableExecuteApiEndpoint
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf apiKeySource
      `Prelude.seq` Prelude.rnf minimumCompressionSize
      `Prelude.seq` Prelude.rnf version
