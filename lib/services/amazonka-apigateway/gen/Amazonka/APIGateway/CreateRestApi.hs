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
-- Module      : Amazonka.APIGateway.CreateRestApi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new RestApi resource.
module Amazonka.APIGateway.CreateRestApi
  ( -- * Creating a Request
    CreateRestApi (..),
    newCreateRestApi,

    -- * Request Lenses
    createRestApi_apiKeySource,
    createRestApi_binaryMediaTypes,
    createRestApi_cloneFrom,
    createRestApi_description,
    createRestApi_disableExecuteApiEndpoint,
    createRestApi_endpointConfiguration,
    createRestApi_minimumCompressionSize,
    createRestApi_policy,
    createRestApi_tags,
    createRestApi_version,
    createRestApi_name,

    -- * Destructuring the Response
    RestApi (..),
    newRestApi,

    -- * Response Lenses
    restApi_apiKeySource,
    restApi_binaryMediaTypes,
    restApi_createdDate,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_endpointConfiguration,
    restApi_id,
    restApi_minimumCompressionSize,
    restApi_name,
    restApi_policy,
    restApi_tags,
    restApi_version,
    restApi_warnings,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The POST Request to add a new RestApi resource to your collection.
--
-- /See:/ 'newCreateRestApi' smart constructor.
data CreateRestApi = CreateRestApi'
  { -- | The source of the API key for metering requests according to a usage
    -- plan. Valid values are: >@HEADER@ to read the API key from the
    -- @X-API-Key@ header of a request. @AUTHORIZER@ to read the API key from
    -- the @UsageIdentifierKey@ from a custom authorizer.
    apiKeySource :: Prelude.Maybe ApiKeySourceType,
    -- | The list of binary media types supported by the RestApi. By default, the
    -- RestApi supports only UTF-8-encoded text payloads.
    binaryMediaTypes :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the RestApi that you want to clone from.
    cloneFrom :: Prelude.Maybe Prelude.Text,
    -- | The description of the RestApi.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether clients can invoke your API by using the default
    -- @execute-api@ endpoint. By default, clients can invoke your API with the
    -- default @https:\/\/{api_id}.execute-api.{region}.amazonaws.com@
    -- endpoint. To require that clients use a custom domain name to invoke
    -- your API, disable the default endpoint
    disableExecuteApiEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | The endpoint configuration of this RestApi showing the endpoint types of
    -- the API.
    endpointConfiguration :: Prelude.Maybe EndpointConfiguration,
    -- | A nullable integer that is used to enable compression (with non-negative
    -- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
    -- (with a null value) on an API. When compression is enabled, compression
    -- or decompression is not applied on the payload if the payload size is
    -- smaller than this value. Setting it to zero allows compression for any
    -- payload size.
    minimumCompressionSize :: Prelude.Maybe Prelude.Int,
    -- | A stringified JSON policy document that applies to this RestApi
    -- regardless of the caller and Method configuration.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A version identifier for the API.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the RestApi.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeySource', 'createRestApi_apiKeySource' - The source of the API key for metering requests according to a usage
-- plan. Valid values are: >@HEADER@ to read the API key from the
-- @X-API-Key@ header of a request. @AUTHORIZER@ to read the API key from
-- the @UsageIdentifierKey@ from a custom authorizer.
--
-- 'binaryMediaTypes', 'createRestApi_binaryMediaTypes' - The list of binary media types supported by the RestApi. By default, the
-- RestApi supports only UTF-8-encoded text payloads.
--
-- 'cloneFrom', 'createRestApi_cloneFrom' - The ID of the RestApi that you want to clone from.
--
-- 'description', 'createRestApi_description' - The description of the RestApi.
--
-- 'disableExecuteApiEndpoint', 'createRestApi_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- @execute-api@ endpoint. By default, clients can invoke your API with the
-- default @https:\/\/{api_id}.execute-api.{region}.amazonaws.com@
-- endpoint. To require that clients use a custom domain name to invoke
-- your API, disable the default endpoint
--
-- 'endpointConfiguration', 'createRestApi_endpointConfiguration' - The endpoint configuration of this RestApi showing the endpoint types of
-- the API.
--
-- 'minimumCompressionSize', 'createRestApi_minimumCompressionSize' - A nullable integer that is used to enable compression (with non-negative
-- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
-- (with a null value) on an API. When compression is enabled, compression
-- or decompression is not applied on the payload if the payload size is
-- smaller than this value. Setting it to zero allows compression for any
-- payload size.
--
-- 'policy', 'createRestApi_policy' - A stringified JSON policy document that applies to this RestApi
-- regardless of the caller and Method configuration.
--
-- 'tags', 'createRestApi_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'version', 'createRestApi_version' - A version identifier for the API.
--
-- 'name', 'createRestApi_name' - The name of the RestApi.
newCreateRestApi ::
  -- | 'name'
  Prelude.Text ->
  CreateRestApi
newCreateRestApi pName_ =
  CreateRestApi'
    { apiKeySource = Prelude.Nothing,
      binaryMediaTypes = Prelude.Nothing,
      cloneFrom = Prelude.Nothing,
      description = Prelude.Nothing,
      disableExecuteApiEndpoint = Prelude.Nothing,
      endpointConfiguration = Prelude.Nothing,
      minimumCompressionSize = Prelude.Nothing,
      policy = Prelude.Nothing,
      tags = Prelude.Nothing,
      version = Prelude.Nothing,
      name = pName_
    }

-- | The source of the API key for metering requests according to a usage
-- plan. Valid values are: >@HEADER@ to read the API key from the
-- @X-API-Key@ header of a request. @AUTHORIZER@ to read the API key from
-- the @UsageIdentifierKey@ from a custom authorizer.
createRestApi_apiKeySource :: Lens.Lens' CreateRestApi (Prelude.Maybe ApiKeySourceType)
createRestApi_apiKeySource = Lens.lens (\CreateRestApi' {apiKeySource} -> apiKeySource) (\s@CreateRestApi' {} a -> s {apiKeySource = a} :: CreateRestApi)

-- | The list of binary media types supported by the RestApi. By default, the
-- RestApi supports only UTF-8-encoded text payloads.
createRestApi_binaryMediaTypes :: Lens.Lens' CreateRestApi (Prelude.Maybe [Prelude.Text])
createRestApi_binaryMediaTypes = Lens.lens (\CreateRestApi' {binaryMediaTypes} -> binaryMediaTypes) (\s@CreateRestApi' {} a -> s {binaryMediaTypes = a} :: CreateRestApi) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the RestApi that you want to clone from.
createRestApi_cloneFrom :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Text)
createRestApi_cloneFrom = Lens.lens (\CreateRestApi' {cloneFrom} -> cloneFrom) (\s@CreateRestApi' {} a -> s {cloneFrom = a} :: CreateRestApi)

-- | The description of the RestApi.
createRestApi_description :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Text)
createRestApi_description = Lens.lens (\CreateRestApi' {description} -> description) (\s@CreateRestApi' {} a -> s {description = a} :: CreateRestApi)

-- | Specifies whether clients can invoke your API by using the default
-- @execute-api@ endpoint. By default, clients can invoke your API with the
-- default @https:\/\/{api_id}.execute-api.{region}.amazonaws.com@
-- endpoint. To require that clients use a custom domain name to invoke
-- your API, disable the default endpoint
createRestApi_disableExecuteApiEndpoint :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Bool)
createRestApi_disableExecuteApiEndpoint = Lens.lens (\CreateRestApi' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@CreateRestApi' {} a -> s {disableExecuteApiEndpoint = a} :: CreateRestApi)

-- | The endpoint configuration of this RestApi showing the endpoint types of
-- the API.
createRestApi_endpointConfiguration :: Lens.Lens' CreateRestApi (Prelude.Maybe EndpointConfiguration)
createRestApi_endpointConfiguration = Lens.lens (\CreateRestApi' {endpointConfiguration} -> endpointConfiguration) (\s@CreateRestApi' {} a -> s {endpointConfiguration = a} :: CreateRestApi)

-- | A nullable integer that is used to enable compression (with non-negative
-- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
-- (with a null value) on an API. When compression is enabled, compression
-- or decompression is not applied on the payload if the payload size is
-- smaller than this value. Setting it to zero allows compression for any
-- payload size.
createRestApi_minimumCompressionSize :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Int)
createRestApi_minimumCompressionSize = Lens.lens (\CreateRestApi' {minimumCompressionSize} -> minimumCompressionSize) (\s@CreateRestApi' {} a -> s {minimumCompressionSize = a} :: CreateRestApi)

-- | A stringified JSON policy document that applies to this RestApi
-- regardless of the caller and Method configuration.
createRestApi_policy :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Text)
createRestApi_policy = Lens.lens (\CreateRestApi' {policy} -> policy) (\s@CreateRestApi' {} a -> s {policy = a} :: CreateRestApi)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createRestApi_tags :: Lens.Lens' CreateRestApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRestApi_tags = Lens.lens (\CreateRestApi' {tags} -> tags) (\s@CreateRestApi' {} a -> s {tags = a} :: CreateRestApi) Prelude.. Lens.mapping Lens.coerced

-- | A version identifier for the API.
createRestApi_version :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Text)
createRestApi_version = Lens.lens (\CreateRestApi' {version} -> version) (\s@CreateRestApi' {} a -> s {version = a} :: CreateRestApi)

-- | The name of the RestApi.
createRestApi_name :: Lens.Lens' CreateRestApi Prelude.Text
createRestApi_name = Lens.lens (\CreateRestApi' {name} -> name) (\s@CreateRestApi' {} a -> s {name = a} :: CreateRestApi)

instance Core.AWSRequest CreateRestApi where
  type AWSResponse CreateRestApi = RestApi
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateRestApi where
  hashWithSalt _salt CreateRestApi' {..} =
    _salt `Prelude.hashWithSalt` apiKeySource
      `Prelude.hashWithSalt` binaryMediaTypes
      `Prelude.hashWithSalt` cloneFrom
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` disableExecuteApiEndpoint
      `Prelude.hashWithSalt` endpointConfiguration
      `Prelude.hashWithSalt` minimumCompressionSize
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateRestApi where
  rnf CreateRestApi' {..} =
    Prelude.rnf apiKeySource
      `Prelude.seq` Prelude.rnf binaryMediaTypes
      `Prelude.seq` Prelude.rnf cloneFrom
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf disableExecuteApiEndpoint
      `Prelude.seq` Prelude.rnf endpointConfiguration
      `Prelude.seq` Prelude.rnf minimumCompressionSize
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateRestApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateRestApi where
  toJSON CreateRestApi' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("apiKeySource" Data..=) Prelude.<$> apiKeySource,
            ("binaryMediaTypes" Data..=)
              Prelude.<$> binaryMediaTypes,
            ("cloneFrom" Data..=) Prelude.<$> cloneFrom,
            ("description" Data..=) Prelude.<$> description,
            ("disableExecuteApiEndpoint" Data..=)
              Prelude.<$> disableExecuteApiEndpoint,
            ("endpointConfiguration" Data..=)
              Prelude.<$> endpointConfiguration,
            ("minimumCompressionSize" Data..=)
              Prelude.<$> minimumCompressionSize,
            ("policy" Data..=) Prelude.<$> policy,
            ("tags" Data..=) Prelude.<$> tags,
            ("version" Data..=) Prelude.<$> version,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateRestApi where
  toPath = Prelude.const "/restapis"

instance Data.ToQuery CreateRestApi where
  toQuery = Prelude.const Prelude.mempty
