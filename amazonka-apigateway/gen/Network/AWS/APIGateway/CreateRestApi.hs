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
-- Module      : Network.AWS.APIGateway.CreateRestApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new RestApi resource.
module Network.AWS.APIGateway.CreateRestApi
  ( -- * Creating a Request
    CreateRestApi (..),
    newCreateRestApi,

    -- * Request Lenses
    createRestApi_endpointConfiguration,
    createRestApi_binaryMediaTypes,
    createRestApi_version,
    createRestApi_tags,
    createRestApi_description,
    createRestApi_disableExecuteApiEndpoint,
    createRestApi_policy,
    createRestApi_cloneFrom,
    createRestApi_minimumCompressionSize,
    createRestApi_apiKeySource,
    createRestApi_name,

    -- * Destructuring the Response
    RestApi (..),
    newRestApi,

    -- * Response Lenses
    restApi_createdDate,
    restApi_warnings,
    restApi_endpointConfiguration,
    restApi_binaryMediaTypes,
    restApi_id,
    restApi_version,
    restApi_name,
    restApi_tags,
    restApi_description,
    restApi_disableExecuteApiEndpoint,
    restApi_policy,
    restApi_minimumCompressionSize,
    restApi_apiKeySource,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The POST Request to add a new RestApi resource to your collection.
--
-- /See:/ 'newCreateRestApi' smart constructor.
data CreateRestApi = CreateRestApi'
  { -- | The endpoint configuration of this RestApi showing the endpoint types of
    -- the API.
    endpointConfiguration :: Core.Maybe EndpointConfiguration,
    -- | The list of binary media types supported by the RestApi. By default, the
    -- RestApi supports only UTF-8-encoded text payloads.
    binaryMediaTypes :: Core.Maybe [Core.Text],
    -- | A version identifier for the API.
    version :: Core.Maybe Core.Text,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the RestApi.
    description :: Core.Maybe Core.Text,
    -- | Specifies whether clients can invoke your API by using the default
    -- @execute-api@ endpoint. By default, clients can invoke your API with the
    -- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
    -- To require that clients use a custom domain name to invoke your API,
    -- disable the default endpoint.
    disableExecuteApiEndpoint :: Core.Maybe Core.Bool,
    -- | A stringified JSON policy document that applies to this RestApi
    -- regardless of the caller and Method configuration.
    policy :: Core.Maybe Core.Text,
    -- | The ID of the RestApi that you want to clone from.
    cloneFrom :: Core.Maybe Core.Text,
    -- | A nullable integer that is used to enable compression (with non-negative
    -- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
    -- (with a null value) on an API. When compression is enabled, compression
    -- or decompression is not applied on the payload if the payload size is
    -- smaller than this value. Setting it to zero allows compression for any
    -- payload size.
    minimumCompressionSize :: Core.Maybe Core.Int,
    -- | The source of the API key for metering requests according to a usage
    -- plan. Valid values are:
    --
    -- -   @HEADER@ to read the API key from the @X-API-Key@ header of a
    --     request.
    -- -   @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from
    --     a custom authorizer.
    apiKeySource :: Core.Maybe ApiKeySourceType,
    -- | [Required] The name of the RestApi.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointConfiguration', 'createRestApi_endpointConfiguration' - The endpoint configuration of this RestApi showing the endpoint types of
-- the API.
--
-- 'binaryMediaTypes', 'createRestApi_binaryMediaTypes' - The list of binary media types supported by the RestApi. By default, the
-- RestApi supports only UTF-8-encoded text payloads.
--
-- 'version', 'createRestApi_version' - A version identifier for the API.
--
-- 'tags', 'createRestApi_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'description', 'createRestApi_description' - The description of the RestApi.
--
-- 'disableExecuteApiEndpoint', 'createRestApi_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- @execute-api@ endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
--
-- 'policy', 'createRestApi_policy' - A stringified JSON policy document that applies to this RestApi
-- regardless of the caller and Method configuration.
--
-- 'cloneFrom', 'createRestApi_cloneFrom' - The ID of the RestApi that you want to clone from.
--
-- 'minimumCompressionSize', 'createRestApi_minimumCompressionSize' - A nullable integer that is used to enable compression (with non-negative
-- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
-- (with a null value) on an API. When compression is enabled, compression
-- or decompression is not applied on the payload if the payload size is
-- smaller than this value. Setting it to zero allows compression for any
-- payload size.
--
-- 'apiKeySource', 'createRestApi_apiKeySource' - The source of the API key for metering requests according to a usage
-- plan. Valid values are:
--
-- -   @HEADER@ to read the API key from the @X-API-Key@ header of a
--     request.
-- -   @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from
--     a custom authorizer.
--
-- 'name', 'createRestApi_name' - [Required] The name of the RestApi.
newCreateRestApi ::
  -- | 'name'
  Core.Text ->
  CreateRestApi
newCreateRestApi pName_ =
  CreateRestApi'
    { endpointConfiguration =
        Core.Nothing,
      binaryMediaTypes = Core.Nothing,
      version = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      disableExecuteApiEndpoint = Core.Nothing,
      policy = Core.Nothing,
      cloneFrom = Core.Nothing,
      minimumCompressionSize = Core.Nothing,
      apiKeySource = Core.Nothing,
      name = pName_
    }

-- | The endpoint configuration of this RestApi showing the endpoint types of
-- the API.
createRestApi_endpointConfiguration :: Lens.Lens' CreateRestApi (Core.Maybe EndpointConfiguration)
createRestApi_endpointConfiguration = Lens.lens (\CreateRestApi' {endpointConfiguration} -> endpointConfiguration) (\s@CreateRestApi' {} a -> s {endpointConfiguration = a} :: CreateRestApi)

-- | The list of binary media types supported by the RestApi. By default, the
-- RestApi supports only UTF-8-encoded text payloads.
createRestApi_binaryMediaTypes :: Lens.Lens' CreateRestApi (Core.Maybe [Core.Text])
createRestApi_binaryMediaTypes = Lens.lens (\CreateRestApi' {binaryMediaTypes} -> binaryMediaTypes) (\s@CreateRestApi' {} a -> s {binaryMediaTypes = a} :: CreateRestApi) Core.. Lens.mapping Lens._Coerce

-- | A version identifier for the API.
createRestApi_version :: Lens.Lens' CreateRestApi (Core.Maybe Core.Text)
createRestApi_version = Lens.lens (\CreateRestApi' {version} -> version) (\s@CreateRestApi' {} a -> s {version = a} :: CreateRestApi)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createRestApi_tags :: Lens.Lens' CreateRestApi (Core.Maybe (Core.HashMap Core.Text Core.Text))
createRestApi_tags = Lens.lens (\CreateRestApi' {tags} -> tags) (\s@CreateRestApi' {} a -> s {tags = a} :: CreateRestApi) Core.. Lens.mapping Lens._Coerce

-- | The description of the RestApi.
createRestApi_description :: Lens.Lens' CreateRestApi (Core.Maybe Core.Text)
createRestApi_description = Lens.lens (\CreateRestApi' {description} -> description) (\s@CreateRestApi' {} a -> s {description = a} :: CreateRestApi)

-- | Specifies whether clients can invoke your API by using the default
-- @execute-api@ endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
createRestApi_disableExecuteApiEndpoint :: Lens.Lens' CreateRestApi (Core.Maybe Core.Bool)
createRestApi_disableExecuteApiEndpoint = Lens.lens (\CreateRestApi' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@CreateRestApi' {} a -> s {disableExecuteApiEndpoint = a} :: CreateRestApi)

-- | A stringified JSON policy document that applies to this RestApi
-- regardless of the caller and Method configuration.
createRestApi_policy :: Lens.Lens' CreateRestApi (Core.Maybe Core.Text)
createRestApi_policy = Lens.lens (\CreateRestApi' {policy} -> policy) (\s@CreateRestApi' {} a -> s {policy = a} :: CreateRestApi)

-- | The ID of the RestApi that you want to clone from.
createRestApi_cloneFrom :: Lens.Lens' CreateRestApi (Core.Maybe Core.Text)
createRestApi_cloneFrom = Lens.lens (\CreateRestApi' {cloneFrom} -> cloneFrom) (\s@CreateRestApi' {} a -> s {cloneFrom = a} :: CreateRestApi)

-- | A nullable integer that is used to enable compression (with non-negative
-- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
-- (with a null value) on an API. When compression is enabled, compression
-- or decompression is not applied on the payload if the payload size is
-- smaller than this value. Setting it to zero allows compression for any
-- payload size.
createRestApi_minimumCompressionSize :: Lens.Lens' CreateRestApi (Core.Maybe Core.Int)
createRestApi_minimumCompressionSize = Lens.lens (\CreateRestApi' {minimumCompressionSize} -> minimumCompressionSize) (\s@CreateRestApi' {} a -> s {minimumCompressionSize = a} :: CreateRestApi)

-- | The source of the API key for metering requests according to a usage
-- plan. Valid values are:
--
-- -   @HEADER@ to read the API key from the @X-API-Key@ header of a
--     request.
-- -   @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from
--     a custom authorizer.
createRestApi_apiKeySource :: Lens.Lens' CreateRestApi (Core.Maybe ApiKeySourceType)
createRestApi_apiKeySource = Lens.lens (\CreateRestApi' {apiKeySource} -> apiKeySource) (\s@CreateRestApi' {} a -> s {apiKeySource = a} :: CreateRestApi)

-- | [Required] The name of the RestApi.
createRestApi_name :: Lens.Lens' CreateRestApi Core.Text
createRestApi_name = Lens.lens (\CreateRestApi' {name} -> name) (\s@CreateRestApi' {} a -> s {name = a} :: CreateRestApi)

instance Core.AWSRequest CreateRestApi where
  type AWSResponse CreateRestApi = RestApi
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateRestApi

instance Core.NFData CreateRestApi

instance Core.ToHeaders CreateRestApi where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRestApi where
  toJSON CreateRestApi' {..} =
    Core.object
      ( Core.catMaybes
          [ ("endpointConfiguration" Core..=)
              Core.<$> endpointConfiguration,
            ("binaryMediaTypes" Core..=)
              Core.<$> binaryMediaTypes,
            ("version" Core..=) Core.<$> version,
            ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            ("disableExecuteApiEndpoint" Core..=)
              Core.<$> disableExecuteApiEndpoint,
            ("policy" Core..=) Core.<$> policy,
            ("cloneFrom" Core..=) Core.<$> cloneFrom,
            ("minimumCompressionSize" Core..=)
              Core.<$> minimumCompressionSize,
            ("apiKeySource" Core..=) Core.<$> apiKeySource,
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateRestApi where
  toPath = Core.const "/restapis"

instance Core.ToQuery CreateRestApi where
  toQuery = Core.const Core.mempty
