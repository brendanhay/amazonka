{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The POST Request to add a new RestApi resource to your collection.
--
-- /See:/ 'newCreateRestApi' smart constructor.
data CreateRestApi = CreateRestApi'
  { -- | The endpoint configuration of this RestApi showing the endpoint types of
    -- the API.
    endpointConfiguration :: Prelude.Maybe EndpointConfiguration,
    -- | The list of binary media types supported by the RestApi. By default, the
    -- RestApi supports only UTF-8-encoded text payloads.
    binaryMediaTypes :: Prelude.Maybe [Prelude.Text],
    -- | A version identifier for the API.
    version :: Prelude.Maybe Prelude.Text,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the RestApi.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether clients can invoke your API by using the default
    -- @execute-api@ endpoint. By default, clients can invoke your API with the
    -- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
    -- To require that clients use a custom domain name to invoke your API,
    -- disable the default endpoint.
    disableExecuteApiEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | A stringified JSON policy document that applies to this RestApi
    -- regardless of the caller and Method configuration.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the RestApi that you want to clone from.
    cloneFrom :: Prelude.Maybe Prelude.Text,
    -- | A nullable integer that is used to enable compression (with non-negative
    -- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
    -- (with a null value) on an API. When compression is enabled, compression
    -- or decompression is not applied on the payload if the payload size is
    -- smaller than this value. Setting it to zero allows compression for any
    -- payload size.
    minimumCompressionSize :: Prelude.Maybe Prelude.Int,
    -- | The source of the API key for metering requests according to a usage
    -- plan. Valid values are:
    --
    -- -   @HEADER@ to read the API key from the @X-API-Key@ header of a
    --     request.
    -- -   @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from
    --     a custom authorizer.
    apiKeySource :: Prelude.Maybe ApiKeySourceType,
    -- | [Required] The name of the RestApi.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateRestApi
newCreateRestApi pName_ =
  CreateRestApi'
    { endpointConfiguration =
        Prelude.Nothing,
      binaryMediaTypes = Prelude.Nothing,
      version = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      disableExecuteApiEndpoint = Prelude.Nothing,
      policy = Prelude.Nothing,
      cloneFrom = Prelude.Nothing,
      minimumCompressionSize = Prelude.Nothing,
      apiKeySource = Prelude.Nothing,
      name = pName_
    }

-- | The endpoint configuration of this RestApi showing the endpoint types of
-- the API.
createRestApi_endpointConfiguration :: Lens.Lens' CreateRestApi (Prelude.Maybe EndpointConfiguration)
createRestApi_endpointConfiguration = Lens.lens (\CreateRestApi' {endpointConfiguration} -> endpointConfiguration) (\s@CreateRestApi' {} a -> s {endpointConfiguration = a} :: CreateRestApi)

-- | The list of binary media types supported by the RestApi. By default, the
-- RestApi supports only UTF-8-encoded text payloads.
createRestApi_binaryMediaTypes :: Lens.Lens' CreateRestApi (Prelude.Maybe [Prelude.Text])
createRestApi_binaryMediaTypes = Lens.lens (\CreateRestApi' {binaryMediaTypes} -> binaryMediaTypes) (\s@CreateRestApi' {} a -> s {binaryMediaTypes = a} :: CreateRestApi) Prelude.. Lens.mapping Prelude._Coerce

-- | A version identifier for the API.
createRestApi_version :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Text)
createRestApi_version = Lens.lens (\CreateRestApi' {version} -> version) (\s@CreateRestApi' {} a -> s {version = a} :: CreateRestApi)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createRestApi_tags :: Lens.Lens' CreateRestApi (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRestApi_tags = Lens.lens (\CreateRestApi' {tags} -> tags) (\s@CreateRestApi' {} a -> s {tags = a} :: CreateRestApi) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the RestApi.
createRestApi_description :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Text)
createRestApi_description = Lens.lens (\CreateRestApi' {description} -> description) (\s@CreateRestApi' {} a -> s {description = a} :: CreateRestApi)

-- | Specifies whether clients can invoke your API by using the default
-- @execute-api@ endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
createRestApi_disableExecuteApiEndpoint :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Bool)
createRestApi_disableExecuteApiEndpoint = Lens.lens (\CreateRestApi' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@CreateRestApi' {} a -> s {disableExecuteApiEndpoint = a} :: CreateRestApi)

-- | A stringified JSON policy document that applies to this RestApi
-- regardless of the caller and Method configuration.
createRestApi_policy :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Text)
createRestApi_policy = Lens.lens (\CreateRestApi' {policy} -> policy) (\s@CreateRestApi' {} a -> s {policy = a} :: CreateRestApi)

-- | The ID of the RestApi that you want to clone from.
createRestApi_cloneFrom :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Text)
createRestApi_cloneFrom = Lens.lens (\CreateRestApi' {cloneFrom} -> cloneFrom) (\s@CreateRestApi' {} a -> s {cloneFrom = a} :: CreateRestApi)

-- | A nullable integer that is used to enable compression (with non-negative
-- between 0 and 10485760 (10M) bytes, inclusive) or disable compression
-- (with a null value) on an API. When compression is enabled, compression
-- or decompression is not applied on the payload if the payload size is
-- smaller than this value. Setting it to zero allows compression for any
-- payload size.
createRestApi_minimumCompressionSize :: Lens.Lens' CreateRestApi (Prelude.Maybe Prelude.Int)
createRestApi_minimumCompressionSize = Lens.lens (\CreateRestApi' {minimumCompressionSize} -> minimumCompressionSize) (\s@CreateRestApi' {} a -> s {minimumCompressionSize = a} :: CreateRestApi)

-- | The source of the API key for metering requests according to a usage
-- plan. Valid values are:
--
-- -   @HEADER@ to read the API key from the @X-API-Key@ header of a
--     request.
-- -   @AUTHORIZER@ to read the API key from the @UsageIdentifierKey@ from
--     a custom authorizer.
createRestApi_apiKeySource :: Lens.Lens' CreateRestApi (Prelude.Maybe ApiKeySourceType)
createRestApi_apiKeySource = Lens.lens (\CreateRestApi' {apiKeySource} -> apiKeySource) (\s@CreateRestApi' {} a -> s {apiKeySource = a} :: CreateRestApi)

-- | [Required] The name of the RestApi.
createRestApi_name :: Lens.Lens' CreateRestApi Prelude.Text
createRestApi_name = Lens.lens (\CreateRestApi' {name} -> name) (\s@CreateRestApi' {} a -> s {name = a} :: CreateRestApi)

instance Prelude.AWSRequest CreateRestApi where
  type Rs CreateRestApi = RestApi
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable CreateRestApi

instance Prelude.NFData CreateRestApi

instance Prelude.ToHeaders CreateRestApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON CreateRestApi where
  toJSON CreateRestApi' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("endpointConfiguration" Prelude..=)
              Prelude.<$> endpointConfiguration,
            ("binaryMediaTypes" Prelude..=)
              Prelude.<$> binaryMediaTypes,
            ("version" Prelude..=) Prelude.<$> version,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("description" Prelude..=) Prelude.<$> description,
            ("disableExecuteApiEndpoint" Prelude..=)
              Prelude.<$> disableExecuteApiEndpoint,
            ("policy" Prelude..=) Prelude.<$> policy,
            ("cloneFrom" Prelude..=) Prelude.<$> cloneFrom,
            ("minimumCompressionSize" Prelude..=)
              Prelude.<$> minimumCompressionSize,
            ("apiKeySource" Prelude..=) Prelude.<$> apiKeySource,
            Prelude.Just ("name" Prelude..= name)
          ]
      )

instance Prelude.ToPath CreateRestApi where
  toPath = Prelude.const "/restapis"

instance Prelude.ToQuery CreateRestApi where
  toQuery = Prelude.const Prelude.mempty
