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
-- Module      : Network.AWS.APIGateway.CreateApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an ApiKey resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/create-api-key.html AWS CLI>
module Network.AWS.APIGateway.CreateApiKey
  ( -- * Creating a Request
    CreateApiKey (..),
    newCreateApiKey,

    -- * Request Lenses
    createApiKey_customerId,
    createApiKey_stageKeys,
    createApiKey_enabled,
    createApiKey_name,
    createApiKey_generateDistinctId,
    createApiKey_tags,
    createApiKey_description,
    createApiKey_value,

    -- * Destructuring the Response
    ApiKey (..),
    newApiKey,

    -- * Response Lenses
    apiKey_createdDate,
    apiKey_customerId,
    apiKey_lastUpdatedDate,
    apiKey_stageKeys,
    apiKey_enabled,
    apiKey_id,
    apiKey_name,
    apiKey_tags,
    apiKey_description,
    apiKey_value,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create an ApiKey resource.
--
-- /See:/ 'newCreateApiKey' smart constructor.
data CreateApiKey = CreateApiKey'
  { -- | An AWS Marketplace customer identifier , when integrating with the AWS
    -- SaaS Marketplace.
    customerId :: Core.Maybe Core.Text,
    -- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
    -- key.
    stageKeys :: Core.Maybe [StageKey],
    -- | Specifies whether the ApiKey can be used by callers.
    enabled :: Core.Maybe Core.Bool,
    -- | The name of the ApiKey.
    name :: Core.Maybe Core.Text,
    -- | Specifies whether (@true@) or not (@false@) the key identifier is
    -- distinct from the created API key value. This parameter is deprecated
    -- and should not be used.
    generateDistinctId :: Core.Maybe Core.Bool,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the ApiKey.
    description :: Core.Maybe Core.Text,
    -- | Specifies a value of the API key.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerId', 'createApiKey_customerId' - An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
--
-- 'stageKeys', 'createApiKey_stageKeys' - DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
-- key.
--
-- 'enabled', 'createApiKey_enabled' - Specifies whether the ApiKey can be used by callers.
--
-- 'name', 'createApiKey_name' - The name of the ApiKey.
--
-- 'generateDistinctId', 'createApiKey_generateDistinctId' - Specifies whether (@true@) or not (@false@) the key identifier is
-- distinct from the created API key value. This parameter is deprecated
-- and should not be used.
--
-- 'tags', 'createApiKey_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'description', 'createApiKey_description' - The description of the ApiKey.
--
-- 'value', 'createApiKey_value' - Specifies a value of the API key.
newCreateApiKey ::
  CreateApiKey
newCreateApiKey =
  CreateApiKey'
    { customerId = Core.Nothing,
      stageKeys = Core.Nothing,
      enabled = Core.Nothing,
      name = Core.Nothing,
      generateDistinctId = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      value = Core.Nothing
    }

-- | An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
createApiKey_customerId :: Lens.Lens' CreateApiKey (Core.Maybe Core.Text)
createApiKey_customerId = Lens.lens (\CreateApiKey' {customerId} -> customerId) (\s@CreateApiKey' {} a -> s {customerId = a} :: CreateApiKey)

-- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
-- key.
createApiKey_stageKeys :: Lens.Lens' CreateApiKey (Core.Maybe [StageKey])
createApiKey_stageKeys = Lens.lens (\CreateApiKey' {stageKeys} -> stageKeys) (\s@CreateApiKey' {} a -> s {stageKeys = a} :: CreateApiKey) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether the ApiKey can be used by callers.
createApiKey_enabled :: Lens.Lens' CreateApiKey (Core.Maybe Core.Bool)
createApiKey_enabled = Lens.lens (\CreateApiKey' {enabled} -> enabled) (\s@CreateApiKey' {} a -> s {enabled = a} :: CreateApiKey)

-- | The name of the ApiKey.
createApiKey_name :: Lens.Lens' CreateApiKey (Core.Maybe Core.Text)
createApiKey_name = Lens.lens (\CreateApiKey' {name} -> name) (\s@CreateApiKey' {} a -> s {name = a} :: CreateApiKey)

-- | Specifies whether (@true@) or not (@false@) the key identifier is
-- distinct from the created API key value. This parameter is deprecated
-- and should not be used.
createApiKey_generateDistinctId :: Lens.Lens' CreateApiKey (Core.Maybe Core.Bool)
createApiKey_generateDistinctId = Lens.lens (\CreateApiKey' {generateDistinctId} -> generateDistinctId) (\s@CreateApiKey' {} a -> s {generateDistinctId = a} :: CreateApiKey)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createApiKey_tags :: Lens.Lens' CreateApiKey (Core.Maybe (Core.HashMap Core.Text Core.Text))
createApiKey_tags = Lens.lens (\CreateApiKey' {tags} -> tags) (\s@CreateApiKey' {} a -> s {tags = a} :: CreateApiKey) Core.. Lens.mapping Lens._Coerce

-- | The description of the ApiKey.
createApiKey_description :: Lens.Lens' CreateApiKey (Core.Maybe Core.Text)
createApiKey_description = Lens.lens (\CreateApiKey' {description} -> description) (\s@CreateApiKey' {} a -> s {description = a} :: CreateApiKey)

-- | Specifies a value of the API key.
createApiKey_value :: Lens.Lens' CreateApiKey (Core.Maybe Core.Text)
createApiKey_value = Lens.lens (\CreateApiKey' {value} -> value) (\s@CreateApiKey' {} a -> s {value = a} :: CreateApiKey)

instance Core.AWSRequest CreateApiKey where
  type AWSResponse CreateApiKey = ApiKey
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateApiKey

instance Core.NFData CreateApiKey

instance Core.ToHeaders CreateApiKey where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateApiKey where
  toJSON CreateApiKey' {..} =
    Core.object
      ( Core.catMaybes
          [ ("customerId" Core..=) Core.<$> customerId,
            ("stageKeys" Core..=) Core.<$> stageKeys,
            ("enabled" Core..=) Core.<$> enabled,
            ("name" Core..=) Core.<$> name,
            ("generateDistinctId" Core..=)
              Core.<$> generateDistinctId,
            ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            ("value" Core..=) Core.<$> value
          ]
      )

instance Core.ToPath CreateApiKey where
  toPath = Core.const "/apikeys"

instance Core.ToQuery CreateApiKey where
  toQuery = Core.const Core.mempty
