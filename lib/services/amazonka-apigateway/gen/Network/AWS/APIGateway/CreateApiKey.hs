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
    createApiKey_enabled,
    createApiKey_value,
    createApiKey_customerId,
    createApiKey_generateDistinctId,
    createApiKey_name,
    createApiKey_stageKeys,
    createApiKey_description,
    createApiKey_tags,

    -- * Destructuring the Response
    ApiKey (..),
    newApiKey,

    -- * Response Lenses
    apiKey_enabled,
    apiKey_value,
    apiKey_customerId,
    apiKey_createdDate,
    apiKey_name,
    apiKey_id,
    apiKey_stageKeys,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_tags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create an ApiKey resource.
--
-- /See:/ 'newCreateApiKey' smart constructor.
data CreateApiKey = CreateApiKey'
  { -- | Specifies whether the ApiKey can be used by callers.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies a value of the API key.
    value :: Prelude.Maybe Prelude.Text,
    -- | An AWS Marketplace customer identifier , when integrating with the AWS
    -- SaaS Marketplace.
    customerId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether (@true@) or not (@false@) the key identifier is
    -- distinct from the created API key value. This parameter is deprecated
    -- and should not be used.
    generateDistinctId :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ApiKey.
    name :: Prelude.Maybe Prelude.Text,
    -- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
    -- key.
    stageKeys :: Prelude.Maybe [StageKey],
    -- | The description of the ApiKey.
    description :: Prelude.Maybe Prelude.Text,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'createApiKey_enabled' - Specifies whether the ApiKey can be used by callers.
--
-- 'value', 'createApiKey_value' - Specifies a value of the API key.
--
-- 'customerId', 'createApiKey_customerId' - An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
--
-- 'generateDistinctId', 'createApiKey_generateDistinctId' - Specifies whether (@true@) or not (@false@) the key identifier is
-- distinct from the created API key value. This parameter is deprecated
-- and should not be used.
--
-- 'name', 'createApiKey_name' - The name of the ApiKey.
--
-- 'stageKeys', 'createApiKey_stageKeys' - DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
-- key.
--
-- 'description', 'createApiKey_description' - The description of the ApiKey.
--
-- 'tags', 'createApiKey_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
newCreateApiKey ::
  CreateApiKey
newCreateApiKey =
  CreateApiKey'
    { enabled = Prelude.Nothing,
      value = Prelude.Nothing,
      customerId = Prelude.Nothing,
      generateDistinctId = Prelude.Nothing,
      name = Prelude.Nothing,
      stageKeys = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Specifies whether the ApiKey can be used by callers.
createApiKey_enabled :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Bool)
createApiKey_enabled = Lens.lens (\CreateApiKey' {enabled} -> enabled) (\s@CreateApiKey' {} a -> s {enabled = a} :: CreateApiKey)

-- | Specifies a value of the API key.
createApiKey_value :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_value = Lens.lens (\CreateApiKey' {value} -> value) (\s@CreateApiKey' {} a -> s {value = a} :: CreateApiKey)

-- | An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
createApiKey_customerId :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_customerId = Lens.lens (\CreateApiKey' {customerId} -> customerId) (\s@CreateApiKey' {} a -> s {customerId = a} :: CreateApiKey)

-- | Specifies whether (@true@) or not (@false@) the key identifier is
-- distinct from the created API key value. This parameter is deprecated
-- and should not be used.
createApiKey_generateDistinctId :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Bool)
createApiKey_generateDistinctId = Lens.lens (\CreateApiKey' {generateDistinctId} -> generateDistinctId) (\s@CreateApiKey' {} a -> s {generateDistinctId = a} :: CreateApiKey)

-- | The name of the ApiKey.
createApiKey_name :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_name = Lens.lens (\CreateApiKey' {name} -> name) (\s@CreateApiKey' {} a -> s {name = a} :: CreateApiKey)

-- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
-- key.
createApiKey_stageKeys :: Lens.Lens' CreateApiKey (Prelude.Maybe [StageKey])
createApiKey_stageKeys = Lens.lens (\CreateApiKey' {stageKeys} -> stageKeys) (\s@CreateApiKey' {} a -> s {stageKeys = a} :: CreateApiKey) Prelude.. Lens.mapping Lens.coerced

-- | The description of the ApiKey.
createApiKey_description :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_description = Lens.lens (\CreateApiKey' {description} -> description) (\s@CreateApiKey' {} a -> s {description = a} :: CreateApiKey)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createApiKey_tags :: Lens.Lens' CreateApiKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApiKey_tags = Lens.lens (\CreateApiKey' {tags} -> tags) (\s@CreateApiKey' {} a -> s {tags = a} :: CreateApiKey) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateApiKey where
  type AWSResponse CreateApiKey = ApiKey
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateApiKey

instance Prelude.NFData CreateApiKey

instance Core.ToHeaders CreateApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON CreateApiKey where
  toJSON CreateApiKey' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("enabled" Core..=) Prelude.<$> enabled,
            ("value" Core..=) Prelude.<$> value,
            ("customerId" Core..=) Prelude.<$> customerId,
            ("generateDistinctId" Core..=)
              Prelude.<$> generateDistinctId,
            ("name" Core..=) Prelude.<$> name,
            ("stageKeys" Core..=) Prelude.<$> stageKeys,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateApiKey where
  toPath = Prelude.const "/apikeys"

instance Core.ToQuery CreateApiKey where
  toQuery = Prelude.const Prelude.mempty
