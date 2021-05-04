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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create an ApiKey resource.
--
-- /See:/ 'newCreateApiKey' smart constructor.
data CreateApiKey = CreateApiKey'
  { -- | An AWS Marketplace customer identifier , when integrating with the AWS
    -- SaaS Marketplace.
    customerId :: Prelude.Maybe Prelude.Text,
    -- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
    -- key.
    stageKeys :: Prelude.Maybe [StageKey],
    -- | Specifies whether the ApiKey can be used by callers.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ApiKey.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether (@true@) or not (@false@) the key identifier is
    -- distinct from the created API key value. This parameter is deprecated
    -- and should not be used.
    generateDistinctId :: Prelude.Maybe Prelude.Bool,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the ApiKey.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies a value of the API key.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { customerId = Prelude.Nothing,
      stageKeys = Prelude.Nothing,
      enabled = Prelude.Nothing,
      name = Prelude.Nothing,
      generateDistinctId = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
createApiKey_customerId :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_customerId = Lens.lens (\CreateApiKey' {customerId} -> customerId) (\s@CreateApiKey' {} a -> s {customerId = a} :: CreateApiKey)

-- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
-- key.
createApiKey_stageKeys :: Lens.Lens' CreateApiKey (Prelude.Maybe [StageKey])
createApiKey_stageKeys = Lens.lens (\CreateApiKey' {stageKeys} -> stageKeys) (\s@CreateApiKey' {} a -> s {stageKeys = a} :: CreateApiKey) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies whether the ApiKey can be used by callers.
createApiKey_enabled :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Bool)
createApiKey_enabled = Lens.lens (\CreateApiKey' {enabled} -> enabled) (\s@CreateApiKey' {} a -> s {enabled = a} :: CreateApiKey)

-- | The name of the ApiKey.
createApiKey_name :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_name = Lens.lens (\CreateApiKey' {name} -> name) (\s@CreateApiKey' {} a -> s {name = a} :: CreateApiKey)

-- | Specifies whether (@true@) or not (@false@) the key identifier is
-- distinct from the created API key value. This parameter is deprecated
-- and should not be used.
createApiKey_generateDistinctId :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Bool)
createApiKey_generateDistinctId = Lens.lens (\CreateApiKey' {generateDistinctId} -> generateDistinctId) (\s@CreateApiKey' {} a -> s {generateDistinctId = a} :: CreateApiKey)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createApiKey_tags :: Lens.Lens' CreateApiKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApiKey_tags = Lens.lens (\CreateApiKey' {tags} -> tags) (\s@CreateApiKey' {} a -> s {tags = a} :: CreateApiKey) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the ApiKey.
createApiKey_description :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_description = Lens.lens (\CreateApiKey' {description} -> description) (\s@CreateApiKey' {} a -> s {description = a} :: CreateApiKey)

-- | Specifies a value of the API key.
createApiKey_value :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_value = Lens.lens (\CreateApiKey' {value} -> value) (\s@CreateApiKey' {} a -> s {value = a} :: CreateApiKey)

instance Prelude.AWSRequest CreateApiKey where
  type Rs CreateApiKey = ApiKey
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable CreateApiKey

instance Prelude.NFData CreateApiKey

instance Prelude.ToHeaders CreateApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON CreateApiKey where
  toJSON CreateApiKey' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("customerId" Prelude..=) Prelude.<$> customerId,
            ("stageKeys" Prelude..=) Prelude.<$> stageKeys,
            ("enabled" Prelude..=) Prelude.<$> enabled,
            ("name" Prelude..=) Prelude.<$> name,
            ("generateDistinctId" Prelude..=)
              Prelude.<$> generateDistinctId,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("description" Prelude..=) Prelude.<$> description,
            ("value" Prelude..=) Prelude.<$> value
          ]
      )

instance Prelude.ToPath CreateApiKey where
  toPath = Prelude.const "/apikeys"

instance Prelude.ToQuery CreateApiKey where
  toQuery = Prelude.const Prelude.mempty
