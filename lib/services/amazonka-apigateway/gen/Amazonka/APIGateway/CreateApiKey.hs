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
-- Module      : Amazonka.APIGateway.CreateApiKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an ApiKey resource.
module Amazonka.APIGateway.CreateApiKey
  ( -- * Creating a Request
    CreateApiKey (..),
    newCreateApiKey,

    -- * Request Lenses
    createApiKey_tags,
    createApiKey_generateDistinctId,
    createApiKey_customerId,
    createApiKey_name,
    createApiKey_description,
    createApiKey_stageKeys,
    createApiKey_enabled,
    createApiKey_value,

    -- * Destructuring the Response
    ApiKey (..),
    newApiKey,

    -- * Response Lenses
    apiKey_tags,
    apiKey_customerId,
    apiKey_name,
    apiKey_lastUpdatedDate,
    apiKey_description,
    apiKey_stageKeys,
    apiKey_id,
    apiKey_enabled,
    apiKey_createdDate,
    apiKey_value,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to create an ApiKey resource.
--
-- /See:/ 'newCreateApiKey' smart constructor.
data CreateApiKey = CreateApiKey'
  { -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether (@true@) or not (@false@) the key identifier is
    -- distinct from the created API key value. This parameter is deprecated
    -- and should not be used.
    generateDistinctId :: Prelude.Maybe Prelude.Bool,
    -- | An AWS Marketplace customer identifier , when integrating with the AWS
    -- SaaS Marketplace.
    customerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the ApiKey.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the ApiKey.
    description :: Prelude.Maybe Prelude.Text,
    -- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
    -- key.
    stageKeys :: Prelude.Maybe [StageKey],
    -- | Specifies whether the ApiKey can be used by callers.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies a value of the API key.
    value :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'createApiKey_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'generateDistinctId', 'createApiKey_generateDistinctId' - Specifies whether (@true@) or not (@false@) the key identifier is
-- distinct from the created API key value. This parameter is deprecated
-- and should not be used.
--
-- 'customerId', 'createApiKey_customerId' - An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
--
-- 'name', 'createApiKey_name' - The name of the ApiKey.
--
-- 'description', 'createApiKey_description' - The description of the ApiKey.
--
-- 'stageKeys', 'createApiKey_stageKeys' - DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
-- key.
--
-- 'enabled', 'createApiKey_enabled' - Specifies whether the ApiKey can be used by callers.
--
-- 'value', 'createApiKey_value' - Specifies a value of the API key.
newCreateApiKey ::
  CreateApiKey
newCreateApiKey =
  CreateApiKey'
    { tags = Prelude.Nothing,
      generateDistinctId = Prelude.Nothing,
      customerId = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      stageKeys = Prelude.Nothing,
      enabled = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createApiKey_tags :: Lens.Lens' CreateApiKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApiKey_tags = Lens.lens (\CreateApiKey' {tags} -> tags) (\s@CreateApiKey' {} a -> s {tags = a} :: CreateApiKey) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether (@true@) or not (@false@) the key identifier is
-- distinct from the created API key value. This parameter is deprecated
-- and should not be used.
createApiKey_generateDistinctId :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Bool)
createApiKey_generateDistinctId = Lens.lens (\CreateApiKey' {generateDistinctId} -> generateDistinctId) (\s@CreateApiKey' {} a -> s {generateDistinctId = a} :: CreateApiKey)

-- | An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
createApiKey_customerId :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_customerId = Lens.lens (\CreateApiKey' {customerId} -> customerId) (\s@CreateApiKey' {} a -> s {customerId = a} :: CreateApiKey)

-- | The name of the ApiKey.
createApiKey_name :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_name = Lens.lens (\CreateApiKey' {name} -> name) (\s@CreateApiKey' {} a -> s {name = a} :: CreateApiKey)

-- | The description of the ApiKey.
createApiKey_description :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_description = Lens.lens (\CreateApiKey' {description} -> description) (\s@CreateApiKey' {} a -> s {description = a} :: CreateApiKey)

-- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API
-- key.
createApiKey_stageKeys :: Lens.Lens' CreateApiKey (Prelude.Maybe [StageKey])
createApiKey_stageKeys = Lens.lens (\CreateApiKey' {stageKeys} -> stageKeys) (\s@CreateApiKey' {} a -> s {stageKeys = a} :: CreateApiKey) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the ApiKey can be used by callers.
createApiKey_enabled :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Bool)
createApiKey_enabled = Lens.lens (\CreateApiKey' {enabled} -> enabled) (\s@CreateApiKey' {} a -> s {enabled = a} :: CreateApiKey)

-- | Specifies a value of the API key.
createApiKey_value :: Lens.Lens' CreateApiKey (Prelude.Maybe Prelude.Text)
createApiKey_value = Lens.lens (\CreateApiKey' {value} -> value) (\s@CreateApiKey' {} a -> s {value = a} :: CreateApiKey)

instance Core.AWSRequest CreateApiKey where
  type AWSResponse CreateApiKey = ApiKey
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateApiKey where
  hashWithSalt _salt CreateApiKey' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` generateDistinctId
      `Prelude.hashWithSalt` customerId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stageKeys
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` value

instance Prelude.NFData CreateApiKey where
  rnf CreateApiKey' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf generateDistinctId
      `Prelude.seq` Prelude.rnf customerId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf stageKeys
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf value

instance Data.ToHeaders CreateApiKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateApiKey where
  toJSON CreateApiKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("generateDistinctId" Data..=)
              Prelude.<$> generateDistinctId,
            ("customerId" Data..=) Prelude.<$> customerId,
            ("name" Data..=) Prelude.<$> name,
            ("description" Data..=) Prelude.<$> description,
            ("stageKeys" Data..=) Prelude.<$> stageKeys,
            ("enabled" Data..=) Prelude.<$> enabled,
            ("value" Data..=) Prelude.<$> value
          ]
      )

instance Data.ToPath CreateApiKey where
  toPath = Prelude.const "/apikeys"

instance Data.ToQuery CreateApiKey where
  toQuery = Prelude.const Prelude.mempty
