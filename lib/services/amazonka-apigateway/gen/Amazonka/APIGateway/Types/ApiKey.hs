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
-- Module      : Amazonka.APIGateway.Types.ApiKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.ApiKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A resource that can be distributed to callers for executing Method
-- resources that require an API key. API keys can be mapped to any Stage
-- on any RestApi, which indicates that the callers with the API key can
-- make requests to that stage.
--
-- /See:/ 'newApiKey' smart constructor.
data ApiKey = ApiKey'
  { -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An AWS Marketplace customer identifier , when integrating with the AWS
    -- SaaS Marketplace.
    customerId :: Prelude.Maybe Prelude.Text,
    -- | The name of the API Key.
    name :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the API Key was last updated.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | The description of the API Key.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of Stage resources that are associated with the ApiKey resource.
    stageKeys :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the API Key.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the API Key can be used by callers.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp when the API Key was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The value of the API Key.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'apiKey_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'customerId', 'apiKey_customerId' - An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
--
-- 'name', 'apiKey_name' - The name of the API Key.
--
-- 'lastUpdatedDate', 'apiKey_lastUpdatedDate' - The timestamp when the API Key was last updated.
--
-- 'description', 'apiKey_description' - The description of the API Key.
--
-- 'stageKeys', 'apiKey_stageKeys' - A list of Stage resources that are associated with the ApiKey resource.
--
-- 'id', 'apiKey_id' - The identifier of the API Key.
--
-- 'enabled', 'apiKey_enabled' - Specifies whether the API Key can be used by callers.
--
-- 'createdDate', 'apiKey_createdDate' - The timestamp when the API Key was created.
--
-- 'value', 'apiKey_value' - The value of the API Key.
newApiKey ::
  ApiKey
newApiKey =
  ApiKey'
    { tags = Prelude.Nothing,
      customerId = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      description = Prelude.Nothing,
      stageKeys = Prelude.Nothing,
      id = Prelude.Nothing,
      enabled = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The collection of tags. Each tag element is associated with a given
-- resource.
apiKey_tags :: Lens.Lens' ApiKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
apiKey_tags = Lens.lens (\ApiKey' {tags} -> tags) (\s@ApiKey' {} a -> s {tags = a} :: ApiKey) Prelude.. Lens.mapping Lens.coerced

-- | An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
apiKey_customerId :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Text)
apiKey_customerId = Lens.lens (\ApiKey' {customerId} -> customerId) (\s@ApiKey' {} a -> s {customerId = a} :: ApiKey)

-- | The name of the API Key.
apiKey_name :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Text)
apiKey_name = Lens.lens (\ApiKey' {name} -> name) (\s@ApiKey' {} a -> s {name = a} :: ApiKey)

-- | The timestamp when the API Key was last updated.
apiKey_lastUpdatedDate :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.UTCTime)
apiKey_lastUpdatedDate = Lens.lens (\ApiKey' {lastUpdatedDate} -> lastUpdatedDate) (\s@ApiKey' {} a -> s {lastUpdatedDate = a} :: ApiKey) Prelude.. Lens.mapping Core._Time

-- | The description of the API Key.
apiKey_description :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Text)
apiKey_description = Lens.lens (\ApiKey' {description} -> description) (\s@ApiKey' {} a -> s {description = a} :: ApiKey)

-- | A list of Stage resources that are associated with the ApiKey resource.
apiKey_stageKeys :: Lens.Lens' ApiKey (Prelude.Maybe [Prelude.Text])
apiKey_stageKeys = Lens.lens (\ApiKey' {stageKeys} -> stageKeys) (\s@ApiKey' {} a -> s {stageKeys = a} :: ApiKey) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the API Key.
apiKey_id :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Text)
apiKey_id = Lens.lens (\ApiKey' {id} -> id) (\s@ApiKey' {} a -> s {id = a} :: ApiKey)

-- | Specifies whether the API Key can be used by callers.
apiKey_enabled :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Bool)
apiKey_enabled = Lens.lens (\ApiKey' {enabled} -> enabled) (\s@ApiKey' {} a -> s {enabled = a} :: ApiKey)

-- | The timestamp when the API Key was created.
apiKey_createdDate :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.UTCTime)
apiKey_createdDate = Lens.lens (\ApiKey' {createdDate} -> createdDate) (\s@ApiKey' {} a -> s {createdDate = a} :: ApiKey) Prelude.. Lens.mapping Core._Time

-- | The value of the API Key.
apiKey_value :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Text)
apiKey_value = Lens.lens (\ApiKey' {value} -> value) (\s@ApiKey' {} a -> s {value = a} :: ApiKey)

instance Core.FromJSON ApiKey where
  parseJSON =
    Core.withObject
      "ApiKey"
      ( \x ->
          ApiKey'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "customerId")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "lastUpdatedDate")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "stageKeys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "enabled")
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "value")
      )

instance Prelude.Hashable ApiKey where
  hashWithSalt _salt ApiKey' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` customerId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stageKeys
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` value

instance Prelude.NFData ApiKey where
  rnf ApiKey' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf customerId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf stageKeys
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf value
