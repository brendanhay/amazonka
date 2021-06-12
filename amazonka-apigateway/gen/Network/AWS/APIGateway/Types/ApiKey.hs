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
-- Module      : Network.AWS.APIGateway.Types.ApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.ApiKey where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A resource that can be distributed to callers for executing Method
-- resources that require an API key. API keys can be mapped to any Stage
-- on any RestApi, which indicates that the callers with the API key can
-- make requests to that stage.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html Use API Keys>
--
-- /See:/ 'newApiKey' smart constructor.
data ApiKey = ApiKey'
  { -- | The timestamp when the API Key was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | An AWS Marketplace customer identifier , when integrating with the AWS
    -- SaaS Marketplace.
    customerId :: Core.Maybe Core.Text,
    -- | The timestamp when the API Key was last updated.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    -- | A list of Stage resources that are associated with the ApiKey resource.
    stageKeys :: Core.Maybe [Core.Text],
    -- | Specifies whether the API Key can be used by callers.
    enabled :: Core.Maybe Core.Bool,
    -- | The identifier of the API Key.
    id :: Core.Maybe Core.Text,
    -- | The name of the API Key.
    name :: Core.Maybe Core.Text,
    -- | The collection of tags. Each tag element is associated with a given
    -- resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the API Key.
    description :: Core.Maybe Core.Text,
    -- | The value of the API Key.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'apiKey_createdDate' - The timestamp when the API Key was created.
--
-- 'customerId', 'apiKey_customerId' - An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
--
-- 'lastUpdatedDate', 'apiKey_lastUpdatedDate' - The timestamp when the API Key was last updated.
--
-- 'stageKeys', 'apiKey_stageKeys' - A list of Stage resources that are associated with the ApiKey resource.
--
-- 'enabled', 'apiKey_enabled' - Specifies whether the API Key can be used by callers.
--
-- 'id', 'apiKey_id' - The identifier of the API Key.
--
-- 'name', 'apiKey_name' - The name of the API Key.
--
-- 'tags', 'apiKey_tags' - The collection of tags. Each tag element is associated with a given
-- resource.
--
-- 'description', 'apiKey_description' - The description of the API Key.
--
-- 'value', 'apiKey_value' - The value of the API Key.
newApiKey ::
  ApiKey
newApiKey =
  ApiKey'
    { createdDate = Core.Nothing,
      customerId = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      stageKeys = Core.Nothing,
      enabled = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      value = Core.Nothing
    }

-- | The timestamp when the API Key was created.
apiKey_createdDate :: Lens.Lens' ApiKey (Core.Maybe Core.UTCTime)
apiKey_createdDate = Lens.lens (\ApiKey' {createdDate} -> createdDate) (\s@ApiKey' {} a -> s {createdDate = a} :: ApiKey) Core.. Lens.mapping Core._Time

-- | An AWS Marketplace customer identifier , when integrating with the AWS
-- SaaS Marketplace.
apiKey_customerId :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
apiKey_customerId = Lens.lens (\ApiKey' {customerId} -> customerId) (\s@ApiKey' {} a -> s {customerId = a} :: ApiKey)

-- | The timestamp when the API Key was last updated.
apiKey_lastUpdatedDate :: Lens.Lens' ApiKey (Core.Maybe Core.UTCTime)
apiKey_lastUpdatedDate = Lens.lens (\ApiKey' {lastUpdatedDate} -> lastUpdatedDate) (\s@ApiKey' {} a -> s {lastUpdatedDate = a} :: ApiKey) Core.. Lens.mapping Core._Time

-- | A list of Stage resources that are associated with the ApiKey resource.
apiKey_stageKeys :: Lens.Lens' ApiKey (Core.Maybe [Core.Text])
apiKey_stageKeys = Lens.lens (\ApiKey' {stageKeys} -> stageKeys) (\s@ApiKey' {} a -> s {stageKeys = a} :: ApiKey) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether the API Key can be used by callers.
apiKey_enabled :: Lens.Lens' ApiKey (Core.Maybe Core.Bool)
apiKey_enabled = Lens.lens (\ApiKey' {enabled} -> enabled) (\s@ApiKey' {} a -> s {enabled = a} :: ApiKey)

-- | The identifier of the API Key.
apiKey_id :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
apiKey_id = Lens.lens (\ApiKey' {id} -> id) (\s@ApiKey' {} a -> s {id = a} :: ApiKey)

-- | The name of the API Key.
apiKey_name :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
apiKey_name = Lens.lens (\ApiKey' {name} -> name) (\s@ApiKey' {} a -> s {name = a} :: ApiKey)

-- | The collection of tags. Each tag element is associated with a given
-- resource.
apiKey_tags :: Lens.Lens' ApiKey (Core.Maybe (Core.HashMap Core.Text Core.Text))
apiKey_tags = Lens.lens (\ApiKey' {tags} -> tags) (\s@ApiKey' {} a -> s {tags = a} :: ApiKey) Core.. Lens.mapping Lens._Coerce

-- | The description of the API Key.
apiKey_description :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
apiKey_description = Lens.lens (\ApiKey' {description} -> description) (\s@ApiKey' {} a -> s {description = a} :: ApiKey)

-- | The value of the API Key.
apiKey_value :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
apiKey_value = Lens.lens (\ApiKey' {value} -> value) (\s@ApiKey' {} a -> s {value = a} :: ApiKey)

instance Core.FromJSON ApiKey where
  parseJSON =
    Core.withObject
      "ApiKey"
      ( \x ->
          ApiKey'
            Core.<$> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "customerId")
            Core.<*> (x Core..:? "lastUpdatedDate")
            Core.<*> (x Core..:? "stageKeys" Core..!= Core.mempty)
            Core.<*> (x Core..:? "enabled")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "value")
      )

instance Core.Hashable ApiKey

instance Core.NFData ApiKey
