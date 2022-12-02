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
-- Module      : Amazonka.CodeBuild.Types.Webhook
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.Webhook where

import Amazonka.CodeBuild.Types.WebhookBuildType
import Amazonka.CodeBuild.Types.WebhookFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a webhook that connects repository events to a build
-- project in CodeBuild.
--
-- /See:/ 'newWebhook' smart constructor.
data Webhook = Webhook'
  { -- | A timestamp that indicates the last time a repository\'s secret token
    -- was modified.
    lastModifiedSecret :: Prelude.Maybe Data.POSIX,
    -- | The URL to the webhook.
    url :: Prelude.Maybe Prelude.Text,
    -- | The CodeBuild endpoint where webhook events are sent.
    payloadUrl :: Prelude.Maybe Prelude.Text,
    -- | The secret token of the associated repository.
    --
    -- A Bitbucket webhook does not support @secret@.
    secret :: Prelude.Maybe Prelude.Text,
    -- | A regular expression used to determine which repository branches are
    -- built when a webhook is triggered. If the name of a branch matches the
    -- regular expression, then it is built. If @branchFilter@ is empty, then
    -- all branches are built.
    --
    -- It is recommended that you use @filterGroups@ instead of @branchFilter@.
    branchFilter :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Prelude.Maybe WebhookBuildType,
    -- | An array of arrays of @WebhookFilter@ objects used to determine which
    -- webhooks are triggered. At least one @WebhookFilter@ in the array must
    -- specify @EVENT@ as its @type@.
    --
    -- For a build to be triggered, at least one filter group in the
    -- @filterGroups@ array must pass. For a filter group to pass, each of its
    -- filters must pass.
    filterGroups :: Prelude.Maybe [[WebhookFilter]]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Webhook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedSecret', 'webhook_lastModifiedSecret' - A timestamp that indicates the last time a repository\'s secret token
-- was modified.
--
-- 'url', 'webhook_url' - The URL to the webhook.
--
-- 'payloadUrl', 'webhook_payloadUrl' - The CodeBuild endpoint where webhook events are sent.
--
-- 'secret', 'webhook_secret' - The secret token of the associated repository.
--
-- A Bitbucket webhook does not support @secret@.
--
-- 'branchFilter', 'webhook_branchFilter' - A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
--
-- 'buildType', 'webhook_buildType' - Specifies the type of build this webhook will trigger.
--
-- 'filterGroups', 'webhook_filterGroups' - An array of arrays of @WebhookFilter@ objects used to determine which
-- webhooks are triggered. At least one @WebhookFilter@ in the array must
-- specify @EVENT@ as its @type@.
--
-- For a build to be triggered, at least one filter group in the
-- @filterGroups@ array must pass. For a filter group to pass, each of its
-- filters must pass.
newWebhook ::
  Webhook
newWebhook =
  Webhook'
    { lastModifiedSecret = Prelude.Nothing,
      url = Prelude.Nothing,
      payloadUrl = Prelude.Nothing,
      secret = Prelude.Nothing,
      branchFilter = Prelude.Nothing,
      buildType = Prelude.Nothing,
      filterGroups = Prelude.Nothing
    }

-- | A timestamp that indicates the last time a repository\'s secret token
-- was modified.
webhook_lastModifiedSecret :: Lens.Lens' Webhook (Prelude.Maybe Prelude.UTCTime)
webhook_lastModifiedSecret = Lens.lens (\Webhook' {lastModifiedSecret} -> lastModifiedSecret) (\s@Webhook' {} a -> s {lastModifiedSecret = a} :: Webhook) Prelude.. Lens.mapping Data._Time

-- | The URL to the webhook.
webhook_url :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_url = Lens.lens (\Webhook' {url} -> url) (\s@Webhook' {} a -> s {url = a} :: Webhook)

-- | The CodeBuild endpoint where webhook events are sent.
webhook_payloadUrl :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_payloadUrl = Lens.lens (\Webhook' {payloadUrl} -> payloadUrl) (\s@Webhook' {} a -> s {payloadUrl = a} :: Webhook)

-- | The secret token of the associated repository.
--
-- A Bitbucket webhook does not support @secret@.
webhook_secret :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_secret = Lens.lens (\Webhook' {secret} -> secret) (\s@Webhook' {} a -> s {secret = a} :: Webhook)

-- | A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
webhook_branchFilter :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_branchFilter = Lens.lens (\Webhook' {branchFilter} -> branchFilter) (\s@Webhook' {} a -> s {branchFilter = a} :: Webhook)

-- | Specifies the type of build this webhook will trigger.
webhook_buildType :: Lens.Lens' Webhook (Prelude.Maybe WebhookBuildType)
webhook_buildType = Lens.lens (\Webhook' {buildType} -> buildType) (\s@Webhook' {} a -> s {buildType = a} :: Webhook)

-- | An array of arrays of @WebhookFilter@ objects used to determine which
-- webhooks are triggered. At least one @WebhookFilter@ in the array must
-- specify @EVENT@ as its @type@.
--
-- For a build to be triggered, at least one filter group in the
-- @filterGroups@ array must pass. For a filter group to pass, each of its
-- filters must pass.
webhook_filterGroups :: Lens.Lens' Webhook (Prelude.Maybe [[WebhookFilter]])
webhook_filterGroups = Lens.lens (\Webhook' {filterGroups} -> filterGroups) (\s@Webhook' {} a -> s {filterGroups = a} :: Webhook) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Webhook where
  parseJSON =
    Data.withObject
      "Webhook"
      ( \x ->
          Webhook'
            Prelude.<$> (x Data..:? "lastModifiedSecret")
            Prelude.<*> (x Data..:? "url")
            Prelude.<*> (x Data..:? "payloadUrl")
            Prelude.<*> (x Data..:? "secret")
            Prelude.<*> (x Data..:? "branchFilter")
            Prelude.<*> (x Data..:? "buildType")
            Prelude.<*> (x Data..:? "filterGroups" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Webhook where
  hashWithSalt _salt Webhook' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedSecret
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` payloadUrl
      `Prelude.hashWithSalt` secret
      `Prelude.hashWithSalt` branchFilter
      `Prelude.hashWithSalt` buildType
      `Prelude.hashWithSalt` filterGroups

instance Prelude.NFData Webhook where
  rnf Webhook' {..} =
    Prelude.rnf lastModifiedSecret
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf payloadUrl
      `Prelude.seq` Prelude.rnf secret
      `Prelude.seq` Prelude.rnf branchFilter
      `Prelude.seq` Prelude.rnf buildType
      `Prelude.seq` Prelude.rnf filterGroups
