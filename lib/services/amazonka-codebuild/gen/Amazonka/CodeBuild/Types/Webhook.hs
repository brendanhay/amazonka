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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.Webhook where

import Amazonka.CodeBuild.Types.WebhookBuildType
import Amazonka.CodeBuild.Types.WebhookFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a webhook that connects repository events to a build
-- project in CodeBuild.
--
-- /See:/ 'newWebhook' smart constructor.
data Webhook = Webhook'
  { -- | A regular expression used to determine which repository branches are
    -- built when a webhook is triggered. If the name of a branch matches the
    -- regular expression, then it is built. If @branchFilter@ is empty, then
    -- all branches are built.
    --
    -- It is recommended that you use @filterGroups@ instead of @branchFilter@.
    branchFilter :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates the last time a repository\'s secret token
    -- was modified.
    lastModifiedSecret :: Prelude.Maybe Core.POSIX,
    -- | The URL to the webhook.
    url :: Prelude.Maybe Prelude.Text,
    -- | The secret token of the associated repository.
    --
    -- A Bitbucket webhook does not support @secret@.
    secret :: Prelude.Maybe Prelude.Text,
    -- | An array of arrays of @WebhookFilter@ objects used to determine which
    -- webhooks are triggered. At least one @WebhookFilter@ in the array must
    -- specify @EVENT@ as its @type@.
    --
    -- For a build to be triggered, at least one filter group in the
    -- @filterGroups@ array must pass. For a filter group to pass, each of its
    -- filters must pass.
    filterGroups :: Prelude.Maybe [[WebhookFilter]],
    -- | The CodeBuild endpoint where webhook events are sent.
    payloadUrl :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Prelude.Maybe WebhookBuildType
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
-- 'branchFilter', 'webhook_branchFilter' - A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
--
-- 'lastModifiedSecret', 'webhook_lastModifiedSecret' - A timestamp that indicates the last time a repository\'s secret token
-- was modified.
--
-- 'url', 'webhook_url' - The URL to the webhook.
--
-- 'secret', 'webhook_secret' - The secret token of the associated repository.
--
-- A Bitbucket webhook does not support @secret@.
--
-- 'filterGroups', 'webhook_filterGroups' - An array of arrays of @WebhookFilter@ objects used to determine which
-- webhooks are triggered. At least one @WebhookFilter@ in the array must
-- specify @EVENT@ as its @type@.
--
-- For a build to be triggered, at least one filter group in the
-- @filterGroups@ array must pass. For a filter group to pass, each of its
-- filters must pass.
--
-- 'payloadUrl', 'webhook_payloadUrl' - The CodeBuild endpoint where webhook events are sent.
--
-- 'buildType', 'webhook_buildType' - Specifies the type of build this webhook will trigger.
newWebhook ::
  Webhook
newWebhook =
  Webhook'
    { branchFilter = Prelude.Nothing,
      lastModifiedSecret = Prelude.Nothing,
      url = Prelude.Nothing,
      secret = Prelude.Nothing,
      filterGroups = Prelude.Nothing,
      payloadUrl = Prelude.Nothing,
      buildType = Prelude.Nothing
    }

-- | A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
webhook_branchFilter :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_branchFilter = Lens.lens (\Webhook' {branchFilter} -> branchFilter) (\s@Webhook' {} a -> s {branchFilter = a} :: Webhook)

-- | A timestamp that indicates the last time a repository\'s secret token
-- was modified.
webhook_lastModifiedSecret :: Lens.Lens' Webhook (Prelude.Maybe Prelude.UTCTime)
webhook_lastModifiedSecret = Lens.lens (\Webhook' {lastModifiedSecret} -> lastModifiedSecret) (\s@Webhook' {} a -> s {lastModifiedSecret = a} :: Webhook) Prelude.. Lens.mapping Core._Time

-- | The URL to the webhook.
webhook_url :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_url = Lens.lens (\Webhook' {url} -> url) (\s@Webhook' {} a -> s {url = a} :: Webhook)

-- | The secret token of the associated repository.
--
-- A Bitbucket webhook does not support @secret@.
webhook_secret :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_secret = Lens.lens (\Webhook' {secret} -> secret) (\s@Webhook' {} a -> s {secret = a} :: Webhook)

-- | An array of arrays of @WebhookFilter@ objects used to determine which
-- webhooks are triggered. At least one @WebhookFilter@ in the array must
-- specify @EVENT@ as its @type@.
--
-- For a build to be triggered, at least one filter group in the
-- @filterGroups@ array must pass. For a filter group to pass, each of its
-- filters must pass.
webhook_filterGroups :: Lens.Lens' Webhook (Prelude.Maybe [[WebhookFilter]])
webhook_filterGroups = Lens.lens (\Webhook' {filterGroups} -> filterGroups) (\s@Webhook' {} a -> s {filterGroups = a} :: Webhook) Prelude.. Lens.mapping Lens.coerced

-- | The CodeBuild endpoint where webhook events are sent.
webhook_payloadUrl :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_payloadUrl = Lens.lens (\Webhook' {payloadUrl} -> payloadUrl) (\s@Webhook' {} a -> s {payloadUrl = a} :: Webhook)

-- | Specifies the type of build this webhook will trigger.
webhook_buildType :: Lens.Lens' Webhook (Prelude.Maybe WebhookBuildType)
webhook_buildType = Lens.lens (\Webhook' {buildType} -> buildType) (\s@Webhook' {} a -> s {buildType = a} :: Webhook)

instance Core.FromJSON Webhook where
  parseJSON =
    Core.withObject
      "Webhook"
      ( \x ->
          Webhook'
            Prelude.<$> (x Core..:? "branchFilter")
            Prelude.<*> (x Core..:? "lastModifiedSecret")
            Prelude.<*> (x Core..:? "url")
            Prelude.<*> (x Core..:? "secret")
            Prelude.<*> (x Core..:? "filterGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "payloadUrl")
            Prelude.<*> (x Core..:? "buildType")
      )

instance Prelude.Hashable Webhook

instance Prelude.NFData Webhook
