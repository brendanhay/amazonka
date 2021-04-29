{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.Webhook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Webhook where

import Network.AWS.CodeBuild.Types.WebhookBuildType
import Network.AWS.CodeBuild.Types.WebhookFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a webhook that connects repository events to a build
-- project in AWS CodeBuild.
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
    -- | The AWS CodeBuild endpoint where webhook events are sent.
    payloadUrl :: Prelude.Maybe Prelude.Text,
    -- | An array of arrays of @WebhookFilter@ objects used to determine which
    -- webhooks are triggered. At least one @WebhookFilter@ in the array must
    -- specify @EVENT@ as its @type@.
    --
    -- For a build to be triggered, at least one filter group in the
    -- @filterGroups@ array must pass. For a filter group to pass, each of its
    -- filters must pass.
    filterGroups :: Prelude.Maybe [[WebhookFilter]],
    -- | The secret token of the associated repository.
    --
    -- A Bitbucket webhook does not support @secret@.
    secret :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Prelude.Maybe WebhookBuildType,
    -- | The URL to the webhook.
    url :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates the last time a repository\'s secret token
    -- was modified.
    lastModifiedSecret :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'payloadUrl', 'webhook_payloadUrl' - The AWS CodeBuild endpoint where webhook events are sent.
--
-- 'filterGroups', 'webhook_filterGroups' - An array of arrays of @WebhookFilter@ objects used to determine which
-- webhooks are triggered. At least one @WebhookFilter@ in the array must
-- specify @EVENT@ as its @type@.
--
-- For a build to be triggered, at least one filter group in the
-- @filterGroups@ array must pass. For a filter group to pass, each of its
-- filters must pass.
--
-- 'secret', 'webhook_secret' - The secret token of the associated repository.
--
-- A Bitbucket webhook does not support @secret@.
--
-- 'buildType', 'webhook_buildType' - Specifies the type of build this webhook will trigger.
--
-- 'url', 'webhook_url' - The URL to the webhook.
--
-- 'lastModifiedSecret', 'webhook_lastModifiedSecret' - A timestamp that indicates the last time a repository\'s secret token
-- was modified.
newWebhook ::
  Webhook
newWebhook =
  Webhook'
    { branchFilter = Prelude.Nothing,
      payloadUrl = Prelude.Nothing,
      filterGroups = Prelude.Nothing,
      secret = Prelude.Nothing,
      buildType = Prelude.Nothing,
      url = Prelude.Nothing,
      lastModifiedSecret = Prelude.Nothing
    }

-- | A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
webhook_branchFilter :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_branchFilter = Lens.lens (\Webhook' {branchFilter} -> branchFilter) (\s@Webhook' {} a -> s {branchFilter = a} :: Webhook)

-- | The AWS CodeBuild endpoint where webhook events are sent.
webhook_payloadUrl :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_payloadUrl = Lens.lens (\Webhook' {payloadUrl} -> payloadUrl) (\s@Webhook' {} a -> s {payloadUrl = a} :: Webhook)

-- | An array of arrays of @WebhookFilter@ objects used to determine which
-- webhooks are triggered. At least one @WebhookFilter@ in the array must
-- specify @EVENT@ as its @type@.
--
-- For a build to be triggered, at least one filter group in the
-- @filterGroups@ array must pass. For a filter group to pass, each of its
-- filters must pass.
webhook_filterGroups :: Lens.Lens' Webhook (Prelude.Maybe [[WebhookFilter]])
webhook_filterGroups = Lens.lens (\Webhook' {filterGroups} -> filterGroups) (\s@Webhook' {} a -> s {filterGroups = a} :: Webhook) Prelude.. Lens.mapping Prelude._Coerce

-- | The secret token of the associated repository.
--
-- A Bitbucket webhook does not support @secret@.
webhook_secret :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_secret = Lens.lens (\Webhook' {secret} -> secret) (\s@Webhook' {} a -> s {secret = a} :: Webhook)

-- | Specifies the type of build this webhook will trigger.
webhook_buildType :: Lens.Lens' Webhook (Prelude.Maybe WebhookBuildType)
webhook_buildType = Lens.lens (\Webhook' {buildType} -> buildType) (\s@Webhook' {} a -> s {buildType = a} :: Webhook)

-- | The URL to the webhook.
webhook_url :: Lens.Lens' Webhook (Prelude.Maybe Prelude.Text)
webhook_url = Lens.lens (\Webhook' {url} -> url) (\s@Webhook' {} a -> s {url = a} :: Webhook)

-- | A timestamp that indicates the last time a repository\'s secret token
-- was modified.
webhook_lastModifiedSecret :: Lens.Lens' Webhook (Prelude.Maybe Prelude.UTCTime)
webhook_lastModifiedSecret = Lens.lens (\Webhook' {lastModifiedSecret} -> lastModifiedSecret) (\s@Webhook' {} a -> s {lastModifiedSecret = a} :: Webhook) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON Webhook where
  parseJSON =
    Prelude.withObject
      "Webhook"
      ( \x ->
          Webhook'
            Prelude.<$> (x Prelude..:? "branchFilter")
            Prelude.<*> (x Prelude..:? "payloadUrl")
            Prelude.<*> ( x Prelude..:? "filterGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "secret")
            Prelude.<*> (x Prelude..:? "buildType")
            Prelude.<*> (x Prelude..:? "url")
            Prelude.<*> (x Prelude..:? "lastModifiedSecret")
      )

instance Prelude.Hashable Webhook

instance Prelude.NFData Webhook
