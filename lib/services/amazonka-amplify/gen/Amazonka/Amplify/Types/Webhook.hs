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
-- Module      : Amazonka.Amplify.Types.Webhook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.Webhook where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a webhook that connects repository events to an Amplify app.
--
-- /See:/ 'newWebhook' smart constructor.
data Webhook = Webhook'
  { -- | The Amazon Resource Name (ARN) for the webhook.
    webhookArn :: Prelude.Text,
    -- | The ID of the webhook.
    webhookId :: Prelude.Text,
    -- | The URL of the webhook.
    webhookUrl :: Prelude.Text,
    -- | The name for a branch that is part of an Amplify app.
    branchName :: Prelude.Text,
    -- | The description for a webhook.
    description :: Prelude.Text,
    -- | The create date and time for a webhook.
    createTime :: Data.POSIX,
    -- | Updates the date and time for a webhook.
    updateTime :: Data.POSIX
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
-- 'webhookArn', 'webhook_webhookArn' - The Amazon Resource Name (ARN) for the webhook.
--
-- 'webhookId', 'webhook_webhookId' - The ID of the webhook.
--
-- 'webhookUrl', 'webhook_webhookUrl' - The URL of the webhook.
--
-- 'branchName', 'webhook_branchName' - The name for a branch that is part of an Amplify app.
--
-- 'description', 'webhook_description' - The description for a webhook.
--
-- 'createTime', 'webhook_createTime' - The create date and time for a webhook.
--
-- 'updateTime', 'webhook_updateTime' - Updates the date and time for a webhook.
newWebhook ::
  -- | 'webhookArn'
  Prelude.Text ->
  -- | 'webhookId'
  Prelude.Text ->
  -- | 'webhookUrl'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  Webhook
newWebhook
  pWebhookArn_
  pWebhookId_
  pWebhookUrl_
  pBranchName_
  pDescription_
  pCreateTime_
  pUpdateTime_ =
    Webhook'
      { webhookArn = pWebhookArn_,
        webhookId = pWebhookId_,
        webhookUrl = pWebhookUrl_,
        branchName = pBranchName_,
        description = pDescription_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The Amazon Resource Name (ARN) for the webhook.
webhook_webhookArn :: Lens.Lens' Webhook Prelude.Text
webhook_webhookArn = Lens.lens (\Webhook' {webhookArn} -> webhookArn) (\s@Webhook' {} a -> s {webhookArn = a} :: Webhook)

-- | The ID of the webhook.
webhook_webhookId :: Lens.Lens' Webhook Prelude.Text
webhook_webhookId = Lens.lens (\Webhook' {webhookId} -> webhookId) (\s@Webhook' {} a -> s {webhookId = a} :: Webhook)

-- | The URL of the webhook.
webhook_webhookUrl :: Lens.Lens' Webhook Prelude.Text
webhook_webhookUrl = Lens.lens (\Webhook' {webhookUrl} -> webhookUrl) (\s@Webhook' {} a -> s {webhookUrl = a} :: Webhook)

-- | The name for a branch that is part of an Amplify app.
webhook_branchName :: Lens.Lens' Webhook Prelude.Text
webhook_branchName = Lens.lens (\Webhook' {branchName} -> branchName) (\s@Webhook' {} a -> s {branchName = a} :: Webhook)

-- | The description for a webhook.
webhook_description :: Lens.Lens' Webhook Prelude.Text
webhook_description = Lens.lens (\Webhook' {description} -> description) (\s@Webhook' {} a -> s {description = a} :: Webhook)

-- | The create date and time for a webhook.
webhook_createTime :: Lens.Lens' Webhook Prelude.UTCTime
webhook_createTime = Lens.lens (\Webhook' {createTime} -> createTime) (\s@Webhook' {} a -> s {createTime = a} :: Webhook) Prelude.. Data._Time

-- | Updates the date and time for a webhook.
webhook_updateTime :: Lens.Lens' Webhook Prelude.UTCTime
webhook_updateTime = Lens.lens (\Webhook' {updateTime} -> updateTime) (\s@Webhook' {} a -> s {updateTime = a} :: Webhook) Prelude.. Data._Time

instance Data.FromJSON Webhook where
  parseJSON =
    Data.withObject
      "Webhook"
      ( \x ->
          Webhook'
            Prelude.<$> (x Data..: "webhookArn")
            Prelude.<*> (x Data..: "webhookId")
            Prelude.<*> (x Data..: "webhookUrl")
            Prelude.<*> (x Data..: "branchName")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
      )

instance Prelude.Hashable Webhook where
  hashWithSalt _salt Webhook' {..} =
    _salt
      `Prelude.hashWithSalt` webhookArn
      `Prelude.hashWithSalt` webhookId
      `Prelude.hashWithSalt` webhookUrl
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData Webhook where
  rnf Webhook' {..} =
    Prelude.rnf webhookArn
      `Prelude.seq` Prelude.rnf webhookId
      `Prelude.seq` Prelude.rnf webhookUrl
      `Prelude.seq` Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
