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
-- Module      : Amazonka.LexV2Models.UpdateBotAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing bot alias.
module Amazonka.LexV2Models.UpdateBotAlias
  ( -- * Creating a Request
    UpdateBotAlias (..),
    newUpdateBotAlias,

    -- * Request Lenses
    updateBotAlias_botVersion,
    updateBotAlias_description,
    updateBotAlias_sentimentAnalysisSettings,
    updateBotAlias_conversationLogSettings,
    updateBotAlias_botAliasLocaleSettings,
    updateBotAlias_botAliasId,
    updateBotAlias_botAliasName,
    updateBotAlias_botId,

    -- * Destructuring the Response
    UpdateBotAliasResponse (..),
    newUpdateBotAliasResponse,

    -- * Response Lenses
    updateBotAliasResponse_botAliasStatus,
    updateBotAliasResponse_botVersion,
    updateBotAliasResponse_creationDateTime,
    updateBotAliasResponse_description,
    updateBotAliasResponse_sentimentAnalysisSettings,
    updateBotAliasResponse_botId,
    updateBotAliasResponse_botAliasId,
    updateBotAliasResponse_conversationLogSettings,
    updateBotAliasResponse_botAliasLocaleSettings,
    updateBotAliasResponse_botAliasName,
    updateBotAliasResponse_lastUpdatedDateTime,
    updateBotAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBotAlias' smart constructor.
data UpdateBotAlias = UpdateBotAlias'
  { -- | The new bot version to assign to the bot alias.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The new description to assign to the bot alias.
    description :: Prelude.Maybe Prelude.Text,
    sentimentAnalysisSettings :: Prelude.Maybe SentimentAnalysisSettings,
    -- | The new settings for storing conversation logs in Amazon CloudWatch Logs
    -- and Amazon S3 buckets.
    conversationLogSettings :: Prelude.Maybe ConversationLogSettings,
    -- | The new Lambda functions to use in each locale for the bot alias.
    botAliasLocaleSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings),
    -- | The unique identifier of the bot alias.
    botAliasId :: Prelude.Text,
    -- | The new name to assign to the bot alias.
    botAliasName :: Prelude.Text,
    -- | The identifier of the bot with the updated alias.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBotAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botVersion', 'updateBotAlias_botVersion' - The new bot version to assign to the bot alias.
--
-- 'description', 'updateBotAlias_description' - The new description to assign to the bot alias.
--
-- 'sentimentAnalysisSettings', 'updateBotAlias_sentimentAnalysisSettings' - Undocumented member.
--
-- 'conversationLogSettings', 'updateBotAlias_conversationLogSettings' - The new settings for storing conversation logs in Amazon CloudWatch Logs
-- and Amazon S3 buckets.
--
-- 'botAliasLocaleSettings', 'updateBotAlias_botAliasLocaleSettings' - The new Lambda functions to use in each locale for the bot alias.
--
-- 'botAliasId', 'updateBotAlias_botAliasId' - The unique identifier of the bot alias.
--
-- 'botAliasName', 'updateBotAlias_botAliasName' - The new name to assign to the bot alias.
--
-- 'botId', 'updateBotAlias_botId' - The identifier of the bot with the updated alias.
newUpdateBotAlias ::
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'botAliasName'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  UpdateBotAlias
newUpdateBotAlias pBotAliasId_ pBotAliasName_ pBotId_ =
  UpdateBotAlias'
    { botVersion = Prelude.Nothing,
      description = Prelude.Nothing,
      sentimentAnalysisSettings = Prelude.Nothing,
      conversationLogSettings = Prelude.Nothing,
      botAliasLocaleSettings = Prelude.Nothing,
      botAliasId = pBotAliasId_,
      botAliasName = pBotAliasName_,
      botId = pBotId_
    }

-- | The new bot version to assign to the bot alias.
updateBotAlias_botVersion :: Lens.Lens' UpdateBotAlias (Prelude.Maybe Prelude.Text)
updateBotAlias_botVersion = Lens.lens (\UpdateBotAlias' {botVersion} -> botVersion) (\s@UpdateBotAlias' {} a -> s {botVersion = a} :: UpdateBotAlias)

-- | The new description to assign to the bot alias.
updateBotAlias_description :: Lens.Lens' UpdateBotAlias (Prelude.Maybe Prelude.Text)
updateBotAlias_description = Lens.lens (\UpdateBotAlias' {description} -> description) (\s@UpdateBotAlias' {} a -> s {description = a} :: UpdateBotAlias)

-- | Undocumented member.
updateBotAlias_sentimentAnalysisSettings :: Lens.Lens' UpdateBotAlias (Prelude.Maybe SentimentAnalysisSettings)
updateBotAlias_sentimentAnalysisSettings = Lens.lens (\UpdateBotAlias' {sentimentAnalysisSettings} -> sentimentAnalysisSettings) (\s@UpdateBotAlias' {} a -> s {sentimentAnalysisSettings = a} :: UpdateBotAlias)

-- | The new settings for storing conversation logs in Amazon CloudWatch Logs
-- and Amazon S3 buckets.
updateBotAlias_conversationLogSettings :: Lens.Lens' UpdateBotAlias (Prelude.Maybe ConversationLogSettings)
updateBotAlias_conversationLogSettings = Lens.lens (\UpdateBotAlias' {conversationLogSettings} -> conversationLogSettings) (\s@UpdateBotAlias' {} a -> s {conversationLogSettings = a} :: UpdateBotAlias)

-- | The new Lambda functions to use in each locale for the bot alias.
updateBotAlias_botAliasLocaleSettings :: Lens.Lens' UpdateBotAlias (Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings))
updateBotAlias_botAliasLocaleSettings = Lens.lens (\UpdateBotAlias' {botAliasLocaleSettings} -> botAliasLocaleSettings) (\s@UpdateBotAlias' {} a -> s {botAliasLocaleSettings = a} :: UpdateBotAlias) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the bot alias.
updateBotAlias_botAliasId :: Lens.Lens' UpdateBotAlias Prelude.Text
updateBotAlias_botAliasId = Lens.lens (\UpdateBotAlias' {botAliasId} -> botAliasId) (\s@UpdateBotAlias' {} a -> s {botAliasId = a} :: UpdateBotAlias)

-- | The new name to assign to the bot alias.
updateBotAlias_botAliasName :: Lens.Lens' UpdateBotAlias Prelude.Text
updateBotAlias_botAliasName = Lens.lens (\UpdateBotAlias' {botAliasName} -> botAliasName) (\s@UpdateBotAlias' {} a -> s {botAliasName = a} :: UpdateBotAlias)

-- | The identifier of the bot with the updated alias.
updateBotAlias_botId :: Lens.Lens' UpdateBotAlias Prelude.Text
updateBotAlias_botId = Lens.lens (\UpdateBotAlias' {botId} -> botId) (\s@UpdateBotAlias' {} a -> s {botId = a} :: UpdateBotAlias)

instance Core.AWSRequest UpdateBotAlias where
  type
    AWSResponse UpdateBotAlias =
      UpdateBotAliasResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBotAliasResponse'
            Prelude.<$> (x Core..?> "botAliasStatus")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "sentimentAnalysisSettings")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "botAliasId")
            Prelude.<*> (x Core..?> "conversationLogSettings")
            Prelude.<*> ( x Core..?> "botAliasLocaleSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "botAliasName")
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBotAlias where
  hashWithSalt _salt UpdateBotAlias' {..} =
    _salt `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sentimentAnalysisSettings
      `Prelude.hashWithSalt` conversationLogSettings
      `Prelude.hashWithSalt` botAliasLocaleSettings
      `Prelude.hashWithSalt` botAliasId
      `Prelude.hashWithSalt` botAliasName
      `Prelude.hashWithSalt` botId

instance Prelude.NFData UpdateBotAlias where
  rnf UpdateBotAlias' {..} =
    Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sentimentAnalysisSettings
      `Prelude.seq` Prelude.rnf conversationLogSettings
      `Prelude.seq` Prelude.rnf botAliasLocaleSettings
      `Prelude.seq` Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf botAliasName
      `Prelude.seq` Prelude.rnf botId

instance Core.ToHeaders UpdateBotAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateBotAlias where
  toJSON UpdateBotAlias' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("botVersion" Core..=) Prelude.<$> botVersion,
            ("description" Core..=) Prelude.<$> description,
            ("sentimentAnalysisSettings" Core..=)
              Prelude.<$> sentimentAnalysisSettings,
            ("conversationLogSettings" Core..=)
              Prelude.<$> conversationLogSettings,
            ("botAliasLocaleSettings" Core..=)
              Prelude.<$> botAliasLocaleSettings,
            Prelude.Just ("botAliasName" Core..= botAliasName)
          ]
      )

instance Core.ToPath UpdateBotAlias where
  toPath UpdateBotAlias' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botaliases/",
        Core.toBS botAliasId,
        "/"
      ]

instance Core.ToQuery UpdateBotAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBotAliasResponse' smart constructor.
data UpdateBotAliasResponse = UpdateBotAliasResponse'
  { -- | The current status of the bot alias. When the status is @Available@ the
    -- alias is ready for use.
    botAliasStatus :: Prelude.Maybe BotAliasStatus,
    -- | The updated version of the bot that the alias points to.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The updated description of the bot alias.
    description :: Prelude.Maybe Prelude.Text,
    sentimentAnalysisSettings :: Prelude.Maybe SentimentAnalysisSettings,
    -- | The identifier of the bot with the updated alias.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the updated bot alias.
    botAliasId :: Prelude.Maybe Prelude.Text,
    -- | The updated settings for storing conversation logs in Amazon CloudWatch
    -- Logs and Amazon S3 buckets.
    conversationLogSettings :: Prelude.Maybe ConversationLogSettings,
    -- | The updated Lambda functions to use in each locale for the bot alias.
    botAliasLocaleSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings),
    -- | The updated name of the bot alias.
    botAliasName :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBotAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasStatus', 'updateBotAliasResponse_botAliasStatus' - The current status of the bot alias. When the status is @Available@ the
-- alias is ready for use.
--
-- 'botVersion', 'updateBotAliasResponse_botVersion' - The updated version of the bot that the alias points to.
--
-- 'creationDateTime', 'updateBotAliasResponse_creationDateTime' - A timestamp of the date and time that the bot was created.
--
-- 'description', 'updateBotAliasResponse_description' - The updated description of the bot alias.
--
-- 'sentimentAnalysisSettings', 'updateBotAliasResponse_sentimentAnalysisSettings' - Undocumented member.
--
-- 'botId', 'updateBotAliasResponse_botId' - The identifier of the bot with the updated alias.
--
-- 'botAliasId', 'updateBotAliasResponse_botAliasId' - The identifier of the updated bot alias.
--
-- 'conversationLogSettings', 'updateBotAliasResponse_conversationLogSettings' - The updated settings for storing conversation logs in Amazon CloudWatch
-- Logs and Amazon S3 buckets.
--
-- 'botAliasLocaleSettings', 'updateBotAliasResponse_botAliasLocaleSettings' - The updated Lambda functions to use in each locale for the bot alias.
--
-- 'botAliasName', 'updateBotAliasResponse_botAliasName' - The updated name of the bot alias.
--
-- 'lastUpdatedDateTime', 'updateBotAliasResponse_lastUpdatedDateTime' - A timestamp of the date and time that the bot was last updated.
--
-- 'httpStatus', 'updateBotAliasResponse_httpStatus' - The response's http status code.
newUpdateBotAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBotAliasResponse
newUpdateBotAliasResponse pHttpStatus_ =
  UpdateBotAliasResponse'
    { botAliasStatus =
        Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      sentimentAnalysisSettings = Prelude.Nothing,
      botId = Prelude.Nothing,
      botAliasId = Prelude.Nothing,
      conversationLogSettings = Prelude.Nothing,
      botAliasLocaleSettings = Prelude.Nothing,
      botAliasName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the bot alias. When the status is @Available@ the
-- alias is ready for use.
updateBotAliasResponse_botAliasStatus :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe BotAliasStatus)
updateBotAliasResponse_botAliasStatus = Lens.lens (\UpdateBotAliasResponse' {botAliasStatus} -> botAliasStatus) (\s@UpdateBotAliasResponse' {} a -> s {botAliasStatus = a} :: UpdateBotAliasResponse)

-- | The updated version of the bot that the alias points to.
updateBotAliasResponse_botVersion :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe Prelude.Text)
updateBotAliasResponse_botVersion = Lens.lens (\UpdateBotAliasResponse' {botVersion} -> botVersion) (\s@UpdateBotAliasResponse' {} a -> s {botVersion = a} :: UpdateBotAliasResponse)

-- | A timestamp of the date and time that the bot was created.
updateBotAliasResponse_creationDateTime :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
updateBotAliasResponse_creationDateTime = Lens.lens (\UpdateBotAliasResponse' {creationDateTime} -> creationDateTime) (\s@UpdateBotAliasResponse' {} a -> s {creationDateTime = a} :: UpdateBotAliasResponse) Prelude.. Lens.mapping Core._Time

-- | The updated description of the bot alias.
updateBotAliasResponse_description :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe Prelude.Text)
updateBotAliasResponse_description = Lens.lens (\UpdateBotAliasResponse' {description} -> description) (\s@UpdateBotAliasResponse' {} a -> s {description = a} :: UpdateBotAliasResponse)

-- | Undocumented member.
updateBotAliasResponse_sentimentAnalysisSettings :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe SentimentAnalysisSettings)
updateBotAliasResponse_sentimentAnalysisSettings = Lens.lens (\UpdateBotAliasResponse' {sentimentAnalysisSettings} -> sentimentAnalysisSettings) (\s@UpdateBotAliasResponse' {} a -> s {sentimentAnalysisSettings = a} :: UpdateBotAliasResponse)

-- | The identifier of the bot with the updated alias.
updateBotAliasResponse_botId :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe Prelude.Text)
updateBotAliasResponse_botId = Lens.lens (\UpdateBotAliasResponse' {botId} -> botId) (\s@UpdateBotAliasResponse' {} a -> s {botId = a} :: UpdateBotAliasResponse)

-- | The identifier of the updated bot alias.
updateBotAliasResponse_botAliasId :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe Prelude.Text)
updateBotAliasResponse_botAliasId = Lens.lens (\UpdateBotAliasResponse' {botAliasId} -> botAliasId) (\s@UpdateBotAliasResponse' {} a -> s {botAliasId = a} :: UpdateBotAliasResponse)

-- | The updated settings for storing conversation logs in Amazon CloudWatch
-- Logs and Amazon S3 buckets.
updateBotAliasResponse_conversationLogSettings :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe ConversationLogSettings)
updateBotAliasResponse_conversationLogSettings = Lens.lens (\UpdateBotAliasResponse' {conversationLogSettings} -> conversationLogSettings) (\s@UpdateBotAliasResponse' {} a -> s {conversationLogSettings = a} :: UpdateBotAliasResponse)

-- | The updated Lambda functions to use in each locale for the bot alias.
updateBotAliasResponse_botAliasLocaleSettings :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings))
updateBotAliasResponse_botAliasLocaleSettings = Lens.lens (\UpdateBotAliasResponse' {botAliasLocaleSettings} -> botAliasLocaleSettings) (\s@UpdateBotAliasResponse' {} a -> s {botAliasLocaleSettings = a} :: UpdateBotAliasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The updated name of the bot alias.
updateBotAliasResponse_botAliasName :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe Prelude.Text)
updateBotAliasResponse_botAliasName = Lens.lens (\UpdateBotAliasResponse' {botAliasName} -> botAliasName) (\s@UpdateBotAliasResponse' {} a -> s {botAliasName = a} :: UpdateBotAliasResponse)

-- | A timestamp of the date and time that the bot was last updated.
updateBotAliasResponse_lastUpdatedDateTime :: Lens.Lens' UpdateBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
updateBotAliasResponse_lastUpdatedDateTime = Lens.lens (\UpdateBotAliasResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateBotAliasResponse' {} a -> s {lastUpdatedDateTime = a} :: UpdateBotAliasResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
updateBotAliasResponse_httpStatus :: Lens.Lens' UpdateBotAliasResponse Prelude.Int
updateBotAliasResponse_httpStatus = Lens.lens (\UpdateBotAliasResponse' {httpStatus} -> httpStatus) (\s@UpdateBotAliasResponse' {} a -> s {httpStatus = a} :: UpdateBotAliasResponse)

instance Prelude.NFData UpdateBotAliasResponse where
  rnf UpdateBotAliasResponse' {..} =
    Prelude.rnf botAliasStatus
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sentimentAnalysisSettings
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf conversationLogSettings
      `Prelude.seq` Prelude.rnf botAliasLocaleSettings
      `Prelude.seq` Prelude.rnf botAliasName
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf httpStatus
