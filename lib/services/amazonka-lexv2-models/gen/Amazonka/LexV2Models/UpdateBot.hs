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
-- Module      : Amazonka.LexV2Models.UpdateBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration of an existing bot.
module Amazonka.LexV2Models.UpdateBot
  ( -- * Creating a Request
    UpdateBot (..),
    newUpdateBot,

    -- * Request Lenses
    updateBot_botMembers,
    updateBot_botType,
    updateBot_description,
    updateBot_botId,
    updateBot_botName,
    updateBot_roleArn,
    updateBot_dataPrivacy,
    updateBot_idleSessionTTLInSeconds,

    -- * Destructuring the Response
    UpdateBotResponse (..),
    newUpdateBotResponse,

    -- * Response Lenses
    updateBotResponse_botId,
    updateBotResponse_botMembers,
    updateBotResponse_botName,
    updateBotResponse_botStatus,
    updateBotResponse_botType,
    updateBotResponse_creationDateTime,
    updateBotResponse_dataPrivacy,
    updateBotResponse_description,
    updateBotResponse_idleSessionTTLInSeconds,
    updateBotResponse_lastUpdatedDateTime,
    updateBotResponse_roleArn,
    updateBotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBot' smart constructor.
data UpdateBot = UpdateBot'
  { -- | The list of bot members in the network associated with the update
    -- action.
    botMembers :: Prelude.Maybe [BotMember],
    -- | The type of the bot to be updated.
    botType :: Prelude.Maybe BotType,
    -- | A description of the bot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the bot to update. This identifier is returned
    -- by the
    -- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_CreateBot.html CreateBot>
    -- operation.
    botId :: Prelude.Text,
    -- | The new name of the bot. The name must be unique in the account that
    -- creates the bot.
    botName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
    -- access the bot.
    roleArn :: Prelude.Text,
    -- | Provides information on additional privacy protections Amazon Lex should
    -- use with the bot\'s data.
    dataPrivacy :: DataPrivacy,
    -- | The time, in seconds, that Amazon Lex should keep information about a
    -- user\'s conversation with the bot.
    --
    -- A user interaction remains active for the amount of time specified. If
    -- no conversation occurs during this time, the session expires and Amazon
    -- Lex deletes any data provided before the timeout.
    --
    -- You can specify between 60 (1 minute) and 86,400 (24 hours) seconds.
    idleSessionTTLInSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botMembers', 'updateBot_botMembers' - The list of bot members in the network associated with the update
-- action.
--
-- 'botType', 'updateBot_botType' - The type of the bot to be updated.
--
-- 'description', 'updateBot_description' - A description of the bot.
--
-- 'botId', 'updateBot_botId' - The unique identifier of the bot to update. This identifier is returned
-- by the
-- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_CreateBot.html CreateBot>
-- operation.
--
-- 'botName', 'updateBot_botName' - The new name of the bot. The name must be unique in the account that
-- creates the bot.
--
-- 'roleArn', 'updateBot_roleArn' - The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the bot.
--
-- 'dataPrivacy', 'updateBot_dataPrivacy' - Provides information on additional privacy protections Amazon Lex should
-- use with the bot\'s data.
--
-- 'idleSessionTTLInSeconds', 'updateBot_idleSessionTTLInSeconds' - The time, in seconds, that Amazon Lex should keep information about a
-- user\'s conversation with the bot.
--
-- A user interaction remains active for the amount of time specified. If
-- no conversation occurs during this time, the session expires and Amazon
-- Lex deletes any data provided before the timeout.
--
-- You can specify between 60 (1 minute) and 86,400 (24 hours) seconds.
newUpdateBot ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'dataPrivacy'
  DataPrivacy ->
  -- | 'idleSessionTTLInSeconds'
  Prelude.Natural ->
  UpdateBot
newUpdateBot
  pBotId_
  pBotName_
  pRoleArn_
  pDataPrivacy_
  pIdleSessionTTLInSeconds_ =
    UpdateBot'
      { botMembers = Prelude.Nothing,
        botType = Prelude.Nothing,
        description = Prelude.Nothing,
        botId = pBotId_,
        botName = pBotName_,
        roleArn = pRoleArn_,
        dataPrivacy = pDataPrivacy_,
        idleSessionTTLInSeconds = pIdleSessionTTLInSeconds_
      }

-- | The list of bot members in the network associated with the update
-- action.
updateBot_botMembers :: Lens.Lens' UpdateBot (Prelude.Maybe [BotMember])
updateBot_botMembers = Lens.lens (\UpdateBot' {botMembers} -> botMembers) (\s@UpdateBot' {} a -> s {botMembers = a} :: UpdateBot) Prelude.. Lens.mapping Lens.coerced

-- | The type of the bot to be updated.
updateBot_botType :: Lens.Lens' UpdateBot (Prelude.Maybe BotType)
updateBot_botType = Lens.lens (\UpdateBot' {botType} -> botType) (\s@UpdateBot' {} a -> s {botType = a} :: UpdateBot)

-- | A description of the bot.
updateBot_description :: Lens.Lens' UpdateBot (Prelude.Maybe Prelude.Text)
updateBot_description = Lens.lens (\UpdateBot' {description} -> description) (\s@UpdateBot' {} a -> s {description = a} :: UpdateBot)

-- | The unique identifier of the bot to update. This identifier is returned
-- by the
-- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_CreateBot.html CreateBot>
-- operation.
updateBot_botId :: Lens.Lens' UpdateBot Prelude.Text
updateBot_botId = Lens.lens (\UpdateBot' {botId} -> botId) (\s@UpdateBot' {} a -> s {botId = a} :: UpdateBot)

-- | The new name of the bot. The name must be unique in the account that
-- creates the bot.
updateBot_botName :: Lens.Lens' UpdateBot Prelude.Text
updateBot_botName = Lens.lens (\UpdateBot' {botName} -> botName) (\s@UpdateBot' {} a -> s {botName = a} :: UpdateBot)

-- | The Amazon Resource Name (ARN) of an IAM role that has permissions to
-- access the bot.
updateBot_roleArn :: Lens.Lens' UpdateBot Prelude.Text
updateBot_roleArn = Lens.lens (\UpdateBot' {roleArn} -> roleArn) (\s@UpdateBot' {} a -> s {roleArn = a} :: UpdateBot)

-- | Provides information on additional privacy protections Amazon Lex should
-- use with the bot\'s data.
updateBot_dataPrivacy :: Lens.Lens' UpdateBot DataPrivacy
updateBot_dataPrivacy = Lens.lens (\UpdateBot' {dataPrivacy} -> dataPrivacy) (\s@UpdateBot' {} a -> s {dataPrivacy = a} :: UpdateBot)

-- | The time, in seconds, that Amazon Lex should keep information about a
-- user\'s conversation with the bot.
--
-- A user interaction remains active for the amount of time specified. If
-- no conversation occurs during this time, the session expires and Amazon
-- Lex deletes any data provided before the timeout.
--
-- You can specify between 60 (1 minute) and 86,400 (24 hours) seconds.
updateBot_idleSessionTTLInSeconds :: Lens.Lens' UpdateBot Prelude.Natural
updateBot_idleSessionTTLInSeconds = Lens.lens (\UpdateBot' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@UpdateBot' {} a -> s {idleSessionTTLInSeconds = a} :: UpdateBot)

instance Core.AWSRequest UpdateBot where
  type AWSResponse UpdateBot = UpdateBotResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBotResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botMembers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "botName")
            Prelude.<*> (x Data..?> "botStatus")
            Prelude.<*> (x Data..?> "botType")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "dataPrivacy")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "idleSessionTTLInSeconds")
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBot where
  hashWithSalt _salt UpdateBot' {..} =
    _salt
      `Prelude.hashWithSalt` botMembers
      `Prelude.hashWithSalt` botType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` dataPrivacy
      `Prelude.hashWithSalt` idleSessionTTLInSeconds

instance Prelude.NFData UpdateBot where
  rnf UpdateBot' {..} =
    Prelude.rnf botMembers
      `Prelude.seq` Prelude.rnf botType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf dataPrivacy
      `Prelude.seq` Prelude.rnf idleSessionTTLInSeconds

instance Data.ToHeaders UpdateBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBot where
  toJSON UpdateBot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("botMembers" Data..=) Prelude.<$> botMembers,
            ("botType" Data..=) Prelude.<$> botType,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("botName" Data..= botName),
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("dataPrivacy" Data..= dataPrivacy),
            Prelude.Just
              ( "idleSessionTTLInSeconds"
                  Data..= idleSessionTTLInSeconds
              )
          ]
      )

instance Data.ToPath UpdateBot where
  toPath UpdateBot' {..} =
    Prelude.mconcat ["/bots/", Data.toBS botId, "/"]

instance Data.ToQuery UpdateBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBotResponse' smart constructor.
data UpdateBotResponse = UpdateBotResponse'
  { -- | The unique identifier of the bot that was updated.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The list of bot members in the network that was updated.
    botMembers :: Prelude.Maybe [BotMember],
    -- | The name of the bot after the update.
    botName :: Prelude.Maybe Prelude.Text,
    -- | Shows the current status of the bot. The bot is first in the @Creating@
    -- status. Once the bot is read for use, it changes to the @Available@
    -- status. After the bot is created, you can use the @DRAFT@ version of the
    -- bot.
    botStatus :: Prelude.Maybe BotStatus,
    -- | The type of the bot that was updated.
    botType :: Prelude.Maybe BotType,
    -- | A timestamp of the date and time that the bot was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The data privacy settings for the bot after the update.
    dataPrivacy :: Prelude.Maybe DataPrivacy,
    -- | The description of the bot after the update.
    description :: Prelude.Maybe Prelude.Text,
    -- | The session timeout, in seconds, for the bot after the update.
    idleSessionTTLInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A timestamp of the date and time that the bot was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the IAM role used by the bot after the
    -- update.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'updateBotResponse_botId' - The unique identifier of the bot that was updated.
--
-- 'botMembers', 'updateBotResponse_botMembers' - The list of bot members in the network that was updated.
--
-- 'botName', 'updateBotResponse_botName' - The name of the bot after the update.
--
-- 'botStatus', 'updateBotResponse_botStatus' - Shows the current status of the bot. The bot is first in the @Creating@
-- status. Once the bot is read for use, it changes to the @Available@
-- status. After the bot is created, you can use the @DRAFT@ version of the
-- bot.
--
-- 'botType', 'updateBotResponse_botType' - The type of the bot that was updated.
--
-- 'creationDateTime', 'updateBotResponse_creationDateTime' - A timestamp of the date and time that the bot was created.
--
-- 'dataPrivacy', 'updateBotResponse_dataPrivacy' - The data privacy settings for the bot after the update.
--
-- 'description', 'updateBotResponse_description' - The description of the bot after the update.
--
-- 'idleSessionTTLInSeconds', 'updateBotResponse_idleSessionTTLInSeconds' - The session timeout, in seconds, for the bot after the update.
--
-- 'lastUpdatedDateTime', 'updateBotResponse_lastUpdatedDateTime' - A timestamp of the date and time that the bot was last updated.
--
-- 'roleArn', 'updateBotResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role used by the bot after the
-- update.
--
-- 'httpStatus', 'updateBotResponse_httpStatus' - The response's http status code.
newUpdateBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBotResponse
newUpdateBotResponse pHttpStatus_ =
  UpdateBotResponse'
    { botId = Prelude.Nothing,
      botMembers = Prelude.Nothing,
      botName = Prelude.Nothing,
      botStatus = Prelude.Nothing,
      botType = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      dataPrivacy = Prelude.Nothing,
      description = Prelude.Nothing,
      idleSessionTTLInSeconds = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the bot that was updated.
updateBotResponse_botId :: Lens.Lens' UpdateBotResponse (Prelude.Maybe Prelude.Text)
updateBotResponse_botId = Lens.lens (\UpdateBotResponse' {botId} -> botId) (\s@UpdateBotResponse' {} a -> s {botId = a} :: UpdateBotResponse)

-- | The list of bot members in the network that was updated.
updateBotResponse_botMembers :: Lens.Lens' UpdateBotResponse (Prelude.Maybe [BotMember])
updateBotResponse_botMembers = Lens.lens (\UpdateBotResponse' {botMembers} -> botMembers) (\s@UpdateBotResponse' {} a -> s {botMembers = a} :: UpdateBotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the bot after the update.
updateBotResponse_botName :: Lens.Lens' UpdateBotResponse (Prelude.Maybe Prelude.Text)
updateBotResponse_botName = Lens.lens (\UpdateBotResponse' {botName} -> botName) (\s@UpdateBotResponse' {} a -> s {botName = a} :: UpdateBotResponse)

-- | Shows the current status of the bot. The bot is first in the @Creating@
-- status. Once the bot is read for use, it changes to the @Available@
-- status. After the bot is created, you can use the @DRAFT@ version of the
-- bot.
updateBotResponse_botStatus :: Lens.Lens' UpdateBotResponse (Prelude.Maybe BotStatus)
updateBotResponse_botStatus = Lens.lens (\UpdateBotResponse' {botStatus} -> botStatus) (\s@UpdateBotResponse' {} a -> s {botStatus = a} :: UpdateBotResponse)

-- | The type of the bot that was updated.
updateBotResponse_botType :: Lens.Lens' UpdateBotResponse (Prelude.Maybe BotType)
updateBotResponse_botType = Lens.lens (\UpdateBotResponse' {botType} -> botType) (\s@UpdateBotResponse' {} a -> s {botType = a} :: UpdateBotResponse)

-- | A timestamp of the date and time that the bot was created.
updateBotResponse_creationDateTime :: Lens.Lens' UpdateBotResponse (Prelude.Maybe Prelude.UTCTime)
updateBotResponse_creationDateTime = Lens.lens (\UpdateBotResponse' {creationDateTime} -> creationDateTime) (\s@UpdateBotResponse' {} a -> s {creationDateTime = a} :: UpdateBotResponse) Prelude.. Lens.mapping Data._Time

-- | The data privacy settings for the bot after the update.
updateBotResponse_dataPrivacy :: Lens.Lens' UpdateBotResponse (Prelude.Maybe DataPrivacy)
updateBotResponse_dataPrivacy = Lens.lens (\UpdateBotResponse' {dataPrivacy} -> dataPrivacy) (\s@UpdateBotResponse' {} a -> s {dataPrivacy = a} :: UpdateBotResponse)

-- | The description of the bot after the update.
updateBotResponse_description :: Lens.Lens' UpdateBotResponse (Prelude.Maybe Prelude.Text)
updateBotResponse_description = Lens.lens (\UpdateBotResponse' {description} -> description) (\s@UpdateBotResponse' {} a -> s {description = a} :: UpdateBotResponse)

-- | The session timeout, in seconds, for the bot after the update.
updateBotResponse_idleSessionTTLInSeconds :: Lens.Lens' UpdateBotResponse (Prelude.Maybe Prelude.Natural)
updateBotResponse_idleSessionTTLInSeconds = Lens.lens (\UpdateBotResponse' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@UpdateBotResponse' {} a -> s {idleSessionTTLInSeconds = a} :: UpdateBotResponse)

-- | A timestamp of the date and time that the bot was last updated.
updateBotResponse_lastUpdatedDateTime :: Lens.Lens' UpdateBotResponse (Prelude.Maybe Prelude.UTCTime)
updateBotResponse_lastUpdatedDateTime = Lens.lens (\UpdateBotResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateBotResponse' {} a -> s {lastUpdatedDateTime = a} :: UpdateBotResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the IAM role used by the bot after the
-- update.
updateBotResponse_roleArn :: Lens.Lens' UpdateBotResponse (Prelude.Maybe Prelude.Text)
updateBotResponse_roleArn = Lens.lens (\UpdateBotResponse' {roleArn} -> roleArn) (\s@UpdateBotResponse' {} a -> s {roleArn = a} :: UpdateBotResponse)

-- | The response's http status code.
updateBotResponse_httpStatus :: Lens.Lens' UpdateBotResponse Prelude.Int
updateBotResponse_httpStatus = Lens.lens (\UpdateBotResponse' {httpStatus} -> httpStatus) (\s@UpdateBotResponse' {} a -> s {httpStatus = a} :: UpdateBotResponse)

instance Prelude.NFData UpdateBotResponse where
  rnf UpdateBotResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botMembers
      `Prelude.seq` Prelude.rnf botName
      `Prelude.seq` Prelude.rnf botStatus
      `Prelude.seq` Prelude.rnf botType
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf dataPrivacy
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf idleSessionTTLInSeconds
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
