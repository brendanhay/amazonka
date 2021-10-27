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
-- Module      : Network.AWS.LexV2Models.DescribeBot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides metadata information about a bot.
module Network.AWS.LexV2Models.DescribeBot
  ( -- * Creating a Request
    DescribeBot (..),
    newDescribeBot,

    -- * Request Lenses
    describeBot_botId,

    -- * Destructuring the Response
    DescribeBotResponse (..),
    newDescribeBotResponse,

    -- * Response Lenses
    describeBotResponse_botStatus,
    describeBotResponse_botName,
    describeBotResponse_lastUpdatedDateTime,
    describeBotResponse_dataPrivacy,
    describeBotResponse_botId,
    describeBotResponse_idleSessionTTLInSeconds,
    describeBotResponse_creationDateTime,
    describeBotResponse_description,
    describeBotResponse_roleArn,
    describeBotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBot' smart constructor.
data DescribeBot = DescribeBot'
  { -- | The unique identifier of the bot to describe.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'describeBot_botId' - The unique identifier of the bot to describe.
newDescribeBot ::
  -- | 'botId'
  Prelude.Text ->
  DescribeBot
newDescribeBot pBotId_ =
  DescribeBot' {botId = pBotId_}

-- | The unique identifier of the bot to describe.
describeBot_botId :: Lens.Lens' DescribeBot Prelude.Text
describeBot_botId = Lens.lens (\DescribeBot' {botId} -> botId) (\s@DescribeBot' {} a -> s {botId = a} :: DescribeBot)

instance Core.AWSRequest DescribeBot where
  type AWSResponse DescribeBot = DescribeBotResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBotResponse'
            Prelude.<$> (x Core..?> "botStatus")
            Prelude.<*> (x Core..?> "botName")
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (x Core..?> "dataPrivacy")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "idleSessionTTLInSeconds")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "roleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBot

instance Prelude.NFData DescribeBot

instance Core.ToHeaders DescribeBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeBot where
  toPath DescribeBot' {..} =
    Prelude.mconcat ["/bots/", Core.toBS botId, "/"]

instance Core.ToQuery DescribeBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBotResponse' smart constructor.
data DescribeBotResponse = DescribeBotResponse'
  { -- | The current status of the bot. When the status is @Available@ the bot is
    -- ready to be used in conversations with users.
    botStatus :: Prelude.Maybe BotStatus,
    -- | The name of the bot.
    botName :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | Settings for managing data privacy of the bot and its conversations with
    -- users.
    dataPrivacy :: Prelude.Maybe DataPrivacy,
    -- | The unique identifier of the bot.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The maximum time in seconds that Amazon Lex retains the data gathered in
    -- a conversation.
    idleSessionTTLInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A timestamp of the date and time that the bot was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The description of the bot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permission to
    -- access the bot.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botStatus', 'describeBotResponse_botStatus' - The current status of the bot. When the status is @Available@ the bot is
-- ready to be used in conversations with users.
--
-- 'botName', 'describeBotResponse_botName' - The name of the bot.
--
-- 'lastUpdatedDateTime', 'describeBotResponse_lastUpdatedDateTime' - A timestamp of the date and time that the bot was last updated.
--
-- 'dataPrivacy', 'describeBotResponse_dataPrivacy' - Settings for managing data privacy of the bot and its conversations with
-- users.
--
-- 'botId', 'describeBotResponse_botId' - The unique identifier of the bot.
--
-- 'idleSessionTTLInSeconds', 'describeBotResponse_idleSessionTTLInSeconds' - The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation.
--
-- 'creationDateTime', 'describeBotResponse_creationDateTime' - A timestamp of the date and time that the bot was created.
--
-- 'description', 'describeBotResponse_description' - The description of the bot.
--
-- 'roleArn', 'describeBotResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the bot.
--
-- 'httpStatus', 'describeBotResponse_httpStatus' - The response's http status code.
newDescribeBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBotResponse
newDescribeBotResponse pHttpStatus_ =
  DescribeBotResponse'
    { botStatus = Prelude.Nothing,
      botName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      dataPrivacy = Prelude.Nothing,
      botId = Prelude.Nothing,
      idleSessionTTLInSeconds = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the bot. When the status is @Available@ the bot is
-- ready to be used in conversations with users.
describeBotResponse_botStatus :: Lens.Lens' DescribeBotResponse (Prelude.Maybe BotStatus)
describeBotResponse_botStatus = Lens.lens (\DescribeBotResponse' {botStatus} -> botStatus) (\s@DescribeBotResponse' {} a -> s {botStatus = a} :: DescribeBotResponse)

-- | The name of the bot.
describeBotResponse_botName :: Lens.Lens' DescribeBotResponse (Prelude.Maybe Prelude.Text)
describeBotResponse_botName = Lens.lens (\DescribeBotResponse' {botName} -> botName) (\s@DescribeBotResponse' {} a -> s {botName = a} :: DescribeBotResponse)

-- | A timestamp of the date and time that the bot was last updated.
describeBotResponse_lastUpdatedDateTime :: Lens.Lens' DescribeBotResponse (Prelude.Maybe Prelude.UTCTime)
describeBotResponse_lastUpdatedDateTime = Lens.lens (\DescribeBotResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeBotResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeBotResponse) Prelude.. Lens.mapping Core._Time

-- | Settings for managing data privacy of the bot and its conversations with
-- users.
describeBotResponse_dataPrivacy :: Lens.Lens' DescribeBotResponse (Prelude.Maybe DataPrivacy)
describeBotResponse_dataPrivacy = Lens.lens (\DescribeBotResponse' {dataPrivacy} -> dataPrivacy) (\s@DescribeBotResponse' {} a -> s {dataPrivacy = a} :: DescribeBotResponse)

-- | The unique identifier of the bot.
describeBotResponse_botId :: Lens.Lens' DescribeBotResponse (Prelude.Maybe Prelude.Text)
describeBotResponse_botId = Lens.lens (\DescribeBotResponse' {botId} -> botId) (\s@DescribeBotResponse' {} a -> s {botId = a} :: DescribeBotResponse)

-- | The maximum time in seconds that Amazon Lex retains the data gathered in
-- a conversation.
describeBotResponse_idleSessionTTLInSeconds :: Lens.Lens' DescribeBotResponse (Prelude.Maybe Prelude.Natural)
describeBotResponse_idleSessionTTLInSeconds = Lens.lens (\DescribeBotResponse' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@DescribeBotResponse' {} a -> s {idleSessionTTLInSeconds = a} :: DescribeBotResponse)

-- | A timestamp of the date and time that the bot was created.
describeBotResponse_creationDateTime :: Lens.Lens' DescribeBotResponse (Prelude.Maybe Prelude.UTCTime)
describeBotResponse_creationDateTime = Lens.lens (\DescribeBotResponse' {creationDateTime} -> creationDateTime) (\s@DescribeBotResponse' {} a -> s {creationDateTime = a} :: DescribeBotResponse) Prelude.. Lens.mapping Core._Time

-- | The description of the bot.
describeBotResponse_description :: Lens.Lens' DescribeBotResponse (Prelude.Maybe Prelude.Text)
describeBotResponse_description = Lens.lens (\DescribeBotResponse' {description} -> description) (\s@DescribeBotResponse' {} a -> s {description = a} :: DescribeBotResponse)

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the bot.
describeBotResponse_roleArn :: Lens.Lens' DescribeBotResponse (Prelude.Maybe Prelude.Text)
describeBotResponse_roleArn = Lens.lens (\DescribeBotResponse' {roleArn} -> roleArn) (\s@DescribeBotResponse' {} a -> s {roleArn = a} :: DescribeBotResponse)

-- | The response's http status code.
describeBotResponse_httpStatus :: Lens.Lens' DescribeBotResponse Prelude.Int
describeBotResponse_httpStatus = Lens.lens (\DescribeBotResponse' {httpStatus} -> httpStatus) (\s@DescribeBotResponse' {} a -> s {httpStatus = a} :: DescribeBotResponse)

instance Prelude.NFData DescribeBotResponse
