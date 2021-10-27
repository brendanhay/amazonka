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
-- Module      : Network.AWS.LexV2Models.CreateBot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lex conversational bot.
module Network.AWS.LexV2Models.CreateBot
  ( -- * Creating a Request
    CreateBot (..),
    newCreateBot,

    -- * Request Lenses
    createBot_testBotAliasTags,
    createBot_botTags,
    createBot_description,
    createBot_botName,
    createBot_roleArn,
    createBot_dataPrivacy,
    createBot_idleSessionTTLInSeconds,

    -- * Destructuring the Response
    CreateBotResponse (..),
    newCreateBotResponse,

    -- * Response Lenses
    createBotResponse_botStatus,
    createBotResponse_testBotAliasTags,
    createBotResponse_botName,
    createBotResponse_dataPrivacy,
    createBotResponse_botId,
    createBotResponse_idleSessionTTLInSeconds,
    createBotResponse_creationDateTime,
    createBotResponse_botTags,
    createBotResponse_description,
    createBotResponse_roleArn,
    createBotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBot' smart constructor.
data CreateBot = CreateBot'
  { -- | A list of tags to add to the test alias for a bot. You can only add tags
    -- when you create a bot. You can\'t use the @UpdateAlias@ operation to
    -- update tags. To update tags on the test alias, use the @TagResource@
    -- operation.
    testBotAliasTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of tags to add to the bot. You can only add tags when you create
    -- a bot. You can\'t use the @UpdateBot@ operation to update tags. To
    -- update tags, use the @TagResource@ operation.
    botTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description of the bot. It appears in lists to help you identify a
    -- particular bot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot. The bot name must be unique in the account that
    -- creates the bot.
    botName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that has permission to
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
-- Create a value of 'CreateBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testBotAliasTags', 'createBot_testBotAliasTags' - A list of tags to add to the test alias for a bot. You can only add tags
-- when you create a bot. You can\'t use the @UpdateAlias@ operation to
-- update tags. To update tags on the test alias, use the @TagResource@
-- operation.
--
-- 'botTags', 'createBot_botTags' - A list of tags to add to the bot. You can only add tags when you create
-- a bot. You can\'t use the @UpdateBot@ operation to update tags. To
-- update tags, use the @TagResource@ operation.
--
-- 'description', 'createBot_description' - A description of the bot. It appears in lists to help you identify a
-- particular bot.
--
-- 'botName', 'createBot_botName' - The name of the bot. The bot name must be unique in the account that
-- creates the bot.
--
-- 'roleArn', 'createBot_roleArn' - The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the bot.
--
-- 'dataPrivacy', 'createBot_dataPrivacy' - Provides information on additional privacy protections Amazon Lex should
-- use with the bot\'s data.
--
-- 'idleSessionTTLInSeconds', 'createBot_idleSessionTTLInSeconds' - The time, in seconds, that Amazon Lex should keep information about a
-- user\'s conversation with the bot.
--
-- A user interaction remains active for the amount of time specified. If
-- no conversation occurs during this time, the session expires and Amazon
-- Lex deletes any data provided before the timeout.
--
-- You can specify between 60 (1 minute) and 86,400 (24 hours) seconds.
newCreateBot ::
  -- | 'botName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'dataPrivacy'
  DataPrivacy ->
  -- | 'idleSessionTTLInSeconds'
  Prelude.Natural ->
  CreateBot
newCreateBot
  pBotName_
  pRoleArn_
  pDataPrivacy_
  pIdleSessionTTLInSeconds_ =
    CreateBot'
      { testBotAliasTags = Prelude.Nothing,
        botTags = Prelude.Nothing,
        description = Prelude.Nothing,
        botName = pBotName_,
        roleArn = pRoleArn_,
        dataPrivacy = pDataPrivacy_,
        idleSessionTTLInSeconds = pIdleSessionTTLInSeconds_
      }

-- | A list of tags to add to the test alias for a bot. You can only add tags
-- when you create a bot. You can\'t use the @UpdateAlias@ operation to
-- update tags. To update tags on the test alias, use the @TagResource@
-- operation.
createBot_testBotAliasTags :: Lens.Lens' CreateBot (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBot_testBotAliasTags = Lens.lens (\CreateBot' {testBotAliasTags} -> testBotAliasTags) (\s@CreateBot' {} a -> s {testBotAliasTags = a} :: CreateBot) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags to add to the bot. You can only add tags when you create
-- a bot. You can\'t use the @UpdateBot@ operation to update tags. To
-- update tags, use the @TagResource@ operation.
createBot_botTags :: Lens.Lens' CreateBot (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBot_botTags = Lens.lens (\CreateBot' {botTags} -> botTags) (\s@CreateBot' {} a -> s {botTags = a} :: CreateBot) Prelude.. Lens.mapping Lens.coerced

-- | A description of the bot. It appears in lists to help you identify a
-- particular bot.
createBot_description :: Lens.Lens' CreateBot (Prelude.Maybe Prelude.Text)
createBot_description = Lens.lens (\CreateBot' {description} -> description) (\s@CreateBot' {} a -> s {description = a} :: CreateBot)

-- | The name of the bot. The bot name must be unique in the account that
-- creates the bot.
createBot_botName :: Lens.Lens' CreateBot Prelude.Text
createBot_botName = Lens.lens (\CreateBot' {botName} -> botName) (\s@CreateBot' {} a -> s {botName = a} :: CreateBot)

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the bot.
createBot_roleArn :: Lens.Lens' CreateBot Prelude.Text
createBot_roleArn = Lens.lens (\CreateBot' {roleArn} -> roleArn) (\s@CreateBot' {} a -> s {roleArn = a} :: CreateBot)

-- | Provides information on additional privacy protections Amazon Lex should
-- use with the bot\'s data.
createBot_dataPrivacy :: Lens.Lens' CreateBot DataPrivacy
createBot_dataPrivacy = Lens.lens (\CreateBot' {dataPrivacy} -> dataPrivacy) (\s@CreateBot' {} a -> s {dataPrivacy = a} :: CreateBot)

-- | The time, in seconds, that Amazon Lex should keep information about a
-- user\'s conversation with the bot.
--
-- A user interaction remains active for the amount of time specified. If
-- no conversation occurs during this time, the session expires and Amazon
-- Lex deletes any data provided before the timeout.
--
-- You can specify between 60 (1 minute) and 86,400 (24 hours) seconds.
createBot_idleSessionTTLInSeconds :: Lens.Lens' CreateBot Prelude.Natural
createBot_idleSessionTTLInSeconds = Lens.lens (\CreateBot' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@CreateBot' {} a -> s {idleSessionTTLInSeconds = a} :: CreateBot)

instance Core.AWSRequest CreateBot where
  type AWSResponse CreateBot = CreateBotResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBotResponse'
            Prelude.<$> (x Core..?> "botStatus")
            Prelude.<*> ( x Core..?> "testBotAliasTags"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "botName")
            Prelude.<*> (x Core..?> "dataPrivacy")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "idleSessionTTLInSeconds")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "botTags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "roleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBot

instance Prelude.NFData CreateBot

instance Core.ToHeaders CreateBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBot where
  toJSON CreateBot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("testBotAliasTags" Core..=)
              Prelude.<$> testBotAliasTags,
            ("botTags" Core..=) Prelude.<$> botTags,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("botName" Core..= botName),
            Prelude.Just ("roleArn" Core..= roleArn),
            Prelude.Just ("dataPrivacy" Core..= dataPrivacy),
            Prelude.Just
              ( "idleSessionTTLInSeconds"
                  Core..= idleSessionTTLInSeconds
              )
          ]
      )

instance Core.ToPath CreateBot where
  toPath = Prelude.const "/bots/"

instance Core.ToQuery CreateBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBotResponse' smart constructor.
data CreateBotResponse = CreateBotResponse'
  { -- | Shows the current status of the bot. The bot is first in the @Creating@
    -- status. Once the bot is read for use, it changes to the @Available@
    -- status. After the bot is created, you can use the @Draft@ version of the
    -- bot.
    botStatus :: Prelude.Maybe BotStatus,
    -- | A list of tags associated with the test alias for the bot.
    testBotAliasTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name specified for the bot.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The data privacy settings specified for the bot.
    dataPrivacy :: Prelude.Maybe DataPrivacy,
    -- | A unique identifier for a particular bot. You use this to identify the
    -- bot when you call other Amazon Lex API operations.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The session idle time specified for the bot.
    idleSessionTTLInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A timestamp indicating the date and time that the bot was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | A list of tags associated with the bot.
    botTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description specified for the bot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The IAM role specified for the bot.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botStatus', 'createBotResponse_botStatus' - Shows the current status of the bot. The bot is first in the @Creating@
-- status. Once the bot is read for use, it changes to the @Available@
-- status. After the bot is created, you can use the @Draft@ version of the
-- bot.
--
-- 'testBotAliasTags', 'createBotResponse_testBotAliasTags' - A list of tags associated with the test alias for the bot.
--
-- 'botName', 'createBotResponse_botName' - The name specified for the bot.
--
-- 'dataPrivacy', 'createBotResponse_dataPrivacy' - The data privacy settings specified for the bot.
--
-- 'botId', 'createBotResponse_botId' - A unique identifier for a particular bot. You use this to identify the
-- bot when you call other Amazon Lex API operations.
--
-- 'idleSessionTTLInSeconds', 'createBotResponse_idleSessionTTLInSeconds' - The session idle time specified for the bot.
--
-- 'creationDateTime', 'createBotResponse_creationDateTime' - A timestamp indicating the date and time that the bot was created.
--
-- 'botTags', 'createBotResponse_botTags' - A list of tags associated with the bot.
--
-- 'description', 'createBotResponse_description' - The description specified for the bot.
--
-- 'roleArn', 'createBotResponse_roleArn' - The IAM role specified for the bot.
--
-- 'httpStatus', 'createBotResponse_httpStatus' - The response's http status code.
newCreateBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBotResponse
newCreateBotResponse pHttpStatus_ =
  CreateBotResponse'
    { botStatus = Prelude.Nothing,
      testBotAliasTags = Prelude.Nothing,
      botName = Prelude.Nothing,
      dataPrivacy = Prelude.Nothing,
      botId = Prelude.Nothing,
      idleSessionTTLInSeconds = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      botTags = Prelude.Nothing,
      description = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Shows the current status of the bot. The bot is first in the @Creating@
-- status. Once the bot is read for use, it changes to the @Available@
-- status. After the bot is created, you can use the @Draft@ version of the
-- bot.
createBotResponse_botStatus :: Lens.Lens' CreateBotResponse (Prelude.Maybe BotStatus)
createBotResponse_botStatus = Lens.lens (\CreateBotResponse' {botStatus} -> botStatus) (\s@CreateBotResponse' {} a -> s {botStatus = a} :: CreateBotResponse)

-- | A list of tags associated with the test alias for the bot.
createBotResponse_testBotAliasTags :: Lens.Lens' CreateBotResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBotResponse_testBotAliasTags = Lens.lens (\CreateBotResponse' {testBotAliasTags} -> testBotAliasTags) (\s@CreateBotResponse' {} a -> s {testBotAliasTags = a} :: CreateBotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name specified for the bot.
createBotResponse_botName :: Lens.Lens' CreateBotResponse (Prelude.Maybe Prelude.Text)
createBotResponse_botName = Lens.lens (\CreateBotResponse' {botName} -> botName) (\s@CreateBotResponse' {} a -> s {botName = a} :: CreateBotResponse)

-- | The data privacy settings specified for the bot.
createBotResponse_dataPrivacy :: Lens.Lens' CreateBotResponse (Prelude.Maybe DataPrivacy)
createBotResponse_dataPrivacy = Lens.lens (\CreateBotResponse' {dataPrivacy} -> dataPrivacy) (\s@CreateBotResponse' {} a -> s {dataPrivacy = a} :: CreateBotResponse)

-- | A unique identifier for a particular bot. You use this to identify the
-- bot when you call other Amazon Lex API operations.
createBotResponse_botId :: Lens.Lens' CreateBotResponse (Prelude.Maybe Prelude.Text)
createBotResponse_botId = Lens.lens (\CreateBotResponse' {botId} -> botId) (\s@CreateBotResponse' {} a -> s {botId = a} :: CreateBotResponse)

-- | The session idle time specified for the bot.
createBotResponse_idleSessionTTLInSeconds :: Lens.Lens' CreateBotResponse (Prelude.Maybe Prelude.Natural)
createBotResponse_idleSessionTTLInSeconds = Lens.lens (\CreateBotResponse' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@CreateBotResponse' {} a -> s {idleSessionTTLInSeconds = a} :: CreateBotResponse)

-- | A timestamp indicating the date and time that the bot was created.
createBotResponse_creationDateTime :: Lens.Lens' CreateBotResponse (Prelude.Maybe Prelude.UTCTime)
createBotResponse_creationDateTime = Lens.lens (\CreateBotResponse' {creationDateTime} -> creationDateTime) (\s@CreateBotResponse' {} a -> s {creationDateTime = a} :: CreateBotResponse) Prelude.. Lens.mapping Core._Time

-- | A list of tags associated with the bot.
createBotResponse_botTags :: Lens.Lens' CreateBotResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBotResponse_botTags = Lens.lens (\CreateBotResponse' {botTags} -> botTags) (\s@CreateBotResponse' {} a -> s {botTags = a} :: CreateBotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The description specified for the bot.
createBotResponse_description :: Lens.Lens' CreateBotResponse (Prelude.Maybe Prelude.Text)
createBotResponse_description = Lens.lens (\CreateBotResponse' {description} -> description) (\s@CreateBotResponse' {} a -> s {description = a} :: CreateBotResponse)

-- | The IAM role specified for the bot.
createBotResponse_roleArn :: Lens.Lens' CreateBotResponse (Prelude.Maybe Prelude.Text)
createBotResponse_roleArn = Lens.lens (\CreateBotResponse' {roleArn} -> roleArn) (\s@CreateBotResponse' {} a -> s {roleArn = a} :: CreateBotResponse)

-- | The response's http status code.
createBotResponse_httpStatus :: Lens.Lens' CreateBotResponse Prelude.Int
createBotResponse_httpStatus = Lens.lens (\CreateBotResponse' {httpStatus} -> httpStatus) (\s@CreateBotResponse' {} a -> s {httpStatus = a} :: CreateBotResponse)

instance Prelude.NFData CreateBotResponse
