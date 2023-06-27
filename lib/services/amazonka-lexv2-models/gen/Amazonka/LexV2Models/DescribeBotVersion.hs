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
-- Module      : Amazonka.LexV2Models.DescribeBotVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides metadata about a version of a bot.
module Amazonka.LexV2Models.DescribeBotVersion
  ( -- * Creating a Request
    DescribeBotVersion (..),
    newDescribeBotVersion,

    -- * Request Lenses
    describeBotVersion_botId,
    describeBotVersion_botVersion,

    -- * Destructuring the Response
    DescribeBotVersionResponse (..),
    newDescribeBotVersionResponse,

    -- * Response Lenses
    describeBotVersionResponse_botId,
    describeBotVersionResponse_botMembers,
    describeBotVersionResponse_botName,
    describeBotVersionResponse_botStatus,
    describeBotVersionResponse_botType,
    describeBotVersionResponse_botVersion,
    describeBotVersionResponse_creationDateTime,
    describeBotVersionResponse_dataPrivacy,
    describeBotVersionResponse_description,
    describeBotVersionResponse_failureReasons,
    describeBotVersionResponse_idleSessionTTLInSeconds,
    describeBotVersionResponse_parentBotNetworks,
    describeBotVersionResponse_roleArn,
    describeBotVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBotVersion' smart constructor.
data DescribeBotVersion = DescribeBotVersion'
  { -- | The identifier of the bot containing the version to return metadata for.
    botId :: Prelude.Text,
    -- | The version of the bot to return metadata for.
    botVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBotVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'describeBotVersion_botId' - The identifier of the bot containing the version to return metadata for.
--
-- 'botVersion', 'describeBotVersion_botVersion' - The version of the bot to return metadata for.
newDescribeBotVersion ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  DescribeBotVersion
newDescribeBotVersion pBotId_ pBotVersion_ =
  DescribeBotVersion'
    { botId = pBotId_,
      botVersion = pBotVersion_
    }

-- | The identifier of the bot containing the version to return metadata for.
describeBotVersion_botId :: Lens.Lens' DescribeBotVersion Prelude.Text
describeBotVersion_botId = Lens.lens (\DescribeBotVersion' {botId} -> botId) (\s@DescribeBotVersion' {} a -> s {botId = a} :: DescribeBotVersion)

-- | The version of the bot to return metadata for.
describeBotVersion_botVersion :: Lens.Lens' DescribeBotVersion Prelude.Text
describeBotVersion_botVersion = Lens.lens (\DescribeBotVersion' {botVersion} -> botVersion) (\s@DescribeBotVersion' {} a -> s {botVersion = a} :: DescribeBotVersion)

instance Core.AWSRequest DescribeBotVersion where
  type
    AWSResponse DescribeBotVersion =
      DescribeBotVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBotVersionResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botMembers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "botName")
            Prelude.<*> (x Data..?> "botStatus")
            Prelude.<*> (x Data..?> "botType")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "dataPrivacy")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "failureReasons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "idleSessionTTLInSeconds")
            Prelude.<*> ( x
                            Data..?> "parentBotNetworks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBotVersion where
  hashWithSalt _salt DescribeBotVersion' {..} =
    _salt
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion

instance Prelude.NFData DescribeBotVersion where
  rnf DescribeBotVersion' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion

instance Data.ToHeaders DescribeBotVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeBotVersion where
  toPath DescribeBotVersion' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/"
      ]

instance Data.ToQuery DescribeBotVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBotVersionResponse' smart constructor.
data DescribeBotVersionResponse = DescribeBotVersionResponse'
  { -- | The identifier of the bot that contains the version.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The members of bot network in the version that was described.
    botMembers :: Prelude.Maybe [BotMember],
    -- | The name of the bot that contains the version.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the bot. When the status is @Available@, the bot
    -- version is ready for use.
    botStatus :: Prelude.Maybe BotStatus,
    -- | The type of the bot in the version that was described.
    botType :: Prelude.Maybe BotType,
    -- | The version of the bot that was described.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot version was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | Data privacy settings for the bot version.
    dataPrivacy :: Prelude.Maybe DataPrivacy,
    -- | The description specified for the bot.
    description :: Prelude.Maybe Prelude.Text,
    -- | If the @botStatus@ is @Failed@, this contains a list of reasons that the
    -- version couldn\'t be built.
    failureReasons :: Prelude.Maybe [Prelude.Text],
    -- | The number of seconds that a session with the bot remains active before
    -- it is discarded by Amazon Lex.
    idleSessionTTLInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A list of the networks to which the bot version you described belongs.
    parentBotNetworks :: Prelude.Maybe [ParentBotNetwork],
    -- | The Amazon Resource Name (ARN) of an IAM role that has permission to
    -- access the bot version.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBotVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'describeBotVersionResponse_botId' - The identifier of the bot that contains the version.
--
-- 'botMembers', 'describeBotVersionResponse_botMembers' - The members of bot network in the version that was described.
--
-- 'botName', 'describeBotVersionResponse_botName' - The name of the bot that contains the version.
--
-- 'botStatus', 'describeBotVersionResponse_botStatus' - The current status of the bot. When the status is @Available@, the bot
-- version is ready for use.
--
-- 'botType', 'describeBotVersionResponse_botType' - The type of the bot in the version that was described.
--
-- 'botVersion', 'describeBotVersionResponse_botVersion' - The version of the bot that was described.
--
-- 'creationDateTime', 'describeBotVersionResponse_creationDateTime' - A timestamp of the date and time that the bot version was created.
--
-- 'dataPrivacy', 'describeBotVersionResponse_dataPrivacy' - Data privacy settings for the bot version.
--
-- 'description', 'describeBotVersionResponse_description' - The description specified for the bot.
--
-- 'failureReasons', 'describeBotVersionResponse_failureReasons' - If the @botStatus@ is @Failed@, this contains a list of reasons that the
-- version couldn\'t be built.
--
-- 'idleSessionTTLInSeconds', 'describeBotVersionResponse_idleSessionTTLInSeconds' - The number of seconds that a session with the bot remains active before
-- it is discarded by Amazon Lex.
--
-- 'parentBotNetworks', 'describeBotVersionResponse_parentBotNetworks' - A list of the networks to which the bot version you described belongs.
--
-- 'roleArn', 'describeBotVersionResponse_roleArn' - The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the bot version.
--
-- 'httpStatus', 'describeBotVersionResponse_httpStatus' - The response's http status code.
newDescribeBotVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBotVersionResponse
newDescribeBotVersionResponse pHttpStatus_ =
  DescribeBotVersionResponse'
    { botId =
        Prelude.Nothing,
      botMembers = Prelude.Nothing,
      botName = Prelude.Nothing,
      botStatus = Prelude.Nothing,
      botType = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      dataPrivacy = Prelude.Nothing,
      description = Prelude.Nothing,
      failureReasons = Prelude.Nothing,
      idleSessionTTLInSeconds = Prelude.Nothing,
      parentBotNetworks = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the bot that contains the version.
describeBotVersionResponse_botId :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe Prelude.Text)
describeBotVersionResponse_botId = Lens.lens (\DescribeBotVersionResponse' {botId} -> botId) (\s@DescribeBotVersionResponse' {} a -> s {botId = a} :: DescribeBotVersionResponse)

-- | The members of bot network in the version that was described.
describeBotVersionResponse_botMembers :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe [BotMember])
describeBotVersionResponse_botMembers = Lens.lens (\DescribeBotVersionResponse' {botMembers} -> botMembers) (\s@DescribeBotVersionResponse' {} a -> s {botMembers = a} :: DescribeBotVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the bot that contains the version.
describeBotVersionResponse_botName :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe Prelude.Text)
describeBotVersionResponse_botName = Lens.lens (\DescribeBotVersionResponse' {botName} -> botName) (\s@DescribeBotVersionResponse' {} a -> s {botName = a} :: DescribeBotVersionResponse)

-- | The current status of the bot. When the status is @Available@, the bot
-- version is ready for use.
describeBotVersionResponse_botStatus :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe BotStatus)
describeBotVersionResponse_botStatus = Lens.lens (\DescribeBotVersionResponse' {botStatus} -> botStatus) (\s@DescribeBotVersionResponse' {} a -> s {botStatus = a} :: DescribeBotVersionResponse)

-- | The type of the bot in the version that was described.
describeBotVersionResponse_botType :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe BotType)
describeBotVersionResponse_botType = Lens.lens (\DescribeBotVersionResponse' {botType} -> botType) (\s@DescribeBotVersionResponse' {} a -> s {botType = a} :: DescribeBotVersionResponse)

-- | The version of the bot that was described.
describeBotVersionResponse_botVersion :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe Prelude.Text)
describeBotVersionResponse_botVersion = Lens.lens (\DescribeBotVersionResponse' {botVersion} -> botVersion) (\s@DescribeBotVersionResponse' {} a -> s {botVersion = a} :: DescribeBotVersionResponse)

-- | A timestamp of the date and time that the bot version was created.
describeBotVersionResponse_creationDateTime :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe Prelude.UTCTime)
describeBotVersionResponse_creationDateTime = Lens.lens (\DescribeBotVersionResponse' {creationDateTime} -> creationDateTime) (\s@DescribeBotVersionResponse' {} a -> s {creationDateTime = a} :: DescribeBotVersionResponse) Prelude.. Lens.mapping Data._Time

-- | Data privacy settings for the bot version.
describeBotVersionResponse_dataPrivacy :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe DataPrivacy)
describeBotVersionResponse_dataPrivacy = Lens.lens (\DescribeBotVersionResponse' {dataPrivacy} -> dataPrivacy) (\s@DescribeBotVersionResponse' {} a -> s {dataPrivacy = a} :: DescribeBotVersionResponse)

-- | The description specified for the bot.
describeBotVersionResponse_description :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe Prelude.Text)
describeBotVersionResponse_description = Lens.lens (\DescribeBotVersionResponse' {description} -> description) (\s@DescribeBotVersionResponse' {} a -> s {description = a} :: DescribeBotVersionResponse)

-- | If the @botStatus@ is @Failed@, this contains a list of reasons that the
-- version couldn\'t be built.
describeBotVersionResponse_failureReasons :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe [Prelude.Text])
describeBotVersionResponse_failureReasons = Lens.lens (\DescribeBotVersionResponse' {failureReasons} -> failureReasons) (\s@DescribeBotVersionResponse' {} a -> s {failureReasons = a} :: DescribeBotVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds that a session with the bot remains active before
-- it is discarded by Amazon Lex.
describeBotVersionResponse_idleSessionTTLInSeconds :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe Prelude.Natural)
describeBotVersionResponse_idleSessionTTLInSeconds = Lens.lens (\DescribeBotVersionResponse' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@DescribeBotVersionResponse' {} a -> s {idleSessionTTLInSeconds = a} :: DescribeBotVersionResponse)

-- | A list of the networks to which the bot version you described belongs.
describeBotVersionResponse_parentBotNetworks :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe [ParentBotNetwork])
describeBotVersionResponse_parentBotNetworks = Lens.lens (\DescribeBotVersionResponse' {parentBotNetworks} -> parentBotNetworks) (\s@DescribeBotVersionResponse' {} a -> s {parentBotNetworks = a} :: DescribeBotVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of an IAM role that has permission to
-- access the bot version.
describeBotVersionResponse_roleArn :: Lens.Lens' DescribeBotVersionResponse (Prelude.Maybe Prelude.Text)
describeBotVersionResponse_roleArn = Lens.lens (\DescribeBotVersionResponse' {roleArn} -> roleArn) (\s@DescribeBotVersionResponse' {} a -> s {roleArn = a} :: DescribeBotVersionResponse)

-- | The response's http status code.
describeBotVersionResponse_httpStatus :: Lens.Lens' DescribeBotVersionResponse Prelude.Int
describeBotVersionResponse_httpStatus = Lens.lens (\DescribeBotVersionResponse' {httpStatus} -> httpStatus) (\s@DescribeBotVersionResponse' {} a -> s {httpStatus = a} :: DescribeBotVersionResponse)

instance Prelude.NFData DescribeBotVersionResponse where
  rnf DescribeBotVersionResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botMembers
      `Prelude.seq` Prelude.rnf botName
      `Prelude.seq` Prelude.rnf botStatus
      `Prelude.seq` Prelude.rnf botType
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf dataPrivacy
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf failureReasons
      `Prelude.seq` Prelude.rnf idleSessionTTLInSeconds
      `Prelude.seq` Prelude.rnf parentBotNetworks
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf httpStatus
