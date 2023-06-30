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
-- Module      : Amazonka.Chime.Types.Bot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Bot where

import Amazonka.Chime.Types.BotType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource that allows Enterprise account administrators to configure an
-- interface to receive events from Amazon Chime.
--
-- /See:/ 'newBot' smart constructor.
data Bot = Bot'
  { -- | The bot email address.
    botEmail :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The bot ID.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The bot type.
    botType :: Prelude.Maybe BotType,
    -- | The bot creation timestamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | When true, the bot is stopped from running in your account.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | The bot display name.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The security token used to authenticate Amazon Chime with the outgoing
    -- event endpoint.
    securityToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The updated bot timestamp, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The unique ID for the bot user.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Bot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botEmail', 'bot_botEmail' - The bot email address.
--
-- 'botId', 'bot_botId' - The bot ID.
--
-- 'botType', 'bot_botType' - The bot type.
--
-- 'createdTimestamp', 'bot_createdTimestamp' - The bot creation timestamp, in ISO 8601 format.
--
-- 'disabled', 'bot_disabled' - When true, the bot is stopped from running in your account.
--
-- 'displayName', 'bot_displayName' - The bot display name.
--
-- 'securityToken', 'bot_securityToken' - The security token used to authenticate Amazon Chime with the outgoing
-- event endpoint.
--
-- 'updatedTimestamp', 'bot_updatedTimestamp' - The updated bot timestamp, in ISO 8601 format.
--
-- 'userId', 'bot_userId' - The unique ID for the bot user.
newBot ::
  Bot
newBot =
  Bot'
    { botEmail = Prelude.Nothing,
      botId = Prelude.Nothing,
      botType = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      disabled = Prelude.Nothing,
      displayName = Prelude.Nothing,
      securityToken = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | The bot email address.
bot_botEmail :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_botEmail = Lens.lens (\Bot' {botEmail} -> botEmail) (\s@Bot' {} a -> s {botEmail = a} :: Bot) Prelude.. Lens.mapping Data._Sensitive

-- | The bot ID.
bot_botId :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_botId = Lens.lens (\Bot' {botId} -> botId) (\s@Bot' {} a -> s {botId = a} :: Bot)

-- | The bot type.
bot_botType :: Lens.Lens' Bot (Prelude.Maybe BotType)
bot_botType = Lens.lens (\Bot' {botType} -> botType) (\s@Bot' {} a -> s {botType = a} :: Bot)

-- | The bot creation timestamp, in ISO 8601 format.
bot_createdTimestamp :: Lens.Lens' Bot (Prelude.Maybe Prelude.UTCTime)
bot_createdTimestamp = Lens.lens (\Bot' {createdTimestamp} -> createdTimestamp) (\s@Bot' {} a -> s {createdTimestamp = a} :: Bot) Prelude.. Lens.mapping Data._Time

-- | When true, the bot is stopped from running in your account.
bot_disabled :: Lens.Lens' Bot (Prelude.Maybe Prelude.Bool)
bot_disabled = Lens.lens (\Bot' {disabled} -> disabled) (\s@Bot' {} a -> s {disabled = a} :: Bot)

-- | The bot display name.
bot_displayName :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_displayName = Lens.lens (\Bot' {displayName} -> displayName) (\s@Bot' {} a -> s {displayName = a} :: Bot) Prelude.. Lens.mapping Data._Sensitive

-- | The security token used to authenticate Amazon Chime with the outgoing
-- event endpoint.
bot_securityToken :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_securityToken = Lens.lens (\Bot' {securityToken} -> securityToken) (\s@Bot' {} a -> s {securityToken = a} :: Bot) Prelude.. Lens.mapping Data._Sensitive

-- | The updated bot timestamp, in ISO 8601 format.
bot_updatedTimestamp :: Lens.Lens' Bot (Prelude.Maybe Prelude.UTCTime)
bot_updatedTimestamp = Lens.lens (\Bot' {updatedTimestamp} -> updatedTimestamp) (\s@Bot' {} a -> s {updatedTimestamp = a} :: Bot) Prelude.. Lens.mapping Data._Time

-- | The unique ID for the bot user.
bot_userId :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_userId = Lens.lens (\Bot' {userId} -> userId) (\s@Bot' {} a -> s {userId = a} :: Bot)

instance Data.FromJSON Bot where
  parseJSON =
    Data.withObject
      "Bot"
      ( \x ->
          Bot'
            Prelude.<$> (x Data..:? "BotEmail")
            Prelude.<*> (x Data..:? "BotId")
            Prelude.<*> (x Data..:? "BotType")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Disabled")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "SecurityToken")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "UserId")
      )

instance Prelude.Hashable Bot where
  hashWithSalt _salt Bot' {..} =
    _salt
      `Prelude.hashWithSalt` botEmail
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botType
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` securityToken
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` userId

instance Prelude.NFData Bot where
  rnf Bot' {..} =
    Prelude.rnf botEmail
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botType
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf securityToken
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf userId
