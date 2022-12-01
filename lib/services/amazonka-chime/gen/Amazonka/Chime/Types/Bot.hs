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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Bot where

import Amazonka.Chime.Types.BotType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A resource that allows Enterprise account administrators to configure an
-- interface to receive events from Amazon Chime.
--
-- /See:/ 'newBot' smart constructor.
data Bot = Bot'
  { -- | The bot email address.
    botEmail :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The bot creation timestamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The updated bot timestamp, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The bot display name.
    displayName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The bot ID.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The security token used to authenticate Amazon Chime with the outgoing
    -- event endpoint.
    securityToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The bot type.
    botType :: Prelude.Maybe BotType,
    -- | The unique ID for the bot user.
    userId :: Prelude.Maybe Prelude.Text,
    -- | When true, the bot is stopped from running in your account.
    disabled :: Prelude.Maybe Prelude.Bool
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
-- 'createdTimestamp', 'bot_createdTimestamp' - The bot creation timestamp, in ISO 8601 format.
--
-- 'updatedTimestamp', 'bot_updatedTimestamp' - The updated bot timestamp, in ISO 8601 format.
--
-- 'displayName', 'bot_displayName' - The bot display name.
--
-- 'botId', 'bot_botId' - The bot ID.
--
-- 'securityToken', 'bot_securityToken' - The security token used to authenticate Amazon Chime with the outgoing
-- event endpoint.
--
-- 'botType', 'bot_botType' - The bot type.
--
-- 'userId', 'bot_userId' - The unique ID for the bot user.
--
-- 'disabled', 'bot_disabled' - When true, the bot is stopped from running in your account.
newBot ::
  Bot
newBot =
  Bot'
    { botEmail = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      displayName = Prelude.Nothing,
      botId = Prelude.Nothing,
      securityToken = Prelude.Nothing,
      botType = Prelude.Nothing,
      userId = Prelude.Nothing,
      disabled = Prelude.Nothing
    }

-- | The bot email address.
bot_botEmail :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_botEmail = Lens.lens (\Bot' {botEmail} -> botEmail) (\s@Bot' {} a -> s {botEmail = a} :: Bot) Prelude.. Lens.mapping Core._Sensitive

-- | The bot creation timestamp, in ISO 8601 format.
bot_createdTimestamp :: Lens.Lens' Bot (Prelude.Maybe Prelude.UTCTime)
bot_createdTimestamp = Lens.lens (\Bot' {createdTimestamp} -> createdTimestamp) (\s@Bot' {} a -> s {createdTimestamp = a} :: Bot) Prelude.. Lens.mapping Core._Time

-- | The updated bot timestamp, in ISO 8601 format.
bot_updatedTimestamp :: Lens.Lens' Bot (Prelude.Maybe Prelude.UTCTime)
bot_updatedTimestamp = Lens.lens (\Bot' {updatedTimestamp} -> updatedTimestamp) (\s@Bot' {} a -> s {updatedTimestamp = a} :: Bot) Prelude.. Lens.mapping Core._Time

-- | The bot display name.
bot_displayName :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_displayName = Lens.lens (\Bot' {displayName} -> displayName) (\s@Bot' {} a -> s {displayName = a} :: Bot) Prelude.. Lens.mapping Core._Sensitive

-- | The bot ID.
bot_botId :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_botId = Lens.lens (\Bot' {botId} -> botId) (\s@Bot' {} a -> s {botId = a} :: Bot)

-- | The security token used to authenticate Amazon Chime with the outgoing
-- event endpoint.
bot_securityToken :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_securityToken = Lens.lens (\Bot' {securityToken} -> securityToken) (\s@Bot' {} a -> s {securityToken = a} :: Bot) Prelude.. Lens.mapping Core._Sensitive

-- | The bot type.
bot_botType :: Lens.Lens' Bot (Prelude.Maybe BotType)
bot_botType = Lens.lens (\Bot' {botType} -> botType) (\s@Bot' {} a -> s {botType = a} :: Bot)

-- | The unique ID for the bot user.
bot_userId :: Lens.Lens' Bot (Prelude.Maybe Prelude.Text)
bot_userId = Lens.lens (\Bot' {userId} -> userId) (\s@Bot' {} a -> s {userId = a} :: Bot)

-- | When true, the bot is stopped from running in your account.
bot_disabled :: Lens.Lens' Bot (Prelude.Maybe Prelude.Bool)
bot_disabled = Lens.lens (\Bot' {disabled} -> disabled) (\s@Bot' {} a -> s {disabled = a} :: Bot)

instance Core.FromJSON Bot where
  parseJSON =
    Core.withObject
      "Bot"
      ( \x ->
          Bot'
            Prelude.<$> (x Core..:? "BotEmail")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "UpdatedTimestamp")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "BotId")
            Prelude.<*> (x Core..:? "SecurityToken")
            Prelude.<*> (x Core..:? "BotType")
            Prelude.<*> (x Core..:? "UserId")
            Prelude.<*> (x Core..:? "Disabled")
      )

instance Prelude.Hashable Bot where
  hashWithSalt _salt Bot' {..} =
    _salt `Prelude.hashWithSalt` botEmail
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` securityToken
      `Prelude.hashWithSalt` botType
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` disabled

instance Prelude.NFData Bot where
  rnf Bot' {..} =
    Prelude.rnf botEmail
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf securityToken
      `Prelude.seq` Prelude.rnf botType
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf disabled
