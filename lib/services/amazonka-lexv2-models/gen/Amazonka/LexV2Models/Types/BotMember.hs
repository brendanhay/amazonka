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
-- Module      : Amazonka.LexV2Models.Types.BotMember
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotMember where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A bot that is a member of a network of bots.
--
-- /See:/ 'newBotMember' smart constructor.
data BotMember = BotMember'
  { -- | The unique ID of a bot that is a member of this network of bots.
    botMemberId :: Prelude.Text,
    -- | The unique name of a bot that is a member of this network of bots.
    botMemberName :: Prelude.Text,
    -- | The alias ID of a bot that is a member of this network of bots.
    botMemberAliasId :: Prelude.Text,
    -- | The alias name of a bot that is a member of this network of bots.
    botMemberAliasName :: Prelude.Text,
    -- | The version of a bot that is a member of this network of bots.
    botMemberVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botMemberId', 'botMember_botMemberId' - The unique ID of a bot that is a member of this network of bots.
--
-- 'botMemberName', 'botMember_botMemberName' - The unique name of a bot that is a member of this network of bots.
--
-- 'botMemberAliasId', 'botMember_botMemberAliasId' - The alias ID of a bot that is a member of this network of bots.
--
-- 'botMemberAliasName', 'botMember_botMemberAliasName' - The alias name of a bot that is a member of this network of bots.
--
-- 'botMemberVersion', 'botMember_botMemberVersion' - The version of a bot that is a member of this network of bots.
newBotMember ::
  -- | 'botMemberId'
  Prelude.Text ->
  -- | 'botMemberName'
  Prelude.Text ->
  -- | 'botMemberAliasId'
  Prelude.Text ->
  -- | 'botMemberAliasName'
  Prelude.Text ->
  -- | 'botMemberVersion'
  Prelude.Text ->
  BotMember
newBotMember
  pBotMemberId_
  pBotMemberName_
  pBotMemberAliasId_
  pBotMemberAliasName_
  pBotMemberVersion_ =
    BotMember'
      { botMemberId = pBotMemberId_,
        botMemberName = pBotMemberName_,
        botMemberAliasId = pBotMemberAliasId_,
        botMemberAliasName = pBotMemberAliasName_,
        botMemberVersion = pBotMemberVersion_
      }

-- | The unique ID of a bot that is a member of this network of bots.
botMember_botMemberId :: Lens.Lens' BotMember Prelude.Text
botMember_botMemberId = Lens.lens (\BotMember' {botMemberId} -> botMemberId) (\s@BotMember' {} a -> s {botMemberId = a} :: BotMember)

-- | The unique name of a bot that is a member of this network of bots.
botMember_botMemberName :: Lens.Lens' BotMember Prelude.Text
botMember_botMemberName = Lens.lens (\BotMember' {botMemberName} -> botMemberName) (\s@BotMember' {} a -> s {botMemberName = a} :: BotMember)

-- | The alias ID of a bot that is a member of this network of bots.
botMember_botMemberAliasId :: Lens.Lens' BotMember Prelude.Text
botMember_botMemberAliasId = Lens.lens (\BotMember' {botMemberAliasId} -> botMemberAliasId) (\s@BotMember' {} a -> s {botMemberAliasId = a} :: BotMember)

-- | The alias name of a bot that is a member of this network of bots.
botMember_botMemberAliasName :: Lens.Lens' BotMember Prelude.Text
botMember_botMemberAliasName = Lens.lens (\BotMember' {botMemberAliasName} -> botMemberAliasName) (\s@BotMember' {} a -> s {botMemberAliasName = a} :: BotMember)

-- | The version of a bot that is a member of this network of bots.
botMember_botMemberVersion :: Lens.Lens' BotMember Prelude.Text
botMember_botMemberVersion = Lens.lens (\BotMember' {botMemberVersion} -> botMemberVersion) (\s@BotMember' {} a -> s {botMemberVersion = a} :: BotMember)

instance Data.FromJSON BotMember where
  parseJSON =
    Data.withObject
      "BotMember"
      ( \x ->
          BotMember'
            Prelude.<$> (x Data..: "botMemberId")
            Prelude.<*> (x Data..: "botMemberName")
            Prelude.<*> (x Data..: "botMemberAliasId")
            Prelude.<*> (x Data..: "botMemberAliasName")
            Prelude.<*> (x Data..: "botMemberVersion")
      )

instance Prelude.Hashable BotMember where
  hashWithSalt _salt BotMember' {..} =
    _salt
      `Prelude.hashWithSalt` botMemberId
      `Prelude.hashWithSalt` botMemberName
      `Prelude.hashWithSalt` botMemberAliasId
      `Prelude.hashWithSalt` botMemberAliasName
      `Prelude.hashWithSalt` botMemberVersion

instance Prelude.NFData BotMember where
  rnf BotMember' {..} =
    Prelude.rnf botMemberId
      `Prelude.seq` Prelude.rnf botMemberName
      `Prelude.seq` Prelude.rnf botMemberAliasId
      `Prelude.seq` Prelude.rnf botMemberAliasName
      `Prelude.seq` Prelude.rnf botMemberVersion

instance Data.ToJSON BotMember where
  toJSON BotMember' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("botMemberId" Data..= botMemberId),
            Prelude.Just ("botMemberName" Data..= botMemberName),
            Prelude.Just
              ("botMemberAliasId" Data..= botMemberAliasId),
            Prelude.Just
              ("botMemberAliasName" Data..= botMemberAliasName),
            Prelude.Just
              ("botMemberVersion" Data..= botMemberVersion)
          ]
      )
