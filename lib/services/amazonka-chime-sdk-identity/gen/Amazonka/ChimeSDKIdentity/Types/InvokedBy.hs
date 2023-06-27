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
-- Module      : Amazonka.ChimeSDKIdentity.Types.InvokedBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.InvokedBy where

import Amazonka.ChimeSDKIdentity.Types.StandardMessages
import Amazonka.ChimeSDKIdentity.Types.TargetedMessages
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the type of message that triggers a bot.
--
-- /See:/ 'newInvokedBy' smart constructor.
data InvokedBy = InvokedBy'
  { -- | Sets standard messages as the bot trigger. For standard messages:
    --
    -- -   @ALL@: The bot processes all standard messages.
    --
    -- -   @AUTO@: The bot responds to ALL messages when the channel has one
    --     other non-hidden member, and responds to MENTIONS when the channel
    --     has more than one other non-hidden member.
    --
    -- -   @MENTIONS@: The bot processes all standard messages that have a
    --     message attribute with @CHIME.mentions@ and a value of the bot ARN.
    --
    -- -   @NONE@: The bot processes no standard messages.
    standardMessages :: StandardMessages,
    -- | Sets targeted messages as the bot trigger. For targeted messages:
    --
    -- -   @ALL@: The bot processes all @TargetedMessages@ sent to it. The bot
    --     then responds with a targeted message back to the sender.
    --
    -- -   @NONE@: The bot processes no targeted messages.
    targetedMessages :: TargetedMessages
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokedBy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardMessages', 'invokedBy_standardMessages' - Sets standard messages as the bot trigger. For standard messages:
--
-- -   @ALL@: The bot processes all standard messages.
--
-- -   @AUTO@: The bot responds to ALL messages when the channel has one
--     other non-hidden member, and responds to MENTIONS when the channel
--     has more than one other non-hidden member.
--
-- -   @MENTIONS@: The bot processes all standard messages that have a
--     message attribute with @CHIME.mentions@ and a value of the bot ARN.
--
-- -   @NONE@: The bot processes no standard messages.
--
-- 'targetedMessages', 'invokedBy_targetedMessages' - Sets targeted messages as the bot trigger. For targeted messages:
--
-- -   @ALL@: The bot processes all @TargetedMessages@ sent to it. The bot
--     then responds with a targeted message back to the sender.
--
-- -   @NONE@: The bot processes no targeted messages.
newInvokedBy ::
  -- | 'standardMessages'
  StandardMessages ->
  -- | 'targetedMessages'
  TargetedMessages ->
  InvokedBy
newInvokedBy pStandardMessages_ pTargetedMessages_ =
  InvokedBy'
    { standardMessages = pStandardMessages_,
      targetedMessages = pTargetedMessages_
    }

-- | Sets standard messages as the bot trigger. For standard messages:
--
-- -   @ALL@: The bot processes all standard messages.
--
-- -   @AUTO@: The bot responds to ALL messages when the channel has one
--     other non-hidden member, and responds to MENTIONS when the channel
--     has more than one other non-hidden member.
--
-- -   @MENTIONS@: The bot processes all standard messages that have a
--     message attribute with @CHIME.mentions@ and a value of the bot ARN.
--
-- -   @NONE@: The bot processes no standard messages.
invokedBy_standardMessages :: Lens.Lens' InvokedBy StandardMessages
invokedBy_standardMessages = Lens.lens (\InvokedBy' {standardMessages} -> standardMessages) (\s@InvokedBy' {} a -> s {standardMessages = a} :: InvokedBy)

-- | Sets targeted messages as the bot trigger. For targeted messages:
--
-- -   @ALL@: The bot processes all @TargetedMessages@ sent to it. The bot
--     then responds with a targeted message back to the sender.
--
-- -   @NONE@: The bot processes no targeted messages.
invokedBy_targetedMessages :: Lens.Lens' InvokedBy TargetedMessages
invokedBy_targetedMessages = Lens.lens (\InvokedBy' {targetedMessages} -> targetedMessages) (\s@InvokedBy' {} a -> s {targetedMessages = a} :: InvokedBy)

instance Data.FromJSON InvokedBy where
  parseJSON =
    Data.withObject
      "InvokedBy"
      ( \x ->
          InvokedBy'
            Prelude.<$> (x Data..: "StandardMessages")
            Prelude.<*> (x Data..: "TargetedMessages")
      )

instance Prelude.Hashable InvokedBy where
  hashWithSalt _salt InvokedBy' {..} =
    _salt
      `Prelude.hashWithSalt` standardMessages
      `Prelude.hashWithSalt` targetedMessages

instance Prelude.NFData InvokedBy where
  rnf InvokedBy' {..} =
    Prelude.rnf standardMessages
      `Prelude.seq` Prelude.rnf targetedMessages

instance Data.ToJSON InvokedBy where
  toJSON InvokedBy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StandardMessages" Data..= standardMessages),
            Prelude.Just
              ("TargetedMessages" Data..= targetedMessages)
          ]
      )
