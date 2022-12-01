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
-- Module      : Amazonka.LexV2Models.Types.MessageGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.MessageGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.Message
import qualified Amazonka.Prelude as Prelude

-- | Provides one or more messages that Amazon Lex should send to the user.
--
-- /See:/ 'newMessageGroup' smart constructor.
data MessageGroup = MessageGroup'
  { -- | Message variations to send to the user. When variations are defined,
    -- Amazon Lex chooses the primary message or one of the variations to send
    -- to the user.
    variations :: Prelude.Maybe [Message],
    -- | The primary message that Amazon Lex should send to the user.
    message :: Message
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variations', 'messageGroup_variations' - Message variations to send to the user. When variations are defined,
-- Amazon Lex chooses the primary message or one of the variations to send
-- to the user.
--
-- 'message', 'messageGroup_message' - The primary message that Amazon Lex should send to the user.
newMessageGroup ::
  -- | 'message'
  Message ->
  MessageGroup
newMessageGroup pMessage_ =
  MessageGroup'
    { variations = Prelude.Nothing,
      message = pMessage_
    }

-- | Message variations to send to the user. When variations are defined,
-- Amazon Lex chooses the primary message or one of the variations to send
-- to the user.
messageGroup_variations :: Lens.Lens' MessageGroup (Prelude.Maybe [Message])
messageGroup_variations = Lens.lens (\MessageGroup' {variations} -> variations) (\s@MessageGroup' {} a -> s {variations = a} :: MessageGroup) Prelude.. Lens.mapping Lens.coerced

-- | The primary message that Amazon Lex should send to the user.
messageGroup_message :: Lens.Lens' MessageGroup Message
messageGroup_message = Lens.lens (\MessageGroup' {message} -> message) (\s@MessageGroup' {} a -> s {message = a} :: MessageGroup)

instance Core.FromJSON MessageGroup where
  parseJSON =
    Core.withObject
      "MessageGroup"
      ( \x ->
          MessageGroup'
            Prelude.<$> (x Core..:? "variations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "message")
      )

instance Prelude.Hashable MessageGroup where
  hashWithSalt _salt MessageGroup' {..} =
    _salt `Prelude.hashWithSalt` variations
      `Prelude.hashWithSalt` message

instance Prelude.NFData MessageGroup where
  rnf MessageGroup' {..} =
    Prelude.rnf variations
      `Prelude.seq` Prelude.rnf message

instance Core.ToJSON MessageGroup where
  toJSON MessageGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("variations" Core..=) Prelude.<$> variations,
            Prelude.Just ("message" Core..= message)
          ]
      )
