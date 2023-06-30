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
-- Module      : Amazonka.LexModels.Types.Statement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.Statement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.Message
import qualified Amazonka.Prelude as Prelude

-- | A collection of messages that convey information to the user. At
-- runtime, Amazon Lex selects the message to convey.
--
-- /See:/ 'newStatement' smart constructor.
data Statement = Statement'
  { -- | At runtime, if the client is using the
    -- <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
    -- API, Amazon Lex includes the response card in the response. It
    -- substitutes all of the session attributes and slot values for
    -- placeholders in the response card.
    responseCard :: Prelude.Maybe Prelude.Text,
    -- | A collection of message objects.
    messages :: Prelude.NonEmpty Message
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Statement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseCard', 'statement_responseCard' - At runtime, if the client is using the
-- <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- API, Amazon Lex includes the response card in the response. It
-- substitutes all of the session attributes and slot values for
-- placeholders in the response card.
--
-- 'messages', 'statement_messages' - A collection of message objects.
newStatement ::
  -- | 'messages'
  Prelude.NonEmpty Message ->
  Statement
newStatement pMessages_ =
  Statement'
    { responseCard = Prelude.Nothing,
      messages = Lens.coerced Lens.# pMessages_
    }

-- | At runtime, if the client is using the
-- <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText>
-- API, Amazon Lex includes the response card in the response. It
-- substitutes all of the session attributes and slot values for
-- placeholders in the response card.
statement_responseCard :: Lens.Lens' Statement (Prelude.Maybe Prelude.Text)
statement_responseCard = Lens.lens (\Statement' {responseCard} -> responseCard) (\s@Statement' {} a -> s {responseCard = a} :: Statement)

-- | A collection of message objects.
statement_messages :: Lens.Lens' Statement (Prelude.NonEmpty Message)
statement_messages = Lens.lens (\Statement' {messages} -> messages) (\s@Statement' {} a -> s {messages = a} :: Statement) Prelude.. Lens.coerced

instance Data.FromJSON Statement where
  parseJSON =
    Data.withObject
      "Statement"
      ( \x ->
          Statement'
            Prelude.<$> (x Data..:? "responseCard")
            Prelude.<*> (x Data..: "messages")
      )

instance Prelude.Hashable Statement where
  hashWithSalt _salt Statement' {..} =
    _salt
      `Prelude.hashWithSalt` responseCard
      `Prelude.hashWithSalt` messages

instance Prelude.NFData Statement where
  rnf Statement' {..} =
    Prelude.rnf responseCard
      `Prelude.seq` Prelude.rnf messages

instance Data.ToJSON Statement where
  toJSON Statement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("responseCard" Data..=) Prelude.<$> responseCard,
            Prelude.Just ("messages" Data..= messages)
          ]
      )
