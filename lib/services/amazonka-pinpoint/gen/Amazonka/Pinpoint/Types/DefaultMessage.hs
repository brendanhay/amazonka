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
-- Module      : Amazonka.Pinpoint.Types.DefaultMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.DefaultMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the default message for all channels.
--
-- /See:/ 'newDefaultMessage' smart constructor.
data DefaultMessage = DefaultMessage'
  { -- | The default body of the message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The default message variables to use in the message. You can override
    -- these default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text])
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'defaultMessage_body' - The default body of the message.
--
-- 'substitutions', 'defaultMessage_substitutions' - The default message variables to use in the message. You can override
-- these default variables with individual address variables.
newDefaultMessage ::
  DefaultMessage
newDefaultMessage =
  DefaultMessage'
    { body = Prelude.Nothing,
      substitutions = Prelude.Nothing
    }

-- | The default body of the message.
defaultMessage_body :: Lens.Lens' DefaultMessage (Prelude.Maybe Prelude.Text)
defaultMessage_body = Lens.lens (\DefaultMessage' {body} -> body) (\s@DefaultMessage' {} a -> s {body = a} :: DefaultMessage)

-- | The default message variables to use in the message. You can override
-- these default variables with individual address variables.
defaultMessage_substitutions :: Lens.Lens' DefaultMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
defaultMessage_substitutions = Lens.lens (\DefaultMessage' {substitutions} -> substitutions) (\s@DefaultMessage' {} a -> s {substitutions = a} :: DefaultMessage) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable DefaultMessage where
  hashWithSalt _salt DefaultMessage' {..} =
    _salt `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` substitutions

instance Prelude.NFData DefaultMessage where
  rnf DefaultMessage' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf substitutions

instance Core.ToJSON DefaultMessage where
  toJSON DefaultMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Body" Core..=) Prelude.<$> body,
            ("Substitutions" Core..=) Prelude.<$> substitutions
          ]
      )
