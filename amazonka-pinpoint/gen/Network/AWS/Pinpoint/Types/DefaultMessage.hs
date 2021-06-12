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
-- Module      : Network.AWS.Pinpoint.Types.DefaultMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DefaultMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the default message for all channels.
--
-- /See:/ 'newDefaultMessage' smart constructor.
data DefaultMessage = DefaultMessage'
  { -- | The default body of the message.
    body :: Core.Maybe Core.Text,
    -- | The default message variables to use in the message. You can override
    -- these default variables with individual address variables.
    substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { body = Core.Nothing,
      substitutions = Core.Nothing
    }

-- | The default body of the message.
defaultMessage_body :: Lens.Lens' DefaultMessage (Core.Maybe Core.Text)
defaultMessage_body = Lens.lens (\DefaultMessage' {body} -> body) (\s@DefaultMessage' {} a -> s {body = a} :: DefaultMessage)

-- | The default message variables to use in the message. You can override
-- these default variables with individual address variables.
defaultMessage_substitutions :: Lens.Lens' DefaultMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
defaultMessage_substitutions = Lens.lens (\DefaultMessage' {substitutions} -> substitutions) (\s@DefaultMessage' {} a -> s {substitutions = a} :: DefaultMessage) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable DefaultMessage

instance Core.NFData DefaultMessage

instance Core.ToJSON DefaultMessage where
  toJSON DefaultMessage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Body" Core..=) Core.<$> body,
            ("Substitutions" Core..=) Core.<$> substitutions
          ]
      )
