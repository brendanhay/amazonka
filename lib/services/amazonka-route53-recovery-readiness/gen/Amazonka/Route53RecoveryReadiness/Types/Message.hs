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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.Message
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.Message where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information relating to readiness check status.
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { -- | The text of a readiness check message.
    messageText :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Message' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageText', 'message_messageText' - The text of a readiness check message.
newMessage ::
  Message
newMessage = Message' {messageText = Prelude.Nothing}

-- | The text of a readiness check message.
message_messageText :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_messageText = Lens.lens (\Message' {messageText} -> messageText) (\s@Message' {} a -> s {messageText = a} :: Message)

instance Data.FromJSON Message where
  parseJSON =
    Data.withObject
      "Message"
      ( \x ->
          Message' Prelude.<$> (x Data..:? "messageText")
      )

instance Prelude.Hashable Message where
  hashWithSalt _salt Message' {..} =
    _salt `Prelude.hashWithSalt` messageText

instance Prelude.NFData Message where
  rnf Message' {..} = Prelude.rnf messageText
