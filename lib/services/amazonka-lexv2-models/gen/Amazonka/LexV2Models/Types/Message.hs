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
-- Module      : Amazonka.LexV2Models.Types.Message
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.Message where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.CustomPayload
import Amazonka.LexV2Models.Types.ImageResponseCard
import Amazonka.LexV2Models.Types.PlainTextMessage
import Amazonka.LexV2Models.Types.SSMLMessage
import qualified Amazonka.Prelude as Prelude

-- | The object that provides message text and it\'s type.
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { -- | A message in a custom format defined by the client application.
    customPayload :: Prelude.Maybe CustomPayload,
    -- | A message that defines a response card that the client application can
    -- show to the user.
    imageResponseCard :: Prelude.Maybe ImageResponseCard,
    -- | A message in plain text format.
    plainTextMessage :: Prelude.Maybe PlainTextMessage,
    -- | A message in Speech Synthesis Markup Language (SSML).
    ssmlMessage :: Prelude.Maybe SSMLMessage
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
-- 'customPayload', 'message_customPayload' - A message in a custom format defined by the client application.
--
-- 'imageResponseCard', 'message_imageResponseCard' - A message that defines a response card that the client application can
-- show to the user.
--
-- 'plainTextMessage', 'message_plainTextMessage' - A message in plain text format.
--
-- 'ssmlMessage', 'message_ssmlMessage' - A message in Speech Synthesis Markup Language (SSML).
newMessage ::
  Message
newMessage =
  Message'
    { customPayload = Prelude.Nothing,
      imageResponseCard = Prelude.Nothing,
      plainTextMessage = Prelude.Nothing,
      ssmlMessage = Prelude.Nothing
    }

-- | A message in a custom format defined by the client application.
message_customPayload :: Lens.Lens' Message (Prelude.Maybe CustomPayload)
message_customPayload = Lens.lens (\Message' {customPayload} -> customPayload) (\s@Message' {} a -> s {customPayload = a} :: Message)

-- | A message that defines a response card that the client application can
-- show to the user.
message_imageResponseCard :: Lens.Lens' Message (Prelude.Maybe ImageResponseCard)
message_imageResponseCard = Lens.lens (\Message' {imageResponseCard} -> imageResponseCard) (\s@Message' {} a -> s {imageResponseCard = a} :: Message)

-- | A message in plain text format.
message_plainTextMessage :: Lens.Lens' Message (Prelude.Maybe PlainTextMessage)
message_plainTextMessage = Lens.lens (\Message' {plainTextMessage} -> plainTextMessage) (\s@Message' {} a -> s {plainTextMessage = a} :: Message)

-- | A message in Speech Synthesis Markup Language (SSML).
message_ssmlMessage :: Lens.Lens' Message (Prelude.Maybe SSMLMessage)
message_ssmlMessage = Lens.lens (\Message' {ssmlMessage} -> ssmlMessage) (\s@Message' {} a -> s {ssmlMessage = a} :: Message)

instance Data.FromJSON Message where
  parseJSON =
    Data.withObject
      "Message"
      ( \x ->
          Message'
            Prelude.<$> (x Data..:? "customPayload")
            Prelude.<*> (x Data..:? "imageResponseCard")
            Prelude.<*> (x Data..:? "plainTextMessage")
            Prelude.<*> (x Data..:? "ssmlMessage")
      )

instance Prelude.Hashable Message where
  hashWithSalt _salt Message' {..} =
    _salt
      `Prelude.hashWithSalt` customPayload
      `Prelude.hashWithSalt` imageResponseCard
      `Prelude.hashWithSalt` plainTextMessage
      `Prelude.hashWithSalt` ssmlMessage

instance Prelude.NFData Message where
  rnf Message' {..} =
    Prelude.rnf customPayload
      `Prelude.seq` Prelude.rnf imageResponseCard
      `Prelude.seq` Prelude.rnf plainTextMessage
      `Prelude.seq` Prelude.rnf ssmlMessage

instance Data.ToJSON Message where
  toJSON Message' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("customPayload" Data..=) Prelude.<$> customPayload,
            ("imageResponseCard" Data..=)
              Prelude.<$> imageResponseCard,
            ("plainTextMessage" Data..=)
              Prelude.<$> plainTextMessage,
            ("ssmlMessage" Data..=) Prelude.<$> ssmlMessage
          ]
      )
