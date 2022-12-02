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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | A message in Speech Synthesis Markup Language (SSML).
    ssmlMessage :: Prelude.Maybe SSMLMessage,
    -- | A message that defines a response card that the client application can
    -- show to the user.
    imageResponseCard :: Prelude.Maybe ImageResponseCard,
    -- | A message in a custom format defined by the client application.
    customPayload :: Prelude.Maybe CustomPayload,
    -- | A message in plain text format.
    plainTextMessage :: Prelude.Maybe PlainTextMessage
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
-- 'ssmlMessage', 'message_ssmlMessage' - A message in Speech Synthesis Markup Language (SSML).
--
-- 'imageResponseCard', 'message_imageResponseCard' - A message that defines a response card that the client application can
-- show to the user.
--
-- 'customPayload', 'message_customPayload' - A message in a custom format defined by the client application.
--
-- 'plainTextMessage', 'message_plainTextMessage' - A message in plain text format.
newMessage ::
  Message
newMessage =
  Message'
    { ssmlMessage = Prelude.Nothing,
      imageResponseCard = Prelude.Nothing,
      customPayload = Prelude.Nothing,
      plainTextMessage = Prelude.Nothing
    }

-- | A message in Speech Synthesis Markup Language (SSML).
message_ssmlMessage :: Lens.Lens' Message (Prelude.Maybe SSMLMessage)
message_ssmlMessage = Lens.lens (\Message' {ssmlMessage} -> ssmlMessage) (\s@Message' {} a -> s {ssmlMessage = a} :: Message)

-- | A message that defines a response card that the client application can
-- show to the user.
message_imageResponseCard :: Lens.Lens' Message (Prelude.Maybe ImageResponseCard)
message_imageResponseCard = Lens.lens (\Message' {imageResponseCard} -> imageResponseCard) (\s@Message' {} a -> s {imageResponseCard = a} :: Message)

-- | A message in a custom format defined by the client application.
message_customPayload :: Lens.Lens' Message (Prelude.Maybe CustomPayload)
message_customPayload = Lens.lens (\Message' {customPayload} -> customPayload) (\s@Message' {} a -> s {customPayload = a} :: Message)

-- | A message in plain text format.
message_plainTextMessage :: Lens.Lens' Message (Prelude.Maybe PlainTextMessage)
message_plainTextMessage = Lens.lens (\Message' {plainTextMessage} -> plainTextMessage) (\s@Message' {} a -> s {plainTextMessage = a} :: Message)

instance Data.FromJSON Message where
  parseJSON =
    Data.withObject
      "Message"
      ( \x ->
          Message'
            Prelude.<$> (x Data..:? "ssmlMessage")
            Prelude.<*> (x Data..:? "imageResponseCard")
            Prelude.<*> (x Data..:? "customPayload")
            Prelude.<*> (x Data..:? "plainTextMessage")
      )

instance Prelude.Hashable Message where
  hashWithSalt _salt Message' {..} =
    _salt `Prelude.hashWithSalt` ssmlMessage
      `Prelude.hashWithSalt` imageResponseCard
      `Prelude.hashWithSalt` customPayload
      `Prelude.hashWithSalt` plainTextMessage

instance Prelude.NFData Message where
  rnf Message' {..} =
    Prelude.rnf ssmlMessage
      `Prelude.seq` Prelude.rnf imageResponseCard
      `Prelude.seq` Prelude.rnf customPayload
      `Prelude.seq` Prelude.rnf plainTextMessage

instance Data.ToJSON Message where
  toJSON Message' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ssmlMessage" Data..=) Prelude.<$> ssmlMessage,
            ("imageResponseCard" Data..=)
              Prelude.<$> imageResponseCard,
            ("customPayload" Data..=) Prelude.<$> customPayload,
            ("plainTextMessage" Data..=)
              Prelude.<$> plainTextMessage
          ]
      )
