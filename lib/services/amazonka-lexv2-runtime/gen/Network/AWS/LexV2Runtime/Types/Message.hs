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
-- Module      : Network.AWS.LexV2Runtime.Types.Message
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.Message where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.ImageResponseCard
import Network.AWS.LexV2Runtime.Types.MessageContentType
import qualified Network.AWS.Prelude as Prelude

-- | Container for text that is returned to the customer..
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { imageResponseCard :: Prelude.Maybe ImageResponseCard,
    -- | The text of the message.
    content :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Indicates the type of response.
    contentType :: MessageContentType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Message' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageResponseCard', 'message_imageResponseCard' - Undocumented member.
--
-- 'content', 'message_content' - The text of the message.
--
-- 'contentType', 'message_contentType' - Indicates the type of response.
newMessage ::
  -- | 'contentType'
  MessageContentType ->
  Message
newMessage pContentType_ =
  Message'
    { imageResponseCard = Prelude.Nothing,
      content = Prelude.Nothing,
      contentType = pContentType_
    }

-- | Undocumented member.
message_imageResponseCard :: Lens.Lens' Message (Prelude.Maybe ImageResponseCard)
message_imageResponseCard = Lens.lens (\Message' {imageResponseCard} -> imageResponseCard) (\s@Message' {} a -> s {imageResponseCard = a} :: Message)

-- | The text of the message.
message_content :: Lens.Lens' Message (Prelude.Maybe Prelude.Text)
message_content = Lens.lens (\Message' {content} -> content) (\s@Message' {} a -> s {content = a} :: Message) Prelude.. Lens.mapping Core._Sensitive

-- | Indicates the type of response.
message_contentType :: Lens.Lens' Message MessageContentType
message_contentType = Lens.lens (\Message' {contentType} -> contentType) (\s@Message' {} a -> s {contentType = a} :: Message)

instance Core.FromJSON Message where
  parseJSON =
    Core.withObject
      "Message"
      ( \x ->
          Message'
            Prelude.<$> (x Core..:? "imageResponseCard")
            Prelude.<*> (x Core..:? "content")
            Prelude.<*> (x Core..: "contentType")
      )

instance Prelude.Hashable Message

instance Prelude.NFData Message

instance Core.ToJSON Message where
  toJSON Message' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("imageResponseCard" Core..=)
              Prelude.<$> imageResponseCard,
            ("content" Core..=) Prelude.<$> content,
            Prelude.Just ("contentType" Core..= contentType)
          ]
      )
