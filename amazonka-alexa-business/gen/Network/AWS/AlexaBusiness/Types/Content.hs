{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.Types.Content
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Content where

import Network.AWS.AlexaBusiness.Types.Audio
import Network.AWS.AlexaBusiness.Types.Ssml
import Network.AWS.AlexaBusiness.Types.TextMessage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The content definition. This can contain only one text, SSML, or audio
-- list object.
--
-- /See:/ 'newContent' smart constructor.
data Content = Content'
  { -- | The list of text messages.
    textList :: Prelude.Maybe [TextMessage],
    -- | The list of SSML messages.
    ssmlList :: Prelude.Maybe [Ssml],
    -- | The list of audio messages.
    audioList :: Prelude.Maybe [Audio]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Content' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textList', 'content_textList' - The list of text messages.
--
-- 'ssmlList', 'content_ssmlList' - The list of SSML messages.
--
-- 'audioList', 'content_audioList' - The list of audio messages.
newContent ::
  Content
newContent =
  Content'
    { textList = Prelude.Nothing,
      ssmlList = Prelude.Nothing,
      audioList = Prelude.Nothing
    }

-- | The list of text messages.
content_textList :: Lens.Lens' Content (Prelude.Maybe [TextMessage])
content_textList = Lens.lens (\Content' {textList} -> textList) (\s@Content' {} a -> s {textList = a} :: Content) Prelude.. Lens.mapping Prelude._Coerce

-- | The list of SSML messages.
content_ssmlList :: Lens.Lens' Content (Prelude.Maybe [Ssml])
content_ssmlList = Lens.lens (\Content' {ssmlList} -> ssmlList) (\s@Content' {} a -> s {ssmlList = a} :: Content) Prelude.. Lens.mapping Prelude._Coerce

-- | The list of audio messages.
content_audioList :: Lens.Lens' Content (Prelude.Maybe [Audio])
content_audioList = Lens.lens (\Content' {audioList} -> audioList) (\s@Content' {} a -> s {audioList = a} :: Content) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable Content

instance Prelude.NFData Content

instance Prelude.ToJSON Content where
  toJSON Content' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TextList" Prelude..=) Prelude.<$> textList,
            ("SsmlList" Prelude..=) Prelude.<$> ssmlList,
            ("AudioList" Prelude..=) Prelude.<$> audioList
          ]
      )
