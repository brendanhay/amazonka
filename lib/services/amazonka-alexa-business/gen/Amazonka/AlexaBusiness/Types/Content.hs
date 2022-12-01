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
-- Module      : Amazonka.AlexaBusiness.Types.Content
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Content where

import Amazonka.AlexaBusiness.Types.Audio
import Amazonka.AlexaBusiness.Types.Ssml
import Amazonka.AlexaBusiness.Types.TextMessage
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The content definition. This can contain only one text, SSML, or audio
-- list object.
--
-- /See:/ 'newContent' smart constructor.
data Content = Content'
  { -- | The list of audio messages.
    audioList :: Prelude.Maybe [Audio],
    -- | The list of text messages.
    textList :: Prelude.Maybe [TextMessage],
    -- | The list of SSML messages.
    ssmlList :: Prelude.Maybe [Ssml]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Content' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioList', 'content_audioList' - The list of audio messages.
--
-- 'textList', 'content_textList' - The list of text messages.
--
-- 'ssmlList', 'content_ssmlList' - The list of SSML messages.
newContent ::
  Content
newContent =
  Content'
    { audioList = Prelude.Nothing,
      textList = Prelude.Nothing,
      ssmlList = Prelude.Nothing
    }

-- | The list of audio messages.
content_audioList :: Lens.Lens' Content (Prelude.Maybe [Audio])
content_audioList = Lens.lens (\Content' {audioList} -> audioList) (\s@Content' {} a -> s {audioList = a} :: Content) Prelude.. Lens.mapping Lens.coerced

-- | The list of text messages.
content_textList :: Lens.Lens' Content (Prelude.Maybe [TextMessage])
content_textList = Lens.lens (\Content' {textList} -> textList) (\s@Content' {} a -> s {textList = a} :: Content) Prelude.. Lens.mapping Lens.coerced

-- | The list of SSML messages.
content_ssmlList :: Lens.Lens' Content (Prelude.Maybe [Ssml])
content_ssmlList = Lens.lens (\Content' {ssmlList} -> ssmlList) (\s@Content' {} a -> s {ssmlList = a} :: Content) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable Content where
  hashWithSalt _salt Content' {..} =
    _salt `Prelude.hashWithSalt` audioList
      `Prelude.hashWithSalt` textList
      `Prelude.hashWithSalt` ssmlList

instance Prelude.NFData Content where
  rnf Content' {..} =
    Prelude.rnf audioList
      `Prelude.seq` Prelude.rnf textList
      `Prelude.seq` Prelude.rnf ssmlList

instance Core.ToJSON Content where
  toJSON Content' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AudioList" Core..=) Prelude.<$> audioList,
            ("TextList" Core..=) Prelude.<$> textList,
            ("SsmlList" Core..=) Prelude.<$> ssmlList
          ]
      )
