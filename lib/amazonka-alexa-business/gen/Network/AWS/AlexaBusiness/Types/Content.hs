{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Content
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Content
  ( Content (..),

    -- * Smart constructor
    mkContent,

    -- * Lenses
    cAudioList,
    cTextList,
    cSsmlList,
  )
where

import Network.AWS.AlexaBusiness.Types.Audio
import Network.AWS.AlexaBusiness.Types.Ssml
import Network.AWS.AlexaBusiness.Types.TextMessage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The content definition. This can contain only one text, SSML, or audio list object.
--
-- /See:/ 'mkContent' smart constructor.
data Content = Content'
  { -- | The list of audio messages.
    audioList :: Lude.Maybe [Audio],
    -- | The list of text messages.
    textList :: Lude.Maybe [TextMessage],
    -- | The list of SSML messages.
    ssmlList :: Lude.Maybe [Ssml]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Content' with the minimum fields required to make a request.
--
-- * 'audioList' - The list of audio messages.
-- * 'textList' - The list of text messages.
-- * 'ssmlList' - The list of SSML messages.
mkContent ::
  Content
mkContent =
  Content'
    { audioList = Lude.Nothing,
      textList = Lude.Nothing,
      ssmlList = Lude.Nothing
    }

-- | The list of audio messages.
--
-- /Note:/ Consider using 'audioList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAudioList :: Lens.Lens' Content (Lude.Maybe [Audio])
cAudioList = Lens.lens (audioList :: Content -> Lude.Maybe [Audio]) (\s a -> s {audioList = a} :: Content)
{-# DEPRECATED cAudioList "Use generic-lens or generic-optics with 'audioList' instead." #-}

-- | The list of text messages.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTextList :: Lens.Lens' Content (Lude.Maybe [TextMessage])
cTextList = Lens.lens (textList :: Content -> Lude.Maybe [TextMessage]) (\s a -> s {textList = a} :: Content)
{-# DEPRECATED cTextList "Use generic-lens or generic-optics with 'textList' instead." #-}

-- | The list of SSML messages.
--
-- /Note:/ Consider using 'ssmlList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSsmlList :: Lens.Lens' Content (Lude.Maybe [Ssml])
cSsmlList = Lens.lens (ssmlList :: Content -> Lude.Maybe [Ssml]) (\s a -> s {ssmlList = a} :: Content)
{-# DEPRECATED cSsmlList "Use generic-lens or generic-optics with 'ssmlList' instead." #-}

instance Lude.ToJSON Content where
  toJSON Content' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AudioList" Lude..=) Lude.<$> audioList,
            ("TextList" Lude..=) Lude.<$> textList,
            ("SsmlList" Lude..=) Lude.<$> ssmlList
          ]
      )
