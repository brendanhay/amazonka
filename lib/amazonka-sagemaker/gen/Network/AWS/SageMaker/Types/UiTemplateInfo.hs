{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiTemplateInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiTemplateInfo
  ( UiTemplateInfo (..),

    -- * Smart constructor
    mkUiTemplateInfo,

    -- * Lenses
    utiURL,
    utiContentSha256,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Container for user interface template information.
--
-- /See:/ 'mkUiTemplateInfo' smart constructor.
data UiTemplateInfo = UiTemplateInfo'
  { url :: Lude.Maybe Lude.Text,
    contentSha256 :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UiTemplateInfo' with the minimum fields required to make a request.
--
-- * 'contentSha256' - The SHA-256 digest of the contents of the template.
-- * 'url' - The URL for the user interface template.
mkUiTemplateInfo ::
  UiTemplateInfo
mkUiTemplateInfo =
  UiTemplateInfo' {url = Lude.Nothing, contentSha256 = Lude.Nothing}

-- | The URL for the user interface template.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utiURL :: Lens.Lens' UiTemplateInfo (Lude.Maybe Lude.Text)
utiURL = Lens.lens (url :: UiTemplateInfo -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: UiTemplateInfo)
{-# DEPRECATED utiURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The SHA-256 digest of the contents of the template.
--
-- /Note:/ Consider using 'contentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utiContentSha256 :: Lens.Lens' UiTemplateInfo (Lude.Maybe Lude.Text)
utiContentSha256 = Lens.lens (contentSha256 :: UiTemplateInfo -> Lude.Maybe Lude.Text) (\s a -> s {contentSha256 = a} :: UiTemplateInfo)
{-# DEPRECATED utiContentSha256 "Use generic-lens or generic-optics with 'contentSha256' instead." #-}

instance Lude.FromJSON UiTemplateInfo where
  parseJSON =
    Lude.withObject
      "UiTemplateInfo"
      ( \x ->
          UiTemplateInfo'
            Lude.<$> (x Lude..:? "Url") Lude.<*> (x Lude..:? "ContentSha256")
      )
