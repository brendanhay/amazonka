{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.ModelSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.ModelSettings
  ( ModelSettings (..),

    -- * Smart constructor
    mkModelSettings,

    -- * Lenses
    msLanguageModelName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The object used to call your custom language model to your transcription job.
--
-- /See:/ 'mkModelSettings' smart constructor.
newtype ModelSettings = ModelSettings'
  { languageModelName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelSettings' with the minimum fields required to make a request.
--
-- * 'languageModelName' - The name of your custom language model.
mkModelSettings ::
  ModelSettings
mkModelSettings = ModelSettings' {languageModelName = Lude.Nothing}

-- | The name of your custom language model.
--
-- /Note:/ Consider using 'languageModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msLanguageModelName :: Lens.Lens' ModelSettings (Lude.Maybe Lude.Text)
msLanguageModelName = Lens.lens (languageModelName :: ModelSettings -> Lude.Maybe Lude.Text) (\s a -> s {languageModelName = a} :: ModelSettings)
{-# DEPRECATED msLanguageModelName "Use generic-lens or generic-optics with 'languageModelName' instead." #-}

instance Lude.FromJSON ModelSettings where
  parseJSON =
    Lude.withObject
      "ModelSettings"
      (\x -> ModelSettings' Lude.<$> (x Lude..:? "LanguageModelName"))

instance Lude.ToJSON ModelSettings where
  toJSON ModelSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [("LanguageModelName" Lude..=) Lude.<$> languageModelName]
      )
