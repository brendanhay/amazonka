{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DominantLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguage
  ( DominantLanguage (..),

    -- * Smart constructor
    mkDominantLanguage,

    -- * Lenses
    dlLanguageCode,
    dlScore,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns the code for the dominant language in the input text and the level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /See:/ 'mkDominantLanguage' smart constructor.
data DominantLanguage = DominantLanguage'
  { -- | The RFC 5646 language code for the dominant language. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
    languageCode :: Lude.Maybe Lude.Text,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
    score :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DominantLanguage' with the minimum fields required to make a request.
--
-- * 'languageCode' - The RFC 5646 language code for the dominant language. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
-- * 'score' - The level of confidence that Amazon Comprehend has in the accuracy of the detection.
mkDominantLanguage ::
  DominantLanguage
mkDominantLanguage =
  DominantLanguage'
    { languageCode = Lude.Nothing,
      score = Lude.Nothing
    }

-- | The RFC 5646 language code for the dominant language. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLanguageCode :: Lens.Lens' DominantLanguage (Lude.Maybe Lude.Text)
dlLanguageCode = Lens.lens (languageCode :: DominantLanguage -> Lude.Maybe Lude.Text) (\s a -> s {languageCode = a} :: DominantLanguage)
{-# DEPRECATED dlLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlScore :: Lens.Lens' DominantLanguage (Lude.Maybe Lude.Double)
dlScore = Lens.lens (score :: DominantLanguage -> Lude.Maybe Lude.Double) (\s a -> s {score = a} :: DominantLanguage)
{-# DEPRECATED dlScore "Use generic-lens or generic-optics with 'score' instead." #-}

instance Lude.FromJSON DominantLanguage where
  parseJSON =
    Lude.withObject
      "DominantLanguage"
      ( \x ->
          DominantLanguage'
            Lude.<$> (x Lude..:? "LanguageCode") Lude.<*> (x Lude..:? "Score")
      )
