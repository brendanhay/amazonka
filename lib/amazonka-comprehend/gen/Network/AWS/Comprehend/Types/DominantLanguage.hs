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

import qualified Network.AWS.Comprehend.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns the code for the dominant language in the input text and the level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /See:/ 'mkDominantLanguage' smart constructor.
data DominantLanguage = DominantLanguage'
  { -- | The RFC 5646 language code for the dominant language. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
    languageCode :: Core.Maybe Types.String,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
    score :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DominantLanguage' value with any optional fields omitted.
mkDominantLanguage ::
  DominantLanguage
mkDominantLanguage =
  DominantLanguage'
    { languageCode = Core.Nothing,
      score = Core.Nothing
    }

-- | The RFC 5646 language code for the dominant language. For more information about RFC 5646, see <https://tools.ietf.org/html/rfc5646 Tags for Identifying Languages> on the /IETF Tools/ web site.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLanguageCode :: Lens.Lens' DominantLanguage (Core.Maybe Types.String)
dlLanguageCode = Lens.field @"languageCode"
{-# DEPRECATED dlLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of the detection.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlScore :: Lens.Lens' DominantLanguage (Core.Maybe Core.Double)
dlScore = Lens.field @"score"
{-# DEPRECATED dlScore "Use generic-lens or generic-optics with 'score' instead." #-}

instance Core.FromJSON DominantLanguage where
  parseJSON =
    Core.withObject "DominantLanguage" Core.$
      \x ->
        DominantLanguage'
          Core.<$> (x Core..:? "LanguageCode") Core.<*> (x Core..:? "Score")
