{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CaptionLanguageMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.CaptionLanguageMapping
  ( CaptionLanguageMapping (..)
  -- * Smart constructor
  , mkCaptionLanguageMapping
  -- * Lenses
  , clmLanguageCode
  , clmLanguageDescription
  , clmCaptionChannel
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Maps a caption channel to an ISO 693-2 language code (http://www.loc.gov/standards/iso639-2), with an optional description.
--
-- /See:/ 'mkCaptionLanguageMapping' smart constructor.
data CaptionLanguageMapping = CaptionLanguageMapping'
  { languageCode :: Core.Text
    -- ^ Three character ISO 639-2 language code (see http://www.loc.gov/standards/iso639-2)
  , languageDescription :: Core.Text
    -- ^ Textual description of language
  , captionChannel :: Core.Natural
    -- ^ The closed caption channel being described by this CaptionLanguageMapping.  Each channel mapping must have a unique channel number (maximum of 4)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptionLanguageMapping' value with any optional fields omitted.
mkCaptionLanguageMapping
    :: Core.Text -- ^ 'languageCode'
    -> Core.Text -- ^ 'languageDescription'
    -> Core.Natural -- ^ 'captionChannel'
    -> CaptionLanguageMapping
mkCaptionLanguageMapping languageCode languageDescription
  captionChannel
  = CaptionLanguageMapping'{languageCode, languageDescription,
                            captionChannel}

-- | Three character ISO 639-2 language code (see http://www.loc.gov/standards/iso639-2)
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmLanguageCode :: Lens.Lens' CaptionLanguageMapping Core.Text
clmLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE clmLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | Textual description of language
--
-- /Note:/ Consider using 'languageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmLanguageDescription :: Lens.Lens' CaptionLanguageMapping Core.Text
clmLanguageDescription = Lens.field @"languageDescription"
{-# INLINEABLE clmLanguageDescription #-}
{-# DEPRECATED languageDescription "Use generic-lens or generic-optics with 'languageDescription' instead"  #-}

-- | The closed caption channel being described by this CaptionLanguageMapping.  Each channel mapping must have a unique channel number (maximum of 4)
--
-- /Note:/ Consider using 'captionChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clmCaptionChannel :: Lens.Lens' CaptionLanguageMapping Core.Natural
clmCaptionChannel = Lens.field @"captionChannel"
{-# INLINEABLE clmCaptionChannel #-}
{-# DEPRECATED captionChannel "Use generic-lens or generic-optics with 'captionChannel' instead"  #-}

instance Core.FromJSON CaptionLanguageMapping where
        toJSON CaptionLanguageMapping{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("languageCode" Core..= languageCode),
                  Core.Just ("languageDescription" Core..= languageDescription),
                  Core.Just ("captionChannel" Core..= captionChannel)])

instance Core.FromJSON CaptionLanguageMapping where
        parseJSON
          = Core.withObject "CaptionLanguageMapping" Core.$
              \ x ->
                CaptionLanguageMapping' Core.<$>
                  (x Core..: "languageCode") Core.<*> x Core..: "languageDescription"
                    Core.<*> x Core..: "captionChannel"
