{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CelebrityRecognition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.CelebrityRecognition
  ( CelebrityRecognition (..)
  -- * Smart constructor
  , mkCelebrityRecognition
  -- * Lenses
  , crCelebrity
  , crTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.CelebrityDetail as Types

-- | Information about a detected celebrity and the time the celebrity was detected in a stored video. For more information, see GetCelebrityRecognition in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkCelebrityRecognition' smart constructor.
data CelebrityRecognition = CelebrityRecognition'
  { celebrity :: Core.Maybe Types.CelebrityDetail
    -- ^ Information about a recognized celebrity.
  , timestamp :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds from the start of the video, that the celebrity was recognized.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CelebrityRecognition' value with any optional fields omitted.
mkCelebrityRecognition
    :: CelebrityRecognition
mkCelebrityRecognition
  = CelebrityRecognition'{celebrity = Core.Nothing,
                          timestamp = Core.Nothing}

-- | Information about a recognized celebrity.
--
-- /Note:/ Consider using 'celebrity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCelebrity :: Lens.Lens' CelebrityRecognition (Core.Maybe Types.CelebrityDetail)
crCelebrity = Lens.field @"celebrity"
{-# INLINEABLE crCelebrity #-}
{-# DEPRECATED celebrity "Use generic-lens or generic-optics with 'celebrity' instead"  #-}

-- | The time, in milliseconds from the start of the video, that the celebrity was recognized.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTimestamp :: Lens.Lens' CelebrityRecognition (Core.Maybe Core.Integer)
crTimestamp = Lens.field @"timestamp"
{-# INLINEABLE crTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromJSON CelebrityRecognition where
        parseJSON
          = Core.withObject "CelebrityRecognition" Core.$
              \ x ->
                CelebrityRecognition' Core.<$>
                  (x Core..:? "Celebrity") Core.<*> x Core..:? "Timestamp"
