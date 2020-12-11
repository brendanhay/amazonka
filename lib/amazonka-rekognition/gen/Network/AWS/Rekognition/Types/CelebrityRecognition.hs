-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.CelebrityRecognition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.CelebrityRecognition
  ( CelebrityRecognition (..),

    -- * Smart constructor
    mkCelebrityRecognition,

    -- * Lenses
    crCelebrity,
    crTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.CelebrityDetail

-- | Information about a detected celebrity and the time the celebrity was detected in a stored video. For more information, see GetCelebrityRecognition in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkCelebrityRecognition' smart constructor.
data CelebrityRecognition = CelebrityRecognition'
  { celebrity ::
      Lude.Maybe CelebrityDetail,
    timestamp :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CelebrityRecognition' with the minimum fields required to make a request.
--
-- * 'celebrity' - Information about a recognized celebrity.
-- * 'timestamp' - The time, in milliseconds from the start of the video, that the celebrity was recognized.
mkCelebrityRecognition ::
  CelebrityRecognition
mkCelebrityRecognition =
  CelebrityRecognition'
    { celebrity = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | Information about a recognized celebrity.
--
-- /Note:/ Consider using 'celebrity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCelebrity :: Lens.Lens' CelebrityRecognition (Lude.Maybe CelebrityDetail)
crCelebrity = Lens.lens (celebrity :: CelebrityRecognition -> Lude.Maybe CelebrityDetail) (\s a -> s {celebrity = a} :: CelebrityRecognition)
{-# DEPRECATED crCelebrity "Use generic-lens or generic-optics with 'celebrity' instead." #-}

-- | The time, in milliseconds from the start of the video, that the celebrity was recognized.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTimestamp :: Lens.Lens' CelebrityRecognition (Lude.Maybe Lude.Integer)
crTimestamp = Lens.lens (timestamp :: CelebrityRecognition -> Lude.Maybe Lude.Integer) (\s a -> s {timestamp = a} :: CelebrityRecognition)
{-# DEPRECATED crTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON CelebrityRecognition where
  parseJSON =
    Lude.withObject
      "CelebrityRecognition"
      ( \x ->
          CelebrityRecognition'
            Lude.<$> (x Lude..:? "Celebrity") Lude.<*> (x Lude..:? "Timestamp")
      )
