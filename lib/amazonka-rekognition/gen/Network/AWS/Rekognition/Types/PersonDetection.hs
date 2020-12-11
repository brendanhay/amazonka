-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.PersonDetection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonDetection
  ( PersonDetection (..),

    -- * Smart constructor
    mkPersonDetection,

    -- * Lenses
    pdPerson,
    pdTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.PersonDetail

-- | Details and path tracking information for a single time a person's path is tracked in a video. Amazon Rekognition operations that track people's paths return an array of @PersonDetection@ objects with elements for each time a person's path is tracked in a video.
--
-- For more information, see GetPersonTracking in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkPersonDetection' smart constructor.
data PersonDetection = PersonDetection'
  { person ::
      Lude.Maybe PersonDetail,
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

-- | Creates a value of 'PersonDetection' with the minimum fields required to make a request.
--
-- * 'person' - Details about a person whose path was tracked in a video.
-- * 'timestamp' - The time, in milliseconds from the start of the video, that the person's path was tracked.
mkPersonDetection ::
  PersonDetection
mkPersonDetection =
  PersonDetection' {person = Lude.Nothing, timestamp = Lude.Nothing}

-- | Details about a person whose path was tracked in a video.
--
-- /Note:/ Consider using 'person' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPerson :: Lens.Lens' PersonDetection (Lude.Maybe PersonDetail)
pdPerson = Lens.lens (person :: PersonDetection -> Lude.Maybe PersonDetail) (\s a -> s {person = a} :: PersonDetection)
{-# DEPRECATED pdPerson "Use generic-lens or generic-optics with 'person' instead." #-}

-- | The time, in milliseconds from the start of the video, that the person's path was tracked.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdTimestamp :: Lens.Lens' PersonDetection (Lude.Maybe Lude.Integer)
pdTimestamp = Lens.lens (timestamp :: PersonDetection -> Lude.Maybe Lude.Integer) (\s a -> s {timestamp = a} :: PersonDetection)
{-# DEPRECATED pdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON PersonDetection where
  parseJSON =
    Lude.withObject
      "PersonDetection"
      ( \x ->
          PersonDetection'
            Lude.<$> (x Lude..:? "Person") Lude.<*> (x Lude..:? "Timestamp")
      )
