-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.PersonMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.PersonMatch
  ( PersonMatch (..),

    -- * Smart constructor
    mkPersonMatch,

    -- * Lenses
    pmFaceMatches,
    pmPerson,
    pmTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.FaceMatch
import Network.AWS.Rekognition.Types.PersonDetail

-- | Information about a person whose face matches a face(s) in an Amazon Rekognition collection. Includes information about the faces in the Amazon Rekognition collection ('FaceMatch' ), information about the person ('PersonDetail' ), and the time stamp for when the person was detected in a video. An array of @PersonMatch@ objects is returned by 'GetFaceSearch' .
--
-- /See:/ 'mkPersonMatch' smart constructor.
data PersonMatch = PersonMatch'
  { faceMatches ::
      Lude.Maybe [FaceMatch],
    person :: Lude.Maybe PersonDetail,
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

-- | Creates a value of 'PersonMatch' with the minimum fields required to make a request.
--
-- * 'faceMatches' - Information about the faces in the input collection that match the face of a person in the video.
-- * 'person' - Information about the matched person.
-- * 'timestamp' - The time, in milliseconds from the beginning of the video, that the person was matched in the video.
mkPersonMatch ::
  PersonMatch
mkPersonMatch =
  PersonMatch'
    { faceMatches = Lude.Nothing,
      person = Lude.Nothing,
      timestamp = Lude.Nothing
    }

-- | Information about the faces in the input collection that match the face of a person in the video.
--
-- /Note:/ Consider using 'faceMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmFaceMatches :: Lens.Lens' PersonMatch (Lude.Maybe [FaceMatch])
pmFaceMatches = Lens.lens (faceMatches :: PersonMatch -> Lude.Maybe [FaceMatch]) (\s a -> s {faceMatches = a} :: PersonMatch)
{-# DEPRECATED pmFaceMatches "Use generic-lens or generic-optics with 'faceMatches' instead." #-}

-- | Information about the matched person.
--
-- /Note:/ Consider using 'person' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmPerson :: Lens.Lens' PersonMatch (Lude.Maybe PersonDetail)
pmPerson = Lens.lens (person :: PersonMatch -> Lude.Maybe PersonDetail) (\s a -> s {person = a} :: PersonMatch)
{-# DEPRECATED pmPerson "Use generic-lens or generic-optics with 'person' instead." #-}

-- | The time, in milliseconds from the beginning of the video, that the person was matched in the video.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmTimestamp :: Lens.Lens' PersonMatch (Lude.Maybe Lude.Integer)
pmTimestamp = Lens.lens (timestamp :: PersonMatch -> Lude.Maybe Lude.Integer) (\s a -> s {timestamp = a} :: PersonMatch)
{-# DEPRECATED pmTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.FromJSON PersonMatch where
  parseJSON =
    Lude.withObject
      "PersonMatch"
      ( \x ->
          PersonMatch'
            Lude.<$> (x Lude..:? "FaceMatches" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Person")
            Lude.<*> (x Lude..:? "Timestamp")
      )
