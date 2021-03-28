{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.PersonMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.PersonMatch
  ( PersonMatch (..)
  -- * Smart constructor
  , mkPersonMatch
  -- * Lenses
  , pmFaceMatches
  , pmPerson
  , pmTimestamp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.FaceMatch as Types
import qualified Network.AWS.Rekognition.Types.PersonDetail as Types

-- | Information about a person whose face matches a face(s) in an Amazon Rekognition collection. Includes information about the faces in the Amazon Rekognition collection ('FaceMatch' ), information about the person ('PersonDetail' ), and the time stamp for when the person was detected in a video. An array of @PersonMatch@ objects is returned by 'GetFaceSearch' . 
--
-- /See:/ 'mkPersonMatch' smart constructor.
data PersonMatch = PersonMatch'
  { faceMatches :: Core.Maybe [Types.FaceMatch]
    -- ^ Information about the faces in the input collection that match the face of a person in the video.
  , person :: Core.Maybe Types.PersonDetail
    -- ^ Information about the matched person.
  , timestamp :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds from the beginning of the video, that the person was matched in the video.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PersonMatch' value with any optional fields omitted.
mkPersonMatch
    :: PersonMatch
mkPersonMatch
  = PersonMatch'{faceMatches = Core.Nothing, person = Core.Nothing,
                 timestamp = Core.Nothing}

-- | Information about the faces in the input collection that match the face of a person in the video.
--
-- /Note:/ Consider using 'faceMatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmFaceMatches :: Lens.Lens' PersonMatch (Core.Maybe [Types.FaceMatch])
pmFaceMatches = Lens.field @"faceMatches"
{-# INLINEABLE pmFaceMatches #-}
{-# DEPRECATED faceMatches "Use generic-lens or generic-optics with 'faceMatches' instead"  #-}

-- | Information about the matched person.
--
-- /Note:/ Consider using 'person' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmPerson :: Lens.Lens' PersonMatch (Core.Maybe Types.PersonDetail)
pmPerson = Lens.field @"person"
{-# INLINEABLE pmPerson #-}
{-# DEPRECATED person "Use generic-lens or generic-optics with 'person' instead"  #-}

-- | The time, in milliseconds from the beginning of the video, that the person was matched in the video.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmTimestamp :: Lens.Lens' PersonMatch (Core.Maybe Core.Integer)
pmTimestamp = Lens.field @"timestamp"
{-# INLINEABLE pmTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromJSON PersonMatch where
        parseJSON
          = Core.withObject "PersonMatch" Core.$
              \ x ->
                PersonMatch' Core.<$>
                  (x Core..:? "FaceMatches") Core.<*> x Core..:? "Person" Core.<*>
                    x Core..:? "Timestamp"
