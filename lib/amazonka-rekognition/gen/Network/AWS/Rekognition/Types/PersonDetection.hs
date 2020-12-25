{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.PersonDetail as Types

-- | Details and path tracking information for a single time a person's path is tracked in a video. Amazon Rekognition operations that track people's paths return an array of @PersonDetection@ objects with elements for each time a person's path is tracked in a video.
--
-- For more information, see GetPersonTracking in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkPersonDetection' smart constructor.
data PersonDetection = PersonDetection'
  { -- | Details about a person whose path was tracked in a video.
    person :: Core.Maybe Types.PersonDetail,
    -- | The time, in milliseconds from the start of the video, that the person's path was tracked.
    timestamp :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PersonDetection' value with any optional fields omitted.
mkPersonDetection ::
  PersonDetection
mkPersonDetection =
  PersonDetection' {person = Core.Nothing, timestamp = Core.Nothing}

-- | Details about a person whose path was tracked in a video.
--
-- /Note:/ Consider using 'person' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPerson :: Lens.Lens' PersonDetection (Core.Maybe Types.PersonDetail)
pdPerson = Lens.field @"person"
{-# DEPRECATED pdPerson "Use generic-lens or generic-optics with 'person' instead." #-}

-- | The time, in milliseconds from the start of the video, that the person's path was tracked.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdTimestamp :: Lens.Lens' PersonDetection (Core.Maybe Core.Integer)
pdTimestamp = Lens.field @"timestamp"
{-# DEPRECATED pdTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromJSON PersonDetection where
  parseJSON =
    Core.withObject "PersonDetection" Core.$
      \x ->
        PersonDetection'
          Core.<$> (x Core..:? "Person") Core.<*> (x Core..:? "Timestamp")
