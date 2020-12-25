{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerFilter
  ( EntityRecognizerFilter (..),

    -- * Smart constructor
    mkEntityRecognizerFilter,

    -- * Lenses
    erfStatus,
    erfSubmitTimeAfter,
    erfSubmitTimeBefore,
  )
where

import qualified Network.AWS.Comprehend.Types.ModelStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information for filtering a list of entity recognizers. You can only specify one filtering parameter in a request. For more information, see the operation./>
--
-- /See:/ 'mkEntityRecognizerFilter' smart constructor.
data EntityRecognizerFilter = EntityRecognizerFilter'
  { -- | The status of an entity recognizer.
    status :: Core.Maybe Types.ModelStatus,
    -- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
    submitTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
    submitTimeBefore :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EntityRecognizerFilter' value with any optional fields omitted.
mkEntityRecognizerFilter ::
  EntityRecognizerFilter
mkEntityRecognizerFilter =
  EntityRecognizerFilter'
    { status = Core.Nothing,
      submitTimeAfter = Core.Nothing,
      submitTimeBefore = Core.Nothing
    }

-- | The status of an entity recognizer.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfStatus :: Lens.Lens' EntityRecognizerFilter (Core.Maybe Types.ModelStatus)
erfStatus = Lens.field @"status"
{-# DEPRECATED erfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfSubmitTimeAfter :: Lens.Lens' EntityRecognizerFilter (Core.Maybe Core.NominalDiffTime)
erfSubmitTimeAfter = Lens.field @"submitTimeAfter"
{-# DEPRECATED erfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfSubmitTimeBefore :: Lens.Lens' EntityRecognizerFilter (Core.Maybe Core.NominalDiffTime)
erfSubmitTimeBefore = Lens.field @"submitTimeBefore"
{-# DEPRECATED erfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

instance Core.FromJSON EntityRecognizerFilter where
  toJSON EntityRecognizerFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
            ("SubmitTimeBefore" Core..=) Core.<$> submitTimeBefore
          ]
      )
