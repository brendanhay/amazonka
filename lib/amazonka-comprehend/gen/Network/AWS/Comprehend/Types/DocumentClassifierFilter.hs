{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierFilter
  ( DocumentClassifierFilter (..),

    -- * Smart constructor
    mkDocumentClassifierFilter,

    -- * Lenses
    dcfStatus,
    dcfSubmitTimeAfter,
    dcfSubmitTimeBefore,
  )
where

import qualified Network.AWS.Comprehend.Types.ModelStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information for filtering a list of document classifiers. You can only specify one filtering parameter in a request. For more information, see the operation.
--
-- /See:/ 'mkDocumentClassifierFilter' smart constructor.
data DocumentClassifierFilter = DocumentClassifierFilter'
  { -- | Filters the list of classifiers based on status.
    status :: Core.Maybe Types.ModelStatus,
    -- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted after the specified time. Classifiers are returned in descending order, newest to oldest.
    submitTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted before the specified time. Classifiers are returned in ascending order, oldest to newest.
    submitTimeBefore :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DocumentClassifierFilter' value with any optional fields omitted.
mkDocumentClassifierFilter ::
  DocumentClassifierFilter
mkDocumentClassifierFilter =
  DocumentClassifierFilter'
    { status = Core.Nothing,
      submitTimeAfter = Core.Nothing,
      submitTimeBefore = Core.Nothing
    }

-- | Filters the list of classifiers based on status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfStatus :: Lens.Lens' DocumentClassifierFilter (Core.Maybe Types.ModelStatus)
dcfStatus = Lens.field @"status"
{-# DEPRECATED dcfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted after the specified time. Classifiers are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfSubmitTimeAfter :: Lens.Lens' DocumentClassifierFilter (Core.Maybe Core.NominalDiffTime)
dcfSubmitTimeAfter = Lens.field @"submitTimeAfter"
{-# DEPRECATED dcfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted before the specified time. Classifiers are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfSubmitTimeBefore :: Lens.Lens' DocumentClassifierFilter (Core.Maybe Core.NominalDiffTime)
dcfSubmitTimeBefore = Lens.field @"submitTimeBefore"
{-# DEPRECATED dcfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

instance Core.FromJSON DocumentClassifierFilter where
  toJSON DocumentClassifierFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
            ("SubmitTimeBefore" Core..=) Core.<$> submitTimeBefore
          ]
      )
