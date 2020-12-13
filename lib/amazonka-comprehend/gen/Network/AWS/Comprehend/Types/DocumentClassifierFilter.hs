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

import Network.AWS.Comprehend.Types.ModelStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information for filtering a list of document classifiers. You can only specify one filtering parameter in a request. For more information, see the operation.
--
-- /See:/ 'mkDocumentClassifierFilter' smart constructor.
data DocumentClassifierFilter = DocumentClassifierFilter'
  { -- | Filters the list of classifiers based on status.
    status :: Lude.Maybe ModelStatus,
    -- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted after the specified time. Classifiers are returned in descending order, newest to oldest.
    submitTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted before the specified time. Classifiers are returned in ascending order, oldest to newest.
    submitTimeBefore :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentClassifierFilter' with the minimum fields required to make a request.
--
-- * 'status' - Filters the list of classifiers based on status.
-- * 'submitTimeAfter' - Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted after the specified time. Classifiers are returned in descending order, newest to oldest.
-- * 'submitTimeBefore' - Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted before the specified time. Classifiers are returned in ascending order, oldest to newest.
mkDocumentClassifierFilter ::
  DocumentClassifierFilter
mkDocumentClassifierFilter =
  DocumentClassifierFilter'
    { status = Lude.Nothing,
      submitTimeAfter = Lude.Nothing,
      submitTimeBefore = Lude.Nothing
    }

-- | Filters the list of classifiers based on status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfStatus :: Lens.Lens' DocumentClassifierFilter (Lude.Maybe ModelStatus)
dcfStatus = Lens.lens (status :: DocumentClassifierFilter -> Lude.Maybe ModelStatus) (\s a -> s {status = a} :: DocumentClassifierFilter)
{-# DEPRECATED dcfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted after the specified time. Classifiers are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfSubmitTimeAfter :: Lens.Lens' DocumentClassifierFilter (Lude.Maybe Lude.Timestamp)
dcfSubmitTimeAfter = Lens.lens (submitTimeAfter :: DocumentClassifierFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeAfter = a} :: DocumentClassifierFilter)
{-# DEPRECATED dcfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of classifiers based on the time that the classifier was submitted for processing. Returns only classifiers submitted before the specified time. Classifiers are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfSubmitTimeBefore :: Lens.Lens' DocumentClassifierFilter (Lude.Maybe Lude.Timestamp)
dcfSubmitTimeBefore = Lens.lens (submitTimeBefore :: DocumentClassifierFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeBefore = a} :: DocumentClassifierFilter)
{-# DEPRECATED dcfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

instance Lude.ToJSON DocumentClassifierFilter where
  toJSON DocumentClassifierFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("SubmitTimeAfter" Lude..=) Lude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Lude..=) Lude.<$> submitTimeBefore
          ]
      )
