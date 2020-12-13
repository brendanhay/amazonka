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

import Network.AWS.Comprehend.Types.ModelStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information for filtering a list of entity recognizers. You can only specify one filtering parameter in a request. For more information, see the operation./>
--
-- /See:/ 'mkEntityRecognizerFilter' smart constructor.
data EntityRecognizerFilter = EntityRecognizerFilter'
  { -- | The status of an entity recognizer.
    status :: Lude.Maybe ModelStatus,
    -- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
    submitTimeAfter :: Lude.Maybe Lude.Timestamp,
    -- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
    submitTimeBefore :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityRecognizerFilter' with the minimum fields required to make a request.
--
-- * 'status' - The status of an entity recognizer.
-- * 'submitTimeAfter' - Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
-- * 'submitTimeBefore' - Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
mkEntityRecognizerFilter ::
  EntityRecognizerFilter
mkEntityRecognizerFilter =
  EntityRecognizerFilter'
    { status = Lude.Nothing,
      submitTimeAfter = Lude.Nothing,
      submitTimeBefore = Lude.Nothing
    }

-- | The status of an entity recognizer.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfStatus :: Lens.Lens' EntityRecognizerFilter (Lude.Maybe ModelStatus)
erfStatus = Lens.lens (status :: EntityRecognizerFilter -> Lude.Maybe ModelStatus) (\s a -> s {status = a} :: EntityRecognizerFilter)
{-# DEPRECATED erfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfSubmitTimeAfter :: Lens.Lens' EntityRecognizerFilter (Lude.Maybe Lude.Timestamp)
erfSubmitTimeAfter = Lens.lens (submitTimeAfter :: EntityRecognizerFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeAfter = a} :: EntityRecognizerFilter)
{-# DEPRECATED erfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of entities based on the time that the list was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erfSubmitTimeBefore :: Lens.Lens' EntityRecognizerFilter (Lude.Maybe Lude.Timestamp)
erfSubmitTimeBefore = Lens.lens (submitTimeBefore :: EntityRecognizerFilter -> Lude.Maybe Lude.Timestamp) (\s a -> s {submitTimeBefore = a} :: EntityRecognizerFilter)
{-# DEPRECATED erfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

instance Lude.ToJSON EntityRecognizerFilter where
  toJSON EntityRecognizerFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("SubmitTimeAfter" Lude..=) Lude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Lude..=) Lude.<$> submitTimeBefore
          ]
      )
