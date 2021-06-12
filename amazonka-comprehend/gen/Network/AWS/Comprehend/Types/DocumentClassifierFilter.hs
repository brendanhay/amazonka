{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierFilter where

import Network.AWS.Comprehend.Types.ModelStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information for filtering a list of document classifiers. You
-- can only specify one filtering parameter in a request. For more
-- information, see the operation.
--
-- /See:/ 'newDocumentClassifierFilter' smart constructor.
data DocumentClassifierFilter = DocumentClassifierFilter'
  { -- | Filters the list of classifiers based on status.
    status :: Core.Maybe ModelStatus,
    -- | Filters the list of classifiers based on the time that the classifier
    -- was submitted for processing. Returns only classifiers submitted before
    -- the specified time. Classifiers are returned in ascending order, oldest
    -- to newest.
    submitTimeBefore :: Core.Maybe Core.POSIX,
    -- | Filters the list of classifiers based on the time that the classifier
    -- was submitted for processing. Returns only classifiers submitted after
    -- the specified time. Classifiers are returned in descending order, newest
    -- to oldest.
    submitTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentClassifierFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'documentClassifierFilter_status' - Filters the list of classifiers based on status.
--
-- 'submitTimeBefore', 'documentClassifierFilter_submitTimeBefore' - Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted before
-- the specified time. Classifiers are returned in ascending order, oldest
-- to newest.
--
-- 'submitTimeAfter', 'documentClassifierFilter_submitTimeAfter' - Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted after
-- the specified time. Classifiers are returned in descending order, newest
-- to oldest.
newDocumentClassifierFilter ::
  DocumentClassifierFilter
newDocumentClassifierFilter =
  DocumentClassifierFilter'
    { status = Core.Nothing,
      submitTimeBefore = Core.Nothing,
      submitTimeAfter = Core.Nothing
    }

-- | Filters the list of classifiers based on status.
documentClassifierFilter_status :: Lens.Lens' DocumentClassifierFilter (Core.Maybe ModelStatus)
documentClassifierFilter_status = Lens.lens (\DocumentClassifierFilter' {status} -> status) (\s@DocumentClassifierFilter' {} a -> s {status = a} :: DocumentClassifierFilter)

-- | Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted before
-- the specified time. Classifiers are returned in ascending order, oldest
-- to newest.
documentClassifierFilter_submitTimeBefore :: Lens.Lens' DocumentClassifierFilter (Core.Maybe Core.UTCTime)
documentClassifierFilter_submitTimeBefore = Lens.lens (\DocumentClassifierFilter' {submitTimeBefore} -> submitTimeBefore) (\s@DocumentClassifierFilter' {} a -> s {submitTimeBefore = a} :: DocumentClassifierFilter) Core.. Lens.mapping Core._Time

-- | Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted after
-- the specified time. Classifiers are returned in descending order, newest
-- to oldest.
documentClassifierFilter_submitTimeAfter :: Lens.Lens' DocumentClassifierFilter (Core.Maybe Core.UTCTime)
documentClassifierFilter_submitTimeAfter = Lens.lens (\DocumentClassifierFilter' {submitTimeAfter} -> submitTimeAfter) (\s@DocumentClassifierFilter' {} a -> s {submitTimeAfter = a} :: DocumentClassifierFilter) Core.. Lens.mapping Core._Time

instance Core.Hashable DocumentClassifierFilter

instance Core.NFData DocumentClassifierFilter

instance Core.ToJSON DocumentClassifierFilter where
  toJSON DocumentClassifierFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("SubmitTimeBefore" Core..=)
              Core.<$> submitTimeBefore,
            ("SubmitTimeAfter" Core..=)
              Core.<$> submitTimeAfter
          ]
      )
