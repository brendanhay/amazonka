{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information for filtering a list of document classifiers. You
-- can only specify one filtering parameter in a request. For more
-- information, see the operation.
--
-- /See:/ 'newDocumentClassifierFilter' smart constructor.
data DocumentClassifierFilter = DocumentClassifierFilter'
  { -- | Filters the list of classifiers based on status.
    status :: Prelude.Maybe ModelStatus,
    -- | Filters the list of classifiers based on the time that the classifier
    -- was submitted for processing. Returns only classifiers submitted before
    -- the specified time. Classifiers are returned in ascending order, oldest
    -- to newest.
    submitTimeBefore :: Prelude.Maybe Prelude.POSIX,
    -- | Filters the list of classifiers based on the time that the classifier
    -- was submitted for processing. Returns only classifiers submitted after
    -- the specified time. Classifiers are returned in descending order, newest
    -- to oldest.
    submitTimeAfter :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing
    }

-- | Filters the list of classifiers based on status.
documentClassifierFilter_status :: Lens.Lens' DocumentClassifierFilter (Prelude.Maybe ModelStatus)
documentClassifierFilter_status = Lens.lens (\DocumentClassifierFilter' {status} -> status) (\s@DocumentClassifierFilter' {} a -> s {status = a} :: DocumentClassifierFilter)

-- | Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted before
-- the specified time. Classifiers are returned in ascending order, oldest
-- to newest.
documentClassifierFilter_submitTimeBefore :: Lens.Lens' DocumentClassifierFilter (Prelude.Maybe Prelude.UTCTime)
documentClassifierFilter_submitTimeBefore = Lens.lens (\DocumentClassifierFilter' {submitTimeBefore} -> submitTimeBefore) (\s@DocumentClassifierFilter' {} a -> s {submitTimeBefore = a} :: DocumentClassifierFilter) Prelude.. Lens.mapping Prelude._Time

-- | Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted after
-- the specified time. Classifiers are returned in descending order, newest
-- to oldest.
documentClassifierFilter_submitTimeAfter :: Lens.Lens' DocumentClassifierFilter (Prelude.Maybe Prelude.UTCTime)
documentClassifierFilter_submitTimeAfter = Lens.lens (\DocumentClassifierFilter' {submitTimeAfter} -> submitTimeAfter) (\s@DocumentClassifierFilter' {} a -> s {submitTimeAfter = a} :: DocumentClassifierFilter) Prelude.. Lens.mapping Prelude._Time

instance Prelude.Hashable DocumentClassifierFilter

instance Prelude.NFData DocumentClassifierFilter

instance Prelude.ToJSON DocumentClassifierFilter where
  toJSON DocumentClassifierFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Status" Prelude..=) Prelude.<$> status,
            ("SubmitTimeBefore" Prelude..=)
              Prelude.<$> submitTimeBefore,
            ("SubmitTimeAfter" Prelude..=)
              Prelude.<$> submitTimeAfter
          ]
      )
