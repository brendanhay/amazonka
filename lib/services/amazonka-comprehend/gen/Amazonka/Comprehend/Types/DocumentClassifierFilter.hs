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
-- Module      : Amazonka.Comprehend.Types.DocumentClassifierFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClassifierFilter where

import Amazonka.Comprehend.Types.ModelStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information for filtering a list of document classifiers. You
-- can only specify one filtering parameter in a request. For more
-- information, see the operation.
--
-- /See:/ 'newDocumentClassifierFilter' smart constructor.
data DocumentClassifierFilter = DocumentClassifierFilter'
  { -- | The name that you assigned to the document classifier
    documentClassifierName :: Prelude.Maybe Prelude.Text,
    -- | Filters the list of classifiers based on status.
    status :: Prelude.Maybe ModelStatus,
    -- | Filters the list of classifiers based on the time that the classifier
    -- was submitted for processing. Returns only classifiers submitted after
    -- the specified time. Classifiers are returned in descending order, newest
    -- to oldest.
    submitTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Filters the list of classifiers based on the time that the classifier
    -- was submitted for processing. Returns only classifiers submitted before
    -- the specified time. Classifiers are returned in ascending order, oldest
    -- to newest.
    submitTimeBefore :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentClassifierFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassifierName', 'documentClassifierFilter_documentClassifierName' - The name that you assigned to the document classifier
--
-- 'status', 'documentClassifierFilter_status' - Filters the list of classifiers based on status.
--
-- 'submitTimeAfter', 'documentClassifierFilter_submitTimeAfter' - Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted after
-- the specified time. Classifiers are returned in descending order, newest
-- to oldest.
--
-- 'submitTimeBefore', 'documentClassifierFilter_submitTimeBefore' - Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted before
-- the specified time. Classifiers are returned in ascending order, oldest
-- to newest.
newDocumentClassifierFilter ::
  DocumentClassifierFilter
newDocumentClassifierFilter =
  DocumentClassifierFilter'
    { documentClassifierName =
        Prelude.Nothing,
      status = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing
    }

-- | The name that you assigned to the document classifier
documentClassifierFilter_documentClassifierName :: Lens.Lens' DocumentClassifierFilter (Prelude.Maybe Prelude.Text)
documentClassifierFilter_documentClassifierName = Lens.lens (\DocumentClassifierFilter' {documentClassifierName} -> documentClassifierName) (\s@DocumentClassifierFilter' {} a -> s {documentClassifierName = a} :: DocumentClassifierFilter)

-- | Filters the list of classifiers based on status.
documentClassifierFilter_status :: Lens.Lens' DocumentClassifierFilter (Prelude.Maybe ModelStatus)
documentClassifierFilter_status = Lens.lens (\DocumentClassifierFilter' {status} -> status) (\s@DocumentClassifierFilter' {} a -> s {status = a} :: DocumentClassifierFilter)

-- | Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted after
-- the specified time. Classifiers are returned in descending order, newest
-- to oldest.
documentClassifierFilter_submitTimeAfter :: Lens.Lens' DocumentClassifierFilter (Prelude.Maybe Prelude.UTCTime)
documentClassifierFilter_submitTimeAfter = Lens.lens (\DocumentClassifierFilter' {submitTimeAfter} -> submitTimeAfter) (\s@DocumentClassifierFilter' {} a -> s {submitTimeAfter = a} :: DocumentClassifierFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of classifiers based on the time that the classifier
-- was submitted for processing. Returns only classifiers submitted before
-- the specified time. Classifiers are returned in ascending order, oldest
-- to newest.
documentClassifierFilter_submitTimeBefore :: Lens.Lens' DocumentClassifierFilter (Prelude.Maybe Prelude.UTCTime)
documentClassifierFilter_submitTimeBefore = Lens.lens (\DocumentClassifierFilter' {submitTimeBefore} -> submitTimeBefore) (\s@DocumentClassifierFilter' {} a -> s {submitTimeBefore = a} :: DocumentClassifierFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable DocumentClassifierFilter where
  hashWithSalt _salt DocumentClassifierFilter' {..} =
    _salt
      `Prelude.hashWithSalt` documentClassifierName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` submitTimeAfter
      `Prelude.hashWithSalt` submitTimeBefore

instance Prelude.NFData DocumentClassifierFilter where
  rnf DocumentClassifierFilter' {..} =
    Prelude.rnf documentClassifierName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf submitTimeAfter
      `Prelude.seq` Prelude.rnf submitTimeBefore

instance Data.ToJSON DocumentClassifierFilter where
  toJSON DocumentClassifierFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentClassifierName" Data..=)
              Prelude.<$> documentClassifierName,
            ("Status" Data..=) Prelude.<$> status,
            ("SubmitTimeAfter" Data..=)
              Prelude.<$> submitTimeAfter,
            ("SubmitTimeBefore" Data..=)
              Prelude.<$> submitTimeBefore
          ]
      )
