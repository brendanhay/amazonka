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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerFilter where

import Amazonka.Comprehend.Types.ModelStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information for filtering a list of entity recognizers. You can
-- only specify one filtering parameter in a request. For more information,
-- see the operation.\/>
--
-- /See:/ 'newEntityRecognizerFilter' smart constructor.
data EntityRecognizerFilter = EntityRecognizerFilter'
  { -- | The name that you assigned the entity recognizer.
    recognizerName :: Prelude.Maybe Prelude.Text,
    -- | The status of an entity recognizer.
    status :: Prelude.Maybe ModelStatus,
    -- | Filters the list of entities based on the time that the list was
    -- submitted for processing. Returns only jobs submitted before the
    -- specified time. Jobs are returned in descending order, newest to oldest.
    submitTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Filters the list of entities based on the time that the list was
    -- submitted for processing. Returns only jobs submitted after the
    -- specified time. Jobs are returned in ascending order, oldest to newest.
    submitTimeAfter :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recognizerName', 'entityRecognizerFilter_recognizerName' - The name that you assigned the entity recognizer.
--
-- 'status', 'entityRecognizerFilter_status' - The status of an entity recognizer.
--
-- 'submitTimeBefore', 'entityRecognizerFilter_submitTimeBefore' - Filters the list of entities based on the time that the list was
-- submitted for processing. Returns only jobs submitted before the
-- specified time. Jobs are returned in descending order, newest to oldest.
--
-- 'submitTimeAfter', 'entityRecognizerFilter_submitTimeAfter' - Filters the list of entities based on the time that the list was
-- submitted for processing. Returns only jobs submitted after the
-- specified time. Jobs are returned in ascending order, oldest to newest.
newEntityRecognizerFilter ::
  EntityRecognizerFilter
newEntityRecognizerFilter =
  EntityRecognizerFilter'
    { recognizerName =
        Prelude.Nothing,
      status = Prelude.Nothing,
      submitTimeBefore = Prelude.Nothing,
      submitTimeAfter = Prelude.Nothing
    }

-- | The name that you assigned the entity recognizer.
entityRecognizerFilter_recognizerName :: Lens.Lens' EntityRecognizerFilter (Prelude.Maybe Prelude.Text)
entityRecognizerFilter_recognizerName = Lens.lens (\EntityRecognizerFilter' {recognizerName} -> recognizerName) (\s@EntityRecognizerFilter' {} a -> s {recognizerName = a} :: EntityRecognizerFilter)

-- | The status of an entity recognizer.
entityRecognizerFilter_status :: Lens.Lens' EntityRecognizerFilter (Prelude.Maybe ModelStatus)
entityRecognizerFilter_status = Lens.lens (\EntityRecognizerFilter' {status} -> status) (\s@EntityRecognizerFilter' {} a -> s {status = a} :: EntityRecognizerFilter)

-- | Filters the list of entities based on the time that the list was
-- submitted for processing. Returns only jobs submitted before the
-- specified time. Jobs are returned in descending order, newest to oldest.
entityRecognizerFilter_submitTimeBefore :: Lens.Lens' EntityRecognizerFilter (Prelude.Maybe Prelude.UTCTime)
entityRecognizerFilter_submitTimeBefore = Lens.lens (\EntityRecognizerFilter' {submitTimeBefore} -> submitTimeBefore) (\s@EntityRecognizerFilter' {} a -> s {submitTimeBefore = a} :: EntityRecognizerFilter) Prelude.. Lens.mapping Data._Time

-- | Filters the list of entities based on the time that the list was
-- submitted for processing. Returns only jobs submitted after the
-- specified time. Jobs are returned in ascending order, oldest to newest.
entityRecognizerFilter_submitTimeAfter :: Lens.Lens' EntityRecognizerFilter (Prelude.Maybe Prelude.UTCTime)
entityRecognizerFilter_submitTimeAfter = Lens.lens (\EntityRecognizerFilter' {submitTimeAfter} -> submitTimeAfter) (\s@EntityRecognizerFilter' {} a -> s {submitTimeAfter = a} :: EntityRecognizerFilter) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable EntityRecognizerFilter where
  hashWithSalt _salt EntityRecognizerFilter' {..} =
    _salt `Prelude.hashWithSalt` recognizerName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` submitTimeBefore
      `Prelude.hashWithSalt` submitTimeAfter

instance Prelude.NFData EntityRecognizerFilter where
  rnf EntityRecognizerFilter' {..} =
    Prelude.rnf recognizerName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf submitTimeBefore
      `Prelude.seq` Prelude.rnf submitTimeAfter

instance Data.ToJSON EntityRecognizerFilter where
  toJSON EntityRecognizerFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RecognizerName" Data..=)
              Prelude.<$> recognizerName,
            ("Status" Data..=) Prelude.<$> status,
            ("SubmitTimeBefore" Data..=)
              Prelude.<$> submitTimeBefore,
            ("SubmitTimeAfter" Data..=)
              Prelude.<$> submitTimeAfter
          ]
      )
