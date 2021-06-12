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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerFilter where

import Network.AWS.Comprehend.Types.ModelStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information for filtering a list of entity recognizers. You can
-- only specify one filtering parameter in a request. For more information,
-- see the operation.\/>
--
-- /See:/ 'newEntityRecognizerFilter' smart constructor.
data EntityRecognizerFilter = EntityRecognizerFilter'
  { -- | The status of an entity recognizer.
    status :: Core.Maybe ModelStatus,
    -- | Filters the list of entities based on the time that the list was
    -- submitted for processing. Returns only jobs submitted before the
    -- specified time. Jobs are returned in descending order, newest to oldest.
    submitTimeBefore :: Core.Maybe Core.POSIX,
    -- | Filters the list of entities based on the time that the list was
    -- submitted for processing. Returns only jobs submitted after the
    -- specified time. Jobs are returned in ascending order, oldest to newest.
    submitTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EntityRecognizerFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
    { status = Core.Nothing,
      submitTimeBefore = Core.Nothing,
      submitTimeAfter = Core.Nothing
    }

-- | The status of an entity recognizer.
entityRecognizerFilter_status :: Lens.Lens' EntityRecognizerFilter (Core.Maybe ModelStatus)
entityRecognizerFilter_status = Lens.lens (\EntityRecognizerFilter' {status} -> status) (\s@EntityRecognizerFilter' {} a -> s {status = a} :: EntityRecognizerFilter)

-- | Filters the list of entities based on the time that the list was
-- submitted for processing. Returns only jobs submitted before the
-- specified time. Jobs are returned in descending order, newest to oldest.
entityRecognizerFilter_submitTimeBefore :: Lens.Lens' EntityRecognizerFilter (Core.Maybe Core.UTCTime)
entityRecognizerFilter_submitTimeBefore = Lens.lens (\EntityRecognizerFilter' {submitTimeBefore} -> submitTimeBefore) (\s@EntityRecognizerFilter' {} a -> s {submitTimeBefore = a} :: EntityRecognizerFilter) Core.. Lens.mapping Core._Time

-- | Filters the list of entities based on the time that the list was
-- submitted for processing. Returns only jobs submitted after the
-- specified time. Jobs are returned in ascending order, oldest to newest.
entityRecognizerFilter_submitTimeAfter :: Lens.Lens' EntityRecognizerFilter (Core.Maybe Core.UTCTime)
entityRecognizerFilter_submitTimeAfter = Lens.lens (\EntityRecognizerFilter' {submitTimeAfter} -> submitTimeAfter) (\s@EntityRecognizerFilter' {} a -> s {submitTimeAfter = a} :: EntityRecognizerFilter) Core.. Lens.mapping Core._Time

instance Core.Hashable EntityRecognizerFilter

instance Core.NFData EntityRecognizerFilter

instance Core.ToJSON EntityRecognizerFilter where
  toJSON EntityRecognizerFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("SubmitTimeBefore" Core..=)
              Core.<$> submitTimeBefore,
            ("SubmitTimeAfter" Core..=)
              Core.<$> submitTimeAfter
          ]
      )
