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
-- Module      : Amazonka.Kendra.Types.FaqStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FaqStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides statistical information about the FAQ questions and answers
-- contained in an index.
--
-- /See:/ 'newFaqStatistics' smart constructor.
data FaqStatistics = FaqStatistics'
  { -- | The total number of FAQ questions and answers contained in the index.
    indexedQuestionAnswersCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaqStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexedQuestionAnswersCount', 'faqStatistics_indexedQuestionAnswersCount' - The total number of FAQ questions and answers contained in the index.
newFaqStatistics ::
  -- | 'indexedQuestionAnswersCount'
  Prelude.Natural ->
  FaqStatistics
newFaqStatistics pIndexedQuestionAnswersCount_ =
  FaqStatistics'
    { indexedQuestionAnswersCount =
        pIndexedQuestionAnswersCount_
    }

-- | The total number of FAQ questions and answers contained in the index.
faqStatistics_indexedQuestionAnswersCount :: Lens.Lens' FaqStatistics Prelude.Natural
faqStatistics_indexedQuestionAnswersCount = Lens.lens (\FaqStatistics' {indexedQuestionAnswersCount} -> indexedQuestionAnswersCount) (\s@FaqStatistics' {} a -> s {indexedQuestionAnswersCount = a} :: FaqStatistics)

instance Data.FromJSON FaqStatistics where
  parseJSON =
    Data.withObject
      "FaqStatistics"
      ( \x ->
          FaqStatistics'
            Prelude.<$> (x Data..: "IndexedQuestionAnswersCount")
      )

instance Prelude.Hashable FaqStatistics where
  hashWithSalt _salt FaqStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` indexedQuestionAnswersCount

instance Prelude.NFData FaqStatistics where
  rnf FaqStatistics' {..} =
    Prelude.rnf indexedQuestionAnswersCount
