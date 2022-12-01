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
-- Module      : Amazonka.LexV2Models.Types.LexTranscriptFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.LexTranscriptFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.DateRangeFilter
import qualified Amazonka.Prelude as Prelude

-- | The object that contains transcript filter details that are associated
-- with a bot recommendation.
--
-- /See:/ 'newLexTranscriptFilter' smart constructor.
data LexTranscriptFilter = LexTranscriptFilter'
  { -- | The object that contains a date range filter that will be applied to the
    -- transcript. Specify this object if you want Amazon Lex to only read the
    -- files that are within the date range.
    dateRangeFilter :: Prelude.Maybe DateRangeFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LexTranscriptFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateRangeFilter', 'lexTranscriptFilter_dateRangeFilter' - The object that contains a date range filter that will be applied to the
-- transcript. Specify this object if you want Amazon Lex to only read the
-- files that are within the date range.
newLexTranscriptFilter ::
  LexTranscriptFilter
newLexTranscriptFilter =
  LexTranscriptFilter'
    { dateRangeFilter =
        Prelude.Nothing
    }

-- | The object that contains a date range filter that will be applied to the
-- transcript. Specify this object if you want Amazon Lex to only read the
-- files that are within the date range.
lexTranscriptFilter_dateRangeFilter :: Lens.Lens' LexTranscriptFilter (Prelude.Maybe DateRangeFilter)
lexTranscriptFilter_dateRangeFilter = Lens.lens (\LexTranscriptFilter' {dateRangeFilter} -> dateRangeFilter) (\s@LexTranscriptFilter' {} a -> s {dateRangeFilter = a} :: LexTranscriptFilter)

instance Core.FromJSON LexTranscriptFilter where
  parseJSON =
    Core.withObject
      "LexTranscriptFilter"
      ( \x ->
          LexTranscriptFilter'
            Prelude.<$> (x Core..:? "dateRangeFilter")
      )

instance Prelude.Hashable LexTranscriptFilter where
  hashWithSalt _salt LexTranscriptFilter' {..} =
    _salt `Prelude.hashWithSalt` dateRangeFilter

instance Prelude.NFData LexTranscriptFilter where
  rnf LexTranscriptFilter' {..} =
    Prelude.rnf dateRangeFilter

instance Core.ToJSON LexTranscriptFilter where
  toJSON LexTranscriptFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("dateRangeFilter" Core..=)
              Prelude.<$> dateRangeFilter
          ]
      )
