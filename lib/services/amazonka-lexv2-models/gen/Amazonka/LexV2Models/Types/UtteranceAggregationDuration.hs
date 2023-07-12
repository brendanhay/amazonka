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
-- Module      : Amazonka.LexV2Models.Types.UtteranceAggregationDuration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.UtteranceAggregationDuration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.RelativeAggregationDuration
import qualified Amazonka.Prelude as Prelude

-- | Provides parameters for setting the time window and duration for
-- aggregating utterance data.
--
-- /See:/ 'newUtteranceAggregationDuration' smart constructor.
data UtteranceAggregationDuration = UtteranceAggregationDuration'
  { -- | The desired time window for aggregating utterances.
    relativeAggregationDuration :: RelativeAggregationDuration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UtteranceAggregationDuration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relativeAggregationDuration', 'utteranceAggregationDuration_relativeAggregationDuration' - The desired time window for aggregating utterances.
newUtteranceAggregationDuration ::
  -- | 'relativeAggregationDuration'
  RelativeAggregationDuration ->
  UtteranceAggregationDuration
newUtteranceAggregationDuration
  pRelativeAggregationDuration_ =
    UtteranceAggregationDuration'
      { relativeAggregationDuration =
          pRelativeAggregationDuration_
      }

-- | The desired time window for aggregating utterances.
utteranceAggregationDuration_relativeAggregationDuration :: Lens.Lens' UtteranceAggregationDuration RelativeAggregationDuration
utteranceAggregationDuration_relativeAggregationDuration = Lens.lens (\UtteranceAggregationDuration' {relativeAggregationDuration} -> relativeAggregationDuration) (\s@UtteranceAggregationDuration' {} a -> s {relativeAggregationDuration = a} :: UtteranceAggregationDuration)

instance Data.FromJSON UtteranceAggregationDuration where
  parseJSON =
    Data.withObject
      "UtteranceAggregationDuration"
      ( \x ->
          UtteranceAggregationDuration'
            Prelude.<$> (x Data..: "relativeAggregationDuration")
      )

instance
  Prelude.Hashable
    UtteranceAggregationDuration
  where
  hashWithSalt _salt UtteranceAggregationDuration' {..} =
    _salt
      `Prelude.hashWithSalt` relativeAggregationDuration

instance Prelude.NFData UtteranceAggregationDuration where
  rnf UtteranceAggregationDuration' {..} =
    Prelude.rnf relativeAggregationDuration

instance Data.ToJSON UtteranceAggregationDuration where
  toJSON UtteranceAggregationDuration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "relativeAggregationDuration"
                  Data..= relativeAggregationDuration
              )
          ]
      )
