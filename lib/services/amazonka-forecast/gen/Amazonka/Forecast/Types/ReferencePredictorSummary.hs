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
-- Module      : Amazonka.Forecast.Types.ReferencePredictorSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ReferencePredictorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.State
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the reference predictor used when retraining or
-- upgrading a predictor.
--
-- /See:/ 'newReferencePredictorSummary' smart constructor.
data ReferencePredictorSummary = ReferencePredictorSummary'
  { -- | The ARN of the reference predictor.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Whether the reference predictor is @Active@ or @Deleted@.
    state :: Prelude.Maybe State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferencePredictorSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'referencePredictorSummary_arn' - The ARN of the reference predictor.
--
-- 'state', 'referencePredictorSummary_state' - Whether the reference predictor is @Active@ or @Deleted@.
newReferencePredictorSummary ::
  ReferencePredictorSummary
newReferencePredictorSummary =
  ReferencePredictorSummary'
    { arn = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The ARN of the reference predictor.
referencePredictorSummary_arn :: Lens.Lens' ReferencePredictorSummary (Prelude.Maybe Prelude.Text)
referencePredictorSummary_arn = Lens.lens (\ReferencePredictorSummary' {arn} -> arn) (\s@ReferencePredictorSummary' {} a -> s {arn = a} :: ReferencePredictorSummary)

-- | Whether the reference predictor is @Active@ or @Deleted@.
referencePredictorSummary_state :: Lens.Lens' ReferencePredictorSummary (Prelude.Maybe State)
referencePredictorSummary_state = Lens.lens (\ReferencePredictorSummary' {state} -> state) (\s@ReferencePredictorSummary' {} a -> s {state = a} :: ReferencePredictorSummary)

instance Data.FromJSON ReferencePredictorSummary where
  parseJSON =
    Data.withObject
      "ReferencePredictorSummary"
      ( \x ->
          ReferencePredictorSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable ReferencePredictorSummary where
  hashWithSalt _salt ReferencePredictorSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state

instance Prelude.NFData ReferencePredictorSummary where
  rnf ReferencePredictorSummary' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf state
