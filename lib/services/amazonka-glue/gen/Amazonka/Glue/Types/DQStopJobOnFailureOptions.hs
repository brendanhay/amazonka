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
-- Module      : Amazonka.Glue.Types.DQStopJobOnFailureOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DQStopJobOnFailureOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DQStopJobOnFailureTiming
import qualified Amazonka.Prelude as Prelude

-- | Options to configure how your job will stop if your data quality
-- evaluation fails.
--
-- /See:/ 'newDQStopJobOnFailureOptions' smart constructor.
data DQStopJobOnFailureOptions = DQStopJobOnFailureOptions'
  { -- | When to stop job if your data quality evaluation fails. Options are
    -- Immediate or AfterDataLoad.
    stopJobOnFailureTiming :: Prelude.Maybe DQStopJobOnFailureTiming
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DQStopJobOnFailureOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stopJobOnFailureTiming', 'dQStopJobOnFailureOptions_stopJobOnFailureTiming' - When to stop job if your data quality evaluation fails. Options are
-- Immediate or AfterDataLoad.
newDQStopJobOnFailureOptions ::
  DQStopJobOnFailureOptions
newDQStopJobOnFailureOptions =
  DQStopJobOnFailureOptions'
    { stopJobOnFailureTiming =
        Prelude.Nothing
    }

-- | When to stop job if your data quality evaluation fails. Options are
-- Immediate or AfterDataLoad.
dQStopJobOnFailureOptions_stopJobOnFailureTiming :: Lens.Lens' DQStopJobOnFailureOptions (Prelude.Maybe DQStopJobOnFailureTiming)
dQStopJobOnFailureOptions_stopJobOnFailureTiming = Lens.lens (\DQStopJobOnFailureOptions' {stopJobOnFailureTiming} -> stopJobOnFailureTiming) (\s@DQStopJobOnFailureOptions' {} a -> s {stopJobOnFailureTiming = a} :: DQStopJobOnFailureOptions)

instance Data.FromJSON DQStopJobOnFailureOptions where
  parseJSON =
    Data.withObject
      "DQStopJobOnFailureOptions"
      ( \x ->
          DQStopJobOnFailureOptions'
            Prelude.<$> (x Data..:? "StopJobOnFailureTiming")
      )

instance Prelude.Hashable DQStopJobOnFailureOptions where
  hashWithSalt _salt DQStopJobOnFailureOptions' {..} =
    _salt `Prelude.hashWithSalt` stopJobOnFailureTiming

instance Prelude.NFData DQStopJobOnFailureOptions where
  rnf DQStopJobOnFailureOptions' {..} =
    Prelude.rnf stopJobOnFailureTiming

instance Data.ToJSON DQStopJobOnFailureOptions where
  toJSON DQStopJobOnFailureOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StopJobOnFailureTiming" Data..=)
              Prelude.<$> stopJobOnFailureTiming
          ]
      )
