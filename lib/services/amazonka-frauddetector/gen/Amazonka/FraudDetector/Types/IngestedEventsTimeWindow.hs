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
-- Module      : Amazonka.FraudDetector.Types.IngestedEventsTimeWindow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.IngestedEventsTimeWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The start and stop time of the ingested events.
--
-- /See:/ 'newIngestedEventsTimeWindow' smart constructor.
data IngestedEventsTimeWindow = IngestedEventsTimeWindow'
  { -- | Timestamp of the first ingensted event.
    startTime :: Prelude.Text,
    -- | Timestamp of the final ingested event.
    endTime :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestedEventsTimeWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'ingestedEventsTimeWindow_startTime' - Timestamp of the first ingensted event.
--
-- 'endTime', 'ingestedEventsTimeWindow_endTime' - Timestamp of the final ingested event.
newIngestedEventsTimeWindow ::
  -- | 'startTime'
  Prelude.Text ->
  -- | 'endTime'
  Prelude.Text ->
  IngestedEventsTimeWindow
newIngestedEventsTimeWindow pStartTime_ pEndTime_ =
  IngestedEventsTimeWindow'
    { startTime = pStartTime_,
      endTime = pEndTime_
    }

-- | Timestamp of the first ingensted event.
ingestedEventsTimeWindow_startTime :: Lens.Lens' IngestedEventsTimeWindow Prelude.Text
ingestedEventsTimeWindow_startTime = Lens.lens (\IngestedEventsTimeWindow' {startTime} -> startTime) (\s@IngestedEventsTimeWindow' {} a -> s {startTime = a} :: IngestedEventsTimeWindow)

-- | Timestamp of the final ingested event.
ingestedEventsTimeWindow_endTime :: Lens.Lens' IngestedEventsTimeWindow Prelude.Text
ingestedEventsTimeWindow_endTime = Lens.lens (\IngestedEventsTimeWindow' {endTime} -> endTime) (\s@IngestedEventsTimeWindow' {} a -> s {endTime = a} :: IngestedEventsTimeWindow)

instance Core.FromJSON IngestedEventsTimeWindow where
  parseJSON =
    Core.withObject
      "IngestedEventsTimeWindow"
      ( \x ->
          IngestedEventsTimeWindow'
            Prelude.<$> (x Core..: "startTime")
            Prelude.<*> (x Core..: "endTime")
      )

instance Prelude.Hashable IngestedEventsTimeWindow where
  hashWithSalt _salt IngestedEventsTimeWindow' {..} =
    _salt `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData IngestedEventsTimeWindow where
  rnf IngestedEventsTimeWindow' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Core.ToJSON IngestedEventsTimeWindow where
  toJSON IngestedEventsTimeWindow' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("startTime" Core..= startTime),
            Prelude.Just ("endTime" Core..= endTime)
          ]
      )
