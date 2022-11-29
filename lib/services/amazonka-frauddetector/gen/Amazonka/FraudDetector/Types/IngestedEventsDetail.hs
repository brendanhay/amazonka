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
-- Module      : Amazonka.FraudDetector.Types.IngestedEventsDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.IngestedEventsDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types.IngestedEventsTimeWindow
import qualified Amazonka.Prelude as Prelude

-- | The details of the ingested event.
--
-- /See:/ 'newIngestedEventsDetail' smart constructor.
data IngestedEventsDetail = IngestedEventsDetail'
  { -- | The start and stop time of the ingested events.
    ingestedEventsTimeWindow :: IngestedEventsTimeWindow
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestedEventsDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestedEventsTimeWindow', 'ingestedEventsDetail_ingestedEventsTimeWindow' - The start and stop time of the ingested events.
newIngestedEventsDetail ::
  -- | 'ingestedEventsTimeWindow'
  IngestedEventsTimeWindow ->
  IngestedEventsDetail
newIngestedEventsDetail pIngestedEventsTimeWindow_ =
  IngestedEventsDetail'
    { ingestedEventsTimeWindow =
        pIngestedEventsTimeWindow_
    }

-- | The start and stop time of the ingested events.
ingestedEventsDetail_ingestedEventsTimeWindow :: Lens.Lens' IngestedEventsDetail IngestedEventsTimeWindow
ingestedEventsDetail_ingestedEventsTimeWindow = Lens.lens (\IngestedEventsDetail' {ingestedEventsTimeWindow} -> ingestedEventsTimeWindow) (\s@IngestedEventsDetail' {} a -> s {ingestedEventsTimeWindow = a} :: IngestedEventsDetail)

instance Core.FromJSON IngestedEventsDetail where
  parseJSON =
    Core.withObject
      "IngestedEventsDetail"
      ( \x ->
          IngestedEventsDetail'
            Prelude.<$> (x Core..: "ingestedEventsTimeWindow")
      )

instance Prelude.Hashable IngestedEventsDetail where
  hashWithSalt _salt IngestedEventsDetail' {..} =
    _salt
      `Prelude.hashWithSalt` ingestedEventsTimeWindow

instance Prelude.NFData IngestedEventsDetail where
  rnf IngestedEventsDetail' {..} =
    Prelude.rnf ingestedEventsTimeWindow

instance Core.ToJSON IngestedEventsDetail where
  toJSON IngestedEventsDetail' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ingestedEventsTimeWindow"
                  Core..= ingestedEventsTimeWindow
              )
          ]
      )
