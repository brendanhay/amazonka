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
-- Module      : Amazonka.StepFunctions.Types.ActivitySucceededEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ActivitySucceededEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an activity that successfully terminated during
-- an execution.
--
-- /See:/ 'newActivitySucceededEventDetails' smart constructor.
data ActivitySucceededEventDetails = ActivitySucceededEventDetails'
  { -- | Contains details about the output of an execution history event.
    outputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The JSON data output by the activity task. Length constraints apply to
    -- the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivitySucceededEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputDetails', 'activitySucceededEventDetails_outputDetails' - Contains details about the output of an execution history event.
--
-- 'output', 'activitySucceededEventDetails_output' - The JSON data output by the activity task. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
newActivitySucceededEventDetails ::
  ActivitySucceededEventDetails
newActivitySucceededEventDetails =
  ActivitySucceededEventDetails'
    { outputDetails =
        Prelude.Nothing,
      output = Prelude.Nothing
    }

-- | Contains details about the output of an execution history event.
activitySucceededEventDetails_outputDetails :: Lens.Lens' ActivitySucceededEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
activitySucceededEventDetails_outputDetails = Lens.lens (\ActivitySucceededEventDetails' {outputDetails} -> outputDetails) (\s@ActivitySucceededEventDetails' {} a -> s {outputDetails = a} :: ActivitySucceededEventDetails)

-- | The JSON data output by the activity task. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
activitySucceededEventDetails_output :: Lens.Lens' ActivitySucceededEventDetails (Prelude.Maybe Prelude.Text)
activitySucceededEventDetails_output = Lens.lens (\ActivitySucceededEventDetails' {output} -> output) (\s@ActivitySucceededEventDetails' {} a -> s {output = a} :: ActivitySucceededEventDetails) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ActivitySucceededEventDetails where
  parseJSON =
    Data.withObject
      "ActivitySucceededEventDetails"
      ( \x ->
          ActivitySucceededEventDetails'
            Prelude.<$> (x Data..:? "outputDetails")
            Prelude.<*> (x Data..:? "output")
      )

instance
  Prelude.Hashable
    ActivitySucceededEventDetails
  where
  hashWithSalt _salt ActivitySucceededEventDetails' {..} =
    _salt `Prelude.hashWithSalt` outputDetails
      `Prelude.hashWithSalt` output

instance Prelude.NFData ActivitySucceededEventDetails where
  rnf ActivitySucceededEventDetails' {..} =
    Prelude.rnf outputDetails
      `Prelude.seq` Prelude.rnf output
