{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about an activity that successfully terminated during
-- an execution.
--
-- /See:/ 'newActivitySucceededEventDetails' smart constructor.
data ActivitySucceededEventDetails = ActivitySucceededEventDetails'
  { -- | The JSON data output by the activity task. Length constraints apply to
    -- the payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ActivitySucceededEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'activitySucceededEventDetails_output' - The JSON data output by the activity task. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'outputDetails', 'activitySucceededEventDetails_outputDetails' - Contains details about the output of an execution history event.
newActivitySucceededEventDetails ::
  ActivitySucceededEventDetails
newActivitySucceededEventDetails =
  ActivitySucceededEventDetails'
    { output =
        Prelude.Nothing,
      outputDetails = Prelude.Nothing
    }

-- | The JSON data output by the activity task. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
activitySucceededEventDetails_output :: Lens.Lens' ActivitySucceededEventDetails (Prelude.Maybe Prelude.Text)
activitySucceededEventDetails_output = Lens.lens (\ActivitySucceededEventDetails' {output} -> output) (\s@ActivitySucceededEventDetails' {} a -> s {output = a} :: ActivitySucceededEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | Contains details about the output of an execution history event.
activitySucceededEventDetails_outputDetails :: Lens.Lens' ActivitySucceededEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
activitySucceededEventDetails_outputDetails = Lens.lens (\ActivitySucceededEventDetails' {outputDetails} -> outputDetails) (\s@ActivitySucceededEventDetails' {} a -> s {outputDetails = a} :: ActivitySucceededEventDetails)

instance
  Prelude.FromJSON
    ActivitySucceededEventDetails
  where
  parseJSON =
    Prelude.withObject
      "ActivitySucceededEventDetails"
      ( \x ->
          ActivitySucceededEventDetails'
            Prelude.<$> (x Prelude..:? "output")
            Prelude.<*> (x Prelude..:? "outputDetails")
      )

instance
  Prelude.Hashable
    ActivitySucceededEventDetails

instance Prelude.NFData ActivitySucceededEventDetails
