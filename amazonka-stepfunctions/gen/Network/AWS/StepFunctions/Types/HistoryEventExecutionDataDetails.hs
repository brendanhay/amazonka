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
-- Module      : Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides details about input or output in an execution history event.
--
-- /See:/ 'newHistoryEventExecutionDataDetails' smart constructor.
data HistoryEventExecutionDataDetails = HistoryEventExecutionDataDetails'
  { -- | Indicates whether input or output was truncated in the response. Always
    -- @false@ for API calls.
    truncated :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HistoryEventExecutionDataDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truncated', 'historyEventExecutionDataDetails_truncated' - Indicates whether input or output was truncated in the response. Always
-- @false@ for API calls.
newHistoryEventExecutionDataDetails ::
  HistoryEventExecutionDataDetails
newHistoryEventExecutionDataDetails =
  HistoryEventExecutionDataDetails'
    { truncated =
        Core.Nothing
    }

-- | Indicates whether input or output was truncated in the response. Always
-- @false@ for API calls.
historyEventExecutionDataDetails_truncated :: Lens.Lens' HistoryEventExecutionDataDetails (Core.Maybe Core.Bool)
historyEventExecutionDataDetails_truncated = Lens.lens (\HistoryEventExecutionDataDetails' {truncated} -> truncated) (\s@HistoryEventExecutionDataDetails' {} a -> s {truncated = a} :: HistoryEventExecutionDataDetails)

instance
  Core.FromJSON
    HistoryEventExecutionDataDetails
  where
  parseJSON =
    Core.withObject
      "HistoryEventExecutionDataDetails"
      ( \x ->
          HistoryEventExecutionDataDetails'
            Core.<$> (x Core..:? "truncated")
      )

instance
  Core.Hashable
    HistoryEventExecutionDataDetails

instance Core.NFData HistoryEventExecutionDataDetails
