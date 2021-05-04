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
-- Module      : Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides details about input or output in an execution history event.
--
-- /See:/ 'newHistoryEventExecutionDataDetails' smart constructor.
data HistoryEventExecutionDataDetails = HistoryEventExecutionDataDetails'
  { -- | Indicates whether input or output was truncated in the response. Always
    -- @false@ for API calls.
    truncated :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Indicates whether input or output was truncated in the response. Always
-- @false@ for API calls.
historyEventExecutionDataDetails_truncated :: Lens.Lens' HistoryEventExecutionDataDetails (Prelude.Maybe Prelude.Bool)
historyEventExecutionDataDetails_truncated = Lens.lens (\HistoryEventExecutionDataDetails' {truncated} -> truncated) (\s@HistoryEventExecutionDataDetails' {} a -> s {truncated = a} :: HistoryEventExecutionDataDetails)

instance
  Prelude.FromJSON
    HistoryEventExecutionDataDetails
  where
  parseJSON =
    Prelude.withObject
      "HistoryEventExecutionDataDetails"
      ( \x ->
          HistoryEventExecutionDataDetails'
            Prelude.<$> (x Prelude..:? "truncated")
      )

instance
  Prelude.Hashable
    HistoryEventExecutionDataDetails

instance
  Prelude.NFData
    HistoryEventExecutionDataDetails
