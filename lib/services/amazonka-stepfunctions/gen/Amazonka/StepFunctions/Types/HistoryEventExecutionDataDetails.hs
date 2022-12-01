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
-- Module      : Amazonka.StepFunctions.Types.HistoryEventExecutionDataDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.HistoryEventExecutionDataDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides details about input or output in an execution history event.
--
-- /See:/ 'newHistoryEventExecutionDataDetails' smart constructor.
data HistoryEventExecutionDataDetails = HistoryEventExecutionDataDetails'
  { -- | Indicates whether input or output was truncated in the response. Always
    -- @false@ for API calls.
    truncated :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.FromJSON
    HistoryEventExecutionDataDetails
  where
  parseJSON =
    Core.withObject
      "HistoryEventExecutionDataDetails"
      ( \x ->
          HistoryEventExecutionDataDetails'
            Prelude.<$> (x Core..:? "truncated")
      )

instance
  Prelude.Hashable
    HistoryEventExecutionDataDetails
  where
  hashWithSalt
    _salt
    HistoryEventExecutionDataDetails' {..} =
      _salt `Prelude.hashWithSalt` truncated

instance
  Prelude.NFData
    HistoryEventExecutionDataDetails
  where
  rnf HistoryEventExecutionDataDetails' {..} =
    Prelude.rnf truncated
