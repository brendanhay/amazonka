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
-- Module      : Amazonka.AppFlow.Types.ExecutionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ExecutionDetails where

import Amazonka.AppFlow.Types.ExecutionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the details of the flow run, including the timestamp, status,
-- and message.
--
-- /See:/ 'newExecutionDetails' smart constructor.
data ExecutionDetails = ExecutionDetails'
  { -- | Describes the details of the most recent flow run.
    mostRecentExecutionMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the status of the most recent flow run.
    mostRecentExecutionStatus :: Prelude.Maybe ExecutionStatus,
    -- | Specifies the time of the most recent flow run.
    mostRecentExecutionTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mostRecentExecutionMessage', 'executionDetails_mostRecentExecutionMessage' - Describes the details of the most recent flow run.
--
-- 'mostRecentExecutionStatus', 'executionDetails_mostRecentExecutionStatus' - Specifies the status of the most recent flow run.
--
-- 'mostRecentExecutionTime', 'executionDetails_mostRecentExecutionTime' - Specifies the time of the most recent flow run.
newExecutionDetails ::
  ExecutionDetails
newExecutionDetails =
  ExecutionDetails'
    { mostRecentExecutionMessage =
        Prelude.Nothing,
      mostRecentExecutionStatus = Prelude.Nothing,
      mostRecentExecutionTime = Prelude.Nothing
    }

-- | Describes the details of the most recent flow run.
executionDetails_mostRecentExecutionMessage :: Lens.Lens' ExecutionDetails (Prelude.Maybe Prelude.Text)
executionDetails_mostRecentExecutionMessage = Lens.lens (\ExecutionDetails' {mostRecentExecutionMessage} -> mostRecentExecutionMessage) (\s@ExecutionDetails' {} a -> s {mostRecentExecutionMessage = a} :: ExecutionDetails)

-- | Specifies the status of the most recent flow run.
executionDetails_mostRecentExecutionStatus :: Lens.Lens' ExecutionDetails (Prelude.Maybe ExecutionStatus)
executionDetails_mostRecentExecutionStatus = Lens.lens (\ExecutionDetails' {mostRecentExecutionStatus} -> mostRecentExecutionStatus) (\s@ExecutionDetails' {} a -> s {mostRecentExecutionStatus = a} :: ExecutionDetails)

-- | Specifies the time of the most recent flow run.
executionDetails_mostRecentExecutionTime :: Lens.Lens' ExecutionDetails (Prelude.Maybe Prelude.UTCTime)
executionDetails_mostRecentExecutionTime = Lens.lens (\ExecutionDetails' {mostRecentExecutionTime} -> mostRecentExecutionTime) (\s@ExecutionDetails' {} a -> s {mostRecentExecutionTime = a} :: ExecutionDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ExecutionDetails where
  parseJSON =
    Data.withObject
      "ExecutionDetails"
      ( \x ->
          ExecutionDetails'
            Prelude.<$> (x Data..:? "mostRecentExecutionMessage")
            Prelude.<*> (x Data..:? "mostRecentExecutionStatus")
            Prelude.<*> (x Data..:? "mostRecentExecutionTime")
      )

instance Prelude.Hashable ExecutionDetails where
  hashWithSalt _salt ExecutionDetails' {..} =
    _salt
      `Prelude.hashWithSalt` mostRecentExecutionMessage
      `Prelude.hashWithSalt` mostRecentExecutionStatus
      `Prelude.hashWithSalt` mostRecentExecutionTime

instance Prelude.NFData ExecutionDetails where
  rnf ExecutionDetails' {..} =
    Prelude.rnf mostRecentExecutionMessage
      `Prelude.seq` Prelude.rnf mostRecentExecutionStatus
      `Prelude.seq` Prelude.rnf mostRecentExecutionTime
