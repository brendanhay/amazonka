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
-- Module      : Network.AWS.AppFlow.Types.ExecutionDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.ExecutionDetails where

import Network.AWS.AppFlow.Types.ExecutionStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    mostRecentExecutionTime :: Prelude.Maybe Core.POSIX
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
executionDetails_mostRecentExecutionTime = Lens.lens (\ExecutionDetails' {mostRecentExecutionTime} -> mostRecentExecutionTime) (\s@ExecutionDetails' {} a -> s {mostRecentExecutionTime = a} :: ExecutionDetails) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ExecutionDetails where
  parseJSON =
    Core.withObject
      "ExecutionDetails"
      ( \x ->
          ExecutionDetails'
            Prelude.<$> (x Core..:? "mostRecentExecutionMessage")
            Prelude.<*> (x Core..:? "mostRecentExecutionStatus")
            Prelude.<*> (x Core..:? "mostRecentExecutionTime")
      )

instance Prelude.Hashable ExecutionDetails

instance Prelude.NFData ExecutionDetails
