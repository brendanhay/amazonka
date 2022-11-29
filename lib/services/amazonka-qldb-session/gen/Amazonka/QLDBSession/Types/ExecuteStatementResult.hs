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
-- Module      : Amazonka.QLDBSession.Types.ExecuteStatementResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.ExecuteStatementResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.IOUsage
import Amazonka.QLDBSession.Types.Page
import Amazonka.QLDBSession.Types.TimingInformation

-- | Contains the details of the executed statement.
--
-- /See:/ 'newExecuteStatementResult' smart constructor.
data ExecuteStatementResult = ExecuteStatementResult'
  { -- | Contains server-side performance information for the command.
    timingInformation :: Prelude.Maybe TimingInformation,
    -- | Contains metrics about the number of I\/O requests that were consumed.
    consumedIOs :: Prelude.Maybe IOUsage,
    -- | Contains the details of the first fetched page.
    firstPage :: Prelude.Maybe Page
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecuteStatementResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timingInformation', 'executeStatementResult_timingInformation' - Contains server-side performance information for the command.
--
-- 'consumedIOs', 'executeStatementResult_consumedIOs' - Contains metrics about the number of I\/O requests that were consumed.
--
-- 'firstPage', 'executeStatementResult_firstPage' - Contains the details of the first fetched page.
newExecuteStatementResult ::
  ExecuteStatementResult
newExecuteStatementResult =
  ExecuteStatementResult'
    { timingInformation =
        Prelude.Nothing,
      consumedIOs = Prelude.Nothing,
      firstPage = Prelude.Nothing
    }

-- | Contains server-side performance information for the command.
executeStatementResult_timingInformation :: Lens.Lens' ExecuteStatementResult (Prelude.Maybe TimingInformation)
executeStatementResult_timingInformation = Lens.lens (\ExecuteStatementResult' {timingInformation} -> timingInformation) (\s@ExecuteStatementResult' {} a -> s {timingInformation = a} :: ExecuteStatementResult)

-- | Contains metrics about the number of I\/O requests that were consumed.
executeStatementResult_consumedIOs :: Lens.Lens' ExecuteStatementResult (Prelude.Maybe IOUsage)
executeStatementResult_consumedIOs = Lens.lens (\ExecuteStatementResult' {consumedIOs} -> consumedIOs) (\s@ExecuteStatementResult' {} a -> s {consumedIOs = a} :: ExecuteStatementResult)

-- | Contains the details of the first fetched page.
executeStatementResult_firstPage :: Lens.Lens' ExecuteStatementResult (Prelude.Maybe Page)
executeStatementResult_firstPage = Lens.lens (\ExecuteStatementResult' {firstPage} -> firstPage) (\s@ExecuteStatementResult' {} a -> s {firstPage = a} :: ExecuteStatementResult)

instance Core.FromJSON ExecuteStatementResult where
  parseJSON =
    Core.withObject
      "ExecuteStatementResult"
      ( \x ->
          ExecuteStatementResult'
            Prelude.<$> (x Core..:? "TimingInformation")
            Prelude.<*> (x Core..:? "ConsumedIOs")
            Prelude.<*> (x Core..:? "FirstPage")
      )

instance Prelude.Hashable ExecuteStatementResult where
  hashWithSalt _salt ExecuteStatementResult' {..} =
    _salt `Prelude.hashWithSalt` timingInformation
      `Prelude.hashWithSalt` consumedIOs
      `Prelude.hashWithSalt` firstPage

instance Prelude.NFData ExecuteStatementResult where
  rnf ExecuteStatementResult' {..} =
    Prelude.rnf timingInformation
      `Prelude.seq` Prelude.rnf consumedIOs
      `Prelude.seq` Prelude.rnf firstPage
