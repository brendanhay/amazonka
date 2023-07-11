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
-- Module      : Amazonka.Rum.Types.DataStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.DataStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rum.Types.CwLog

-- | A structure that contains information about whether this app monitor
-- stores a copy of the telemetry data that RUM collects using CloudWatch
-- Logs.
--
-- /See:/ 'newDataStorage' smart constructor.
data DataStorage = DataStorage'
  { -- | A structure that contains the information about whether the app monitor
    -- stores copies of the data that RUM collects in CloudWatch Logs. If it
    -- does, this structure also contains the name of the log group.
    cwLog :: Prelude.Maybe CwLog
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cwLog', 'dataStorage_cwLog' - A structure that contains the information about whether the app monitor
-- stores copies of the data that RUM collects in CloudWatch Logs. If it
-- does, this structure also contains the name of the log group.
newDataStorage ::
  DataStorage
newDataStorage =
  DataStorage' {cwLog = Prelude.Nothing}

-- | A structure that contains the information about whether the app monitor
-- stores copies of the data that RUM collects in CloudWatch Logs. If it
-- does, this structure also contains the name of the log group.
dataStorage_cwLog :: Lens.Lens' DataStorage (Prelude.Maybe CwLog)
dataStorage_cwLog = Lens.lens (\DataStorage' {cwLog} -> cwLog) (\s@DataStorage' {} a -> s {cwLog = a} :: DataStorage)

instance Data.FromJSON DataStorage where
  parseJSON =
    Data.withObject
      "DataStorage"
      ( \x ->
          DataStorage' Prelude.<$> (x Data..:? "CwLog")
      )

instance Prelude.Hashable DataStorage where
  hashWithSalt _salt DataStorage' {..} =
    _salt `Prelude.hashWithSalt` cwLog

instance Prelude.NFData DataStorage where
  rnf DataStorage' {..} = Prelude.rnf cwLog
