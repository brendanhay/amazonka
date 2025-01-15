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
-- Module      : Amazonka.Forecast.Types.MonitorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.MonitorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the monitor resource.
--
-- /See:/ 'newMonitorInfo' smart constructor.
data MonitorInfo = MonitorInfo'
  { -- | The Amazon Resource Name (ARN) of the monitor resource.
    monitorArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the monitor. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @ACTIVE_STOPPING@, @ACTIVE_STOPPED@
    --
    -- -   @UPDATE_IN_PROGRESS@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitorArn', 'monitorInfo_monitorArn' - The Amazon Resource Name (ARN) of the monitor resource.
--
-- 'status', 'monitorInfo_status' - The status of the monitor. States include:
--
-- -   @ACTIVE@
--
-- -   @ACTIVE_STOPPING@, @ACTIVE_STOPPED@
--
-- -   @UPDATE_IN_PROGRESS@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
newMonitorInfo ::
  MonitorInfo
newMonitorInfo =
  MonitorInfo'
    { monitorArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the monitor resource.
monitorInfo_monitorArn :: Lens.Lens' MonitorInfo (Prelude.Maybe Prelude.Text)
monitorInfo_monitorArn = Lens.lens (\MonitorInfo' {monitorArn} -> monitorArn) (\s@MonitorInfo' {} a -> s {monitorArn = a} :: MonitorInfo)

-- | The status of the monitor. States include:
--
-- -   @ACTIVE@
--
-- -   @ACTIVE_STOPPING@, @ACTIVE_STOPPED@
--
-- -   @UPDATE_IN_PROGRESS@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
monitorInfo_status :: Lens.Lens' MonitorInfo (Prelude.Maybe Prelude.Text)
monitorInfo_status = Lens.lens (\MonitorInfo' {status} -> status) (\s@MonitorInfo' {} a -> s {status = a} :: MonitorInfo)

instance Data.FromJSON MonitorInfo where
  parseJSON =
    Data.withObject
      "MonitorInfo"
      ( \x ->
          MonitorInfo'
            Prelude.<$> (x Data..:? "MonitorArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable MonitorInfo where
  hashWithSalt _salt MonitorInfo' {..} =
    _salt
      `Prelude.hashWithSalt` monitorArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData MonitorInfo where
  rnf MonitorInfo' {..} =
    Prelude.rnf monitorArn `Prelude.seq`
      Prelude.rnf status
