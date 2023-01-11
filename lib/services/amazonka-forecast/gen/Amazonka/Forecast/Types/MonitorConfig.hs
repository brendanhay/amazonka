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
-- Module      : Amazonka.Forecast.Types.MonitorConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.MonitorConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration details for the predictor monitor.
--
-- /See:/ 'newMonitorConfig' smart constructor.
data MonitorConfig = MonitorConfig'
  { -- | The name of the monitor resource.
    monitorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitorConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitorName', 'monitorConfig_monitorName' - The name of the monitor resource.
newMonitorConfig ::
  -- | 'monitorName'
  Prelude.Text ->
  MonitorConfig
newMonitorConfig pMonitorName_ =
  MonitorConfig' {monitorName = pMonitorName_}

-- | The name of the monitor resource.
monitorConfig_monitorName :: Lens.Lens' MonitorConfig Prelude.Text
monitorConfig_monitorName = Lens.lens (\MonitorConfig' {monitorName} -> monitorName) (\s@MonitorConfig' {} a -> s {monitorName = a} :: MonitorConfig)

instance Prelude.Hashable MonitorConfig where
  hashWithSalt _salt MonitorConfig' {..} =
    _salt `Prelude.hashWithSalt` monitorName

instance Prelude.NFData MonitorConfig where
  rnf MonitorConfig' {..} = Prelude.rnf monitorName

instance Data.ToJSON MonitorConfig where
  toJSON MonitorConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("MonitorName" Data..= monitorName)]
      )
