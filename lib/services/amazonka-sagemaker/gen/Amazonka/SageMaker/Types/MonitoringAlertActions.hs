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
-- Module      : Amazonka.SageMaker.Types.MonitoringAlertActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringAlertActions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelDashboardIndicatorAction

-- | A list of alert actions taken in response to an alert going into
-- @InAlert@ status.
--
-- /See:/ 'newMonitoringAlertActions' smart constructor.
data MonitoringAlertActions = MonitoringAlertActions'
  { -- | An alert action taken to light up an icon on the Model Dashboard when an
    -- alert goes into @InAlert@ status.
    modelDashboardIndicator :: Prelude.Maybe ModelDashboardIndicatorAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringAlertActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelDashboardIndicator', 'monitoringAlertActions_modelDashboardIndicator' - An alert action taken to light up an icon on the Model Dashboard when an
-- alert goes into @InAlert@ status.
newMonitoringAlertActions ::
  MonitoringAlertActions
newMonitoringAlertActions =
  MonitoringAlertActions'
    { modelDashboardIndicator =
        Prelude.Nothing
    }

-- | An alert action taken to light up an icon on the Model Dashboard when an
-- alert goes into @InAlert@ status.
monitoringAlertActions_modelDashboardIndicator :: Lens.Lens' MonitoringAlertActions (Prelude.Maybe ModelDashboardIndicatorAction)
monitoringAlertActions_modelDashboardIndicator = Lens.lens (\MonitoringAlertActions' {modelDashboardIndicator} -> modelDashboardIndicator) (\s@MonitoringAlertActions' {} a -> s {modelDashboardIndicator = a} :: MonitoringAlertActions)

instance Data.FromJSON MonitoringAlertActions where
  parseJSON =
    Data.withObject
      "MonitoringAlertActions"
      ( \x ->
          MonitoringAlertActions'
            Prelude.<$> (x Data..:? "ModelDashboardIndicator")
      )

instance Prelude.Hashable MonitoringAlertActions where
  hashWithSalt _salt MonitoringAlertActions' {..} =
    _salt
      `Prelude.hashWithSalt` modelDashboardIndicator

instance Prelude.NFData MonitoringAlertActions where
  rnf MonitoringAlertActions' {..} =
    Prelude.rnf modelDashboardIndicator
