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
-- Module      : Amazonka.GuardDuty.Types.FlowLogsConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.FlowLogsConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DataSourceStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the status of VPC flow logs as a data source.
--
-- /See:/ 'newFlowLogsConfigurationResult' smart constructor.
data FlowLogsConfigurationResult = FlowLogsConfigurationResult'
  { -- | Denotes whether VPC flow logs is enabled as a data source.
    status :: DataSourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlowLogsConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'flowLogsConfigurationResult_status' - Denotes whether VPC flow logs is enabled as a data source.
newFlowLogsConfigurationResult ::
  -- | 'status'
  DataSourceStatus ->
  FlowLogsConfigurationResult
newFlowLogsConfigurationResult pStatus_ =
  FlowLogsConfigurationResult' {status = pStatus_}

-- | Denotes whether VPC flow logs is enabled as a data source.
flowLogsConfigurationResult_status :: Lens.Lens' FlowLogsConfigurationResult DataSourceStatus
flowLogsConfigurationResult_status = Lens.lens (\FlowLogsConfigurationResult' {status} -> status) (\s@FlowLogsConfigurationResult' {} a -> s {status = a} :: FlowLogsConfigurationResult)

instance Data.FromJSON FlowLogsConfigurationResult where
  parseJSON =
    Data.withObject
      "FlowLogsConfigurationResult"
      ( \x ->
          FlowLogsConfigurationResult'
            Prelude.<$> (x Data..: "status")
      )

instance Prelude.Hashable FlowLogsConfigurationResult where
  hashWithSalt _salt FlowLogsConfigurationResult' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData FlowLogsConfigurationResult where
  rnf FlowLogsConfigurationResult' {..} =
    Prelude.rnf status
