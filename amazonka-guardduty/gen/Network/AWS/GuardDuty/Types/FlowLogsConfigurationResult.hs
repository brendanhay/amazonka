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
-- Module      : Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information on the status of VPC flow logs as a data source.
--
-- /See:/ 'newFlowLogsConfigurationResult' smart constructor.
data FlowLogsConfigurationResult = FlowLogsConfigurationResult'
  { -- | Denotes whether VPC flow logs is enabled as a data source.
    status :: DataSourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON FlowLogsConfigurationResult where
  parseJSON =
    Prelude.withObject
      "FlowLogsConfigurationResult"
      ( \x ->
          FlowLogsConfigurationResult'
            Prelude.<$> (x Prelude..: "status")
      )

instance Prelude.Hashable FlowLogsConfigurationResult

instance Prelude.NFData FlowLogsConfigurationResult
