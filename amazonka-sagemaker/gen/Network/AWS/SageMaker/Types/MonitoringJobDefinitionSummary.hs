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
-- Module      : Network.AWS.SageMaker.Types.MonitoringJobDefinitionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringJobDefinitionSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about a monitoring job.
--
-- /See:/ 'newMonitoringJobDefinitionSummary' smart constructor.
data MonitoringJobDefinitionSummary = MonitoringJobDefinitionSummary'
  { -- | The name of the monitoring job.
    monitoringJobDefinitionName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the monitoring job.
    monitoringJobDefinitionArn :: Prelude.Text,
    -- | The time that the monitoring job was created.
    creationTime :: Prelude.POSIX,
    -- | The name of the endpoint that the job monitors.
    endpointName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MonitoringJobDefinitionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringJobDefinitionName', 'monitoringJobDefinitionSummary_monitoringJobDefinitionName' - The name of the monitoring job.
--
-- 'monitoringJobDefinitionArn', 'monitoringJobDefinitionSummary_monitoringJobDefinitionArn' - The Amazon Resource Name (ARN) of the monitoring job.
--
-- 'creationTime', 'monitoringJobDefinitionSummary_creationTime' - The time that the monitoring job was created.
--
-- 'endpointName', 'monitoringJobDefinitionSummary_endpointName' - The name of the endpoint that the job monitors.
newMonitoringJobDefinitionSummary ::
  -- | 'monitoringJobDefinitionName'
  Prelude.Text ->
  -- | 'monitoringJobDefinitionArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'endpointName'
  Prelude.Text ->
  MonitoringJobDefinitionSummary
newMonitoringJobDefinitionSummary
  pMonitoringJobDefinitionName_
  pMonitoringJobDefinitionArn_
  pCreationTime_
  pEndpointName_ =
    MonitoringJobDefinitionSummary'
      { monitoringJobDefinitionName =
          pMonitoringJobDefinitionName_,
        monitoringJobDefinitionArn =
          pMonitoringJobDefinitionArn_,
        creationTime =
          Prelude._Time Lens.# pCreationTime_,
        endpointName = pEndpointName_
      }

-- | The name of the monitoring job.
monitoringJobDefinitionSummary_monitoringJobDefinitionName :: Lens.Lens' MonitoringJobDefinitionSummary Prelude.Text
monitoringJobDefinitionSummary_monitoringJobDefinitionName = Lens.lens (\MonitoringJobDefinitionSummary' {monitoringJobDefinitionName} -> monitoringJobDefinitionName) (\s@MonitoringJobDefinitionSummary' {} a -> s {monitoringJobDefinitionName = a} :: MonitoringJobDefinitionSummary)

-- | The Amazon Resource Name (ARN) of the monitoring job.
monitoringJobDefinitionSummary_monitoringJobDefinitionArn :: Lens.Lens' MonitoringJobDefinitionSummary Prelude.Text
monitoringJobDefinitionSummary_monitoringJobDefinitionArn = Lens.lens (\MonitoringJobDefinitionSummary' {monitoringJobDefinitionArn} -> monitoringJobDefinitionArn) (\s@MonitoringJobDefinitionSummary' {} a -> s {monitoringJobDefinitionArn = a} :: MonitoringJobDefinitionSummary)

-- | The time that the monitoring job was created.
monitoringJobDefinitionSummary_creationTime :: Lens.Lens' MonitoringJobDefinitionSummary Prelude.UTCTime
monitoringJobDefinitionSummary_creationTime = Lens.lens (\MonitoringJobDefinitionSummary' {creationTime} -> creationTime) (\s@MonitoringJobDefinitionSummary' {} a -> s {creationTime = a} :: MonitoringJobDefinitionSummary) Prelude.. Prelude._Time

-- | The name of the endpoint that the job monitors.
monitoringJobDefinitionSummary_endpointName :: Lens.Lens' MonitoringJobDefinitionSummary Prelude.Text
monitoringJobDefinitionSummary_endpointName = Lens.lens (\MonitoringJobDefinitionSummary' {endpointName} -> endpointName) (\s@MonitoringJobDefinitionSummary' {} a -> s {endpointName = a} :: MonitoringJobDefinitionSummary)

instance
  Prelude.FromJSON
    MonitoringJobDefinitionSummary
  where
  parseJSON =
    Prelude.withObject
      "MonitoringJobDefinitionSummary"
      ( \x ->
          MonitoringJobDefinitionSummary'
            Prelude.<$> (x Prelude..: "MonitoringJobDefinitionName")
            Prelude.<*> (x Prelude..: "MonitoringJobDefinitionArn")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "EndpointName")
      )

instance
  Prelude.Hashable
    MonitoringJobDefinitionSummary

instance
  Prelude.NFData
    MonitoringJobDefinitionSummary
