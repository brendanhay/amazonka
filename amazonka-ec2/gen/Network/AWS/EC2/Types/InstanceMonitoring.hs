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
-- Module      : Network.AWS.EC2.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMonitoring where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Monitoring
import qualified Network.AWS.Lens as Lens

-- | Describes the monitoring of an instance.
--
-- /See:/ 'newInstanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The monitoring for the instance.
    monitoring :: Core.Maybe Monitoring
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'instanceMonitoring_instanceId' - The ID of the instance.
--
-- 'monitoring', 'instanceMonitoring_monitoring' - The monitoring for the instance.
newInstanceMonitoring ::
  InstanceMonitoring
newInstanceMonitoring =
  InstanceMonitoring'
    { instanceId = Core.Nothing,
      monitoring = Core.Nothing
    }

-- | The ID of the instance.
instanceMonitoring_instanceId :: Lens.Lens' InstanceMonitoring (Core.Maybe Core.Text)
instanceMonitoring_instanceId = Lens.lens (\InstanceMonitoring' {instanceId} -> instanceId) (\s@InstanceMonitoring' {} a -> s {instanceId = a} :: InstanceMonitoring)

-- | The monitoring for the instance.
instanceMonitoring_monitoring :: Lens.Lens' InstanceMonitoring (Core.Maybe Monitoring)
instanceMonitoring_monitoring = Lens.lens (\InstanceMonitoring' {monitoring} -> monitoring) (\s@InstanceMonitoring' {} a -> s {monitoring = a} :: InstanceMonitoring)

instance Core.FromXML InstanceMonitoring where
  parseXML x =
    InstanceMonitoring'
      Core.<$> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "monitoring")

instance Core.Hashable InstanceMonitoring

instance Core.NFData InstanceMonitoring
