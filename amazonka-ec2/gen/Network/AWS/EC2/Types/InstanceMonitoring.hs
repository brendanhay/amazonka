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
-- Module      : Network.AWS.EC2.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMonitoring where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Monitoring
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the monitoring of an instance.
--
-- /See:/ 'newInstanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The monitoring for the instance.
    monitoring :: Prelude.Maybe Monitoring
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceId = Prelude.Nothing,
      monitoring = Prelude.Nothing
    }

-- | The ID of the instance.
instanceMonitoring_instanceId :: Lens.Lens' InstanceMonitoring (Prelude.Maybe Prelude.Text)
instanceMonitoring_instanceId = Lens.lens (\InstanceMonitoring' {instanceId} -> instanceId) (\s@InstanceMonitoring' {} a -> s {instanceId = a} :: InstanceMonitoring)

-- | The monitoring for the instance.
instanceMonitoring_monitoring :: Lens.Lens' InstanceMonitoring (Prelude.Maybe Monitoring)
instanceMonitoring_monitoring = Lens.lens (\InstanceMonitoring' {monitoring} -> monitoring) (\s@InstanceMonitoring' {} a -> s {monitoring = a} :: InstanceMonitoring)

instance Prelude.FromXML InstanceMonitoring where
  parseXML x =
    InstanceMonitoring'
      Prelude.<$> (x Prelude..@? "instanceId")
      Prelude.<*> (x Prelude..@? "monitoring")

instance Prelude.Hashable InstanceMonitoring

instance Prelude.NFData InstanceMonitoring
