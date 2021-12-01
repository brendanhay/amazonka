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
-- Module      : Amazonka.Lightsail.Types.MonitoredResourceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.MonitoredResourceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes resource being monitored by an alarm.
--
-- An alarm is a way to monitor your Amazon Lightsail resource metrics. For
-- more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail>.
--
-- /See:/ 'newMonitoredResourceInfo' smart constructor.
data MonitoredResourceInfo = MonitoredResourceInfo'
  { -- | The Lightsail resource type of the resource being monitored.
    --
    -- Instances, load balancers, and relational databases are the only
    -- Lightsail resources that can currently be monitored by alarms.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the resource being monitored.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lightsail resource being monitored.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoredResourceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'monitoredResourceInfo_resourceType' - The Lightsail resource type of the resource being monitored.
--
-- Instances, load balancers, and relational databases are the only
-- Lightsail resources that can currently be monitored by alarms.
--
-- 'arn', 'monitoredResourceInfo_arn' - The Amazon Resource Name (ARN) of the resource being monitored.
--
-- 'name', 'monitoredResourceInfo_name' - The name of the Lightsail resource being monitored.
newMonitoredResourceInfo ::
  MonitoredResourceInfo
newMonitoredResourceInfo =
  MonitoredResourceInfo'
    { resourceType =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Lightsail resource type of the resource being monitored.
--
-- Instances, load balancers, and relational databases are the only
-- Lightsail resources that can currently be monitored by alarms.
monitoredResourceInfo_resourceType :: Lens.Lens' MonitoredResourceInfo (Prelude.Maybe ResourceType)
monitoredResourceInfo_resourceType = Lens.lens (\MonitoredResourceInfo' {resourceType} -> resourceType) (\s@MonitoredResourceInfo' {} a -> s {resourceType = a} :: MonitoredResourceInfo)

-- | The Amazon Resource Name (ARN) of the resource being monitored.
monitoredResourceInfo_arn :: Lens.Lens' MonitoredResourceInfo (Prelude.Maybe Prelude.Text)
monitoredResourceInfo_arn = Lens.lens (\MonitoredResourceInfo' {arn} -> arn) (\s@MonitoredResourceInfo' {} a -> s {arn = a} :: MonitoredResourceInfo)

-- | The name of the Lightsail resource being monitored.
monitoredResourceInfo_name :: Lens.Lens' MonitoredResourceInfo (Prelude.Maybe Prelude.Text)
monitoredResourceInfo_name = Lens.lens (\MonitoredResourceInfo' {name} -> name) (\s@MonitoredResourceInfo' {} a -> s {name = a} :: MonitoredResourceInfo)

instance Core.FromJSON MonitoredResourceInfo where
  parseJSON =
    Core.withObject
      "MonitoredResourceInfo"
      ( \x ->
          MonitoredResourceInfo'
            Prelude.<$> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "name")
      )

instance Prelude.Hashable MonitoredResourceInfo where
  hashWithSalt salt' MonitoredResourceInfo' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData MonitoredResourceInfo where
  rnf MonitoredResourceInfo' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
