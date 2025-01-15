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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.MonitoredResourceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The Amazon Resource Name (ARN) of the resource being monitored.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lightsail resource being monitored.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type of the resource being monitored.
    --
    -- Instances, load balancers, and relational databases are the only
    -- Lightsail resources that can currently be monitored by alarms.
    resourceType :: Prelude.Maybe ResourceType
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
-- 'arn', 'monitoredResourceInfo_arn' - The Amazon Resource Name (ARN) of the resource being monitored.
--
-- 'name', 'monitoredResourceInfo_name' - The name of the Lightsail resource being monitored.
--
-- 'resourceType', 'monitoredResourceInfo_resourceType' - The Lightsail resource type of the resource being monitored.
--
-- Instances, load balancers, and relational databases are the only
-- Lightsail resources that can currently be monitored by alarms.
newMonitoredResourceInfo ::
  MonitoredResourceInfo
newMonitoredResourceInfo =
  MonitoredResourceInfo'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource being monitored.
monitoredResourceInfo_arn :: Lens.Lens' MonitoredResourceInfo (Prelude.Maybe Prelude.Text)
monitoredResourceInfo_arn = Lens.lens (\MonitoredResourceInfo' {arn} -> arn) (\s@MonitoredResourceInfo' {} a -> s {arn = a} :: MonitoredResourceInfo)

-- | The name of the Lightsail resource being monitored.
monitoredResourceInfo_name :: Lens.Lens' MonitoredResourceInfo (Prelude.Maybe Prelude.Text)
monitoredResourceInfo_name = Lens.lens (\MonitoredResourceInfo' {name} -> name) (\s@MonitoredResourceInfo' {} a -> s {name = a} :: MonitoredResourceInfo)

-- | The Lightsail resource type of the resource being monitored.
--
-- Instances, load balancers, and relational databases are the only
-- Lightsail resources that can currently be monitored by alarms.
monitoredResourceInfo_resourceType :: Lens.Lens' MonitoredResourceInfo (Prelude.Maybe ResourceType)
monitoredResourceInfo_resourceType = Lens.lens (\MonitoredResourceInfo' {resourceType} -> resourceType) (\s@MonitoredResourceInfo' {} a -> s {resourceType = a} :: MonitoredResourceInfo)

instance Data.FromJSON MonitoredResourceInfo where
  parseJSON =
    Data.withObject
      "MonitoredResourceInfo"
      ( \x ->
          MonitoredResourceInfo'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resourceType")
      )

instance Prelude.Hashable MonitoredResourceInfo where
  hashWithSalt _salt MonitoredResourceInfo' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData MonitoredResourceInfo where
  rnf MonitoredResourceInfo' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf resourceType
