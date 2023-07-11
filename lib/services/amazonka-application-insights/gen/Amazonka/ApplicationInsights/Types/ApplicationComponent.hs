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
-- Module      : Amazonka.ApplicationInsights.Types.ApplicationComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.ApplicationComponent where

import Amazonka.ApplicationInsights.Types.OsType
import Amazonka.ApplicationInsights.Types.Tier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a standalone resource or similarly grouped resources that the
-- application is made up of.
--
-- /See:/ 'newApplicationComponent' smart constructor.
data ApplicationComponent = ApplicationComponent'
  { -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | If logging is supported for the resource type, indicates whether the
    -- component has configured logs to be monitored.
    componentRemarks :: Prelude.Maybe Prelude.Text,
    -- | Workloads detected in the application component.
    detectedWorkload :: Prelude.Maybe (Prelude.HashMap Tier (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Indicates whether the application component is monitored.
    monitor :: Prelude.Maybe Prelude.Bool,
    -- | The operating system of the component.
    osType :: Prelude.Maybe OsType,
    -- | The resource type. Supported resource types include EC2 instances, Auto
    -- Scaling group, Classic ELB, Application ELB, and SQS Queue.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The stack tier of the application component.
    tier :: Prelude.Maybe Tier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentName', 'applicationComponent_componentName' - The name of the component.
--
-- 'componentRemarks', 'applicationComponent_componentRemarks' - If logging is supported for the resource type, indicates whether the
-- component has configured logs to be monitored.
--
-- 'detectedWorkload', 'applicationComponent_detectedWorkload' - Workloads detected in the application component.
--
-- 'monitor', 'applicationComponent_monitor' - Indicates whether the application component is monitored.
--
-- 'osType', 'applicationComponent_osType' - The operating system of the component.
--
-- 'resourceType', 'applicationComponent_resourceType' - The resource type. Supported resource types include EC2 instances, Auto
-- Scaling group, Classic ELB, Application ELB, and SQS Queue.
--
-- 'tier', 'applicationComponent_tier' - The stack tier of the application component.
newApplicationComponent ::
  ApplicationComponent
newApplicationComponent =
  ApplicationComponent'
    { componentName =
        Prelude.Nothing,
      componentRemarks = Prelude.Nothing,
      detectedWorkload = Prelude.Nothing,
      monitor = Prelude.Nothing,
      osType = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      tier = Prelude.Nothing
    }

-- | The name of the component.
applicationComponent_componentName :: Lens.Lens' ApplicationComponent (Prelude.Maybe Prelude.Text)
applicationComponent_componentName = Lens.lens (\ApplicationComponent' {componentName} -> componentName) (\s@ApplicationComponent' {} a -> s {componentName = a} :: ApplicationComponent)

-- | If logging is supported for the resource type, indicates whether the
-- component has configured logs to be monitored.
applicationComponent_componentRemarks :: Lens.Lens' ApplicationComponent (Prelude.Maybe Prelude.Text)
applicationComponent_componentRemarks = Lens.lens (\ApplicationComponent' {componentRemarks} -> componentRemarks) (\s@ApplicationComponent' {} a -> s {componentRemarks = a} :: ApplicationComponent)

-- | Workloads detected in the application component.
applicationComponent_detectedWorkload :: Lens.Lens' ApplicationComponent (Prelude.Maybe (Prelude.HashMap Tier (Prelude.HashMap Prelude.Text Prelude.Text)))
applicationComponent_detectedWorkload = Lens.lens (\ApplicationComponent' {detectedWorkload} -> detectedWorkload) (\s@ApplicationComponent' {} a -> s {detectedWorkload = a} :: ApplicationComponent) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the application component is monitored.
applicationComponent_monitor :: Lens.Lens' ApplicationComponent (Prelude.Maybe Prelude.Bool)
applicationComponent_monitor = Lens.lens (\ApplicationComponent' {monitor} -> monitor) (\s@ApplicationComponent' {} a -> s {monitor = a} :: ApplicationComponent)

-- | The operating system of the component.
applicationComponent_osType :: Lens.Lens' ApplicationComponent (Prelude.Maybe OsType)
applicationComponent_osType = Lens.lens (\ApplicationComponent' {osType} -> osType) (\s@ApplicationComponent' {} a -> s {osType = a} :: ApplicationComponent)

-- | The resource type. Supported resource types include EC2 instances, Auto
-- Scaling group, Classic ELB, Application ELB, and SQS Queue.
applicationComponent_resourceType :: Lens.Lens' ApplicationComponent (Prelude.Maybe Prelude.Text)
applicationComponent_resourceType = Lens.lens (\ApplicationComponent' {resourceType} -> resourceType) (\s@ApplicationComponent' {} a -> s {resourceType = a} :: ApplicationComponent)

-- | The stack tier of the application component.
applicationComponent_tier :: Lens.Lens' ApplicationComponent (Prelude.Maybe Tier)
applicationComponent_tier = Lens.lens (\ApplicationComponent' {tier} -> tier) (\s@ApplicationComponent' {} a -> s {tier = a} :: ApplicationComponent)

instance Data.FromJSON ApplicationComponent where
  parseJSON =
    Data.withObject
      "ApplicationComponent"
      ( \x ->
          ApplicationComponent'
            Prelude.<$> (x Data..:? "ComponentName")
            Prelude.<*> (x Data..:? "ComponentRemarks")
            Prelude.<*> ( x
                            Data..:? "DetectedWorkload"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Monitor")
            Prelude.<*> (x Data..:? "OsType")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "Tier")
      )

instance Prelude.Hashable ApplicationComponent where
  hashWithSalt _salt ApplicationComponent' {..} =
    _salt
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` componentRemarks
      `Prelude.hashWithSalt` detectedWorkload
      `Prelude.hashWithSalt` monitor
      `Prelude.hashWithSalt` osType
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` tier

instance Prelude.NFData ApplicationComponent where
  rnf ApplicationComponent' {..} =
    Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf componentRemarks
      `Prelude.seq` Prelude.rnf detectedWorkload
      `Prelude.seq` Prelude.rnf monitor
      `Prelude.seq` Prelude.rnf osType
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf tier
