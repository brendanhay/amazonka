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
-- Module      : Amazonka.ApplicationInsights.Types.ApplicationInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.ApplicationInfo where

import Amazonka.ApplicationInsights.Types.DiscoveryType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of the application.
--
-- /See:/ 'newApplicationInfo' smart constructor.
data ApplicationInfo = ApplicationInfo'
  { -- | The lifecycle of the application.
    lifeCycle :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether auto-configuration is turned on for this application.
    autoConfigEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The method used by Application Insights to onboard your resources.
    discoveryType :: Prelude.Maybe DiscoveryType,
    -- | The SNS topic provided to Application Insights that is associated to the
    -- created opsItems to receive SNS notifications for opsItem updates.
    opsItemSNSTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Application Insights can listen to CloudWatch events
    -- for the application resources, such as @instance terminated@,
    -- @failed deployment@, and others.
    cWEMonitorEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the resource group used for the application.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | The issues on the user side that block Application Insights from
    -- successfully monitoring an application. Example remarks include:
    --
    -- -   “Configuring application, detected 1 Errors, 3 Warnings”
    --
    -- -   “Configuring application, detected 1 Unconfigured Components”
    remarks :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Application Insights will create opsItems for any
    -- problem detected by Application Insights for an application.
    opsCenterEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifeCycle', 'applicationInfo_lifeCycle' - The lifecycle of the application.
--
-- 'autoConfigEnabled', 'applicationInfo_autoConfigEnabled' - Indicates whether auto-configuration is turned on for this application.
--
-- 'discoveryType', 'applicationInfo_discoveryType' - The method used by Application Insights to onboard your resources.
--
-- 'opsItemSNSTopicArn', 'applicationInfo_opsItemSNSTopicArn' - The SNS topic provided to Application Insights that is associated to the
-- created opsItems to receive SNS notifications for opsItem updates.
--
-- 'cWEMonitorEnabled', 'applicationInfo_cWEMonitorEnabled' - Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
--
-- 'resourceGroupName', 'applicationInfo_resourceGroupName' - The name of the resource group used for the application.
--
-- 'remarks', 'applicationInfo_remarks' - The issues on the user side that block Application Insights from
-- successfully monitoring an application. Example remarks include:
--
-- -   “Configuring application, detected 1 Errors, 3 Warnings”
--
-- -   “Configuring application, detected 1 Unconfigured Components”
--
-- 'opsCenterEnabled', 'applicationInfo_opsCenterEnabled' - Indicates whether Application Insights will create opsItems for any
-- problem detected by Application Insights for an application.
newApplicationInfo ::
  ApplicationInfo
newApplicationInfo =
  ApplicationInfo'
    { lifeCycle = Prelude.Nothing,
      autoConfigEnabled = Prelude.Nothing,
      discoveryType = Prelude.Nothing,
      opsItemSNSTopicArn = Prelude.Nothing,
      cWEMonitorEnabled = Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      remarks = Prelude.Nothing,
      opsCenterEnabled = Prelude.Nothing
    }

-- | The lifecycle of the application.
applicationInfo_lifeCycle :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_lifeCycle = Lens.lens (\ApplicationInfo' {lifeCycle} -> lifeCycle) (\s@ApplicationInfo' {} a -> s {lifeCycle = a} :: ApplicationInfo)

-- | Indicates whether auto-configuration is turned on for this application.
applicationInfo_autoConfigEnabled :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Bool)
applicationInfo_autoConfigEnabled = Lens.lens (\ApplicationInfo' {autoConfigEnabled} -> autoConfigEnabled) (\s@ApplicationInfo' {} a -> s {autoConfigEnabled = a} :: ApplicationInfo)

-- | The method used by Application Insights to onboard your resources.
applicationInfo_discoveryType :: Lens.Lens' ApplicationInfo (Prelude.Maybe DiscoveryType)
applicationInfo_discoveryType = Lens.lens (\ApplicationInfo' {discoveryType} -> discoveryType) (\s@ApplicationInfo' {} a -> s {discoveryType = a} :: ApplicationInfo)

-- | The SNS topic provided to Application Insights that is associated to the
-- created opsItems to receive SNS notifications for opsItem updates.
applicationInfo_opsItemSNSTopicArn :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_opsItemSNSTopicArn = Lens.lens (\ApplicationInfo' {opsItemSNSTopicArn} -> opsItemSNSTopicArn) (\s@ApplicationInfo' {} a -> s {opsItemSNSTopicArn = a} :: ApplicationInfo)

-- | Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
applicationInfo_cWEMonitorEnabled :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Bool)
applicationInfo_cWEMonitorEnabled = Lens.lens (\ApplicationInfo' {cWEMonitorEnabled} -> cWEMonitorEnabled) (\s@ApplicationInfo' {} a -> s {cWEMonitorEnabled = a} :: ApplicationInfo)

-- | The name of the resource group used for the application.
applicationInfo_resourceGroupName :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_resourceGroupName = Lens.lens (\ApplicationInfo' {resourceGroupName} -> resourceGroupName) (\s@ApplicationInfo' {} a -> s {resourceGroupName = a} :: ApplicationInfo)

-- | The issues on the user side that block Application Insights from
-- successfully monitoring an application. Example remarks include:
--
-- -   “Configuring application, detected 1 Errors, 3 Warnings”
--
-- -   “Configuring application, detected 1 Unconfigured Components”
applicationInfo_remarks :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_remarks = Lens.lens (\ApplicationInfo' {remarks} -> remarks) (\s@ApplicationInfo' {} a -> s {remarks = a} :: ApplicationInfo)

-- | Indicates whether Application Insights will create opsItems for any
-- problem detected by Application Insights for an application.
applicationInfo_opsCenterEnabled :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Bool)
applicationInfo_opsCenterEnabled = Lens.lens (\ApplicationInfo' {opsCenterEnabled} -> opsCenterEnabled) (\s@ApplicationInfo' {} a -> s {opsCenterEnabled = a} :: ApplicationInfo)

instance Data.FromJSON ApplicationInfo where
  parseJSON =
    Data.withObject
      "ApplicationInfo"
      ( \x ->
          ApplicationInfo'
            Prelude.<$> (x Data..:? "LifeCycle")
            Prelude.<*> (x Data..:? "AutoConfigEnabled")
            Prelude.<*> (x Data..:? "DiscoveryType")
            Prelude.<*> (x Data..:? "OpsItemSNSTopicArn")
            Prelude.<*> (x Data..:? "CWEMonitorEnabled")
            Prelude.<*> (x Data..:? "ResourceGroupName")
            Prelude.<*> (x Data..:? "Remarks")
            Prelude.<*> (x Data..:? "OpsCenterEnabled")
      )

instance Prelude.Hashable ApplicationInfo where
  hashWithSalt _salt ApplicationInfo' {..} =
    _salt `Prelude.hashWithSalt` lifeCycle
      `Prelude.hashWithSalt` autoConfigEnabled
      `Prelude.hashWithSalt` discoveryType
      `Prelude.hashWithSalt` opsItemSNSTopicArn
      `Prelude.hashWithSalt` cWEMonitorEnabled
      `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` remarks
      `Prelude.hashWithSalt` opsCenterEnabled

instance Prelude.NFData ApplicationInfo where
  rnf ApplicationInfo' {..} =
    Prelude.rnf lifeCycle
      `Prelude.seq` Prelude.rnf autoConfigEnabled
      `Prelude.seq` Prelude.rnf discoveryType
      `Prelude.seq` Prelude.rnf opsItemSNSTopicArn
      `Prelude.seq` Prelude.rnf cWEMonitorEnabled
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf remarks
      `Prelude.seq` Prelude.rnf opsCenterEnabled
