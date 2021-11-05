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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.ApplicationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of the application.
--
-- /See:/ 'newApplicationInfo' smart constructor.
data ApplicationInfo = ApplicationInfo'
  { -- | The name of the resource group used for the application.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Application Insights can listen to CloudWatch events
    -- for the application resources, such as @instance terminated@,
    -- @failed deployment@, and others.
    cWEMonitorEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The SNS topic provided to Application Insights that is associated to the
    -- created opsItems to receive SNS notifications for opsItem updates.
    opsItemSNSTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle of the application.
    lifeCycle :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Application Insights will create opsItems for any
    -- problem detected by Application Insights for an application.
    opsCenterEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The issues on the user side that block Application Insights from
    -- successfully monitoring an application. Example remarks include:
    --
    -- -   “Configuring application, detected 1 Errors, 3 Warnings”
    --
    -- -   “Configuring application, detected 1 Unconfigured Components”
    remarks :: Prelude.Maybe Prelude.Text
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
-- 'resourceGroupName', 'applicationInfo_resourceGroupName' - The name of the resource group used for the application.
--
-- 'cWEMonitorEnabled', 'applicationInfo_cWEMonitorEnabled' - Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
--
-- 'opsItemSNSTopicArn', 'applicationInfo_opsItemSNSTopicArn' - The SNS topic provided to Application Insights that is associated to the
-- created opsItems to receive SNS notifications for opsItem updates.
--
-- 'lifeCycle', 'applicationInfo_lifeCycle' - The lifecycle of the application.
--
-- 'opsCenterEnabled', 'applicationInfo_opsCenterEnabled' - Indicates whether Application Insights will create opsItems for any
-- problem detected by Application Insights for an application.
--
-- 'remarks', 'applicationInfo_remarks' - The issues on the user side that block Application Insights from
-- successfully monitoring an application. Example remarks include:
--
-- -   “Configuring application, detected 1 Errors, 3 Warnings”
--
-- -   “Configuring application, detected 1 Unconfigured Components”
newApplicationInfo ::
  ApplicationInfo
newApplicationInfo =
  ApplicationInfo'
    { resourceGroupName =
        Prelude.Nothing,
      cWEMonitorEnabled = Prelude.Nothing,
      opsItemSNSTopicArn = Prelude.Nothing,
      lifeCycle = Prelude.Nothing,
      opsCenterEnabled = Prelude.Nothing,
      remarks = Prelude.Nothing
    }

-- | The name of the resource group used for the application.
applicationInfo_resourceGroupName :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_resourceGroupName = Lens.lens (\ApplicationInfo' {resourceGroupName} -> resourceGroupName) (\s@ApplicationInfo' {} a -> s {resourceGroupName = a} :: ApplicationInfo)

-- | Indicates whether Application Insights can listen to CloudWatch events
-- for the application resources, such as @instance terminated@,
-- @failed deployment@, and others.
applicationInfo_cWEMonitorEnabled :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Bool)
applicationInfo_cWEMonitorEnabled = Lens.lens (\ApplicationInfo' {cWEMonitorEnabled} -> cWEMonitorEnabled) (\s@ApplicationInfo' {} a -> s {cWEMonitorEnabled = a} :: ApplicationInfo)

-- | The SNS topic provided to Application Insights that is associated to the
-- created opsItems to receive SNS notifications for opsItem updates.
applicationInfo_opsItemSNSTopicArn :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_opsItemSNSTopicArn = Lens.lens (\ApplicationInfo' {opsItemSNSTopicArn} -> opsItemSNSTopicArn) (\s@ApplicationInfo' {} a -> s {opsItemSNSTopicArn = a} :: ApplicationInfo)

-- | The lifecycle of the application.
applicationInfo_lifeCycle :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_lifeCycle = Lens.lens (\ApplicationInfo' {lifeCycle} -> lifeCycle) (\s@ApplicationInfo' {} a -> s {lifeCycle = a} :: ApplicationInfo)

-- | Indicates whether Application Insights will create opsItems for any
-- problem detected by Application Insights for an application.
applicationInfo_opsCenterEnabled :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Bool)
applicationInfo_opsCenterEnabled = Lens.lens (\ApplicationInfo' {opsCenterEnabled} -> opsCenterEnabled) (\s@ApplicationInfo' {} a -> s {opsCenterEnabled = a} :: ApplicationInfo)

-- | The issues on the user side that block Application Insights from
-- successfully monitoring an application. Example remarks include:
--
-- -   “Configuring application, detected 1 Errors, 3 Warnings”
--
-- -   “Configuring application, detected 1 Unconfigured Components”
applicationInfo_remarks :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_remarks = Lens.lens (\ApplicationInfo' {remarks} -> remarks) (\s@ApplicationInfo' {} a -> s {remarks = a} :: ApplicationInfo)

instance Core.FromJSON ApplicationInfo where
  parseJSON =
    Core.withObject
      "ApplicationInfo"
      ( \x ->
          ApplicationInfo'
            Prelude.<$> (x Core..:? "ResourceGroupName")
            Prelude.<*> (x Core..:? "CWEMonitorEnabled")
            Prelude.<*> (x Core..:? "OpsItemSNSTopicArn")
            Prelude.<*> (x Core..:? "LifeCycle")
            Prelude.<*> (x Core..:? "OpsCenterEnabled")
            Prelude.<*> (x Core..:? "Remarks")
      )

instance Prelude.Hashable ApplicationInfo

instance Prelude.NFData ApplicationInfo
