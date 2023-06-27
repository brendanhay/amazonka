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
-- Module      : Amazonka.GreengrassV2.Types.InstalledComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.InstalledComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.InstalledComponentLifecycleState
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a component on a Greengrass core device.
--
-- /See:/ 'newInstalledComponent' smart constructor.
data InstalledComponent = InstalledComponent'
  { -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | Whether or not the component is a root component.
    isRoot :: Prelude.Maybe Prelude.Bool,
    -- | The most recent deployment source that brought the component to the
    -- Greengrass core device. For a thing group deployment or thing
    -- deployment, the source will be the The ID of the deployment. and for
    -- local deployments it will be @LOCAL@.
    --
    -- Any deployment will attempt to reinstall currently broken components on
    -- the device, which will update the last installation source.
    lastInstallationSource :: Prelude.Maybe Prelude.Text,
    -- | The last time the Greengrass core device sent a message containing a
    -- component\'s state to the Amazon Web Services Cloud.
    --
    -- A component does not need to see a state change for this field to
    -- update.
    lastReportedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The status of how current the data is.
    --
    -- This response is based off of component state changes. The status
    -- reflects component disruptions and deployments. If a component only sees
    -- a configuration update during a deployment, it might not undergo a state
    -- change and this status would not be updated.
    lastStatusChangeTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The lifecycle state of the component.
    lifecycleState :: Prelude.Maybe InstalledComponentLifecycleState,
    -- | A detailed response about the lifecycle state of the component that
    -- explains the reason why a component has an error or is broken.
    lifecycleStateDetails :: Prelude.Maybe Prelude.Text,
    -- | The status codes that indicate the reason for failure whenever the
    -- @lifecycleState@ has an error or is in a broken state.
    --
    -- Greengrass nucleus v2.8.0 or later is required to get an accurate
    -- @lifecycleStatusCodes@ response. This response can be inaccurate in
    -- earlier Greengrass nucleus versions.
    lifecycleStatusCodes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstalledComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentName', 'installedComponent_componentName' - The name of the component.
--
-- 'componentVersion', 'installedComponent_componentVersion' - The version of the component.
--
-- 'isRoot', 'installedComponent_isRoot' - Whether or not the component is a root component.
--
-- 'lastInstallationSource', 'installedComponent_lastInstallationSource' - The most recent deployment source that brought the component to the
-- Greengrass core device. For a thing group deployment or thing
-- deployment, the source will be the The ID of the deployment. and for
-- local deployments it will be @LOCAL@.
--
-- Any deployment will attempt to reinstall currently broken components on
-- the device, which will update the last installation source.
--
-- 'lastReportedTimestamp', 'installedComponent_lastReportedTimestamp' - The last time the Greengrass core device sent a message containing a
-- component\'s state to the Amazon Web Services Cloud.
--
-- A component does not need to see a state change for this field to
-- update.
--
-- 'lastStatusChangeTimestamp', 'installedComponent_lastStatusChangeTimestamp' - The status of how current the data is.
--
-- This response is based off of component state changes. The status
-- reflects component disruptions and deployments. If a component only sees
-- a configuration update during a deployment, it might not undergo a state
-- change and this status would not be updated.
--
-- 'lifecycleState', 'installedComponent_lifecycleState' - The lifecycle state of the component.
--
-- 'lifecycleStateDetails', 'installedComponent_lifecycleStateDetails' - A detailed response about the lifecycle state of the component that
-- explains the reason why a component has an error or is broken.
--
-- 'lifecycleStatusCodes', 'installedComponent_lifecycleStatusCodes' - The status codes that indicate the reason for failure whenever the
-- @lifecycleState@ has an error or is in a broken state.
--
-- Greengrass nucleus v2.8.0 or later is required to get an accurate
-- @lifecycleStatusCodes@ response. This response can be inaccurate in
-- earlier Greengrass nucleus versions.
newInstalledComponent ::
  InstalledComponent
newInstalledComponent =
  InstalledComponent'
    { componentName =
        Prelude.Nothing,
      componentVersion = Prelude.Nothing,
      isRoot = Prelude.Nothing,
      lastInstallationSource = Prelude.Nothing,
      lastReportedTimestamp = Prelude.Nothing,
      lastStatusChangeTimestamp = Prelude.Nothing,
      lifecycleState = Prelude.Nothing,
      lifecycleStateDetails = Prelude.Nothing,
      lifecycleStatusCodes = Prelude.Nothing
    }

-- | The name of the component.
installedComponent_componentName :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.Text)
installedComponent_componentName = Lens.lens (\InstalledComponent' {componentName} -> componentName) (\s@InstalledComponent' {} a -> s {componentName = a} :: InstalledComponent)

-- | The version of the component.
installedComponent_componentVersion :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.Text)
installedComponent_componentVersion = Lens.lens (\InstalledComponent' {componentVersion} -> componentVersion) (\s@InstalledComponent' {} a -> s {componentVersion = a} :: InstalledComponent)

-- | Whether or not the component is a root component.
installedComponent_isRoot :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.Bool)
installedComponent_isRoot = Lens.lens (\InstalledComponent' {isRoot} -> isRoot) (\s@InstalledComponent' {} a -> s {isRoot = a} :: InstalledComponent)

-- | The most recent deployment source that brought the component to the
-- Greengrass core device. For a thing group deployment or thing
-- deployment, the source will be the The ID of the deployment. and for
-- local deployments it will be @LOCAL@.
--
-- Any deployment will attempt to reinstall currently broken components on
-- the device, which will update the last installation source.
installedComponent_lastInstallationSource :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.Text)
installedComponent_lastInstallationSource = Lens.lens (\InstalledComponent' {lastInstallationSource} -> lastInstallationSource) (\s@InstalledComponent' {} a -> s {lastInstallationSource = a} :: InstalledComponent)

-- | The last time the Greengrass core device sent a message containing a
-- component\'s state to the Amazon Web Services Cloud.
--
-- A component does not need to see a state change for this field to
-- update.
installedComponent_lastReportedTimestamp :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.UTCTime)
installedComponent_lastReportedTimestamp = Lens.lens (\InstalledComponent' {lastReportedTimestamp} -> lastReportedTimestamp) (\s@InstalledComponent' {} a -> s {lastReportedTimestamp = a} :: InstalledComponent) Prelude.. Lens.mapping Data._Time

-- | The status of how current the data is.
--
-- This response is based off of component state changes. The status
-- reflects component disruptions and deployments. If a component only sees
-- a configuration update during a deployment, it might not undergo a state
-- change and this status would not be updated.
installedComponent_lastStatusChangeTimestamp :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.UTCTime)
installedComponent_lastStatusChangeTimestamp = Lens.lens (\InstalledComponent' {lastStatusChangeTimestamp} -> lastStatusChangeTimestamp) (\s@InstalledComponent' {} a -> s {lastStatusChangeTimestamp = a} :: InstalledComponent) Prelude.. Lens.mapping Data._Time

-- | The lifecycle state of the component.
installedComponent_lifecycleState :: Lens.Lens' InstalledComponent (Prelude.Maybe InstalledComponentLifecycleState)
installedComponent_lifecycleState = Lens.lens (\InstalledComponent' {lifecycleState} -> lifecycleState) (\s@InstalledComponent' {} a -> s {lifecycleState = a} :: InstalledComponent)

-- | A detailed response about the lifecycle state of the component that
-- explains the reason why a component has an error or is broken.
installedComponent_lifecycleStateDetails :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.Text)
installedComponent_lifecycleStateDetails = Lens.lens (\InstalledComponent' {lifecycleStateDetails} -> lifecycleStateDetails) (\s@InstalledComponent' {} a -> s {lifecycleStateDetails = a} :: InstalledComponent)

-- | The status codes that indicate the reason for failure whenever the
-- @lifecycleState@ has an error or is in a broken state.
--
-- Greengrass nucleus v2.8.0 or later is required to get an accurate
-- @lifecycleStatusCodes@ response. This response can be inaccurate in
-- earlier Greengrass nucleus versions.
installedComponent_lifecycleStatusCodes :: Lens.Lens' InstalledComponent (Prelude.Maybe [Prelude.Text])
installedComponent_lifecycleStatusCodes = Lens.lens (\InstalledComponent' {lifecycleStatusCodes} -> lifecycleStatusCodes) (\s@InstalledComponent' {} a -> s {lifecycleStatusCodes = a} :: InstalledComponent) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON InstalledComponent where
  parseJSON =
    Data.withObject
      "InstalledComponent"
      ( \x ->
          InstalledComponent'
            Prelude.<$> (x Data..:? "componentName")
            Prelude.<*> (x Data..:? "componentVersion")
            Prelude.<*> (x Data..:? "isRoot")
            Prelude.<*> (x Data..:? "lastInstallationSource")
            Prelude.<*> (x Data..:? "lastReportedTimestamp")
            Prelude.<*> (x Data..:? "lastStatusChangeTimestamp")
            Prelude.<*> (x Data..:? "lifecycleState")
            Prelude.<*> (x Data..:? "lifecycleStateDetails")
            Prelude.<*> ( x
                            Data..:? "lifecycleStatusCodes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InstalledComponent where
  hashWithSalt _salt InstalledComponent' {..} =
    _salt
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` componentVersion
      `Prelude.hashWithSalt` isRoot
      `Prelude.hashWithSalt` lastInstallationSource
      `Prelude.hashWithSalt` lastReportedTimestamp
      `Prelude.hashWithSalt` lastStatusChangeTimestamp
      `Prelude.hashWithSalt` lifecycleState
      `Prelude.hashWithSalt` lifecycleStateDetails
      `Prelude.hashWithSalt` lifecycleStatusCodes

instance Prelude.NFData InstalledComponent where
  rnf InstalledComponent' {..} =
    Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf componentVersion
      `Prelude.seq` Prelude.rnf isRoot
      `Prelude.seq` Prelude.rnf lastInstallationSource
      `Prelude.seq` Prelude.rnf lastReportedTimestamp
      `Prelude.seq` Prelude.rnf lastStatusChangeTimestamp
      `Prelude.seq` Prelude.rnf lifecycleState
      `Prelude.seq` Prelude.rnf lifecycleStateDetails
      `Prelude.seq` Prelude.rnf lifecycleStatusCodes
