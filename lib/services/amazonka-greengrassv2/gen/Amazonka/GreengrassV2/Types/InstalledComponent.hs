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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.InstalledComponent where

import qualified Amazonka.Core as Core
import Amazonka.GreengrassV2.Types.InstalledComponentLifecycleState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a component on a Greengrass core device.
--
-- /See:/ 'newInstalledComponent' smart constructor.
data InstalledComponent = InstalledComponent'
  { -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle state of the component.
    lifecycleState :: Prelude.Maybe InstalledComponentLifecycleState,
    -- | The details about the lifecycle state of the component.
    lifecycleStateDetails :: Prelude.Maybe Prelude.Text,
    -- | The status of how current the data is.
    --
    -- This response is based off of component state changes. The status
    -- reflects component disruptions and deployments. If a component only sees
    -- a configuration update during a deployment, it might not undergo a state
    -- change and this status would not be updated.
    lastStatusChangeTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Whether or not the component is a root component.
    isRoot :: Prelude.Maybe Prelude.Bool
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
-- 'componentVersion', 'installedComponent_componentVersion' - The version of the component.
--
-- 'componentName', 'installedComponent_componentName' - The name of the component.
--
-- 'lifecycleState', 'installedComponent_lifecycleState' - The lifecycle state of the component.
--
-- 'lifecycleStateDetails', 'installedComponent_lifecycleStateDetails' - The details about the lifecycle state of the component.
--
-- 'lastStatusChangeTimestamp', 'installedComponent_lastStatusChangeTimestamp' - The status of how current the data is.
--
-- This response is based off of component state changes. The status
-- reflects component disruptions and deployments. If a component only sees
-- a configuration update during a deployment, it might not undergo a state
-- change and this status would not be updated.
--
-- 'isRoot', 'installedComponent_isRoot' - Whether or not the component is a root component.
newInstalledComponent ::
  InstalledComponent
newInstalledComponent =
  InstalledComponent'
    { componentVersion =
        Prelude.Nothing,
      componentName = Prelude.Nothing,
      lifecycleState = Prelude.Nothing,
      lifecycleStateDetails = Prelude.Nothing,
      lastStatusChangeTimestamp = Prelude.Nothing,
      isRoot = Prelude.Nothing
    }

-- | The version of the component.
installedComponent_componentVersion :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.Text)
installedComponent_componentVersion = Lens.lens (\InstalledComponent' {componentVersion} -> componentVersion) (\s@InstalledComponent' {} a -> s {componentVersion = a} :: InstalledComponent)

-- | The name of the component.
installedComponent_componentName :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.Text)
installedComponent_componentName = Lens.lens (\InstalledComponent' {componentName} -> componentName) (\s@InstalledComponent' {} a -> s {componentName = a} :: InstalledComponent)

-- | The lifecycle state of the component.
installedComponent_lifecycleState :: Lens.Lens' InstalledComponent (Prelude.Maybe InstalledComponentLifecycleState)
installedComponent_lifecycleState = Lens.lens (\InstalledComponent' {lifecycleState} -> lifecycleState) (\s@InstalledComponent' {} a -> s {lifecycleState = a} :: InstalledComponent)

-- | The details about the lifecycle state of the component.
installedComponent_lifecycleStateDetails :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.Text)
installedComponent_lifecycleStateDetails = Lens.lens (\InstalledComponent' {lifecycleStateDetails} -> lifecycleStateDetails) (\s@InstalledComponent' {} a -> s {lifecycleStateDetails = a} :: InstalledComponent)

-- | The status of how current the data is.
--
-- This response is based off of component state changes. The status
-- reflects component disruptions and deployments. If a component only sees
-- a configuration update during a deployment, it might not undergo a state
-- change and this status would not be updated.
installedComponent_lastStatusChangeTimestamp :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.UTCTime)
installedComponent_lastStatusChangeTimestamp = Lens.lens (\InstalledComponent' {lastStatusChangeTimestamp} -> lastStatusChangeTimestamp) (\s@InstalledComponent' {} a -> s {lastStatusChangeTimestamp = a} :: InstalledComponent) Prelude.. Lens.mapping Core._Time

-- | Whether or not the component is a root component.
installedComponent_isRoot :: Lens.Lens' InstalledComponent (Prelude.Maybe Prelude.Bool)
installedComponent_isRoot = Lens.lens (\InstalledComponent' {isRoot} -> isRoot) (\s@InstalledComponent' {} a -> s {isRoot = a} :: InstalledComponent)

instance Core.FromJSON InstalledComponent where
  parseJSON =
    Core.withObject
      "InstalledComponent"
      ( \x ->
          InstalledComponent'
            Prelude.<$> (x Core..:? "componentVersion")
            Prelude.<*> (x Core..:? "componentName")
            Prelude.<*> (x Core..:? "lifecycleState")
            Prelude.<*> (x Core..:? "lifecycleStateDetails")
            Prelude.<*> (x Core..:? "lastStatusChangeTimestamp")
            Prelude.<*> (x Core..:? "isRoot")
      )

instance Prelude.Hashable InstalledComponent where
  hashWithSalt _salt InstalledComponent' {..} =
    _salt `Prelude.hashWithSalt` componentVersion
      `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` lifecycleState
      `Prelude.hashWithSalt` lifecycleStateDetails
      `Prelude.hashWithSalt` lastStatusChangeTimestamp
      `Prelude.hashWithSalt` isRoot

instance Prelude.NFData InstalledComponent where
  rnf InstalledComponent' {..} =
    Prelude.rnf componentVersion
      `Prelude.seq` Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf lifecycleState
      `Prelude.seq` Prelude.rnf lifecycleStateDetails
      `Prelude.seq` Prelude.rnf lastStatusChangeTimestamp
      `Prelude.seq` Prelude.rnf isRoot
