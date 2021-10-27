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
-- Module      : Network.AWS.Panorama.Types.NodeInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Panorama.Types.NodeInstance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Panorama.Types.NodeInstanceStatus
import qualified Network.AWS.Prelude as Prelude

-- | A node instance.
--
-- /See:/ 'newNodeInstance' smart constructor.
data NodeInstance = NodeInstance'
  { -- | The instance\'s package name.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s package version.
    packageVersion :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s package patch version.
    packagePatchVersion :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s name.
    nodeName :: Prelude.Maybe Prelude.Text,
    -- | The node\'s ID.
    nodeId :: Prelude.Maybe Prelude.Text,
    -- | The instance\'s ID.
    nodeInstanceId :: Prelude.Text,
    -- | The instance\'s current status.
    currentStatus :: NodeInstanceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageName', 'nodeInstance_packageName' - The instance\'s package name.
--
-- 'packageVersion', 'nodeInstance_packageVersion' - The instance\'s package version.
--
-- 'packagePatchVersion', 'nodeInstance_packagePatchVersion' - The instance\'s package patch version.
--
-- 'nodeName', 'nodeInstance_nodeName' - The instance\'s name.
--
-- 'nodeId', 'nodeInstance_nodeId' - The node\'s ID.
--
-- 'nodeInstanceId', 'nodeInstance_nodeInstanceId' - The instance\'s ID.
--
-- 'currentStatus', 'nodeInstance_currentStatus' - The instance\'s current status.
newNodeInstance ::
  -- | 'nodeInstanceId'
  Prelude.Text ->
  -- | 'currentStatus'
  NodeInstanceStatus ->
  NodeInstance
newNodeInstance pNodeInstanceId_ pCurrentStatus_ =
  NodeInstance'
    { packageName = Prelude.Nothing,
      packageVersion = Prelude.Nothing,
      packagePatchVersion = Prelude.Nothing,
      nodeName = Prelude.Nothing,
      nodeId = Prelude.Nothing,
      nodeInstanceId = pNodeInstanceId_,
      currentStatus = pCurrentStatus_
    }

-- | The instance\'s package name.
nodeInstance_packageName :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_packageName = Lens.lens (\NodeInstance' {packageName} -> packageName) (\s@NodeInstance' {} a -> s {packageName = a} :: NodeInstance)

-- | The instance\'s package version.
nodeInstance_packageVersion :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_packageVersion = Lens.lens (\NodeInstance' {packageVersion} -> packageVersion) (\s@NodeInstance' {} a -> s {packageVersion = a} :: NodeInstance)

-- | The instance\'s package patch version.
nodeInstance_packagePatchVersion :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_packagePatchVersion = Lens.lens (\NodeInstance' {packagePatchVersion} -> packagePatchVersion) (\s@NodeInstance' {} a -> s {packagePatchVersion = a} :: NodeInstance)

-- | The instance\'s name.
nodeInstance_nodeName :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_nodeName = Lens.lens (\NodeInstance' {nodeName} -> nodeName) (\s@NodeInstance' {} a -> s {nodeName = a} :: NodeInstance)

-- | The node\'s ID.
nodeInstance_nodeId :: Lens.Lens' NodeInstance (Prelude.Maybe Prelude.Text)
nodeInstance_nodeId = Lens.lens (\NodeInstance' {nodeId} -> nodeId) (\s@NodeInstance' {} a -> s {nodeId = a} :: NodeInstance)

-- | The instance\'s ID.
nodeInstance_nodeInstanceId :: Lens.Lens' NodeInstance Prelude.Text
nodeInstance_nodeInstanceId = Lens.lens (\NodeInstance' {nodeInstanceId} -> nodeInstanceId) (\s@NodeInstance' {} a -> s {nodeInstanceId = a} :: NodeInstance)

-- | The instance\'s current status.
nodeInstance_currentStatus :: Lens.Lens' NodeInstance NodeInstanceStatus
nodeInstance_currentStatus = Lens.lens (\NodeInstance' {currentStatus} -> currentStatus) (\s@NodeInstance' {} a -> s {currentStatus = a} :: NodeInstance)

instance Core.FromJSON NodeInstance where
  parseJSON =
    Core.withObject
      "NodeInstance"
      ( \x ->
          NodeInstance'
            Prelude.<$> (x Core..:? "PackageName")
            Prelude.<*> (x Core..:? "PackageVersion")
            Prelude.<*> (x Core..:? "PackagePatchVersion")
            Prelude.<*> (x Core..:? "NodeName")
            Prelude.<*> (x Core..:? "NodeId")
            Prelude.<*> (x Core..: "NodeInstanceId")
            Prelude.<*> (x Core..: "CurrentStatus")
      )

instance Prelude.Hashable NodeInstance

instance Prelude.NFData NodeInstance
